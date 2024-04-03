

# Using Generalized Estimating Equations (GEEs) to quantify 
# Narwhal presence in Eclipse Sound

# site EE/PI

# A) Analysis limited to 2016-2021

# ______________________________________________________________________________

# load packages & functions
library(car)
library (splines) #required for bs()
library(splines2) #required for mSpline()
library (geepack)
library(corrplot)
library(RColorBrewer)
library(geeasy)
library(dplyr)
library(tidyr)
library(lubridate)
source('C:/Users/HARP/Documents/GitHub/MmBRS/stats/new/myviolin.R')
source('C:/Users/HARP/Documents/GitHub/MmBRS/stats/new/predgridmaker.R')

options(scipen=999)
#_______________________________________________________________________________
#          A) Analysis limited to 2009 - 2014 for comparison with site N and H
#_______________________________________________________________________________


## Load Data
setwd("G:/Shared drives/SWAL_Arctic/Research_projects/JackBRS/Arctic_shiptxClicks/output/stage_two/Combine_all/publication/20km_mask/hour")
mmdata <-  read.csv("hourbinned_40km_UTC.csv")

mmdata = mmdata[-1,]

# add in the solar elevation
solar = read.csv("I:/Projects/BRS_thesis/JackBRS/Arctic_shiptxClicks/code/JPE/Solar/NOAA_Solar_Calculations_day.csv")

mmdata$sun_ele = solar$elevation_atm_accounted
rm(solar)

# ------------------------------------------------------------------------------
# Check correlation and collinearity of variables
# ------------------------------------------------------------------------------
# ACF:
    # graphics.off()
    # {plot.new()
    #   dev.new(width = 4,height = 4,noRStudioGD = TRUE) 
    #   pres = zcdata$zcPres
    #   pres[zcdata$zc == 0] = NaN
    #   acf(zcdata$zcPres,na.action = na.pass)
    # }
    # 
    # graphics.off()
    # {plot.new()
    #   dev.new(width = 4,height = 3,noRStudioGD = TRUE) 
    #   pres = zcdata$zcPres
    #   pres[zcdata$zc == 0] = NaN
    #   acf(zcdata$zcPres,na.action = na.pass,ylim=c(-0.01 ,0.01))
    # }

# Correlations: Pearson correlation (r>0.6)
plotCols = c(4,6,7,10) # index of variables to test correlations
covarList = names(mmdata[c(plotCols)])
data = na.omit(mmdata)
corData = cor(data[covarList])
print(round(corData,2))
Table1 = rbind(data.frame(round(corData,2)))
graphics.off()
{plot.new()
  dev.new(width = 5,height = 5,noRStudioGD = TRUE)  
  corrplot(corData,method = 'color',type = "upper",diag = FALSE,tl.col ="black",addCoef.col = "black",
           tl.srt=45,order="hclust",hclust.method = "ward.D",col=brewer.pal(n=5, name="PuOr"))
}
rm(data)
# -> Correlated variables: sProp/maxRLpp, sProp/cumSEL, maxRLpp/cumSEL

# ------------------------------------------------------------------------------
# Define cyclic splines and select if smooth or linear for sonar-related variables based on QIC
# ------------------------------------------------------------------------------
mmdata$Time <- dmy_hms(mmdata$Time)


# define knots for cyclic splines with 4 degrees of freedom
msJd = mSpline(mmdata$jd,df = 5, Boundary.knots = c(1,365),periodic = T)
kjd = knots(msJd) 
lJd = c(1,365)

# now make mmpres binomial
mmdata$MmPres = as.numeric(mmdata$MmPres > 0)


# We test what is best, smooth or linear
s = geeglm(MmPres ~ Ice_pc,data=mmdata,family=binomial,id=ID,waves=wave,scale.fix=T)
l = geeglm(MmPres ~ bs(Ice_pc), data=mmdata,family=binomial,id=ID,waves=wave,scale.fix=T)
geepack::QIC(s)[1]

#-------------------------------------------------------------------------------
# VIF (VIF> 3 collinearity):
mmGLM<-glm(MmPres ~ 
             # sun_ele+
             bs(Ice_pc)+
             mSpline(jd,knots=kjd,Boundary.knots=lJd,periodic=T),
             # as.factor(year),
           data=mmdata,family=binomial)
vif1 <- 5000
threshVIF = 5
selectedCovariates = attributes(terms (mmGLM))$term.labels
while (vif1>threshVIF){
  
  for (x in 1: length (selectedCovariates)){
    if (x==1){newformula <- selectedCovariates[1]}else{newformula <- paste (newformula, selectedCovariates[x], sep="+")}
  }
  
  newformula = paste ("MmPres~", newformula,sep="")
  modeltemp = glm (as.formula (newformula), data=mmdata,family=binomial) 
  
  viftemp = vif(modeltemp)
  print (data.frame(viftemp),digits=2)
  
  vif2 = match (max(viftemp[,1]), viftemp[,1])
  vif1 = viftemp[vif2, 1]
  if (vif1>threshVIF){   selectedCovariates <- selectedCovariates[-vif2] }
}

# bs(Ice_pc)                                                    1.3  3             1.1
# mSpline(jd, knots = kjd, Boundary.knots = lJd, periodic = T)  1.3  5             1.0

acf_res  = acf(residuals(mmGLM),300)

acf_values <- acf_res$acf[-1]  # Exclude lag 0
N <- length(residuals(mmGLM))
conf_interval_upper <- 1.96 / sqrt(N)
conf_interval_lower <- -1.96 / sqrt(N)
first_lag_within_bounds <- which(acf_values <= conf_interval_upper & acf_values >= conf_interval_lower)[1]
cat("First lag within bounds:", first_lag_within_bounds, "\n")
cat("Autocorrelation value at this lag:", acf_values[first_lag_within_bounds], "\n")

### this creates your wave & ID. the wave & ID as needed based on the results above

# Assuming df is your original dataframe with a datetime column 'Time'
mmdata$Time <- as.POSIXct(mmdata$Time, format = "%Y-%m-%d %H:%M:%S")
# Create a full sequence of hours
full_hours <- seq(min(mmdata$Time), max(mmdata$Time), by = "hour")
# Create a new dataframe from the sequence
full_df <- data.frame(Time = full_hours)

# Merge the full minute range with the original data (this will fill missing minutes with NA for other columns)
# Ensure 'df' is not missing any columns for the merge to work correctly
full_df <- left_join(full_df, mmdata, by = "Time")

# Generate the wave and ID variables based on minutes
mmdata <- full_df %>% 
  mutate(
    hournumber = as.numeric(difftime(Time, min(Time), units = "hours")) + 1, # Calculate the day number from the start
    wave = ((hournumber - 1) %% 269) + 1, # Cycle through
    ID = ((hournumber - 1) %/% 269) + 1 # Increment ID 
  )

mmdata = na.omit(mmdata)

rm(full_df)









# -> patterns of multicollinearity confirmed: sProp and cumSEL removed

# ------------------------------------------------------------------------------
# Select best model based on backward selection p-value
# ------------------------------------------------------------------------------

### IND TEST
gc()
MmGEE_ind = geeglm(MmPres~
                     # sun_ele+
                     bs(Ice_pc)+
                     mSpline(jd,knots=kjd,Boundary.knots=lJd,periodic=T),
                   data=mmdata,family=binomial,id=ID,waves=wave,scale.fix=T) 
D1 = drop1(MmGEE_ind,test = "Wald",method = "robust")
# DF   Wald        Pr(>Chi)    
# bs(Ice_pc)                                                    3 13.256        0.004114 ** 
#   mSpline(jd, knots = kjd, Boundary.knots = lJd, periodic = T)  5 55.534 0.0000000001013 ***

QIC(MmGEE_ind)
# 12256.2822 
anova(MmGEE_ind)


#### AR1 TEST
gc()
MmGEE_ar1 = geeglm(MmPres~
                     # bs(sun_ele)+
                     bs(Ice_pc)+
                     mSpline(jd,knots=kjd,Boundary.knots=lJd,periodic=T),
                   data=mmdata,family=binomial,id=ID,waves=wave,corstr = 'ar1',scale.fix=T) 
D2 = drop1(MmGEE_ar1,test = "Wald",method = "robust")
# DF   Wald         Pr(>Chi)    
# bs(Ice_pc)                                                    3 12.669         0.005411 ** 
#   mSpline(jd, knots = kjd, Boundary.knots = lJd, periodic = T)  5 57.895 0.00000000003306 ***
QIC(MmGEE_ar1)
# 12235.8968 


# Single term deletions
# Model:
#   MmPres ~ bs(Ice_pc) + mSpline(jd, knots = kjd, Boundary.knots = lJd, periodic = T)
#                                                               DF Wald      Pr(>Chi)    
# bs(Ice_pc)                                                    3 10.5         0.015 *  
# mSpline(jd, knots = kjd, Boundary.knots = lJd, periodic = T)  5 55.8 0.00000000009 ***





anova(MmGEE_ar1)

# Analysis of 'Wald statistic' Table
# Model: binomial, link: logit
# Response: MmPres
# Terms added sequentially (first to last)
# 
# Df     X2        P(>|Chi|)    
# bs(Ice_pc)                                                    3 39.446 0.00000001396355 ***
#   mSpline(jd, knots = kjd, Boundary.knots = lJd, periodic = T)  5 57.895 0.00000000003306 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


print(summary(MmGEE_ar1),digits=4)


# geeglm(formula = MmPres ~ bs(Ice_pc) + mSpline(jd, knots = kjd, 
#                                                Boundary.knots = lJd, periodic = T), family = binomial, data = mmdata, 
#        id = ID, waves = wave, corstr = "ar1", scale.fix = T)
# 
# Coefficients:
#   Estimate    Std.err   Wald  Pr(>|W|)    
# (Intercept)                                                     -13.3381     4.0937 10.616  0.001121 ** 
#   bs(Ice_pc)1                                                      -1.9410     1.6088  1.456  0.227623    
# bs(Ice_pc)2                                                       2.3308     1.2494  3.480  0.062098 .  
# bs(Ice_pc)3                                                      -2.3288     0.6998 11.074  0.000875 ***
#   mSpline(jd, knots = kjd, Boundary.knots = lJd, periodic = T)1  1254.8739   355.7923 12.440  0.000420 ***
#   mSpline(jd, knots = kjd, Boundary.knots = lJd, periodic = T)2  -139.6599   134.2070  1.083  0.298047    
# mSpline(jd, knots = kjd, Boundary.knots = lJd, periodic = T)3  1512.0774   381.0422 15.747 0.0000724 ***
#   mSpline(jd, knots = kjd, Boundary.knots = lJd, periodic = T)4 -1631.6418   382.8824 18.160 0.0000203 ***
#   mSpline(jd, knots = kjd, Boundary.knots = lJd, periodic = T)5  1229.4706   432.6492  8.075  0.004487 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation structure = ar1 
# Scale is fixed.
# 
# Link = identity 
# 
# Estimated Correlation Parameters:
#   Estimate Std.err
# alpha   0.6472  0.2879
# Number of clusters:   145  Maximum cluster size: 269 




save(list = c("MmGEE_ar1", "mmdata"), file = "G:/Shared drives/SWAL_Arctic/Research_projects/JackBRS/publication/stats/output/habitat/HMar1_hour.Rdata")


mmGEE = MmGEE_ar1
graphics.off()

{#plot.new()            # Create empty plot in RStudios' default window
  dev.new(width = 7,height = 8,noRStudioGD = TRUE)   # Create new plot window
  par(mfrow=c(1,2))

  
  ###### Plots year
  otherstuff = c("jd")
  otherstuffvalue <- c(200)       #FOR JULY
  namethingstoshow= "Ice_pc"
  axisLabel = "Percent Ice Cover"
  predgridmaker (mmGEE, mmdata$Ice_pc, namethingstoshow, otherstuff, otherstuffvalue,axisLabel, maxy=NA, maxviolin = NA)
  
  otherstuff = c("jd")
  otherstuffvalue <- c(290)       #FOR OCTOBER
  namethingstoshow= "Ice_pc"
  axisLabel = "Percent Ice Cover"
  predgridmaker (mmGEE, mmdata$Ice_pc, namethingstoshow, otherstuff, otherstuffvalue,axisLabel, maxy=NA, maxviolin = NA)
  
  
  ##### Day of Year
  otherstuff = c("Ice_pc")
  otherstuffvalue <- c(0)       ###perhaps better to set jd=183
  namethingstoshow= "jd"
  axisLabel = "Day of Year"
  predgridmaker (mmGEE, mmdata$jd, namethingstoshow, otherstuff, otherstuffvalue,axisLabel, maxy=NA, maxviolin = NA)

  # plots solar elevation angle adjusted for 
  # 
  # otherstuff = c("jd","Ice_pc")
  # otherstuffvalue <- c(190, 0)       #FOR OCTOBER
  # namethingstoshow= "sun_ele"
  # axisLabel = "Sun Elevation Angle Adjusted for Refraction (degrees)"
  # predgridmaker (mmGEE, mmdata$sun_ele, namethingstoshow, otherstuff, otherstuffvalue,axisLabel, maxy=NA, maxviolin = NA)
  # 
}


