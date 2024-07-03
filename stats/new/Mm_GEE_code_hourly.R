
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

# remove first trash row
mmdata = mmdata[-1,]

# now make mmpres binomial
mmdata$MmPres = as.numeric(mmdata$MmPres > 0)

# add in the solar elevation
solar = read.csv("I:/Projects/BRS_thesis/JackBRS/Arctic_shiptxClicks/code/JPE/Solar/NOAA_Solar_Calculations_day.csv")
mmdata$sun_ele = solar$elevation_atm_accounted
rm(solar)


# select time period (not dec - mar)
mmdata$Time <- dmy_hms(mmdata$Time)
mmdata <- mmdata %>%
  filter(month(Time) >3 & month(Time) < 12)


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


# define knots for cyclic splines with 4 degrees of freedom
msJd = mSpline(mmdata$jd,df = 4, Boundary.knots = c(1,365),periodic = T)
kjd = knots(msJd) 
lJd = c(1,365)

# factor year
mmdata$year = as.factor(mmdata$year)


### CLUSTER SIZE SELECTION

# acf_res  = acf(residuals(mmGLM),350)
acf_full = acf(mmdata$MmPres, 500)
windows()
plot(acf_full, xlab = "Lags (Hours)", ylab = "ACF", main = "", xlim = c(400, 500), ylim = c(-0.05, 0.12))
windows(12,6)
plot(acf_full, xlab = "Lags (Hours)", ylab = "ACF", main = "Autocorrelation of Narwhal Echolocation Presence")


# acf_values <- acf_res$acf[-1]  # Exclude lag 0
acf_values <- acf_full$acf[-1]  # Exclude lag 0
N <- length(mmdata$MmPres)
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
    wave = ((hournumber - 1) %% 431) + 1, # Cycle through
    ID = ((hournumber - 1) %/% 431) + 1 # Increment ID 
  )

mmdata = na.omit(mmdata)

rm(full_df)




### TEST LINEAR / SMOOTH

# We test what is best, smooth or linear
s = geeglm(MmPres ~  mSpline(jd,knots=kjd,Boundary.knots=lJd,periodic=T) ,data=mmdata,family=binomial,id=ID,waves=wave,scale.fix=T)
# l = geeglm(MmPres ~ mSpline(jd,df = 4,Boundary.knots=lJd,periodic=T), data=mmdata,family=binomial,id=ID,waves=wave,scale.fix=T)
# j = geeglm(MmPres ~ mSpline(jd,df = 6,Boundary.knots=lJd,periodic=T), data=mmdata,family=binomial,id=ID,waves=wave,scale.fix=T)

s = geeglm(MmPres ~ Ice_pc, data=mmdata,family=binomial,id=ID,waves=wave,scale.fix=T)
l = geeglm(MmPres ~ bs(Ice_pc), data=mmdata,family=binomial,id=ID,waves=wave,scale.fix=T)

geepack::QIC(s)[1]  
geepack::QIC(l)[1]  

j = geeglm(MmPres ~ sun_ele, data=mmdata,family=binomial,id=ID,waves=wave,scale.fix=T)
k = geeglm(MmPres ~ bs(sun_ele), data=mmdata,family=binomial,id=ID,waves=wave,scale.fix=T)

geepack::QIC(j)[1]  
geepack::QIC(k)[1]  

#-------------------------------------------------------------------------------
# VIF (VIF> 3 collinearity):
mmGLM<-glm(MmPres ~ 
             # bs(Ice_pc)+
             # mSpline(jd,knots=kjd,Boundary.knots=lJd,periodic=T)+
             # bs(sun_ele)+
             as.factor(year)+
             mSpline(jd,knots=kjd,Boundary.knots=lJd,periodic=T):bs(sun_ele),
           data=mmdata,family=binomial)
vif1 <- 5000
threshVIF = 3
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

# GVIF Df GVIF..1..2.Df..
# bs(Ice_pc, degree = 3)                    1.3  3               1
# as.factor(year)                           1.4  5               1
# as.factor(season):bs(sun_ele, knots = 1)  1.4  8               1


# # bs(Ice_pc)                                                    1.3  3             1.1
# # mSpline(jd, knots = kjd, Boundary.knots = lJd, periodic = T)  1.3  5             1.0

# bs(Ice_pc, knots = 3)                                         1.5  4             1.1
# mSpline(jd, knots = kjd, Boundary.knots = lJd, periodic = T)  1.5  5             1.0








# -> patterns of multicollinearity confirmed: sProp and cumSEL removed

# ------------------------------------------------------------------------------
# Select best model based on backward selection p-value
# ------------------------------------------------------------------------------

### IND TEST
gc()
MmGEE_ind = geeglm(MmPres~
                     # bs(Ice_pc)+
                     # mSpline(jd,knots=kjd,Boundary.knots=lJd,periodic=T)+
                     # bs(sun_ele)+
                     # as.factor(year)+
                     mSpline(jd,knots=kjd,Boundary.knots=lJd,periodic=T):bs(sun_ele),
            data=mmdata,family=binomial,id=ID,waves=wave,scale.fix=T) 
D1 = drop1(MmGEE_ind,test = "Wald",method = "robust")
# bs(Ice_pc)                                                    3 79.038 < 0.00000000000000022 ***
#   mSpline(jd, knots = kjd, Boundary.knots = lJd, periodic = T)  4 23.735            0.00009026 ***

QIC(MmGEE_ind)
# QIC       QICu  Quasi Lik        CIC     params       QICC 
# 12251.9699 12223.0096 -6103.5048    22.4802     8.0000 12254.4106 
anova(MmGEE_ind)





#### AR1 TEST
gc()
MmGEE_ar1 = geeglm(MmPres~
                     bs(Ice_pc)+
                     # mSpline(jd,knots=kjd,Boundary.knots=lJd,periodic=T)+
                     bs(sun_ele),
                     # as.factor(year)+
                     # mSpline(jd,knots=kjd,Boundary.knots=lJd,periodic=T):bs(sun_ele),
            data=mmdata,family=binomial,id=ID,waves=wave,corstr = 'ar1',scale.fix=T) 

D2 = drop1(MmGEE_ar1,test = "Wald",method = "robust")
# DF   Wald         Pr(>Chi)    
# bs(Ice_pc)                                                    3 12.669         0.005411 ** 
#   mSpline(jd, knots = kjd, Boundary.knots = lJd, periodic = T)  5 57.895 0.00000000003306 ***

QIC(MmGEE_ar1)
# QIC        QICu   Quasi Lik         CIC      params        QICC 
# 12251.63519 12223.56095 -6103.78048    22.03712     8.00000 12254.73864 

anova(MmGEE_ar1)
# bs(Ice_pc)                                                    3 155.201 < 0.00000000000000022 ***
#   mSpline(jd, knots = kjd, Boundary.knots = lJd, periodic = T)  4  23.666            0.00009318 ***







###### RESULT HERE ###########


print(summary(mmGEE),digits=4)

library(xtable)

# Summarize the model
summary_MmGEE <- summary(mmGEE)

# Extract the necessary components
coefficients <- summary_MmGEE$coefficients
# Create a nicer data frame for the table (assuming standard columns: Estimate, Std. Error, z value, Pr(>|z|))
mmgee_results <- data.frame(
  Variable = rownames(coefficients),
  Estimate = coefficients[, "Estimate"],
  `Standard Error` = coefficients[, "Std.err"],
  `Wald` = coefficients[, "Wald"],
  `Pr(>|W|)` = coefficients[, "Pr(>|W|)"]
)
 
 
write.csv(mmgee_results, "G:/Shared drives/SWAL_Arctic/Research_projects/JackBRS/publication/stats/output/habitat/HM_sunday_20240627.csv", row.names = F)







save(list = c("MmGEE_ar1", "mmdata"), file = "G:/Shared drives/SWAL_Arctic/Research_projects/JackBRS/publication/stats/output/habitat/HM_sunday_20240627.Rdata")


mmGEE = MmGEE_ar1
graphics.off()

{#plot.new()            # Create empty plot in RStudios' default window
  dev.new(width = 12,height = 12,noRStudioGD = TRUE)   # Create new plot window
  par(mfrow=c(1,3))
  
  otherstuff = c("jd")
  otherstuffvalue <- c(180)      
  namethingstoshow= "sun_ele"
  axisLabel = "Solar Elevation Angle"
  predgridmaker (mmGEE, mmdata$sun_ele, namethingstoshow, otherstuff, otherstuffvalue,axisLabel, maxy=NA, maxviolin = NA)
  
  
  otherstuff = c("sun_ele")
  otherstuffvalue <- c(0)
  namethingstoshow= "jd"
  axisLabel = "Day of Year"
  predgridmaker (mmGEE, mmdata$jd, namethingstoshow, otherstuff, otherstuffvalue,axisLabel, maxy=NA, maxviolin = NA)
  # 

  otherstuff = c("jd")
  otherstuffvalue <- c(300)      
  namethingstoshow= "sun_ele"
  axisLabel = "Solar Elevation Angle"
  predgridmaker (mmGEE, mmdata$sun_ele, namethingstoshow, otherstuff, otherstuffvalue,axisLabel, maxy=NA, maxviolin = NA)
  
  # otherstuff = c("sun_ele")
  # otherstuffvalue <- c(0)       
  # namethingstoshow= "Ice_pc"
  # axisLabel = "Percent Ice Cover"
  # predgridmaker (mmGEE, mmdata$Ice_pc, namethingstoshow, otherstuff, otherstuffvalue,axisLabel, maxy=NA, maxviolin = NA)
  # 
  # otherstuff = c("sun_ele")
  # otherstuffvalue <- c(0)
  # namethingstoshow= "jd"
  # axisLabel = "Day of Year"
  # predgridmaker (mmGEE, mmdata$jd, namethingstoshow, otherstuff, otherstuffvalue,axisLabel, maxy=NA, maxviolin = NA)
  # # 
  # otherstuff = c("year")
  # otherstuffvalue <- c(180)       
  # namethingstoshow= "year"
  # axisLabel = "Year"
  # predgridmaker (mmGEE, mmdata$year, namethingstoshow, otherstuff, otherstuffvalue,axisLabel, maxy=NA, maxviolin = NA)

  
  }


