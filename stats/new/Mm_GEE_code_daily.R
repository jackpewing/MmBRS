# The short and long-term tolerance of Cuvier's beaked whale under longstanding
# naval sonar pressures
# Solsona-Berga et al.
# PNAS

# Using Generalized Estimating Equations (GEEs) to quantify 
# Cuvier's beaked whale behavioral response to mid-frequency active sonar in
# Souhtern California

# site M

# A) Analysis limited to 2009 - 2014 for comparison with site N and H (site M only has data within these years)

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
source('C:/Users/HARP/Documents/GitHub/MmBRS/stats/new/myviolin.R')
source('C:/Users/HARP/Documents/GitHub/MmBRS/stats/new/predgridmaker.R')

options(scipen=999)
#_______________________________________________________________________________
#          A) Analysis limited to 2009 - 2014 for comparison with site N and H
#_______________________________________________________________________________


## Load Data
setwd("G:/Shared drives/SWAL_Arctic/Research_projects/JackBRS/Arctic_shiptxClicks/output/stage_two/Combine_all/publication/20km_mask/daily")
mmdata <-  read.csv("dailybinned_40km_UTC.csv")



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
plotCols = c(4,6,7) # index of variables to test correlations
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
# -> Correlated variables: sProp/maxRLpp, sProp/cumSEL, maxRLpp/cumSEL

# ------------------------------------------------------------------------------
# Define cyclic splines and select if smooth or linear for sonar-related variables based on QIC
# ------------------------------------------------------------------------------

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
             bs(Ice_pc)+
             mSpline(jd,knots=kjd,Boundary.knots=lJd,periodic=T),
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
    #                                                               GVIF Df GVIF..1..2.Df..
    # bs(sit)                                                        2.6  3             1.2
    # bs(maxSPL)                                                     1.5  3             1.1
    # mSpline(tod, knots = kTd, Boundary.knots = lTd, periodic = T)  1.0  4             1.0
    # as.factor(year)                                                2.9  4             1.1
    # sPres:bs(minRange)                                             1.2  3             1.0

acf_res  = acf(residuals(mmGLM),100)

acf_values <- acf_res$acf[-1]  # Exclude lag 0
N <- length(residuals(mmGLM))
conf_interval_upper <- 1.96 / sqrt(N)
conf_interval_lower <- -1.96 / sqrt(N)
first_lag_within_bounds <- which(acf_values <= conf_interval_upper & acf_values >= conf_interval_lower)[1]
cat("First lag within bounds:", first_lag_within_bounds, "\n")
cat("Autocorrelation value at this lag:", acf_values[first_lag_within_bounds], "\n")

### this creates your wave & ID. the wave & ID as needed based on the results above

# Create a full sequence of dates
mmdata$Time <- as.Date(mmdata$Time, format = "%d-%b-%Y")
full_dates <- seq(min(mmdata$Time), max(mmdata$Time), by = "day")
# Create a new dataframe from the sequence
full_df <- data.frame(Time = full_dates)
# Merge the full date range with the original data (this will fill missing days with NA for other columns)
full_df <- left_join(full_df, mmdata, by = "Time")
# Generate the wave and ID variables
full_df <- full_df %>% 
  mutate(
    DayNumber = as.numeric(Time - min(Time) + 1), # Calculate the day number from the start
    wave = ((DayNumber - 1) %% 17) + 1, # Cycle through for wave
    ID = ((DayNumber - 1) %/% 17) + 1 # Increment ID 
  )
# remove empty rows
full_df <- na.omit(full_df)


#add wave and ID
mmdata$wave = full_df$wave
mmdata$ID = full_df$ID







# -> patterns of multicollinearity confirmed: sProp and cumSEL removed

# ------------------------------------------------------------------------------
# Select best model based on backward selection p-value
# ------------------------------------------------------------------------------

### IND TEST
gc()
MmGEE_ind = geeglm(MmPres~
                     bs(Ice_pc)+
                     mSpline(jd,knots=kjd,Boundary.knots=lJd,periodic=T),
                   data=mmdata,family=binomial,id=ID,waves=wave,scale.fix=T) 
D1 = drop1(MmGEE_ind,test = "Wald",method = "robust")
QIC(MmGEE_ind)



#### AR1 TEST
gc()
MmGEE_ar1 = geeglm(MmPres~
                     bs(Ice_pc)+
                     mSpline(jd,knots=kjd,Boundary.knots=lJd,periodic=T),
                   data=mmdata,family=binomial,id=ID,waves=wave,corstr = 'ar1',scale.fix=T) 
D1 = drop1(MmGEE_ind,test = "Wald",method = "robust")
QIC(MmGEE_ar1)







anova(MmGEE_ar1)
# Analysis of 'Wald statistic' Table
# Model: binomial, link: logit
# Response: MmPres
# Terms added sequentially (first to last)
# 
# Df   X2      P(>|Chi|)    
# bs(Ice_pc)                                                    3 42.3 0.000000003437 ***
#   mSpline(jd, knots = kjd, Boundary.knots = lJd, periodic = T)  5 60.3 0.000000000011 ***


print(summary(MmGEE_ind),digits=4)


# Call:
#   geeglm(formula = MmPres ~ bs(Ice_pc) + mSpline(jd, knots = kjd, 
#                                                  Boundary.knots = lJd, periodic = T), family = binomial, data = mmdata, 
#          id = ID, waves = wave, scale.fix = T)
# 
# Coefficients:
#   Estimate   Std.err  Wald    Pr(>|W|)    
# (Intercept)                                                      -6.439     2.670  5.82      0.0159 *  
#   bs(Ice_pc)1                                                      -2.935     1.722  2.91      0.0882 .  
# bs(Ice_pc)2                                                       4.947     1.507 10.77      0.0010 ** 
#   bs(Ice_pc)3                                                      -1.926     0.918  4.40      0.0359 *  
#   mSpline(jd, knots = kjd, Boundary.knots = lJd, periodic = T)1   725.132   238.325  9.26      0.0023 ** 
#   mSpline(jd, knots = kjd, Boundary.knots = lJd, periodic = T)2  -338.708   128.102  6.99      0.0082 ** 
#   mSpline(jd, knots = kjd, Boundary.knots = lJd, periodic = T)3  1089.800   244.842 19.81 0.000008546 ***
#   mSpline(jd, knots = kjd, Boundary.knots = lJd, periodic = T)4 -1616.029   284.222 32.33 0.000000013 ***
#   mSpline(jd, knots = kjd, Boundary.knots = lJd, periodic = T)5   511.309   326.105  2.46      0.1169    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation structure = independence 
# Scale is fixed.
# 
# Number of clusters:   96  Maximum cluster size: 17 




save(list = c("MmGEE_ind", "mmdata"), file = "G:/Shared drives/SWAL_Arctic/Research_projects/JackBRS/publication/stats/output/HMind.Rdata")


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
  otherstuffvalue <- c(300)       #FOR OCTOBER
  namethingstoshow= "Ice_pc"
  axisLabel = "Percent Ice Cover"
  predgridmaker (mmGEE, mmdata$Ice_pc, namethingstoshow, otherstuff, otherstuffvalue,axisLabel, maxy=NA, maxviolin = NA)
  
  
  ###### Plots jd
  otherstuff = c("Ice_pc")
  otherstuffvalue <- c(0)       ###perhaps better to set jd=183
  namethingstoshow= "jd"
  axisLabel = "Day of Year"
  predgridmaker (mmGEE, mmdata$jd, namethingstoshow, otherstuff, otherstuffvalue,axisLabel, maxy=NA, maxviolin = NA)
  
  
}


