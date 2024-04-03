# GOnna try to do the habitat model with the glmtoolbox package this time.


### GEE's for Narwhal BRS - CANARC_PI ###

library(glmtoolbox)
# library(geepack)
library(tweedie)
library (car)
library (splines) #required for bs()
library(splines2) #required for mSpline()
library(MuMIn)
library(dplyr)
library(gt)
library(lubridate)
library(ggplot2)
library(statmod)



setwd("G:/Shared drives/SWAL_Arctic/Research_projects/JackBRS/Arctic_shiptxClicks/output/stage_two/Combine_all/publication/20km_mask/daily")
#### LOAD DATA  ######
mmdata <-  read.csv("dailybinned_40km_UTC.csv")



##################################################
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



## Edit variables to desired format and resolution
mmdata$year = as.factor(mmdata$year)
mmdata$year = relevel(mmdata$year,"2016") # arrange years with 2016 as base


# set up julian day variable
msJd = mSpline(mmdata$jd,df = 5, Boundary.knots = c(1,365),periodic = T)
kJd = knots(msJd) 
lJd = c(1,365)



# Round data (if poisson)
mmdata$MmPres_eff_adj <- round(mmdata$MmPres_eff_adj)

# make binomial (if binomial)
mmdata$MmPres = as.numeric(mmdata$MmPres > 0)

### now run the glm, this will help us figure out autocorrelation and covariance

mmGLM<-glm(MmPres ~
             bs(Ice_pc)+
             mSpline(jd,knots=kJd,Boundary.knots=lJd,periodic=T),
             # as.factor(year),
           data=mmdata,family=binomial)
summary(mmGLM)
acf_res  = acf(residuals(mmGLM),100)

acf_values <- acf_res$acf[-1]  # Exclude lag 0
N <- length(residuals(mmGLM))
conf_interval_upper <- 1.96 / sqrt(N)
conf_interval_lower <- -1.96 / sqrt(N)
first_lag_within_bounds <- which(acf_values <= conf_interval_upper & acf_values >= conf_interval_lower)[1]
cat("First lag within bounds:", first_lag_within_bounds, "\n")
cat("Autocorrelation value at this lag:", acf_values[first_lag_within_bounds], "\n")

#### Okay change autocorrelation now - based on first significant lag

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
    wave = ((DayNumber - 1) %% 17) + 1, # Cycle throughfor wave
    ID = ((DayNumber - 1) %/% 17) + 1 # Increment ID 
  )
# remove empty rows
full_df <- na.omit(full_df)


#add wave and ID
mmdata$wave = full_df$wave
mmdata$ID = full_df$ID

rm(full_df)


vif1 <- 5000
threshVIF = 5
selectedCovariates = attributes(terms(mmGLM))$term.labels
while (vif1>sqrt(threshVIF)){
  for (x in 1: length (selectedCovariates)){
    if (x==1){newformula <- selectedCovariates[1]}
    else{newformula <- paste (newformula, selectedCovariates[x], sep="+")}
  }
  # make sure to change name and family as needed
  newformula = paste ("MmPres~", newformula,sep="")
  modeltemp = glm (as.formula (newformula), data=mmdata,family=binomial) 
  
  viftemp = vif(modeltemp)
  print (viftemp)
  
  vif2 = match (max(viftemp[,3]), viftemp[,3])
  vif1 = viftemp[vif2, 3]
  if (vif1>sqrt(threshVIF)){   selectedCovariates <- selectedCovariates[-vif2] }
}


# binomial
# GVIF Df GVIF^(1/(2*Df))
# bs(Ice_pc)                                                   4.113  3           1.266
# mSpline(jd, knots = kJd, Boundary.knots = lJd, periodic = T) 4.562  5           1.164
# as.factor(year)                                              3.494  5           1.133
# 
# bs(Ice_pc)                                                   1.955  3           1.118
# mSpline(jd, knots = kJd, Boundary.knots = lJd, periodic = T) 1.955  5           1.069



mmgee<-glmgee(MmPres ~ 
                bs(Ice_pc)+
                mSpline(jd, knots = kJd, Boundary.knots = lJd, periodic = T),
              family = binomial,
              id = ID,
              waves = wave,
              corst = "AR-M-dependent(1)",
              data = mmdata)

stepCriterion(mmgee, direction = "backward",criterion = "p-value")

# Single term deletions
#                                                                 df   QIC  QICu  AGPC  SGPC      P(Chisq>)(*)
# - bs(Ice_pc)                                                     3  1258  1164 -6076 -6058 0.000008875731760
# - mSpline(jd, knots = kJd, Boundary.knots = lJd, periodic = T)   5  1538  1508  -543  -530 0.000000000003717
# <none>                                                              1271  1166 -5914 -5889      


summary(mmgee)

anova(mmgee)

# Wald test 
# Model 1 :  MmPres ~ 1 
# Model 2 :  MmPres ~ bs(Ice_pc) 
# Model 3 :  MmPres ~ bs(Ice_pc) + mSpline(jd, knots = kJd, Boundary.knots = lJd, periodic = T) 
# 
#      Chi       df     Pr(>Chi)    
# 1 vs 2 26.891   3 0.000006204565693 ***
# 2 vs 3 62.488   5 0.000000000003717 ***























