### GEE's for Narwhal BRS - CANARC_PI ###

library(glmtoolbox)

library (car)
library (splines) #required for bs()
library(splines2) #required for mSpline()
library(MuMIn)
library(dplyr)
library(gt)
library(lubridate)
library(ggplot2)

source('C:/Users/HARP/Documents/GitHub/SOCAL_Sonar_Impact/publication/getPvalues.R')
source('C:/Users/HARP/Documents/GitHub/SOCAL_Sonar_Impact/publication/new_plot_code/myviolin.R')
source('C:/Users/HARP/Documents/GitHub/SOCAL_Sonar_Impact/publication/new_plot_code/predgridmaker.R')

# read in data

setwd("G:/Shared drives/SWAL_Arctic/Research_projects/JackBRS/Arctic_shiptxClicks/output/stage_two/Combine_all/publication/20km_mask/effort/5min_effadj")
mmdata <-  read.csv("effadj_binned_5min_UTC.csv")


# Filter for October only
mmdata$Time <- dmy_hms(mmdata$Time)
mmdata <- mmdata %>%
  filter(month(Time) == 10)

#jd becomes day of month
mmdata$day  = day(mmdata$Time)


# pull in thin sea ice thickness
sit = read.csv('G:/Shared drives/SWAL_Arctic/Research_projects/JackBRS/Arctic_shiptxClicks/output/Ice/sit/sit_CANARC_PI.csv')
sit$dates <- as.Date(sit$dates, format="%d-%b-%Y")
mmdata$Time <- as.POSIXct(mmdata$Time, format="%Y-%m-%d %H:%M:%S")
mmdata$date <- as.Date(mmdata$Time)
merged_table <- merge(mmdata, sit, by.x="date", by.y="dates", all.x=TRUE)
mmdata$sit = merged_table$medians

rm(merged_table)
rm(sit)

# prepare year variable
mmdata$year = as.factor (mmdata$year)
mmdata$year = relevel(mmdata$year,"2016") 


# needs to be rounded for poisson regression
mmdata$MmPres_eff_adj <- round(mmdata$MmPres_eff_adj)

mmdata$sPres = as.factor(mmdata$sPres)
mmdata$sPres = relevel(mmdata$sPres,"0") 

mmdata$MmPres[mmdata$MmPres_eff_adj > 0] = 1

# splining for tod
msTd = mSpline(mmdata$tod,df = 4, Boundary.knots = c(-1,1),periodic = T)
kTd = knots(msTd) 
lTd = c(-1,1)

# okay test a GLM to get the autocorrelation
mmGLM<-glm(MmPres ~ 
             bs(sit)+
             bs(day)+
             mSpline(tod,knots=kTd,Boundary.knots=lTd,periodic=T)+
             sPres:bs(minRange),
           data=mmdata,family=binomial)
summary(mmGLM)
acf_res  = acf(residuals(mmGLM),1000)

acf_values <- acf_res$acf[-1]  # Exclude lag 0
N <- length(residuals(mmGLM))
conf_interval_upper <- 1.96 / sqrt(N)
conf_interval_lower <- -1.96 / sqrt(N)
first_lag_within_bounds <- which(acf_values <= conf_interval_upper & acf_values >= conf_interval_lower)[1]
cat("First lag within bounds:", first_lag_within_bounds, "\n")
cat("Autocorrelation value at this lag:", acf_values[first_lag_within_bounds], "\n")

# now we will add the wave and ID based on the autocorrelation first significant lag

mmdata$Time <- as.POSIXct(mmdata$Time, format = "%Y-%m-%d %H:%M:%S")
full_minutes <- seq(min(mmdata$Time), max(mmdata$Time), by = "5 min") # change for binning
full_df <- data.frame(Time = full_minutes)
full_df <- left_join(full_df, mmdata, by = "Time")

# Generate the wave and ID variables based on minutes
mmdata <- full_df %>% 
  mutate(
    MinNumber = as.numeric(difftime(Time, min(Time), units = "mins"))/5 + 1, # change the /# based on binning
    wave = ((MinNumber - 1) %% 745) + 1, # Cycle through
    ID = ((MinNumber - 1) %/% 745) + 1 # Increment ID 
  )

mmdata = na.omit(mmdata)
rm(full_df)



# okay now we go through and test for collinearity


vif1 <- 5000
threshVIF = 5
selectedCovariates = attributes(terms (mmGLM))$term.labels
while (vif1>sqrt(threshVIF)){
  for (x in 1: length (selectedCovariates)){
    if (x==1){newformula <- selectedCovariates[1]}
    else{newformula <- paste (newformula, selectedCovariates[x], sep="+")}
  }
  
  newformula = paste ("MmPres~", newformula,sep="")
  modeltemp = glm (as.formula (newformula), data=mmdata,family=binomial) 
  
  viftemp = vif(modeltemp)
  print (viftemp)
  
  vif2 = match (max(viftemp[,3]), viftemp[,3])
  vif1 = viftemp[vif2, 3]
  if (vif1>sqrt(threshVIF)){   selectedCovariates <- selectedCovariates[-vif2] }
}





# test another GLM, run anova
mmGLM2<-glm(MmPres ~ 
              bs(sit)+
              bs(day)+
              mSpline(tod,knots=kTd,Boundary.knots=lTd,periodic=T)+
              sPres:bs(minRange),
            data=mmdata,family=binomial)
summary(mmGLM2)
Anova(mmGLM2)






# now GEE time


mmgee<-glmgee(MmPres ~
              bs(sit) +
              bs(day) +
              mSpline(tod,knots=kTd,Boundary.knots=lTd,periodic=T)+
              sPres:bs(minRange),
            data=mmdata,family=binomial, id = ID, waves = wave,
            corstr = "AR-M-dependent(1)")

stepCriterion(mmgee, direction = "forward",criterion = "qic")














