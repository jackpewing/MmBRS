### Daily Model

# Using Generalized Estimating Equations (GEEs) to quantify 
# narwhal behavioral response to shipping in Eclipse Sound

# site PI

# ______________________________________________________________________________

# load packages & functions
library (car)
library (splines) #required for bs()
library(splines2) #required for mSpline()
library (geepack)
library(MuMIn)
library(dplyr)
library(gt)
library(tidyr)
#source('C:/Users/Alba/Documents/GitHub/SOCAL_Sonar_Impact/publication/plotKernelDensityDistr.R')
source('C:/Users/HARP/Documents/GitHub/SOCAL_Sonar_Impact/publication/getPvalues.R')
source('C:/Users/HARP/Documents/GitHub/SOCAL_Sonar_Impact/publication/new_plot_code/myviolin.R')
source('C:/Users/HARP/Documents/GitHub/SOCAL_Sonar_Impact/publication/new_plot_code/predgridmaker.R')
# source('C:/Users/HARP/Documents/GitHub/SOCAL_Sonar_Impact/publication/myviolin.R')
# source('C:/Users/HARP/Documents/GitHub/SOCAL_Sonar_Impact/publication/predgridmaker.R')
#source('C:/Users/HARP/Documents/GitHub/SOCAL_Sonar_Impact/publication/predgridmaker2.R')


## Load Data

setwd("G:/Shared drives/SWAL_Arctic/Research_projects/JackBRS/Arctic_shiptxClicks/output/stage_two/Combine_all/publication/20km_mask/daily")
#### LOAD DATA - USE MODEL 7 Rdata for now. ######
mmdata2 <-  read.csv("dailybinned_40km_UTC.csv")

# Filter for October, make day variable
mmdata2$Time <- dmy(mmdata2$Time)
jul <- mmdata2 %>%
  filter(month(Time) == 7)
mmdata$day  = day(mmdata2$Time)


# Can quickly save this data
# Define the file path and name
# file_path <- "G:/Shared drives/SWAL_Arctic/Research_projects/JackBRS/Arctic_shiptxClicks/output/Stage_two/Combine_all/mmdata_20231119R.csv"
# # Write the dataframe to a CSV file
# write.csv(mmdata, file = file_path, row.names = FALSE)


################################
#date filtering in case we want to remove winter
# mmdata$Time <- dmy(mmdata$Time)
# mmdata <- mmdata %>%
#   filter(month(Time) == 10)
################################


## Edit variables to desired format and resolution
mmdata$year = as.factor(mmdata$year)
mmdata$year = relevel(mmdata$year,"2016") # arrange years: 2009 as the baseline

# no cumspres?
# zcdata$cumsPres = zcdata$cumsPres/60 # transform min to hours

## Selection of Covariates:

# Apply a basic GLM assuming all points are independent. 
# Calculate variance inflation factor (VIF) to assess which covariates 
# are collinear. Variables are removed one at a time recalculating VIF values. 
# A VIF value above 5 indicates high correlation and is cause for concern.

# define knots for cyclic splines with 4 degrees of freedom

# bit of a warning here, think about leap years and a 366th julian day
msJd = mSpline(mmdata$jd,df = 5, Boundary.knots = c(1,365),periodic = T)
kJd = knots(msJd) 
lJd = c(1,365)

# no time of day here
#msTd = mSpline(mmdata$timeofd,df = 4, Boundary.knots = c(-1,1),periodic = T)
#kTd = knots(msTd) 
#lTd = c(-1,1)

############ PROCEED WITH CAUTION HERE ############
## ROUNDING - THIS SHOULD NOTED, we are doing this because poisson requires integers
## and duty cycled periods have non integer effort adjusted presence

mmdata$MmPres_eff_adj <- round(mmdata$MmPres_eff_adj)

#for binomial
mmdata$MmPres = as.numeric(mmdata$MmPres > 0)

############ END CAUTION

mmGLM<-glm(MmPres ~
             bs(Ice_pc)+
             mSpline(jd,knots=kJd,Boundary.knots=lJd,periodic=T)+
             as.factor(year),
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


# add wave and ID based on the first lag

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
    wave = ((DayNumber - 1) %% 14) + 1, # Cycle through 1 to 12 for wave
    ID = ((DayNumber - 1) %/% 14) + 1 # Increment ID every 12 days
  )
# remove empty rows
full_df <- na.omit(full_df)


#add wave and ID
mmdata$wave = full_df$wave
mmdata$ID = full_df$ID



# okay weird moment here with the lag times.

vif1 <- 5000
threshVIF = 5
selectedCovariates = attributes(terms(mmGLM))$term.labels
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


# GVIF Df GVIF^(1/(2*Df))
# Ice_pc                                                       2.393385  1        1.547057
# mSpline(jd, knots = kJd, Boundary.knots = lJd, periodic = T) 2.595272  5        1.100065
# as.factor(year)                                              3.549742  5        1.135062



mmGLM2<-glm(MmPres ~ 
              bs(Ice_pc)+
              mSpline(jd,knots=kJd,Boundary.knots=lJd,periodic=T)+
              as.factor(year),
            data=mmdata,family=binomial)

summary(mmGLM2)
Anova(mmGLM2)



# Autocorrelation:
# Data group every x timestep based on ACF function. Cluster size selected to 
# be the same for all sites for comparison.

# Full model with independent correlation structure


# 
# msJd = mSpline(mmdata$jd,df = 5, Boundary.knots = c(1,365),periodic = T)
# kJd = knots(msJd) 
# lJd = c(1,365)





mmGEE_ar1 = geeglm(MmPres~
                     bs(Ice_pc),
                   data=mmdata,family=binomial,id=ID,waves=wave,corstr = 'ar1') 

QIC(mmGEE_ar1)
# 30 km radius: -940581.6 

getPvalues(mmGEE_ar1)


mmGEE_ar1_l = geeglm(MmPres~
                      bs(Ice_pc)+
                     mSpline(jd,knots=kJd,Boundary.knots=lJd,periodic=T)+
                     as.factor(year),
                   data=mmdata,family=binomial,id=ID,waves=wave,corstr = 'ar1')

QIC(mmGEE_ar1_l)
# -940509.6 

getPvalues(mmGEE_ar1_l)

# Variable  p-value
# 1                                                        sPres 0.551435
# 2                                                       Ice_pc 0.083929
# 3 mSpline(jd, knots = kJd, Boundary.knots = lJd, periodic = T)  <0.0001
# 4                                              as.factor(year) 0.123126

print(summary(mmGEE_ar1_l),digits=4)
summary(mmGEE_ar1_l)

coefs <- coef(mmGEE_ar1_l)
std_errors <- sqrt(diag(vcov(mmGEE_ar1_l)))
wald_stats <- (coefs / std_errors)^2
# mmGEE_ar1_l <- summary(mmGEE_ar1_l)
sum_mmGEE_ind$coefficients <- cbind(mmGEE_ar1_l$coefficients, Wald = wald_stats)
print(mmGEE_ar1_l)



# Extract coefficients
coefs_data <- as.data.frame(mmGEE_ar1_l$coefficients)

# Add a new column with variable names
coefs_data$Variable <- rownames(coefs_data)

# Move the Variable column to the first position
coefs_data <- coefs_data[, c(ncol(coefs_data), 1:(ncol(coefs_data)-1))]

library(gt)

gt_table <- gt(coefs_data)

# Optional: Add styling
gt_table <- gt_table %>%
  tab_header(
    title = "Model Summary"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = everything())
  )
windows()
gt_table


library(multcomp)

# Assuming 'year' is your factor variable
contrasts <- contrMat(table(mmdata$year), "Tukey")
posthoc <- glht(mmGEE_ar1_l, linfct = mcp(year = contrasts))

# Adjust p-values, for example using Tukey's method
summary(posthoc, test = adjusted("tukey"))



save(list = c("mmGEE_ar1_l", "mmdata"), file = "G:/Shared drives/SWAL_Arctic/Research_projects/JackBRS/publication/stats/output/habitat/HM3.Rdata")



# plot

# plots for counts look weird.
source('C:/Users/HARP/Documents/GitHub/SOCAL_Sonar_Impact/publication/new_plot_code/predgridmaker.R')
source('C:/Users/HARP/Documents/GitHub/SOCAL_Sonar_Impact/publication/new_plot_code/predgridmaker2.R')
mmGEE = mmGEE_ar1_l

#windows() 



###### Plots year
graphics.off()
windows() 
otherstuff = c("jd", "Ice_pc")
otherstuffvalue <- c(290,mean(mmdata$Ice_pc))       ###perhaps better to set jd=183
namethingstoshow= "year"
axisLabel = "Year"
predgridmaker (mmGEE, mmdata$year, namethingstoshow, otherstuff, otherstuffvalue, axisLabel, maxy=NA, maxviolin = NA)


# plots jd
graphics.off()
windows() 
otherstuff = c("year","Ice_pc")
otherstuffvalue <- c(2016,0)      ###perhaps better to set jd=183
namethingstoshow= "jd"
axisLabel = "Day of Year"
predgridmaker (mmGEE, mmdata$jd, namethingstoshow, otherstuff, otherstuffvalue, axisLabel, maxy=NA, maxviolin = NA)

# plots pc ice cover
graphics.off()
windows() 
otherstuff = c("year","jd")
otherstuffvalue <- c(2020,290)       ###perhaps better to set jd=183
namethingstoshow= "Ice_pc"
axisLabel = "Percent Ice Cover"
predgridmaker (mmGEE, mmdata$Ice_pc,namethingstoshow, otherstuff, otherstuffvalue, axisLabel, maxy=NA, maxviolin = NA)

# plots pc ice cover
graphics.off()
windows() 
otherstuff = c("year","jd", "Ice_pc")
otherstuffvalue <- c(2017,180,0)       ###perhaps better to set jd=183
namethingstoshow= "sPres"
axisLabel = "Percent Ice Cover"
predgridmaker (mmGEE, mmdata$sPres,namethingstoshow, otherstuff, otherstuffvalue, axisLabel, maxy=NA, maxviolin = NA)

# # plots minRange (1)- look below for slight change that is confusing
# #windows()
# otherstuff = c("year","jd", "sPres", "Ice_pc", "maxSPL")
# otherstuffvalue <- c(2016,183,mmdata$sPres[mmdata$sPres ==1],0,0)       ###perhaps better to set jd=183
# namethingstoshow= "minRange"
# axisLabel = "Range to closest ship (meters) V2"
# predgridmaker2(mmGEE, mmdata$minRange,namethingstoshow, otherstuff, otherstuffvalue, axisLabel, maxy=NA, maxviolin = NA)

# plots minRange (2)- Slightly different - higher prob of narwhals, more narrow error bars
windows()
otherstuff = c("year","jd", "sPres", "Ice_pc", "maxSPL")
otherstuffvalue <- c(2016,290,mmdata$sPres==1,0,0)       ###perhaps better to set jd=183
namethingstoshow= "minRange"
axisLabel = "Range to closest ship (meters)"
predgridmaker2 (mmGEE, mmdata$minRange,namethingstoshow, otherstuff, otherstuffvalue, axisLabel, maxy=NA, maxviolin = NA)


# plots ship intensity --> hump @ 3 probably caused by large amounts of 
# windows()
# otherstuff = c("year","jd", "sPres", "minRange","Ice_pc", "maxSPL")
# otherstuffvalue <- c(2019,183,mmdata$sPres[mmdata$sPres==1],0,0)       ###perhaps better to set jd=183
# namethingstoshow= "sIntensity"
# axisLabel = "Ship Intensity (hours/250 km^2)"
# predgridmaker2 (mmGEE, mmdata$sIntensity,namethingstoshow, otherstuff, otherstuffvalue, axisLabel, maxy=NA, maxviolin = NA)

# plots maxSPL when ships present
windows()
otherstuff = c("year","jd", "sPres", "minRange","Ice_pc")
otherstuffvalue <- c(2020,183,mmdata$sPres[mmdata$sPres == 1],0,0)       ###perhaps better to set jd=183
namethingstoshow= "maxSPL"
axisLabel = "Maximum Recieved Level (dB re: 1 microPascal [20-10000 hz])"
predgridmaker2 (mmGEE, mmdata$maxSPL[mmdata$maxSPL>0],namethingstoshow, otherstuff, otherstuffvalue, axisLabel, maxy=NA, maxviolin = NA)

# plots maxSPL when ships not present, ice = 0 ##### WILL NOT WORK, INTERACTION MUST MAKE IT ALWAYS PRESENT #####
#windows()
# otherstuff = c("year","jd", "sPres", "minRange","Ice_pc", "sIntensity","sdur")
# otherstuffvalue <- c(2019,183,mmdata$sPres[mmdata$sPres ==0],0,0,0,0)       ###perhaps better to set jd=183
# namethingstoshow= "maxSPL"
# axisLabel = "Maximum Recieved Level / Minute (dBrms [20-10000 hz])"
# predgridmaker2 (mmGEE, mmdata$maxSPL[mmdata$maxSPL>0],namethingstoshow, otherstuff, otherstuffvalue, axisLabel, maxy=NA, maxviolin = NA)

# plots sdur 
# windows()
# otherstuff = c("year","jd", "sPres", "minRange","Ice_pc", "sIntensity","maxSPL")
# otherstuffvalue <- c(2019,183,mmdata$sPres[mmdata$sPres ==1],0,0,0,0)       ###perhaps better to set jd=183
# namethingstoshow= "sdur"
# axisLabel = "Duration of continuous ship presence (minutes)"
# predgridmaker2 (mmGEE, mmdata$sdur,namethingstoshow, otherstuff, otherstuffvalue, axisLabel, maxy=NA, maxviolin = NA)

# plots sPres 
#windows()
# otherstuff = c("year","jd", "sdur", "minRange","Ice_pc", "sIntensity","maxSPL")
# otherstuffvalue <- c(2019,183,0,0,0,0,0)       ###perhaps better to set jd=183
# namethingstoshow= "sPres"
# axisLabel = "Presence of Ships"
# predgridmaker2 (mmGEE, mmdata$sPres,namethingstoshow, otherstuff, otherstuffvalue, axisLabel, maxy=NA, maxviolin = NA)
# 







#______________________________________________________________________________
#                 Test cummulative effects over previous 1hr
