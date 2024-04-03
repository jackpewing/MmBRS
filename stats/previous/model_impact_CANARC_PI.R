### Fall Minute Model

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
library(lubridate)
library(ggplot2)
#source('C:/Users/Alba/Documents/GitHub/SOCAL_Sonar_Impact/publication/plotKernelDensityDistr.R')
source('C:/Users/HARP/Documents/GitHub/SOCAL_Sonar_Impact/publication/getPvalues.R')
source('C:/Users/HARP/Documents/GitHub/SOCAL_Sonar_Impact/publication/new_plot_code/myviolin.R')
source('C:/Users/HARP/Documents/GitHub/SOCAL_Sonar_Impact/publication/new_plot_code/predgridmaker.R')
# source('C:/Users/HARP/Documents/GitHub/SOCAL_Sonar_Impact/publication/myviolin.R')
# source('C:/Users/HARP/Documents/GitHub/SOCAL_Sonar_Impact/publication/predgridmaker.R')
#source('C:/Users/HARP/Documents/GitHub/SOCAL_Sonar_Impact/publication/predgridmaker2.R')


## Load Data
setwd("G:/Shared drives/SWAL_Arctic/Research_projects/JackBRS/Arctic_shiptxClicks/output/stage_two/Combine_all/publication/20km_mask/effort/all")
#### LOAD DATA - USE MODEL 7 Rdata for now. ######
mmdata <-  read.csv("binned_effort_UTC.csv")
# Can quickly save this data
# Define the file path and name
# file_path <- "G:/Shared drives/SWAL_Arctic/Research_projects/JackBRS/Arctic_shiptxClicks/output/Stage_two/Combine_all/mmdata_20231119R.csv"
# # Write the dataframe to a CSV file
# write.csv(mmdata, file = file_path, row.names = FALSE)


################# Thresholding & Filtering #############

# Date Filtering

# Convert 'Time' to a Date-Time object
mmdata$Time <- dmy_hms(mmdata$Time)
# Filter data to include only dates between September 15th and November 15th of any year
mmdata <- mmdata %>%
  filter(month(Time) == 10)

#make day
mmdata$day  = day(mmdata$Time)

#make hour
#mmdata$hour = hour(mmdata$Time) + 1



# Add median SIT

sit = read.csv('G:/Shared drives/SWAL_Arctic/Research_projects/JackBRS/Arctic_shiptxClicks/output/Ice/sit/sit_CANARC_PI.csv')
sit$dates <- as.Date(sit$dates, format="%d-%b-%Y")
mmdata$Time <- as.POSIXct(mmdata$Time, format="%Y-%m-%d %H:%M:%S")
mmdata$date <- as.Date(mmdata$Time)
merged_table <- merge(mmdata, sit, by.x="date", by.y="dates", all.x=TRUE)
mmdata$sit = merged_table$medians

rm(merged_table)
rm(sit)

#### Ship Range Filtering

# mmdata = mmdata[mmdata$minRange != 0,] # Remove all times when there are no ships


# Update minRange values greater than 30,000 to 0
mmdata$minRange[mmdata$minRange > 40000] <- 0
# Set sPres to 0 wherever minRange is 0
mmdata$sPres[mmdata$minRange == 0] <- 0
# test to make sure it worked
mmdata$sPres[mmdata$minRange>30000 & mmdata$sPres == 0]

####

# minRange SPL THRESHOLDING

# Filter mmdata where minRange is greater than 0
filtered_data <- subset(mmdata, minRange > 0)
# Using hist() to get histogram data without plotting
spl_histdata <- hist(filtered_data$maxSPL, plot = FALSE, breaks = seq(from = floor(min(filtered_data$maxSPL)), to = ceiling(max(filtered_data$maxSPL)), by = 1))
# Finding the maxSPL value associated with the highest count
maxcount_idx <- which.max(spl_histdata$counts)
maxSPL_thresh <- spl_histdata$mids[maxcount_idx]

# filter range based on the maxSPL threshold
mmdata$minRange[mmdata$maxSPL > maxSPL_thresh] <- 0
#check
mmdata$minRange[mmdata$maxSPL > maxSPL_thresh & mmdata$minRange > 0]


# Threshold sPres based on the speed over ground
mmdata$minRange[mmdata$SOG < 4] <- 0
mmdata$sPres[mmdata$minRange == 0] <- 0

mmdata$sPres[mmdata$SOG < 4 & mmdata$sPres == 1] # test

### Remove times without ships
mmdata$minRange[mmdata$minRange == 0] <- NA
mmdata = na.omit(mmdata)


#MAKE NEW VARIABE: presrange
mmdata$presrange = mmdata$sPres/mmdata$minRange
mmdata$presrange[is.nan(mmdata$presrange)] = 0


########## End Thresholding & Filtering  ##########
#lets make ships into km, not meters
mmdata$minRange[mmdata$minRange > 0] = mmdata$minRange[mmdata$minRange>0]/1000




## Edit variables to desired format and resolution
mmdata$year = as.factor (mmdata$year)
mmdata$year = relevel(mmdata$year,"2016") # arrange years: 2009 as the baseline
# mmdata$sLag = mmdata$slag/1440 # transform min to days

# no cumspres?
# zcdata$cumsPres = zcdata$cumsPres/60 # transform min to hours

## Selection of Covariates:

# Apply a basic GLM assuming all points are independent. 
# Calculate variance inflation factor (VIF) to assess which covariates 
# are collinear. Variables are removed one at a time recalculating VIF values. 
# A VIF value above 5 indicates high correlation and is cause for concern.

# define knots for cyclic splines with 4 degrees of freedom

# this is for julian day of year
# msJd = mSpline(mmdata$jd,df = 5, Boundary.knots = c(1,365),periodic = T)
# kJd = knots(msJd) 
# lJd = c(1,365)

# time of day here
msTd = mSpline(mmdata$tod,df = 4, Boundary.knots = c(-1,1),periodic = T)
kTd = knots(msTd) 
lTd = c(-1,1)

# needs to be rounded for poisson regression
mmdata$MmPres_eff_adj <- round(mmdata$MmPres_eff_adj)

# ORRRR Change to Binomial by
mmdata$MmPres[mmdata$MmPres_eff_adj > 0] = 1

mmGLM<-glm(MmPres ~ 
             bs(day)+
             bs(minRange)+
             bs(sit)+
             # mSpline(tod,knots=kTd,Boundary.knots=lTd,periodic=T)+
             as.factor(year),
             # sPres:bs(minRange),
             data=mmdata,family=binomial)
summary(mmGLM)
acf_res  = acf(residuals(mmGLM),5000)

acf_values <- acf_res$acf[-1]  # Exclude lag 0
N <- length(residuals(mmGLM))
conf_interval_upper <- 1.96 / sqrt(N)
conf_interval_lower <- -1.96 / sqrt(N)
first_lag_within_bounds <- which(acf_values <= conf_interval_upper & acf_values >= conf_interval_lower)[1]
cat("First lag within bounds:", first_lag_within_bounds, "\n")
cat("Autocorrelation value at this lag:", acf_values[first_lag_within_bounds], "\n")

#for counts it is 270. can change later

# okay weird moment here with the lag times.

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


# GVIF Df GVIF^(1/(2*Df))
# minRange                                                     1.142429  1        1.068845
# mSpline(jd, knots = kJd, Boundary.knots = lJd, periodic = T) 2.250643  4        1.106721
# bs(Ice_pc)                                                   2.897159  3        1.193975
# bs(maxSPL)                                                   1.548405  3        1.075592
# as.factor(year)                                              3.459911  4        1.167839


mmGLM2<-glm(MmPres ~ 
              bs(day)+
              bs(minRange)+
              bs(sit)+
              # mSpline(tod,knots=kTd,Boundary.knots=lTd,periodic=T)+
              as.factor(year),
            # sPres:bs(minRange),
            data=mmdata,family=binomial)
summary(mmGLM2)
Anova(mmGLM2)


# Analysis of Deviance Table (Type II tests)
# 
# Response: MmPres
# LR Chisq Df Pr(>Chisq)    
# minRange                                                        159.7  1  < 2.2e-16 ***
#   mSpline(jd, knots = kJd, Boundary.knots = lJd, periodic = T)  13661.4  4  < 2.2e-16 ***
#   bs(Ice_pc)                                                     9491.2  3  < 2.2e-16 ***
#   bs(maxSPL)                                                     1055.0  3  < 2.2e-16 ***
#   as.factor(year)                                               10065.9  4  < 2.2e-16 ***


# Autocorrelation:
# Data group every 6 minutes based on ACF function. Cluster size selected to 
# be the same for all sites for comparison.

# Full model with independent correlation structure



msJd = mSpline(mmdata$jd,df = 5, Boundary.knots = c(1,365),periodic = T)
kJd = knots(msJd) 
lJd = c(1,365)

# 20240201 - I have some ratchet code at the bottom to make the wave and ID if need

# mmGEE_ar1 = geeglm(MmPres~
#                      maxSPL+
#                      minRange+
#                      mSpline(jd,knots=kJd,Boundary.knots=lJd,periodic=T)+
#                      bs(Ice_pc)+
#                      as.factor(year),
#                    data=mmdata,family=poisson,id=ID,waves=wave,corstr = 'ar1') 
# 
# QIC(mmGEE_ar1)



mmGEE_ar1_l = geeglm(MmPres~
                       bs(minRange)+
                       bs(day)+
                       bs(sit)+
                       as.factor(year),
                   data=mmdata,family=binomial,id=ID,waves=wave,corstr = 'ar1', scale.fix = T)

QIC(mmGEE_ar1_l)

getPvalues(mmGEE_ar1_l)

# [1] "Getting marginal p-values"
# Variable p-value
# 1 mSpline(jd, knots = kJd, Boundary.knots = lJd, periodic = T) <0.0001
# 2                                                   bs(Ice_pc) <0.0001
# 3                                              as.factor(year) <0.0001

# for cluster ID = 289
# Variable  p-value
# 1 mSpline(jd, knots = kJd, Boundary.knots = lJd, periodic = T)  0.13506
# 2                                                   bs(Ice_pc)  <0.0001
# 3                                              as.factor(year) 0.211307

#for counts, cluster id = 289, but can be changed to 270
# 1 mSpline(jd, knots = kJd, Boundary.knots = lJd, periodic = T) 0.000289
# 2                                                   bs(Ice_pc)  <0.0001
# 3                                              as.factor(year)  <0.0001

print(summary(mmGEE_ar1_l),digits=4)
summary(mmGEE_ar1_l)

coefs <- coef(mmGEE_ar1_l)
std_errors <- sqrt(diag(vcov(mmGEE_ar1_l)))
wald_stats <- (coefs / std_errors)^2
mmGEE_ar1_l <- summary(mmGEE_ar1_l)
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



save(list = c("mmGEE_ar1_l", "mmdata"), file = "G:/Shared drives/SWAL_Arctic/Research_projects/JackBRS/publication/stats/output/impact/IM09.Rdata")



# plot

# plots for counts look weird.
source('C:/Users/HARP/Documents/GitHub/SOCAL_Sonar_Impact/publication/new_plot_code/predgridmaker.R')
source('C:/Users/HARP/Documents/GitHub/SOCAL_Sonar_Impact/publication/new_plot_code/predgridmaker2.R')
mmGEE = mmGEE_ar1_l

#windows() 

graphics.off()
windows()
otherstuff = c("year","day", "Ice_pc" )
otherstuffvalue <- c(2016,15,mean(mmdata$Ice_pc))       
namethingstoshow= "minRange"
axisLabel = "Range to Nearest Ship (km)"
predgridmaker (mmGEE, mmdata$minRange,namethingstoshow, otherstuff, otherstuffvalue, axisLabel, maxy=NA, maxviolin = NA)


###### Plots year
graphics.off()
windows() 
otherstuff = c("day", "Ice_pc", "minRange")
otherstuffvalue <- c(15,mean(mmdata$minRange), mean(mmdata$minRange))      
namethingstoshow= "year"
axisLabel = "Year"
predgridmaker(mmGEE, mmdata$year, namethingstoshow, otherstuff, otherstuffvalue, axisLabel, maxy=NA, maxviolin = NA)


# plots day of month
graphics.off()
windows() 
otherstuff = c("year","Ice_pc","minRange")
otherstuffvalue <- c(2016,mean(mmdata$Ice_pc),mean(mmdata$minRange))      
namethingstoshow= "day"
axisLabel = "Day of Month"
predgridmaker (mmGEE, mmdata$day, namethingstoshow, otherstuff, otherstuffvalue, axisLabel, maxy=NA, maxviolin = NA)

# plots pc ice cover
graphics.off()
windows() 
otherstuff = c("year","day", "minRange")
otherstuffvalue <- c(2016,15,mean(mmdata$minRange))      
namethingstoshow= "Ice_pc"
axisLabel = "Percent Ice Cover"
predgridmaker (mmGEE, mmdata$Ice_pc,namethingstoshow, otherstuff, otherstuffvalue, axisLabel, maxy=NA, maxviolin = NA)

# plots minRange 
windows()
otherstuff = c("sPres","day", "sit","tod")
otherstuffvalue <- c(0, 15,mean(mmdata$sit),0)      
namethingstoshow= "minRange"
axisLabel = "Range to Nearest Vessel (meters)"
predgridmaker (mmGEE, mmdata$minRange,namethingstoshow, otherstuff, otherstuffvalue, axisLabel, maxy=NA, maxviolin = NA)

# plots time of day
windows()
otherstuff = c("year","day", "sit","minRange", "sPres")
otherstuffvalue <- c(2019, 15,6,0)  
namethingstoshow= "tod"
axisLabel = "Normalized Time of Day"
predgridmaker (mmGEE, mmdata$tod,namethingstoshow, otherstuff, otherstuffvalue, axisLabel, maxy=NA, maxviolin = NA)


windows()
otherstuff = c("year","jd", "Ice_pc","minRange")
otherstuffvalue <- c(2016, 290,1,0)      
namethingstoshow= "sit"
axisLabel = "Thin Sea Ice Thickness (cm)"
predgridmaker (mmGEE, mmdata$sit,namethingstoshow, otherstuff, otherstuffvalue, axisLabel, maxy=NA, maxviolin = NA)







# plots maxSPL when ships present
windows()
otherstuff = c("year","jd", "sPres", "minRange","Ice_pc")
otherstuffvalue <- c(2020,183,mmdata$sPres[mmdata$sPres == 1],0,0)       ###perhaps better to set jd=183
namethingstoshow= "maxSPL"
axisLabel = "Maximum Recieved Level (dB re: 1 microPascal [20-10000 hz])"
predgridmaker2 (mmGEE, mmdata$maxSPL[mmdata$maxSPL>0],namethingstoshow, otherstuff, otherstuffvalue, axisLabel, maxy=NA, maxviolin = NA)













# Assuming df is your original dataframe with a datetime column 'Time'
mmdata$Time <- as.POSIXct(mmdata$Time, format = "%Y-%m-%d %H:%M:%S")

# Create a full sequence of minutes
full_minutes <- seq(min(mmdata$Time), max(mmdata$Time), by = "1 min")

# Create a new dataframe from the sequence
full_df <- data.frame(Time = full_minutes)

# Merge the full minute range with the original data (this will fill missing minutes with NA for other columns)
# Ensure 'df' is not missing any columns for the merge to work correctly
full_df <- left_join(full_df, mmdata, by = "Time")

# Generate the wave and ID variables based on minutes
mmdata <- full_df %>% 
  mutate(
    MinNumber = as.numeric(difftime(Time, min(Time), units = "mins"))/1 + 1, # Calculate the day number from the start
    wave = ((MinNumber - 1) %% 3592) + 1, # Cycle through
    ID = ((MinNumber - 1) %/% 3592) + 1 # Increment ID 
  )

mmdata = na.omit(mmdata)

rm(full_df)




#______________________________________________________________________________
#                 Test cummulative effects over previous 1hr
