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
library(lubridate)
library(dplyr)
source('C:/Users/HARP/Documents/GitHub/MmBRS/stats/new/myviolin.R')
# source('C:/Users/HARP/Documents/GitHub/MmBRS/stats/new/predgridmaker.R')
source('C:/Users/HARP/Documents/GitHub/MmBRS/predgridmaker.R')
options(scipen=999)
#_______________________________________________________________________________
#          A) Analysis limited to 2009 - 2014 for comparison with site N and H
#_______________________________________________________________________________

## Load Data
# setwd("I:/Projects/BRS_thesis/JackBRS/Arctic_shiptxClicks/output/Stage_two/Combine_all/publication/20km_mask/effort/all")
# mmdata <-  read.csv("binned_effort_UTC.csv")

setwd("I:/Projects/BRS_thesis/JackBRS/Arctic_shiptxClicks/output/Stage_two/Combine_all/publication/20km_mask/effort/5min_effadj")
mmdata <-  read.csv("effadj_binned_5min_UTC.csv")
mmdata$MmPres = as.numeric(mmdata$MmPres > 0) # if data is lower resolution


# Filter for October, make day variable
mmdata$Time <- dmy_hms(mmdata$Time)
mmdata <- mmdata %>%
  filter(month(Time) == 10)
mmdata$day  = day(mmdata$Time)


sit = read.csv('I:/Projects/BRS_thesis/JackBRS/Arctic_shiptxClicks/output/Ice/sit/sit_CANARC_PI.csv')
sit$dates <- as.Date(sit$dates, format="%d-%b-%Y")
mmdata$Time <- as.POSIXct(mmdata$Time, format="%Y-%m-%d %H:%M:%S")
mmdata$date <- as.Date(mmdata$Time)
merged_table <- merge(mmdata, sit, by.x="date", by.y="dates", all.x=TRUE)
mmdata$sit = merged_table$medians

rm(merged_table)
rm(sit)


# Threshold sPres based on the speed over ground
mmdata$minRange[mmdata$SOG < 4] <- 0
mmdata$sPres[mmdata$minRange == 0] <- 0

mmdata$sPres[mmdata$SOG < 4 & mmdata$sPres == 1] # test



## Edit variables to desired format and resolution
mmdata$minRange = mmdata$minRange/1000 #in km 
mmdata$minRange[mmdata$sPres == 0] = 50
mmdata$tod = mmdata$tod*100

# Filter ships for range
# mmdata <- mmdata %>%
#   filter(minRange > 1)

# ------------------------------------------------------------------------------
# Check correlation and collinearity of variables
# ------------------------------------------------------------------------------


# Correlations: Pearson correlation (r>0.6)
plotCols = c(2,4,5,9,12,15,17) # index of variables to test correlations
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
### GET CLUSTER SIZE
# ------------------------------------------------------------------------------

acf_full  = acf(mmdata$MmPres,3000)
# acf_res  = acf(residuals(mmGLM),3000)
# windows()
# plot(acf_full, xlab = "Lags (Minutes)", ylab = "ACF", main = "", xlim = c(1700, 1900), ylim = c(-0.02, 0.05))
# windows(12,6)
# plot(acf_full, xlab = "Lags (Minutes)", ylab = "ACF", main = "Autocorrelation of Narwhal Echolocation Presence")


acf_values <- acf_full$acf[-1]  # Exclude lag 0
N <- length(mmdata$MmPres)
conf_interval_upper <- 1.96 / sqrt(N)
# conf_interval_upper <- 0.1 # how morgan did it !
conf_interval_lower <- -1.96 / sqrt(N)
# conf_interval_lower <- -0.1 # How Morgan did it

first_lag_within_bounds <- which(acf_values <= conf_interval_upper & acf_values >= conf_interval_lower)[1]
cat("First lag within bounds:", first_lag_within_bounds, "\n")
cat("Autocorrelation value at this lag:", acf_values[first_lag_within_bounds], "\n")
# 2283 ~ 8 days

### CREATE WAVE + ID

# Assuming df is your original dataframe with a datetime column 'Time'
mmdata$Time <- as.POSIXct(mmdata$Time, format = "%Y-%m-%d %H:%M:%S")
# Create a full sequence of minutes
full_minutes <- seq(min(mmdata$Time), max(mmdata$Time), by = "5 min")
# Create a new dataframe from the sequence
full_df <- data.frame(Time = full_minutes)
# Merge the full minute range with the original data (this will fill missing minutes with NA for other columns)
# Ensure 'df' is not missing any columns for the merge to work correctly
full_df <- left_join(full_df, mmdata, by = "Time")
# Generate the wave and ID variables based on minutes
mmdata <- full_df %>% 
  mutate(
    MinNumber = as.numeric(difftime(Time, min(Time), units = "mins"))/5 + 1, # Calculate the day number from the start
    wave = ((MinNumber - 1) %% 1794  ) + 1, # Cycle through
    ID = ((MinNumber - 1) %/% 1794    
    ) + 1 # Increment ID 
  )

mmdata = na.omit(mmdata)

rm(full_df)

# MAKE NARWHALS NUMERIC
mmdata$MmPres = as.numeric(mmdata$MmPres > 0) # if data is lower resolution



# ------------------------------------------------------------------------------
# Define cyclic splines and select if smooth or linear for sonar-related variables based on QIC
# ------------------------------------------------------------------------------

# define knots for cyclic splines with 4 degrees of freedom
msTd = mSpline(mmdata$tod,df = 4, Boundary.knots = c(-100,100),periodic = T)
kTd = knots(msTd) 
lTd = c(-100,100)

# We test what is best, smooth or linear
# sLag
p = geeglm(MmPres ~ Ice_pc, data=mmdata,family=binomial,id=ID,waves=wave,scale.fix=T)
l = geeglm(MmPres ~ bs(Ice_pc), data=mmdata,family=binomial,id=ID,waves=wave,scale.fix=T)
geepack::QIC(p)[1]  
geepack::QIC(l)[1] 

t = geeglm(MmPres ~  sit, data=mmdata,family=binomial,id=ID,waves=wave,scale.fix=T)
u = geeglm(MmPres ~  bs(sit),data=mmdata,family=binomial,id=ID,waves=wave,scale.fix=T)
geepack::QIC(t)[1]  
geepack::QIC(u)[1]  

r = geeglm(MmPres ~  minRange, data=mmdata,family=binomial,id=ID,waves=wave,scale.fix=T)
s = geeglm(MmPres ~  bs(minRange, knots = 40) ,data=mmdata,family=binomial,id=ID,waves=wave,scale.fix=T)
geepack::QIC(r)[1]  
geepack::QIC(s)[1]

x = geeglm(MmPres ~  maxSPL, data=mmdata,family=binomial,id=ID,waves=wave,scale.fix=T)
y = geeglm(MmPres ~  bs(maxSPL) ,data=mmdata,family=binomial,id=ID,waves=wave,scale.fix=T)
geepack::QIC(x)[1]  
geepack::QIC(y)[1]  

a = geeglm(MmPres ~  day, data=mmdata,family=binomial,id=ID,waves=wave,scale.fix=T)
b = geeglm(MmPres ~  bs(day) ,data=mmdata,family=binomial,id=ID,waves=wave,scale.fix=T)
geepack::QIC(a)[1]  
geepack::QIC(b)[1]  


mmdata$MmPres = as.numeric(mmdata$MmPres > 0) # if data is lower resolution

#-------------------------------------------------------------------------------
# VIF (VIF> 3 collinearity):
mmGLM<-glm(MmPres ~ 
             bs(day) + # kts changes nothing here, df does
             bs(sit) +
             # bs(maxSPL, knots = 1)+
             bs(Ice_pc)+
             mSpline(tod,knots=kTd,Boundary.knots=lTd,periodic=T)+
             as.factor(year)+
             bs(minRange, knots = 40),
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
# bs(day, df = 5)                                                1.4  5             1.0
# bs(sit, df = 3)                                                1.3  3             1.1
# mSpline(tod, knots = kTd, Boundary.knots = lTd, periodic = T)  1.0  4             1.0
# sPres:bs(minRange, knots = 1)                                  1.1  4             1.0








# -> patterns of multicollinearity confirmed:

# ------------------------------------------------------------------------------
# Select best model based on backward selection p-value
# ------------------------------------------------------------------------------
gc()
MmGEE_ind = geeglm(MmPres~
                     bs(day) + 
                     bs(sit) +
                     # mSpline(tod, knots = kTd, Boundary.knots = lTd, periodic = T) + 
                     bs(minRange, knots = 40),
                   data=mmdata,family=binomial,id=ID,waves=wave,scale.fix=T) 

D1 = drop1(MmGEE_ind,test = "Wald",method = "robust")


QIC(MmGEE_ind)

anova(MmGEE_ind)

# > anova(MmGEE_ind)
# Analysis of 'Wald statistic' Table
# Model: binomial, link: logit
# Response: MmPres
# Terms added sequentially (first to last)
# 
# Df     X2    P(>|Chi|)    
# bs(day)                   3 15.634     0.001348 ** 
#   bs(sit)                   3 11.811     0.008060 ** 
#   bs(minRange, knots = 40)  4 38.440 0.0000000909 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1




gc()
MmGEE_ar1 = geeglm(MmPres~
                     bs(day) + 
                     bs(sit) +
                     bs(minRange, knots = 40),
                   data=mmdata,family=binomial,id=ID,waves=wave, corstr ="ar1", scale.fix=T) 

D2 = drop1(MmGEE_ar1,test = "Wald",method = "robust")



QIC(MmGEE_ar1)



anova(MmGEE_ar1)

print(summary(MmGEE_ind),digits=4) 






save(list = c("MmGEE_ind", "mmdata"), file = "G:/Shared drives/SWAL_Arctic/Research_projects/JackBRS/publication/stats/output/impact/IM4range_ind_40kt.Rdata")


mmGEE = MmGEE_ind

graphics.off()

{#plot.new()            # Create empty plot in RStudios' default window
  dev.new(width = 14,height = 12,noRStudioGD = TRUE)   # Create new plot window
  par(mfrow=c(2,2))
  
  # Plots RNV
  otherstuff = c("day","sit")
  otherstuffvalue <- c(15,median(mmdata$sit))       ###perhaps better to set jd=183
  namethingstoshow= "minRange"
  axisLabel = "Range to Nearest Vessel (km)"
  predgridmaker (mmGEE, mmdata$minRange[mmdata$sPres == 1], namethingstoshow, otherstuff, otherstuffvalue,axisLabel, maxy=NA, maxviolin = NA)
  
  ###### Plots sit
  otherstuff = c("minRange", "day")
  otherstuffvalue <- c(50, 15)    
  namethingstoshow= "sit"
  axisLabel = "Thin Ice Thickness (cm)"
  predgridmaker (mmGEE, mmdata$sit[mmdata$sit < 40], namethingstoshow, otherstuff, otherstuffvalue,axisLabel, maxy=NA, maxviolin=NA)
  
  
  ###### Plots m
  otherstuff = c("minRange",  "sit")
  otherstuffvalue <- c(50, median(mmdata$sit))    
  namethingstoshow= "day"
  axisLabel = "Day of Month (October)"
  predgridmaker (mmGEE, mmdata$day, namethingstoshow, otherstuff, otherstuffvalue,axisLabel, maxy=NA, maxviolin=NA)
  
  ###### Plots year
  otherstuff = c("sPres","minRange","maxSPL", "tod", "sit")
  otherstuffvalue <- c(0,50,mean(mmdata$maxSPL),0,mean(mmdata$sit))       ###perhaps better to set jd=183
  namethingstoshow= "year"
  axisLabel = "Year"
  predgridmaker (mmGEE, as.factor(mmdata$year), namethingstoshow, otherstuff, otherstuffvalue,axisLabel, maxy=NA, maxviolin = NA)

  ###### Plots timeofd
  otherstuff = c("sPres","minRange", "sit", "day")
  otherstuffvalue <- c(0,50, mean(mmdata$sit), 15)    
  namethingstoshow= "tod"
  axisLabel = "Normalized Time of Day"
  predgridmaker (mmGEE, mmdata$tod, namethingstoshow, otherstuff, otherstuffvalue,axisLabel, maxy=NA, maxviolin=NA)
  
}

graphics.off()

{#plot.new()            # Create empty plot in RStudios' default window
  dev.new(width = 7,height = 8,noRStudioGD = TRUE)   # Create new plot window
  par(mfrow=c(2,2))

  ##### Plots RNV
  otherstuff = c("sPres","year","tod", "sit")
  otherstuffvalue <- c(1,2016,0,mean(mmdata$sit))       ###perhaps better to set jd=183
  namethingstoshow= "minRange"
  axisLabel = "Range to Nearest Vessel (km)"
  predgridmaker (mmGEE, mmdata$minRange[mmdata$minRange<50], namethingstoshow, otherstuff, otherstuffvalue,axisLabel, maxy=NA, maxviolin = NA)
  
  ###### Plots timeofd
  otherstuff = c("year","sPres","minRange", "tod")
  otherstuffvalue <- c(2016,0,50, 0)    
  namethingstoshow= "sit"
  axisLabel = "Thin Ice Thickness (cm)"
  predgridmaker (mmGEE, mmdata$sit, namethingstoshow, otherstuff, otherstuffvalue,axisLabel, maxy=NA, maxviolin=NA)
  
  
  ###### Plots maxSPL
  otherstuff = c("year","sPres","minRange", "tod", "sit")
  otherstuffvalue <- c(2016,0,50, 0, mean(mmdata$sit))    
  namethingstoshow= "maxSPL"
  axisLabel = "Maximum Sound Pressure Level (dB rms [20-10,000 Hz])"
  predgridmaker (mmGEE, mmdata$maxSPL, namethingstoshow, otherstuff, otherstuffvalue,axisLabel, maxy=NA, maxviolin=NA)
  
  ###### Plots year
  otherstuff = c("sPres","minRange", "tod", "sit")
  otherstuffvalue <- c(0,50,0,mean(mmdata$sit))       ###perhaps better to set jd=183
  namethingstoshow= "year"
  axisLabel = "Year"
  predgridmaker (mmGEE, as.factor(mmdata$year), namethingstoshow, otherstuff, otherstuffvalue,axisLabel, maxy=NA, maxviolin = NA)
  
  ###### Plots timeofd
  otherstuff = c("year","sPres","minRange", "sit")
  otherstuffvalue <- c(2016,0,50, mean(mmdata$sit))    
  namethingstoshow= "tod"
  axisLabel = "Normalized time of day"
  predgridmaker (mmGEE, mmdata$tod, namethingstoshow, otherstuff, otherstuffvalue,axisLabel, maxy=NA, maxviolin=NA)
  
}




