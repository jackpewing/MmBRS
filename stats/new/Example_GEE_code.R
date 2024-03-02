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
source('C:/Users/HARP/Documents/GitHub/MmBRS/stats/new/myviolin.R')
source('C:/Users/HARP/Documents/GitHub/MmBRS/stats/new/predgridmaker.R')

options(scipen=999)
#_______________________________________________________________________________
#          A) Analysis limited to 2009 - 2014 for comparison with site N and H
#_______________________________________________________________________________


## Load Data
setwd("G:/Shared drives/SWAL_Arctic/Research_projects/JackBRS/Arctic_shiptxClicks/output/stage_two/Combine_all/publication/20km_mask/effort/all")
mmdata <-  read.csv("binned_effort_UTC.csv")

# Filter for October, make day variable
mmdata$Time <- dmy_hms(mmdata$Time)
mmdata <- mmdata %>%
  filter(month(Time) == 10)
mmdata$day  = day(mmdata$Time)


sit = read.csv('G:/Shared drives/SWAL_Arctic/Research_projects/JackBRS/Arctic_shiptxClicks/output/Ice/sit/sit_CANARC_PI.csv')
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
plotCols = c(5,9,11,30,32,33,35) # index of variables to test correlations
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
msTd = mSpline(mmdata$tod,df = 4, Boundary.knots = c(-100,100),periodic = T)
kTd = knots(msTd) 
lTd = c(-100,100)

# We test what is best, smooth or linear
# sLag
s = geeglm(zcPres ~ bs(sLag),data=zcdata,family=binomial,id=clusterID,waves=clusterObs,scale.fix=T)
l = geeglm(zcPres ~ sLag, data=zcdata,family=binomial,id=clusterID,waves=clusterObs,scale.fix=T)
geepack::QIC(s)[1] 

#-------------------------------------------------------------------------------
# VIF (VIF> 3 collinearity):
mmGLM<-glm(MmPres ~ 
             #bs(day)+
             bs(sit)+
             bs(maxSPL)+
             mSpline(tod,knots=kTd,Boundary.knots=lTd,periodic=T)+
             as.factor(year)+
             sPres:bs(minRange),
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



acf_values <- acf_res$acf[-1]  # Exclude lag 0
N <- length(residuals(mmGLM))
conf_interval_upper <- 1.96 / sqrt(N)
conf_interval_lower <- -1.96 / sqrt(N)
first_lag_within_bounds <- which(acf_values <= conf_interval_upper & acf_values >= conf_interval_lower)[1]
cat("First lag within bounds:", first_lag_within_bounds, "\n")
cat("Autocorrelation value at this lag:", acf_values[first_lag_within_bounds], "\n")


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







# -> patterns of multicollinearity confirmed: sProp and cumSEL removed

# ------------------------------------------------------------------------------
# Select best model based on backward selection p-value
# ------------------------------------------------------------------------------
gc()
ZcGEE_ind = geeglm(MmPres~
                     bs(sit)+
                     bs(maxSPL)+
                     mSpline(tod,knots=kTd,Boundary.knots=lTd,periodic=T)+
                     as.factor(year)+
                     sPres:bs(minRange),
                   data=mmdata,family=binomial,id=ID,waves=wave,scale.fix=T) 
D1 = drop1(ZcGEE_ind,test = "Wald",method = "robust")
# Single term deletions
# 
# Model:
#   zcPres ~ +contsPres + bs(sLag) + mSpline(jd, knots = kJd, Boundary.knots = lJd, 
#      periodic = T) + mSpline(timeofd, knots = kTd, Boundary.knots = lTd, 
#      periodic = T) + as.factor(year) + sPres:bs(maxRLpp)
#                                                                   DF   Wald  Pr(>Chi)    
# contsPres                                                          1   0.93   0.33527    
# bs(sLag)                                                           3  10.46   0.01506 *  
# mSpline(jd, knots = kJd, Boundary.knots = lJd, periodic = T)       4 352.92 < 2.2e-16 ***
# mSpline(timeofd, knots = kTd, Boundary.knots = lTd, periodic = T)  4  90.07 < 2.2e-16 ***
# as.factor(year)                                                    5  66.57 5.301e-13 ***
# sPres:bs(maxRLpp)                                                  3  28.86 2.402e-06 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

gc()
ZcGEE_ind2 = geeglm(zcPres~
                      + bs(sLag) 
                    + mSpline(jd,knots=kJd,Boundary.knots=lJd,periodic=T)
                    + mSpline(timeofd,knots=kTd,Boundary.knots=lTd,periodic=T)
                    + as.factor(year)
                    + sPres:bs(maxRLpp),
                    data=zcdata,family=binomial,id=clusterID,waves=clusterObs,scale.fix=T) 
D2 = drop1(ZcGEE_ind2,test = "Wald",method = "robust")
# Single term deletions
# 
# Model:
#   zcPres ~ +bs(sLag) + mSpline(jd, knots = kJd, Boundary.knots = lJd, 
#      periodic = T) + mSpline(timeofd, knots = kTd, Boundary.knots = lTd, 
#      periodic = T) + as.factor(year) + sPres:bs(maxRLpp)
#                                                                   DF   Wald  Pr(>Chi)    
# bs(sLag)                                                           3  10.43   0.01524 *  
# mSpline(jd, knots = kJd, Boundary.knots = lJd, periodic = T)       4 353.16 < 2.2e-16 ***
# mSpline(timeofd, knots = kTd, Boundary.knots = lTd, periodic = T)  4  90.02 < 2.2e-16 ***
# as.factor(year)                                                    5  66.30 6.031e-13 ***
# sPres:bs(maxRLpp)                                                  3  36.35 6.303e-08 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

anova(ZcGEE_ind2)
# Analysis of 'Wald statistic' Table
# Model: binomial, link: logit
# Response: zcPres
# Terms added sequentially (first to last)
# 
#                                                                   Df     X2             P(>|Chi|)    
# bs(sLag)                                                           3  33.93     0.000000205439178 ***
# mSpline(jd, knots = kJd, Boundary.knots = lJd, periodic = T)       4 338.02 < 0.00000000000000022 ***
# mSpline(timeofd, knots = kTd, Boundary.knots = lTd, periodic = T)  4  85.15 < 0.00000000000000022 ***
# as.factor(year)                                                    5  63.35     0.000000000002464 ***
# sPres:bs(maxRLpp)                                                  3  36.35     0.000000063030139 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

print(summary(ZcGEE_ind2),digits=4) 
# Call:
#   geeglm(formula = zcPres ~ +bs(sLag) + mSpline(jd, knots = kJd, 
#          Boundary.knots = lJd, periodic = T) + mSpline(timeofd, knots = kTd, 
#          Boundary.knots = lTd, periodic = T) + as.factor(year) + sPres:bs(maxRLpp), 
#          family = binomial, data = zcdata, id = clusterID, waves = clusterObs, 
#          scale.fix = T)
# 
# Coefficients:
#                                                                       Estimate     Std.err    Wald             Pr(>|W|)    
# (Intercept)                                                          -2.589572    0.182383 201.598 < 0.0000000000000002 ***
# bs(sLag)1                                                            -0.206318    0.245462   0.706             0.400611    
# bs(sLag)2                                                             0.202922    0.423130   0.230             0.631531    
# bs(sLag)3                                                             0.673491    0.353694   3.626             0.056889 .  
# mSpline(jd, knots = kJd, Boundary.knots = lJd, periodic = T)1      -329.401447   18.347300 322.334 < 0.0000000000000002 ***
# mSpline(jd, knots = kJd, Boundary.knots = lJd, periodic = T)2       -36.695888   10.277278  12.749             0.000356 ***
# mSpline(jd, knots = kJd, Boundary.knots = lJd, periodic = T)3      -139.185498   10.050008 191.803 < 0.0000000000000002 ***
# mSpline(jd, knots = kJd, Boundary.knots = lJd, periodic = T)4      -153.421790   16.028619  91.618 < 0.0000000000000002 ***
# mSpline(timeofd, knots = kTd, Boundary.knots = lTd, periodic = T)1  -11.056180    7.694172   2.065             0.150731    
# mSpline(timeofd, knots = kTd, Boundary.knots = lTd, periodic = T)2   -8.567839    5.488780   2.437             0.118530    
# mSpline(timeofd, knots = kTd, Boundary.knots = lTd, periodic = T)3  -37.720148    5.616146  45.110      0.0000000000186 ***
# mSpline(timeofd, knots = kTd, Boundary.knots = lTd, periodic = T)4   12.297240    7.599701   2.618             0.105636    
# as.factor(year)2010                                                   0.357555    0.082844  18.628      0.0000158888281 ***
# as.factor(year)2011                                                   0.226092    0.081226   7.748             0.005378 ** 
# as.factor(year)2012                                                   0.620950    0.124933  24.704      0.0000006686252 ***
# as.factor(year)2013                                                  -0.172214    0.115675   2.216             0.136545    
# as.factor(year)2014                                                  -0.002114    0.086506   0.001             0.980506    
# sPres:bs(maxRLpp)1                                                  -10.324308    3.852542   7.182             0.007365 ** 
# sPres:bs(maxRLpp)2                                                   11.567998    3.308586  12.225             0.000472 ***
# sPres:bs(maxRLpp)3                                                   -9.674878    2.102233  21.180      0.0000041806869 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation structure = independence 
# Scale is fixed.
# 
# Number of clusters:   39933  Maximum cluster size: 50

save(list = c("ZcGEE_ind2", "zcdata"), file = "G:/Shared drives/SOCAL_Sonar_Impact/Impact_analysis/4.Model_results/publication/SOCAL_H_GEE_ind_2009-2014_drop1.Rdata")


zcGEE = ZcGEE_ind2

graphics.off()

{#plot.new()            # Create empty plot in RStudios' default window
  dev.new(width = 7,height = 8,noRStudioGD = TRUE)   # Create new plot window
  par(mfrow=c(3,3))
  
  ###### Plots year
  otherstuff = c("sPres","sLag","maxRLpp", "timeofd", "jd")
  otherstuffvalue <- c(0,mean(zcdata$sLag[zcdata$sLag > 0]),0,0,1)       ###perhaps better to set jd=183
  namethingstoshow= "year"
  axisLabel = "Year"
  predgridmaker (zcGEE, as.factor(zcdata$year), namethingstoshow, otherstuff, otherstuffvalue,axisLabel, maxy=NA, maxviolin = NA)
  
  ###### Plots jd
  otherstuff = c("sPres","sLag","maxRLpp","timeofd", "year")
  otherstuffvalue <- c(0,mean(zcdata$sLag[zcdata$sLag > 0]),0,0,2009)       ###perhaps better to set jd=183
  namethingstoshow= "jd"
  axisLabel = "Julian Date"
  predgridmaker (zcGEE, zcdata$jd, namethingstoshow, otherstuff, otherstuffvalue,axisLabel, maxy=NA, maxviolin = NA)
  
  ###### Plots timeofd
  otherstuff = c("sPres","sLag","maxRLpp","jd","year")
  otherstuffvalue <- c(0,mean(zcdata$sLag[zcdata$sLag > 0]),0,1,2009)    
  namethingstoshow= "timeofd"
  axisLabel = "Normalized time of day"
  predgridmaker (zcGEE, zcdata$timeofd, namethingstoshow, otherstuff, otherstuffvalue,axisLabel, maxy=NA, maxviolin=NA)
  
  ###### Plots sLag
  otherstuff = c("sPres","maxRLpp", "timeofd", "jd","year")
  otherstuffvalue <- c(0,0, 0, 1,2009)  
  namethingstoshow= "sLag"
  axisLabel = "Sonar lag (days)"
  predgridmaker (zcGEE, zcdata$sLag, namethingstoshow, otherstuff, otherstuffvalue,axisLabel, maxy=0.05, maxviolin=0.05) 
  
  ###### Plots maxRLpp
  otherstuff = c("sPres","sLag", "timeofd", "jd","year")
  otherstuffvalue <- c(1,0,0, 1,2009)  
  namethingstoshow= "maxRLpp"
  axisLabel = "MaxRLpp"
  predgridmaker (zcGEE, zcdata$maxRLpp[zcdata$sPres == 1], namethingstoshow, otherstuff, otherstuffvalue,axisLabel, maxy=0.015, maxviolin=0.015) 
}


