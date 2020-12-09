##########
###Import Packages
##########

##	The "foreign" library is required for the read/write of files like DBF, CSV and Excel:
library(foreign)

## Allows "thicken" and "pad" functions for time series data
library(padr)

#Used to load stepAIC() function
library(MASS)

##########
### Preparation for Data Loading
#########

#Enter path to USGS data
usgs <- "/Users/lmqualls/Desktop/CAMELS/basin_timeseries_v1p2_metForcing_obsFlow/basin_dataset_public_v1p2/basin_mean_forcing/usgs_streamflow"
#Enter region to be analyzed
region <- "01"
#Concatenate file path
usgs <- file.path(usgs,region)
setwd(usgs)
streamflow_files <- list.files()

#Initiate list (to be filled with gauges present in USGS data)
sgauges <- vector()

#Loop to extract gauge ID from USGS streamflow filenames
for (file in streamflow_files) {
  sgauge <- substr(file,1,8)
  sgauges <- c(sgauges,sgauge)
}

#Initiate list (to be filled with names of maurer data, using only gauges also found in USGS data)
precip_files <- vector()

#Loop to fill precip_files
for (gauge in sgauges) {
  precip_file <- paste(gauge, "_lump_maurer_forcing_leap.txt", sep = "")
  precip_files <- cbind(precip_files, precip_file)
}

##########
###Data Loading: Maurer Precipitation
##########

#Enter path to CAMELS Maurer data
maurer <- "/Users/lmqualls/Desktop/CAMELS/basin_timeseries_v1p2_metForcing_obsFlow/basin_dataset_public_v1p2/basin_mean_forcing/maurer/01"
setwd(maurer)

areas_m2 <- vector()

#Extract each wastershed area from Maurer precip. files
for (file in precip_files) {
  area_m2 <- readLines(con = file, n = 3)[3]
  areas_m2 <- c(areas_m2,area_m2)
  areas_m2 <- as.numeric(areas_m2)

}

#Create data frame of gauge ID and area metadata
area_df <- as.data.frame(areas_m2)
area_df$areas_m2 <- as.numeric(area_df$areas_m2)
row.names(area_df) <- sgauges

#Reset working directory to Maurer data
setwd(maurer)

#Create time sequence corresponding with Maurer daily precipitation data
precip_time <- seq.Date(from = as.Date('1980-01-01'), to = as.Date('2008-12-31'), by = 'days')

#Insert time sequence column into precipitation data frame
precip_mm_day <- data.frame(precip_time)

#Scratch dataframe
precip_lump <- data.frame()

#Read in precipitation data
for (file in precip_files) {
  lump <- read.table(file = file, sep = "\t", skip = 4, header = FALSE)
  time <- lump[,1]
  precip_col <- as.numeric(lump[,3])
  
  precip_time <- substr(time,1,10)
  precip_lump <- as.data.frame(cbind(precip_time,precip_col))
  precip_lump[,1] <- as.Date(precip_lump[,1], "%Y %m %d")
  precip_lump <- pad(precip_lump, start_val = as.Date('1980-01-01'), end_val = as.Date('2008-12-31'))
  precip_mm_day <- as.data.frame(cbind(precip_mm_day, precip_lump[,2]))
  
}

#-999 to NAs
precip_mm_day[precip_mm_day < 0] <- NA

#Get rid of unnecessary data
precip_mm_day <- precip_mm_day[precip_mm_day[,1] <= as.Date("2008-09-30"),]

#Convert chr to num in precip_mm_day (except first column)
precip_mm_day[,-1] <- lapply(precip_mm_day[,-1], as.numeric)

#Set column names to (date, and) ID
colnames(precip_mm_day) <- c("date",sgauges)

##########
###Data Loading: USGS Streamflow
##########

setwd(usgs)
streamflow_files <- list.files()

#Create base timeline for all streamflow data
usgs_time <- seq.Date(from = as.Date('1980-01-01'), to = as.Date('2014-12-31'), by = 'days')
streamflow_ft3_sec <- data.frame(usgs_time)

#Scratch dataframe
streamflow_lump <- data.frame()

#Read in streamflow data
for (file in streamflow_files) {
  lump <- read.table(file, header = FALSE)
  streamflow_col <- lump[,5]
  date <- paste(lump[,2],lump[,3],lump[,4], sep = "/")
  streamflow_lump <- as.data.frame(cbind(date, streamflow_col))
  streamflow_lump[,1] <- as.Date(streamflow_lump[,1], "%Y/%m/%d")
  streamflow_lump <- pad(streamflow_lump, start_val = as.Date('1980-01-01'), end_val = as.Date('2014-12-31'))
  streamflow_ft3_sec <- as.data.frame(cbind(streamflow_ft3_sec, streamflow_lump[,2]))
}

#Chr to num
streamflow_ft3_sec[,-1] <- lapply(streamflow_ft3_sec[,-1], as.numeric)

#-999 to NAs
streamflow_ft3_sec[streamflow_ft3_sec < 0] <- NA

#Set column names to ID
colnames(streamflow_ft3_sec) <- c("date",sgauges)

#Loop to normalize streamflow by basin area
for (gauge in sgauges) {
  streamflow_ft3_sec[,gauge] <- (streamflow_ft3_sec[,gauge])/area_df[gauge,"areas_m2"]
}

#Get rid of unnecessary data
streamflow_ft3_sec <- as.data.frame(streamflow_ft3_sec[streamflow_ft3_sec[,1] <= as.Date("2008-09-30"),])

#Convert chr to num in precip_mm_day (except first column)
streamflow_ft3_sec[,-1] <- lapply(streamflow_ft3_sec[,-1], as.numeric)

#Y-Y-X Plot of Precip/Streamflow for last 5 years of data
p_plot <- as.data.frame(precip_mm_day[precip_mm_day[,1] >= as.Date("2003-01-01"),])
q_plot <- as.data.frame(streamflow_ft3_sec[streamflow_ft3_sec[,1] >= as.Date("2003-01-01"),])
pq_plot <- as.data.frame(p_plot[,1])
pq_plot <- as.data.frame(cbind(pq_plot,p_plot[,2],q_plot[,2]))

par(mar = c(5, 4, 4, 4) + 0.3)   # Additional space for second y-axis
plot(pq_plot[,1], pq_plot[,2], type = "l", pch = 16, col = "darkslategrey",
     xlab = "time (year)", ylab = "precip (mm/day)")   # Create first plot
par(new = TRUE)                             # Add new plot
plot(pq_plot[,1], pq_plot[,3], pch = 17, type = "l", col = "lightsalmon",
     axes = FALSE, xlab = "", ylab = "")   # Create second plot without axes
axis(side = 4, at = pretty(range(pq_plot[,3])))   # Add second axis
mtext("streamflow (ft3/day)", side = 4, line = 3)   # Add second axis label

##########
### Data Preparation
##########

#Normalize the precip/streamflow data: (value(in column)-mean(of column)/standard deviation(of column))
for (gauge in sgauges) {
  mean_p <- mean(precip_mm_day[,gauge])
  stdev_p <- sd(precip_mm_day[,gauge], na.rm = TRUE)
  precip_mm_day[,gauge] <- (((precip_mm_day[,gauge])-mean_p)/stdev_p)
  
  
  mean_q <- mean(streamflow_ft3_sec[,gauge], na.rm = TRUE)
  stdev_q <- sd(streamflow_ft3_sec[,gauge], na.rm = TRUE)
  streamflow_ft3_sec[,gauge] <- (((streamflow_ft3_sec[,gauge])-mean_q)/stdev_q)
}

##########
### Training/Test Data Creation
##########

#Create subset data for training the model (vectors for use in models, dfs for future reference)
precip_train <- as.data.frame(precip_mm_day)
precip_train <- as.data.frame(precip_train[precip_train[,1] >= as.Date("1999-10-01"),])
precip_train <- as.data.frame(precip_train[,-1])

streamflow_train <- as.data.frame(streamflow_ft3_sec)
streamflow_train <- as.data.frame(streamflow_train[streamflow_train[,1] >= as.Date("1999-10-01"),])
streamflow_train <- as.data.frame(streamflow_train[,-1])

x_train = vector()
y_train = vector()

#Fill vectors with training data
for (gauge in sgauges) {
  x <- as.vector(precip_train[,gauge])
  x_train <- c(x_train, x)
  
  y <- as.vector(streamflow_train[,gauge])
  y_train <- c(y_train, y)
  
}

#Unlist vectors to one dimension
x_train <- unlist(x_train, recursive = FALSE, use.names = FALSE)
y_train <- unlist(y_train, recursive = FALSE, use.names = FALSE)

#Create subset data for testing the model (vectors for use in models, dfs for future reference)
precip_test <- precip_mm_day
precip_test <- as.data.frame(precip_test[precip_test[,1] <= as.Date("1999-09-30"),])
precip_test <- as.data.frame(precip_test[precip_test[,1] >= as.Date("1989-10-01"),])

streamflow_test <- streamflow_ft3_sec
streamflow_test <- as.data.frame(streamflow_test[streamflow_test[,1] <= as.Date("1999-09-30"),])
streamflow_test <- as.data.frame(streamflow_test[streamflow_test[,1] >= as.Date("1989-10-06"),])

x_test <- vector()
x_test_df <- data.frame()
y_test_df <- data.frame()

#Fill vectors/dfs with test data
for (gauge in sgauges) {
  x <- as.vector(precip_test[,-1][gauge])
  x_test <- c(x_test, x)
  x_test_df <- as.data.frame(c(x_test_df, x))
  
  y <- as.vector(streamflow_test[,-1][gauge])
  y_test_df <- as.data.frame(c(y_test_df, y))
  
}

#Assign column names as gauge values
colnames(x_test_df) <- sgauges
colnames(y_test_df) <- sgauges

##nlist vector to one dimension
x_test <- unlist(x_test, recursive = FALSE, use.names = FALSE)

##########
### Linear Regression Models
##########

#Create linear regression model (BULK, NO LAG)
relation <- lm(y_train ~ x_train, na.action = na.exclude)
r_bulk_nolag <- summary(relation)$adj.r.squared
print(r_bulk_nolag)

###############################################################################
######## WARNING: THE FOLLOWING CODE IS NOT GRACEFUL...But it works :) ########
###############################################################################

#This sucks! Let's make it better by lagging precipitation and streamflow

#Precipitation: lag0
x_train_lag0 <- vector()
x_train_lag0_df <- data.frame()

for (gauge in sgauges) {
  x <- as.data.frame(c(precip_train[,gauge],NA,NA,NA,NA,NA))
  x_train_lag0 <- c(x_train_lag0, x)
  x_train_lag0_df <- as.data.frame(c(x_train_lag0_df, x))
}
colnames(x_train_lag0_df) <- sgauges

##Precipitation: lag1
x_train_lag1 <- vector()
x_train_lag1_df <- data.frame()

for (gauge in sgauges) {
  x <- as.data.frame(c(NA,precip_train[,gauge],NA,NA,NA,NA))
  x_train_lag1 <- c(x_train_lag1, x)
  x_train_lag1_df <- as.data.frame(c(x_train_lag1_df, x))
}
colnames(x_train_lag1_df) <- sgauges

#Precipitation: lag2
x_train_lag2 <- vector()
x_train_lag2_df <- data.frame()

for (gauge in sgauges) {
  x <- as.data.frame(c(NA,NA,precip_train[,gauge],NA,NA,NA))
  x_train_lag2 <- c(x_train_lag2, x)
  x_train_lag2_df <- as.data.frame(c(x_train_lag2_df, x))
}
colnames(x_train_lag2_df) <- sgauges

#Precipitation: lag3
x_train_lag3 <- vector()
x_train_lag3_df <- data.frame()

for (gauge in sgauges) {
  x <- as.data.frame(c(NA,NA,NA,precip_train[,gauge],NA,NA))
  x_train_lag3 <- c(x_train_lag3, x)
  x_train_lag3_df <- as.data.frame(c(x_train_lag3_df, x))
}
colnames(x_train_lag3_df) <- sgauges

#Precipitation: lag4
x_train_lag4 <- vector()
x_train_lag4_df <- data.frame()

for (gauge in sgauges) {
  x <- as.data.frame(c(NA,NA,NA,NA,precip_train[,gauge],NA))
  x_train_lag4 <- c(x_train_lag4, x)
  x_train_lag4_df <- as.data.frame(c(x_train_lag4_df, x))
}
colnames(x_train_lag4_df) <- sgauges

#Precipitation: lag5
x_train_lag5 <- vector()
x_train_lag5_df <- data.frame()

for (gauge in sgauges) {
  x <- as.data.frame(c(NA,NA,NA,NA,NA,precip_train[,gauge]))
  x_train_lag5 <- c(x_train_lag5, x)
  x_train_lag5_df <- as.data.frame(c(x_train_lag5_df, x))
}
colnames(x_train_lag5_df) <- sgauges

#Pad y data for lag
y_train_lag_df <- data.frame()
y_train_lag <- vector()

for (gauge in sgauges) {
  y <- as.data.frame(c(streamflow_train[,gauge],NA,NA,NA,NA,NA))
  y_train_lag <- c(y_train_lag, y)
  y_train_lag_df <- as.data.frame(c(y_train_lag_df, y))
}

colnames(y_train_lag_df) <- sgauges

#Streamflow: lag1
y_train_lag1 <- vector()
y_train_lag1_df <- data.frame()

for (gauge in sgauges) {
  y <- as.data.frame(c(NA,streamflow_train[,gauge],NA,NA,NA,NA))
  y_train_lag1 <- c(y_train_lag1, y)
  y_train_lag1_df <- as.data.frame(c(y_train_lag1_df, y))
}
colnames(y_train_lag1_df) <- sgauges

#Streamflow: lag2
y_train_lag2 <- vector()
y_train_lag2_df <- data.frame()

for (gauge in sgauges) {
  y <- as.data.frame(c(NA,NA,streamflow_train[,gauge],NA,NA,NA))
  y_train_lag2 <- c(y_train_lag2, y)
  y_train_lag2_df <- as.data.frame(c(y_train_lag2_df, y))
}
colnames(y_train_lag2_df) <- sgauges

#Streamflow: lag3
y_train_lag3 <- vector()
y_train_lag3_df <- data.frame()

for (gauge in sgauges) {
  y <- as.data.frame(c(NA,NA,NA,streamflow_train[,gauge],NA,NA))
  y_train_lag3 <- c(y_train_lag3, y)
  y_train_lag3_df <- as.data.frame(c(y_train_lag3_df, y))
}
colnames(y_train_lag3_df) <- sgauges

#Streamflow: lag4
y_train_lag4 <- vector()
y_train_lag4_df <- data.frame()

for (gauge in sgauges) {
  y <- as.data.frame(c(NA,NA,NA,NA,streamflow_train[,gauge],NA))
  y_train_lag4 <- c(y_train_lag4, y)
  y_train_lag4_df <- as.data.frame(c(y_train_lag4_df, y))
}
colnames(y_train_lag4_df) <- sgauges

#Streamflow: lag5
y_train_lag5 <- vector()
y_train_lag5_df <- data.frame()

for (gauge in sgauges) {
  y <- as.data.frame(c(NA,NA,NA,NA,NA,streamflow_train[,gauge]))
  y_train_lag5 <- c(y_train_lag5, y)
  y_train_lag5_df <- as.data.frame(c(y_train_lag5_df, y))
}
colnames(y_train_lag5_df) <- sgauges

#Unlist vectors to 1 dimension...Ugly but effective
x_train_lag0 <- unlist(x_train_lag0, recursive = FALSE, use.names = FALSE)
x_train_lag1 <- unlist(x_train_lag1, recursive = FALSE, use.names = FALSE)
x_train_lag2 <- unlist(x_train_lag2, recursive = FALSE, use.names = FALSE)
x_train_lag3 <- unlist(x_train_lag3, recursive = FALSE, use.names = FALSE)
x_train_lag4 <- unlist(x_train_lag4, recursive = FALSE, use.names = FALSE)
x_train_lag5 <- unlist(x_train_lag5, recursive = FALSE, use.names = FALSE)
y_train_lag <- unlist(y_train_lag, recursive = FALSE, use.names = FALSE)
y_train_lag1 <- unlist(y_train_lag1, recursive = FALSE, use.names = FALSE)
y_train_lag2 <- unlist(y_train_lag2, recursive = FALSE, use.names = FALSE)
y_train_lag3 <- unlist(y_train_lag3, recursive = FALSE, use.names = FALSE)
y_train_lag4 <- unlist(y_train_lag4, recursive = FALSE, use.names = FALSE)
y_train_lag5 <- unlist(y_train_lag5, recursive = FALSE, use.names = FALSE)

#Create linear regression model (BULK, WITH LAG)
relation_lag <- lm(y_train_lag ~ x_train_lag0 + x_train_lag1 + x_train_lag2 + x_train_lag3 +
                     x_train_lag4 + x_train_lag5 + y_train_lag1 + y_train_lag2 + y_train_lag3 +
                     y_train_lag4 + y_train_lag5, na.action = na.exclude)
r_bulk_lag <- summary(relation_lag)$adj.r.squared
print(r_bulk_lag)

dev.off()

#Plots difference in R2 values of bulk models
bulk_r2 <- as.vector(c(r_bulk_nolag,r_bulk_lag))
plot(bulk_r2, ylim = c(0,1), xaxt = "n", yaxt = "n", xlim = c(0.5,2.5), xlab = "Model (Number)",
     ylab = "r-squared", col = c("forestgreen","purple"), pch = 16, main = "Bulk R2: No Lag v. Lag")
axis(side = 1, at = c(1,2))
axis(side = 2, at = c(0.0,0.5,1.0))
text(bulk_r2, labels = format(round(bulk_r2, 3), nsmall = 3), pos = 4)

#Models with lag perform better; AKA models with "memory" of precip. & streamflow do better.
#Would it be even better on individual basins?

#Vector for storing R2 values for each basin (no lag)
R2_lag0 <- vector()

#Create linear regression model (IND., NO LAG)
for (gauge in sgauges) {
  x_lag0 <- vector()
  y_lag0 <- vector()
  
  #prepare precip for each gauge
  x_lag0 <- as.vector(precip_train[,gauge])
  y_lag0 <- as.vector(streamflow_train[,gauge])
  
  relation_lag <- lm(y_lag0 ~ x_lag0, na.action = na.exclude)
  (summary(relation_lag))
  
  r <- summary(relation_lag)$adj.r.squared
  R2_lag0 <- c(R2_lag0,r)
  print(paste(gauge, r, sep =": "))
}

#Vector for storing R2 values for each basin (withlag)
R2_lags <- vector()

#Create linear regression model (IND., WITH LAG)
for (gauge in sgauges) {
  
  y_lag <- vector()
  
  y_lag <- as.vector(y_train_lag_df[,gauge])
  
  predictors <- c("x_lag0", "x_lag1", "x_lag2", "x_lag3", "x_lag4",
                  "x_lag5","y_lag1", "y_lag2", "y_lag3", "y_lag4","y_lag5")
  
  train_data <- na.omit(as.data.frame(cbind(y_lag, as.vector(x_train_lag0_df[,gauge]),
                                            as.vector(x_train_lag1_df[,gauge]),
                                            as.vector(x_train_lag2_df[,gauge]),
                                            as.vector(x_train_lag3_df[,gauge]),
                                            as.vector(x_train_lag4_df[,gauge]),
                                            as.vector(x_train_lag5_df[,gauge]),
                                            as.vector(y_train_lag1_df[,gauge]),
                                            as.vector(y_train_lag2_df[,gauge]),
                                            as.vector(y_train_lag3_df[,gauge]),
                                            as.vector(y_train_lag4_df[,gauge]),
                                            as.vector(y_train_lag5_df[,gauge]))))
  
  colnames(train_data) <- c("y_lag", predictors)
  
  formula <- as.formula(paste(paste("y_lag",'~', sep=''), paste(predictors,collapse='+'), sep=''))
  
  relation_lag <- lm(formula = formula, data = train_data, na.action = na.exclude)
  
  #Stepwise function to optimize R2 based on inclusion/exclusion of variables
  step <- stepAIC(relation_lag, direction = "backward")
  r <- summary(step)$adj.r.squared
  R2_lags <- (c(R2_lags,r))
  print(paste(gauge, r, sep =": "))
}

#Perform Wilcoxon test to quantify improvement of R2 with the addition of lagged data
sig <- wilcox.test(x = R2_lag0,y = R2_lags, paired = TRUE)
p_sig <- sig$p.value  #retreive p-value from Wilcox test

#Transpose R2_lags into data frame for referencing
R2_lags <- as.data.frame(R2_lags)
row.names(R2_lags) <- sgauges

#Finds value of highest R2 
best_r2 <- max(R2_lags[,1])
best_model <- as.numeric(which(R2_lags == best_r2)) #Finds position of highest R2

#Re-train best model
y_lag <- vector()

y_lag <- as.vector(y_train_lag_df[,best_model])

predictors <- c("x_lag0", "x_lag1", "x_lag2", "x_lag3", "x_lag4",
                "x_lag5", "y_lag1", "y_lag2", "y_lag3", "y_lag4","y_lag5")

train_data <- na.omit(as.data.frame(cbind(y_lag, as.vector(x_train_lag0_df[,best_model]),
                                          as.vector(x_train_lag1_df[,best_model]),
                                          as.vector(x_train_lag2_df[,best_model]),
                                          as.vector(x_train_lag3_df[,best_model]),
                                          as.vector(x_train_lag4_df[,best_model]),
                                          as.vector(x_train_lag5_df[,best_model]),
                                          as.vector(y_train_lag1_df[,best_model]),
                                          as.vector(y_train_lag2_df[,best_model]),
                                          as.vector(y_train_lag3_df[,best_model]),
                                          as.vector(y_train_lag4_df[,best_model]),
                                          as.vector(y_train_lag5_df[,best_model]))))

colnames(train_data) <- c("y_lag", predictors)
R2_lag <- vector()

formula <- as.formula(paste(paste("y_lag",'~', sep=''), paste(predictors,collapse='+'), sep=''))

relation_lag <- lm(formula = formula, data = train_data, na.action = na.exclude)
  
#Stepwise function to optimize R2 based on inclusion/exclusion of variables
step <- stepAIC(relation_lag, direction = "backward")
r <- summary(step)$adj.r.squared
R2_lag <- r
print(paste(sgauges[best_model], r, sep =": "))

#Creates test data from basin with best R2
x_test_data <- as.data.frame(x_test_df[,best_model])
y_test_data <- as.data.frame(y_test_df[,best_model])

#Unlists test data into one dimension
x_test_data <- unlist(x_test_data, recursive = FALSE, use.names = FALSE)
y_test_data <- unlist(x_test_data, recursive = FALSE, use.names = FALSE)

#Creates lags for test data
x_test_lag0 <- c(x_test_data,NA,NA,NA,NA,NA)
x_test_lag1 <- c(NA,x_test_data,NA,NA,NA,NA)
x_test_lag2 <- c(NA,NA,x_test_data,NA,NA,NA)
x_test_lag3 <- c(NA,NA,NA,x_test_data,NA,NA)
x_test_lag4 <- c(NA,NA,NA,NA,x_test_data,NA)
x_test_lag5 <- c(NA,NA,NA,NA,NA,x_test_data)
y_test_lag1 <- c(NA,y_test_data,NA,NA,NA,NA)
y_test_lag2 <- c(NA,NA,y_test_data,NA,NA,NA)
y_test_lag3 <- c(NA,NA,NA,y_test_data,NA,NA)
y_test_lag4 <- c(NA,NA,NA,NA,y_test_data,NA)
y_test_lag5 <- c(NA,NA,NA,NA,NA,y_test_data)

#Creates dataframe for test data
test_data <- as.data.frame(cbind(x_test_lag0,x_test_lag1,x_test_lag2,x_test_lag3,x_test_lag4,x_test_lag5,
                                 y_test_lag1,y_test_lag2,y_test_lag3,y_test_lag4,y_test_lag5))
colnames(test_data) <- predictors

#Puts observed streamflow data into dataframe format
y_observed <- as.data.frame(y_test_df[,best_model])

#Does a prediction for streamflow, with test_data dataframe as source)
prediction <- as.vector(predict(relation_lag, test_data, na.action = na.omit))

#Puts predicted streamflow data into dataframe
prediction <- as.data.frame(cbind(prediction, y_observed))
colnames(prediction) <- c("predictedQ","observedQ")

dev.off()

#Y-Y-X Plot: comparing observed and predicted values of streamflow
par(mar = c(5, 4, 4, 4) + 0.3)   # Additional space for second y-axis
plot(streamflow_test[,1], prediction[,1], type = "l", pch = 16, col = "darkslategrey",
     xlab = "Time (day)", ylab = "Observed Streamflow (ft3/sec)", ylim = c(-1,15))   # Create first plot
par(new = TRUE)                             # Add new plot
plot(streamflow_test[,1], prediction[,2], pch = 17, type = "l", col = "lightsalmon",
     axes = FALSE, xlab = "", ylab = "")  # Create second plot without axes
axis(side = 4, at = pretty(range(prediction[,2])))   # Add second axis
mtext("Predicted Streamflow (ft3/sec)", side = 4, line = 3)   # Add second axis label

x_test_obs <- x_test_df[,best_model]

for (row in 1:5) {
  x_test_obs <- x_test_obs[-1]
}

dev.off()

#Y-Y-X Plot: comparing observed precipitation and predicted values of streamflow
par(mar = c(5, 4, 4, 4) + 0.3)   # Additional space for second y-axis
plot(streamflow_test[,1], x_test_obs, type = "l", pch = 16, col = "darkslategrey",
     xlab = "Time (day)", ylab = "Observed Precipitation (mm/day)", ylim = c(-1,15))   # Create first plot
par(new = TRUE)                             # Add new plot
plot(streamflow_test[,1], prediction[,2], pch = 17, type = "l", col = "lightsalmon",
     axes = FALSE, xlab = "", ylab = "")  # Create second plot without axes
axis(side = 4, at = pretty(range(prediction[,2])))   # Add second axis
mtext("Predicted Streamflow (ft3/sec)", side = 4, line = 3)   # Add second axis label
