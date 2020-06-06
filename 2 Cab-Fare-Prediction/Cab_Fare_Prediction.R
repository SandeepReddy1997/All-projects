# Remove all variables
rm(list=ls())

library(naniar)
library(corrgram)

# Set Working Directory
setwd("C:\\Users\\SANDEEP\\Downloads\\EdWisor Data Science\\Portfolio\\Project 2. Cab Fare Prediction")

# Load data
rawData = read.csv('train_cab.csv', header = T) #, na.strings = c(43))

rawData$pickup_datetime[rawData$pickup_datetime %in% c('43')] <- NA

dim(rawData)

str(rawData)

summary(rawData)

#############################################################
######### Missing Values Analysis ###########################

# User defined function to calculate missing values
calMissingValues <- function(data){
  missing_values <<- data.frame(apply(data, 2, function(x) sum(is.na(x))))
  missing_values$Columns <<- row.names(missing_values)
  row.names(missing_values) <<- NULL
  names(missing_values)[1] <<- 'Num_Of_Missing_Values'
  missing_values <<- missing_values[order(-missing_values$Num_Of_Missing_Values),]
  missing_values <<- missing_values[, c(2, 1)]
}

View(calMissingValues(rawData), title='Missing values in rawData')

# Calculate missing value percentage in minimum_payments column
(sum(is.na(rawData))/nrow(rawData)) * 100
# Only 0.5% of the data has missing values. 

# we can impute them or delete the respective rows. For now let's delete them
rawData = na.omit(rawData)
data = rawData
dim(data) # Number of rows reduced from 16067 to 15987 rows.

#######################################################################
####### Outlier Analysis ##############################################

# Removing outliers from latitudes and logitudes column
removeOutOfRangeValuesFromLatitudesAndLongitudes <- function(df){
  df = df[which(df$pickup_latitude < 90),]
  df = df[which(df$pickup_latitude > -90),]
  df = df[which(df$pickup_longitude < 180),]
  df = df[which(df$pickup_longitude > -180),]
  df = df[which(df$dropoff_latitude < 90),]
  df = df[which(df$dropoff_latitude > -90),]
  df = df[which(df$dropoff_longitude < 180),]
  df = df[which(df$dropoff_longitude > -180),]
  return(df)
}

data = removeOutOfRangeValuesFromLatitudesAndLongitudes(data)
dim(data) # Number of rows reduced from 15987 to 15986 rows.

removeValuesEqualToZeroFromLatitudesAndLongitudes <- function(df){
  df = df[which(df$pickup_latitude != 0),]
  df = df[which(df$pickup_longitude != 0),]
  df = df[which(df$dropoff_latitude != 0),]
  df = df[which(df$dropoff_latitude != 0),]
  return(df)
}

data = removeValuesEqualToZeroFromLatitudesAndLongitudes(data)
dim(data) # Number of rows reduced from 15986 to 15664

# Removing outliers from fare_amount column
data[which(data$fare_amount <= 0),] # These are the columns which have fare_amount less than or equal to zero.
data = data[-which(data$fare_amount <= 0),] # Number of rows reduced from 15664 to 15660

# Removing rows with fare amount more than 105, which is 0.9995 quantile
q = quantile(data$fare_amount, probs = 0.9995)
data = data[which(data$fare_amount <= q), ] # Number of rows reduced from 15660 to 15652

# Removing outliers from passenger_count column
for (i in 1:15){
  print(paste('Number of clients with passenger_count value above ', i, ' = ', nrow(data[which(data$passenger_count >= i), ])))
}
# Value of passenger_count is consistent above 6. Hence max passenger_count should be 6. Remove outliers.

# Removing rows with passenger_count value less than 1 and more than 6
data = data[which(data$passenger_count > 0),] # Number of rows reduced from 15652 to 15597
data = data[which(data$passenger_count <= 6),] # Number of rows reduced from 15597 to 15580

# Removing rows having decimal values
keep = c(1,2,3,4,5,6)
data = data[data$passenger_count %in% keep, ] # Number of rows reduced from 15580 to 15578

# Replace all outliers with NA
cnames = names(data)[!names(data) %in% c('passenger_count')]
outliers = data[,'fare_amount'] %in% boxplot.stats(data[,'fare_amount'])$out
data[which(outliers),cnames] = NA # Replacing outliers with NA
View(calMissingValues(data), title = 'Missing Values After Outlier Removal')
data = na.omit(data) # drop rows having NA
# Number of rows reduced from 15578 to 14230

# Feature Engineering
#Convert pickup_datetime from factor to date time
data$date = as.Date(as.character(data$pickup_datetime)) # Converting column to datetime format
data$day_of_week = as.factor(format(data$date,"%u"))# Monday = 1, Tuesday = 2 and so on
data$month = as.factor(format(data$date,"%m"))
data$year = as.factor(format(data$date,"%Y"))
pickup_time = strptime(data$pickup_datetime,"%Y-%m-%d %H:%M:%S")
data$hour = as.factor(format(pickup_time,"%H"))

View(calMissingValues(data), title = 'Missing values after Feature Engineering')

# Since we have extracted features from pickup_datetime, Drop the column.
data = subset(data,select = -c(pickup_datetime,date))

# Function to convert degrees to Radians.
deg_to_rad = function(deg){
  (deg * pi) / 180
}

# Function to calculate Haversine distance.
haversine = function(lat1, long1, lat2, long2){
  #long1rad = deg_to_rad(long1)
  phi1 = deg_to_rad(lat1)
  #long2rad = deg_to_rad(long2)
  phi2 = deg_to_rad(lat2)
  delphi = deg_to_rad(lat2 - lat1)
  dellamda = deg_to_rad(long2 - long1)
  
  a = sin(delphi/2) * sin(delphi/2) + cos(phi1) * cos(phi2) * 
    sin(dellamda/2) * sin(dellamda/2)
  
  c = 2 * atan2(sqrt(a),sqrt(1-a))
  km = 6371 * c
  return(km)
}

data$distance = haversine(data$pickup_latitude, data$pickup_longitude, data$dropoff_latitude, data$dropoff_longitude)

# Remove latitude and longitude variables
data = subset(data,select = -c(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude))

##########################################################################
################ Correlation #############################################

str(data)
#data = sapply(data, as.numeric)
#data = data.frame(data)

numeric_index = sapply(data,is.numeric) # selecting only numeric variables

numeric_data = data[,numeric_index]

cnames = colnames(numeric_data)

#Correlation analysis for numeric variables
corrgram(data[,numeric_index],upper.panel=panel.pie, main = "Correlation Plot")

#########################################################################
##### Feature Scaling ###################################################
data_copy = data
data = data_copy
fare_amount = data$fare_amount
data$fare_amount = log1p(data$fare_amount)
data$distance = log1p(data$distance)

#########################################################################
## Splitting Train and test data

library(caret)
set.seed(42)
tr.idx = createDataPartition(data$fare_amount,p=0.90,list = FALSE) # 75% in trainin and 25% in Validation Datasets
train_data = data[tr.idx,]
test_data = data[-tr.idx,]

########################################################################
################### Model Creation #####################################

View(calMissingValues(train_data), title = 'Missing values of Training Data')

library(DataCombine)
rmExcept(c('test_data','train_data', 'data', 'data_copy', 'calMissingValues', 'deg_to_rad', 'haversine',
           'removeOutOfRangeValuesFromLatitudesAndLongitudes', 'removeValuesEqualToZeroFromLatitudesAndLongitudes'))

# Linear Regression
model_LR = lm(fare_amount ~., data=train_data)

predictions_LR = predict(model_LR,test_data[,2:7])

library(DMwR)
error_metrices_LR = regr.eval(expm1(test_data[,1]),expm1(predictions_LR))

print(paste('Therefore Error Percentage of the model is: ', error_metrices_LR['mape']*100))
print(paste('Which means, the model is ', (1-error_metrices_LR['mape'])*100, ' percentage accurate.'))


result_LR = data.frame(expm1(test_data[,1]))
result_LR$Predicted = expm1(predictions_LR)
names(result_LR)[1] = 'Actual'
#rm(list='result_LR')
View(result_LR)

# Logistic Regression is not possible

# Decison Tree
library(rpart)
model_DT = rpart(fare_amount ~ ., data = train_data, method = "anova")

predictions_DT = predict(model_DT, test_data[,2:7])

error_metrices_DT = regr.eval(expm1(test_data[,1]),expm1(predictions_DT))

print(paste('Therefore Error Percentage of the model is: ', error_metrices_DT['mape']*100))
print(paste('Which means, the model is ', (1-error_metrices_DT['mape'])*100, ' percentage accurate.'))

result_DT = data.frame(expm1(test_data[,1]))
result_DT$Predicted = expm1(predictions_DT)
names(result_DT)[1] = 'Actual'
#rm(list='result_DT')
View(result_DT)

# Random Forest
library(randomForest)
model_RF = randomForest(fare_amount ~.,data=train_data)

predictions_RF = predict(model_RF,test_data[,2:7])

error_metrices_RF = regr.eval(expm1(test_data[,1]),expm1(predictions_RF))

print(paste('Therefore Error Percentage of the model is: ', error_metrices_RF['mape']*100))
print(paste('Which means, the model is ', (1-error_metrices_RF['mape'])*100, ' percentage accurate.'))

result_RF = data.frame(expm1(test_data[,1]))
result_RF$Predicted = expm1(predictions_RF)
names(result_RF)[1] = 'Actual'
#rm(list='result_RF')
View(result_RF)

#######################################################################
################### Test data #########################################

testData = read.csv('test.csv')

View(calMissingValues(testData), title = 'Missing Values of Final Test Data')
testData = removeOutOfRangeValuesFromLatitudesAndLongitudes(testData)
testData = removeValuesEqualToZeroFromLatitudesAndLongitudes(testData)

# Feature Engineering
#Convert pickup_datetime from factor to date time
testData$date = as.Date(as.character(testData$pickup_datetime)) # Converting column to datetime format
testData$day_of_week = as.factor(format(testData$date,"%u"))# Monday = 1, Tuesday = 2 and so on
testData$month = as.factor(format(testData$date,"%m"))
testData$year = as.factor(format(testData$date,"%Y"))
pickup_time = strptime(testData$pickup_datetime,"%Y-%m-%d %H:%M:%S")
testData$hour = as.factor(format(pickup_time,"%H"))

View(calMissingValues(testData), title = 'Missing Values of Final Test Data')

# Since we have extracted features from pickup_datetime, Drop the column.
testData = subset(testData, select = -c(pickup_datetime,date))

# Calculating Haversine Distance
testData$distance = haversine(testData$pickup_latitude, testData$pickup_longitude, testData$dropoff_latitude, testData$dropoff_longitude)

# Remove latitude and longitude variables
testData = subset(testData,select = -c(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude))

# Feature Scaling
testData$distance = log1p(testData$distance)

# Predictions using the trained models
# Linear Regression
test_predictions_LR = predict(model_LR, testData)
#View(1/(log1p(test_predictions_LR)), title = 'Predicted Fare Amount')
testResult_LR = data.frame(testData)
testResult_LR$fare_amount = expm1(test_predictions_LR)
View(testResult_LR, title = 'Linear Regression Result')

# Decision Tree
test_predictions_DT = predict(model_DT, testData)
#View(1/(log1p(test_predictions_DT)), title = 'Predicted Decision Tree')
testResult_DT = data.frame(testData)
testResult_DT$fare_amount = expm1(test_predictions_DT)
View(testResult_DT, title = 'Decision Tree Result')

# Random Forest
test_predictions_RF = predict(model_RF, testData)
#View(1/(log1p(test_predictions_RF)), title = 'Predicted Random Amount')
testResult_RF = data.frame(testData)
testResult_RF$fare_amount = expm1(test_predictions_RF)
View(testResult_RF, title = 'Random Forest Result')

###########################################################################
# # # # # # # # DONE # # # # # # # # # 






