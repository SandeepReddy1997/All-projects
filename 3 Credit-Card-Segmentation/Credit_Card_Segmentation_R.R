# Remove all variables
rm(list=ls())

# Load libraries
library(tidyverse)
library(dplyr)

# Set Working Directory
setwd("C:\\Users\\SANDEEP\\Downloads\\EdWisor Data Science\\Portfolio\\Project 1. Credit Card Segmentation")

# Load Data
credit_data = read.csv('credit-card-data.csv')

# Dimensions of the data
dim(credit_data)

# Structure of credit_data
str(credit_data)

# Summary
summary(credit_data)

####################################################
######### Missing Value Analysis #################

calMissingValues <- function(data){
  missing_values <<- data.frame(apply(data, 2, function(x) sum(is.na(x))))
  missing_values$Columns <<- row.names(missing_values)
  row.names(missing_values) <<- NULL
  names(missing_values)[1] <<- 'Num_Of_Missing_Values'
  missing_values <<- missing_values[order(-missing_values$Num_Of_Missing_Values),]
  missing_values <<- missing_values[, c(2, 1)]
}

# See sum of Missing values in each column
View(calMissingValues(credit_data), title = 'credit_data_Missing_Values')

# Calculate missing value percentage in minimum_payments column
(sum(is.na(credit_data$MINIMUM_PAYMENTS))/nrow(credit_data)) * 100
# Since only 3.4% ie., 313 rows out of 8950 rows contain missing values. 
# we can impute them or delete the respective rows. For now lets delete
credit_data = na.omit(credit_data)

# Check the missing values
View(calMissingValues(credit_data), title = 'credit_data_Missing_Values')

######################################################
########### Deriving KPI's ########################

data_KPI = data.frame(credit_data['CUST_ID'])

# Monthly Average Purchase
data_KPI['Monthly_Average_Purchases'] = credit_data$PURCHASES/credit_data$TENURE

# Monthly Cash Advanced Amount
data_KPI['Monthly_Cash_Advanced_Amount'] = credit_data$CASH_ADVANCE/credit_data$TENURE

# Purchase Type
# Seeing the data, we can infer that there are two types of Purchases. ONE_OFF_PURCHASE and INSTALLMENT_PURCHASE
# Let us Explore little more
# %>% can be found in tidyverse package.
aa = credit_data %>% filter(credit_data$ONEOFF_PURCHASES == 0, credit_data$INSTALLMENTS_PURCHASES == 0) 
dim(aa)
cat('Therefore', dim(aa)[1], 'Clients use either OneOff_Purchases or Installment_Purchases.')

bb = credit_data %>% filter(credit_data$ONEOFF_PURCHASES > 0, credit_data$INSTALLMENTS_PURCHASES == 0) 
dim(bb)
cat('There are', dim(bb)[1], 'Clients use only OneOff_Purchases.')

cc = credit_data %>% filter(credit_data$ONEOFF_PURCHASES == 0, credit_data$INSTALLMENTS_PURCHASES > 0) 
dim(cc)
cat('Therefore', dim(cc)[1], 'Clients use only Installment_Purchases.')

dd = credit_data %>% filter(credit_data$ONEOFF_PURCHASES > 0, credit_data$INSTALLMENTS_PURCHASES > 0) 
dim(dd)
cat('Therefore', dim(dd)[1], 'Clients use BOTH OneOff_Purchases or Installment_Purchases.')

# Result: We found that there are FOUR types of Purchase behaviour in the data.
# 1. People who does not make any purchases.
# 2. People who make both types of purchases.
# 3. People who make only OneOff_Purchases
# 4. People who make only Installment_Purchases

View(calMissingValues(data_KPI), title = 'data_KPI_Missing_Values')

# User Defined function to catogorise the Purchase behaviour of the data
purchaseType <- function(credit_data){
  if ((credit_data['ONEOFF_PURCHASES'] == 0) && (credit_data['INSTALLMENTS_PURCHASES'] == 0)){
    return <- 'Not_Any'
  }
  else if ((credit_data['ONEOFF_PURCHASES'] > 0) && (credit_data['INSTALLMENTS_PURCHASES'] == 0)){
    return <- 'Only_OneOff'
  }
  else if((credit_data['ONEOFF_PURCHASES'] == 0) && (credit_data['INSTALLMENTS_PURCHASES'] > 0)){
    return <- 'Only_Installments'
  }
  else if((credit_data['ONEOFF_PURCHASES'] > 0) && (credit_data['INSTALLMENTS_PURCHASES'] > 0)){
    return <- 'Both'
  }
}

for (row in 1:nrow(credit_data)){
  data_KPI$Purchase_Type[row] = purchaseType(credit_data[row,])
}

View(data_KPI, title = 'data_KPI')

View(table(data_KPI$Purchase_Type), title = 'GroupBy_PurchaseType')

# Plotting a bar graph for Purchase Type vs Count
counts <- table(data_KPI$Purchase_Type)
barplot(counts, main = 'Distribution for Purchase Type', xlab='Purchase Type', ylab='Count')

# Limit Usage [BALANCE to CREDIT_LIMIT Ratio]
data_KPI$Limit_Usage = credit_data$BALANCE/credit_data$CREDIT_LIMIT

# Payments to Minimum_Payments Ratoi
data_KPI$Payments_to_Minimum_payments_Ratio = credit_data$PAYMENTS/credit_data$MINIMUM_PAYMENTS

# Calculate the missing values
View(calMissingValues(data_KPI))

# If there are any NULL/NA/NaN values in data_KPI. Run the below TWO lines of code, and check the missing values.
# Filling NaN values with Zero(0)
data_KPI$Monthly_Average_Purchases[is.na(data_KPI$Monthly_Average_Purchases)] <- 0
data_KPI$Monthly_Cash_Advanced_Amount[is.na(data_KPI$Monthly_Cash_Advanced_Amount)] <- 0

# Checking for missing values
View(calMissingValues(data_KPI), title = 'data_KPI_Missing_Values')

# Final Values of data_KPI
View(data_KPI, title = 'data_KPI')

#### Insights From New KPI's #################

# Average Payment to minimum Payment Ratio for each Purchase Type
a1 <- data.frame(data_KPI %>% group_by(.$Purchase_Type) %>% summarise(Avg_Pymt_minPymt_Ratio = mean(Payments_to_Minimum_payments_Ratio)))
# Ploting Bar Graph for Average Payment to minimum Payment Ratio VS Purchase Type
barplot(unlist(a1[2]), names.arg = unlist(a1[1]), xlab='Purchase Type', ylab='Average Payment to minimum Payment Ratio', main = 'Average Payment to minimum Payment Ratio for each Purchase Type')
# Insight 1: Clients with Purchase Type Installment are with more dues.

# Average Limit Usage for each Purchase Type
a2 = data.frame(data_KPI %>% group_by(.$Purchase_Type) %>% summarise(Avg_Limit_Usage = mean(Limit_Usage)))
# Plotting Bar Graph for Average Limit Usage VS Purchase Type
barplot(unlist(a2[2]), names.arg = unlist(a2[1]), xlab = 'Purchase Type', ylab = 'Average Limit Usage', main = 'Average Limit Usage for each Purchase Type')
# Insight 2: Clients with Purchase type Installment have good Credit Score

# Average Monthly Cash Advance for each Purchase Type
a3 = data.frame(data_KPI %>% group_by(.$Purchase_Type) %>% summarise(Monthly_Cash_Advance__ = mean(Monthly_Cash_Advanced_Amount)))
# Plotting Bar Graph for Average Monthly Cash Advance VS Purchase Type
barplot(unlist(a3[2]), names.arg = unlist(a3[1]), xlab = 'Purchase Type', ylab = 'Average Monthly Cash Advance', main = 'Average Monthly Cash Advance for each Purchase Type')
# Insight 3: Clients who dont do any type of Purchases(OneOff, Installment), Take more Cash on Advance.

###################################################
######### Outliers Analysis ######################

# boxplot for credit_data
boxplot(credit_data, horizontal = F)

# col_names = names(credit_data)
# removing outliers for variables
# for(i in col_names){
#   val = credit_data[,i][credit_data[,i] %in% boxplot.stats(credit_data[,i])$out]
#   credit_data = credit_data[which(!credit_data[,i] %in% val), ]
# }

print('Since it removes 90% of the data, lets not remove it.')

############################################################
######## Feature Selection #################################

# Selecting features for Clustering
library(caret)
correlation_matrix = cor(credit_data[,2:ncol(credit_data)])
heatmap(correlation_matrix, main = 'Correlation Matrix')
highly_corelated = findCorrelation(correlation_matrix, cutoff=0.8, verbose = T, names = FALSE)

View(credit_data[,c('PURCHASES_FREQUENCY', 'PURCHASES_INSTALLMENTS_FREQUENCY')])
View(credit_data[,c('PURCHASES', 'ONEOFF_PURCHASES')])

# Since PURCHASES_FREQUENCY and PURCHASES_INSTALLMENTS_FREQUENCY have high correlation, remove any one.
# Sameway.. Since PURCHASES and ONEOFF_PURCHASES has high correlation, remove any one.
# Also remove 1st column since it is customer_ID and has no dependency on clustering
# Index 1 = CUST_ID, Index 5 = ONEOFF_PURCHASE, Index 10 = PURCHASES_INSTALLMENTS_FREQUENCY
cnames = names(credit_data)[-c(1,5,10)]

credit_data_ForClustering = credit_data[, cnames]

###############################################################
######### Feature Scaling ###################################

# Plotting histogram for each column
for(i in cnames){
  hist(credit_data_ForClustering[, i])
}

# Since data is nor uniformly/normally distributed, we apply normalization
# First convert all columns to numeric
credit_data_ForClustering[] <- lapply(credit_data_ForClustering[], as.numeric) # convert to numeric
str(credit_data_ForClustering) # check whether all columns are converted to numeric

Normalized_Credit_data_ForClustering <- credit_data_ForClustering #copying from one variable to another

for(i in cnames){
  print(i)
  Normalized_Credit_data_ForClustering[,i] = (Normalized_Credit_data_ForClustering[,i] - min(Normalized_Credit_data_ForClustering[,i]))/
    (max(Normalized_Credit_data_ForClustering[,i]) - min(Normalized_Credit_data_ForClustering[,i]))
}

View(calMissingValues(Normalized_Credit_data_ForClustering), title='Normalized Values')

##########################################################
########## Cluster Analysis ##############################

library(NbClust)

# NbClust is used to find the Optimal number of clusters for k-means 
NB_Clust_Result = NbClust(Normalized_Credit_data_ForClustering[,2:ncol(Normalized_Credit_data_ForClustering)], min.nc = 2, max.nc = 15, method = 'kmeans')

# K-means clustering
NumberOfClusters = 4
kmeans_model = kmeans(Normalized_Credit_data_ForClustering, NumberOfClusters, nstart = 25)

credit_data$CLUSTER = kmeans_model$cluster 

credit_data = credit_data[, c(1, 19, 2:18)] # re-arranging columns

credit_data = credit_data[order(credit_data$CLUSTER), ] # re-arranging rows

# Number of Clients is each cluster
table(credit_data$CLUSTER)

for(i in 1:(NumberOfClusters+1)){
  assign(paste0("Cluster_", i), credit_data[credit_data['CLUSTER'] == i,])
}

# we can use the summary to gain information on each cluster.
summary(Cluster_1)
summary(Cluster_2)
summary(Cluster_3)
summary(Cluster_4)

