setwd("/resources/rstudio/clickbait")
#train: https://www.dropbox.com/s/n242yrz9pmzvbgu/train.zip?raw=1
#test: https://www.dropbox.com/s/0ridbjspcq01wlg/test.zip?raw=1

# Data
# The sample data is provided of 14 days, from 21st August to 3rd September. The train file (1.1GB size) contains sample data from first 10 days whereas you are asked to predict on sample data of last four days in the test file (~0.5GB). Also, the public leaderboard would take into account the first day of test predictions, whereas private leaderboard will take into account rest three days of test predictions. Below is the data dictionary:
#   
#   Variable
# Description
# ID
# Unique ID of click
# Country Country Code
# Carrier Wireless Network Operator Code
# TrafficType Whether the advertisement is for Adults or Mainstream
# ClickDate Date at which the advertisement was clicked
# Device Type of Device from which advertisement was clicked
# Browser Type of Browser from which advertisement was clicked
# OS Type of OS from which advertisement was clicked
# RefererUrl Url of Source website
# UserIp IP of User who clicked
# publisherId Unique ID of publisher
# subPublisherId Unique ID of sub publisher
# advertiserCampaignId Unique ID of campaign of advertisement
# Fraud If the click was fraud or not
# ConversionStatus Was the click Converted or not
# ConversionDate Date of Conversion if Conversion happens
# ConversionPayOut Pay Out of Conversion if Conversion happens (in rupees)

#Step 1: Data Download
#Step 2: Read csv from zip files, without unzipping effectively. data.table or h2o.import or load in spark?
#Step 3: Combine Train and Test to get optimized feature modifications
#Step 4: Go through the each column of the data and understand it.
#Step 5: Create a hypothesis for additional features.
#Step 6: Create models & check their performance in terms of 2-3 fold CV.
#Step 6b: Ensemble uncorelated models.
#Step 7: Predict on test data & upload to leaderboard.
#Step 8: Improve the models by hyperparameter tuning and upload again.

#Step 1: Data Download

#Train Data Load: Comment after download
# library(httr)
# GET("https://www.dropbox.com/s/jss19ywmc9uy0p8/train.csv?raw=1",
#                write_disk("train.csv"),
#                progress())

#Test Data Load
#GET("https://www.dropbox.com/s/0ridbjspcq01wlg/test.zip?raw=1",
#    write_disk("test.zip"),
#    progress())
#unzip("test.zip")

#Step 2: Read csv from zip files, without unzipping effectively. data.table or h2o.import or load in spark?

#Reading the train.zip in effective way.

# #Method 1: Spark
# sqlContext <- sparkRSQL.init(sc)
# train_file_path <- file.path('', 'resources','rstudio','clickbait','train.csv')
# 
# system.time(
#   train_df <- read.df(sqlContext, 
#                           train_file_path, 
#                           header='true', 
#                           source = "com.databricks.spark.csv", 
#                           inferSchema='true')
# )

#Method 2: Data.Table
library(data.table)
system.time(train <- fread("train.csv", stringsAsFactors = T))
system.time(test <- fread("test.csv", stringsAsFactors = T))
ID<-test$ID

load("source.RData")

#Save the file to load in later point of time

#Considering the huge data going to omit NA.
train<-(train[complete.cases(train),])

save(list = ls(all = TRUE), file= "source.RData")

#Step 3: Combine Train and Test to get optimized feature modifications
names(train)
names(test)

#ConversionStatus,ConversionDate missing in test.
# Does that mean we need to predict them? Or only converstion status prediction is enough?
# Does the Converstion Date feature engineering will help, like conversion after how many days?

#ID is not needed. Can be removed from both.
#For prediction purposes Test ID is saved already.
train$ID<-NULL
test$ID<-NULL

l = list(train,test)
test.rows<-nrow(test)
train.rows<-nrow(train)

combi<-rbindlist(l, use.names=TRUE,fill=TRUE)

#Removing the large objects for memory optimization.
rm(train,test,l)

save(ID,combi,test.rows,train.rows, file= "combi.RData")


#Step 4: Go through the each column of the data and understand it.
load("combi.RData")
names(combi)

# "Country"
summary(combi$Country)

levels(combi$Country)[levels(combi$Country) == ""] <- "IN"
levels(combi$Country)[levels(combi$Country) == "**"] <- "IN"
levels(combi$Country)
combi$Country<-as.factor(as.numeric(combi$Country))
levels(combi$Country)
#Q.1 What is its relation with target variable, 
# in training rows. 
#1: ConversionStatus 2. ConversionPayOut
#Q2: What can be the other features engineering here?
# Continent? High Volume Countries? Low Volume Countries?

# "Carrier"
combi$Carrier<-as.factor(combi$Carrier)
levels(combi$Carrier)
#Q.1 What is its relation with target variable, 
# in training rows. 
#1: ConversionStatus 2. ConversionPayOut
#Q2: What can be the other features engineering here?

# "TrafficType"
combi$TrafficType<-as.factor(combi$TrafficType)
levels(combi$TrafficType)

levels(combi$TrafficType)[levels(combi$TrafficType) == ""] <- "A"

combi$TrafficType<-as.factor(as.numeric((combi$TrafficType)))

#Q.1 What is its relation with target variable, 
# in training rows. 
#1: ConversionStatus 2. ConversionPayOut
#Q2: What can be the other features engineering here?

# "ClickDate"
combi$ClickDate<-NULL
#Q.1 What is its relation with target variable, 
# in training rows. 
#1: ConversionStatus 2. ConversionPayOut
#Q2: What can be the other features engineering here?

# "Device"   
combi$Device<-as.factor(as.numeric((combi$Device)))
levels(combi$Device)
#Q.1 What is its relation with target variable, 
# in training rows. 
#1: ConversionStatus 2. ConversionPayOut
#Q2: What can be the other features engineering here?

# "Browser"      
levels(combi$Browser)
combi$Browser<-as.factor(as.numeric((combi$Browser)))
levels(combi$Browser)
#Q.1 What is its relation with target variable, 
# in training rows. 
#1: ConversionStatus 2. ConversionPayOut
#Q2: What can be the other features engineering here?

# "OS"  
levels(combi$OS)
combi$OS<-as.factor(as.numeric((combi$OS)))
levels(combi$OS)
#Q.1 What is its relation with target variable, 
# in training rows. 
#1: ConversionStatus 2. ConversionPayOut
#Q2: What can be the other features engineering here?

# "RefererUrl"  
combi$RefererUrl<-NULL
#Q.1 What is its relation with target variable, 
# in training rows. 
#1: ConversionStatus 2. ConversionPayOut
#Q2: What can be the other features engineering here?

# "UserIp"      
combi$UserIp<-NULL
#Q.1 What is its relation with target variable, 
# in training rows. 
#1: ConversionStatus 2. ConversionPayOut
#Q2: What can be the other features engineering here?

# "ConversionStatus"   
summary(combi$ConversionStatus)
combi$ConversionStatus<-as.factor(as.numeric(combi$ConversionStatus))
summary(combi$ConversionStatus)
#Q.1 What is its relation with target variable, 
# in training rows. 
#1: ConversionStatus 2. ConversionPayOut
#Q2: What can be the other features engineering here?

# "ConversionDate"  
combi$ConversionDate<-NULL
#Q.1 What is its relation with target variable, 
# in training rows. 
#1: ConversionStatus 2. ConversionPayOut
#Q2: What can be the other features engineering here?

# "ConversionPayOut"  
summary(combi$ConversionPayOut)
#Q.1 What is its relation with target variable, 
# in training rows. 
#1: ConversionStatus 2. ConversionPayOut
#Q2: What can be the other features engineering here?

# "publisherId"  
combi$publisherId<-as.factor(combi$publisherId)
#Q.1 What is its relation with target variable, 
# in training rows. 
#1: ConversionStatus 2. ConversionPayOut
#Q2: What can be the other features engineering here?

# "subPublisherId"  
levels(combi$subPublisherId)
combi$subPublisherId<-as.factor(as.numeric(combi$subPublisherId))
levels(combi$subPublisherId)
#Q.1 What is its relation with target variable, 
# in training rows. 
#1: ConversionStatus 2. ConversionPayOut
#Q2: What can be the other features engineering here?


# "advertiserCampaignId" 
levels(combi$advertiserCampaignId)
combi$advertiserCampaignId<-as.factor(as.numeric(combi$advertiserCampaignId))
#Q.1 What is its relation with target variable, 
# in training rows. 
#1: ConversionStatus 2. ConversionPayOut
#Q2: What can be the other features engineering here?

# "Fraud"
combi$Fraud<-as.factor(combi$Fraud)
levels(combi$Fraud)
#Q.1 What is its relation with target variable, 
# in training rows. 
#1: ConversionStatus 2. ConversionPayOut
#Q2: What can be the other features engineering here?

summary(combi)

train.final <- combi[1:train.rows,]
test.final <- combi[-(1:train.rows),]

rm(combi)
save(ID,train.final,test.final,train.rows,test.rows, file= "finaltrain.RData")

#Step 5: Create a hypothesis for any additional features.

#Step 6: Create models & check their performance in terms

#Model ConversionStatus: 

# This model is for ConvertionStatus
# c.train <- combi[1:nrow(train),]
# c.test <- combi[-(1:nrow(train)),]
# 

#dependent variable (Index of the target1: "ConversionStatus")
y.dep <- 7

#independent variables (dropping target, "ConversionPayOut")
x.indep <- c(1:6,9:12)

library(h2o)
h2o.init(nthreads = -1)
# 
train.h2o <- as.h2o(train.final)
test.h2o <- as.h2o(test.final)

# It is a classification prediction.

gbm.model.a <- h2o.gbm( y = y.dep, x = x.indep,
                    training_frame = train.h2o
                    )

#Predict the Convertion Status of Test using the model.
pred.cstatus <- h2o.predict( gbm.model.a,newdata =test.h2o)

#Append the convertion status to test data.

#test.h2o$ConversionStatus<-pred.cstatus

#Model ConvertionPayout: This model is for ConvertionPayout

#dependent variable (Index of the target1: "ConversionStatus")
y.dep <- 8

#independent variables (dropping target, "ConversionPayOut")
x.indep <- c(1:7,9:12)


#Model Convestion Payout: Final Model
gbm.model.b <- h2o.gbm( y = y.dep, x = x.indep,
                        training_frame = train.h2o
)

#Step 6b: Ensemble uncorelated models, if needed.

#Step 7: Predict on test data & upload to leaderboard.

pred.cpayout <- h2o.predict( gbm.model.b,newdata =test.h2o)

#Final Submission: data.frame(ID=ID,)
#Step 8: Improve the models by hyperparameter tuning and upload again.

