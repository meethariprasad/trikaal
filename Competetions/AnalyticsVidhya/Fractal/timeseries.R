setwd("~/resources/rstudio/fractal-hiring")

#Unzip Train & Test in the present working directory.

#Problem Statement
# Welcome to Antallagma - a digital exchange for trading goods. Antallagma started its operations 
#5 years back and has 
# supported more than a million transactions till date. 
#The Antallagma platform enables working of a traditional exchange on 
# an online portal. 
# 
# On one hand, buyers make a bid at the value they are willing to buy ("bid value”) 
#and the quantity they are willing to buy. 
#                                                                      
#Sellers on the other hand, ask for an ask price and the quantity they 
#are willing to sell. The portal matches the buyers 
#                                                                      
#and sellers in realtime to create trades. All trades are settled at the end 
#of the day at the median price of all agreed trades. 
#                                                                      
# You are one of the traders on the exchange and can supply all the material being 
#traded on the exchange. In order to 
# improve your logistics, you want to predict the median trade prices and volumes for 
#all the trades happening (at item 
# level) on the exchange. You can then plan to then use these predictions to create an 
#optimized inventory strategy. 
#                                                                      
# You are expected to create trade forecasts for all items being traded on Antallagma along 
#with the trade prices for a 
# period of 6 months. 
# Evaluation Criteria: 
# Overall Error = Lambda1 x RMSE error of volumes + Lambda2 x RMSE error of prices 
# Where Lambda1 and Lambda2 are normalising parameters 

# Data
# Data Dictionary:
# Variable	Definition
# ID	      Unique_transaction_ID
# Item_ID	  Unique ID of the product
# Datetime	Date of Sale
# Price	    Median Price at Sale on that day(Target Variable_1)
# Number_Of_Sales	Total Item Sold on that day(Target Variable_2)
# Category_1	Unordered Masked feature
# Category_2	Ordered Masked feature
# Category_3	Binary Masked feature

#Data Load.

test <- read.csv("test.csv")

train <- read.csv( "train.csv")

#Exploratory Analysis

names(train)
names(test)

#Found that test & train columns are in different order. Let us fix that first.
test<-test[,c(6,1,2,5,4,3)]
#Test it
names(train)
names(test)
#Make columns equal
test$Price<-NA
test$Number_Of_Sales<-NA

#Let us merge them in one 
#Let us store the number of rows in train & test in variables for future reference.
nrowtest<-nrow(test)
nrowtrain<-nrow(train)
#Let us combine test & train now.
combi<-rbind(train,test)

#Let us check the datatypes.
str(combi)

#Let us check variables one at a time.
#ID
summary(combi$ID)
#Analysis: The ID has too many levels as well as it is not contributing.
combi$ID<-NULL

#ITEM_ID
#Analysis: The ID of Item. 

#I hypothize that trades are Item & Time Specific. We can't generalize them across.
#So we have to model train it by grouping Itemwise then datewise.
#Predict for each item.

#Let us check if there are any NA in other columns.
summary(combi$Category_1)
summary(combi$Category_2)
#There are huge number of NA.
#We have 3 choice. Predictive replacement
#Replace it with Mean
#or ignore columns.
#let us do predictive replacement after checking Cat3
#Category 3
summary(combi$Category_3)
#It is a binary factor. Better to make it so.
combi$Category_3<-as.factor(combi$Category_3)
summary(combi$Category_3)
#Let us revisit the summary again.

summary(combi)
#Item ID needs to be Factorized.
combi$Item_ID<-as.factor(combi$Item_ID)

#Let us check combi again
summary(combi)

train<-combi[1:nrowtrain,]
test<-combi[(nrowtrain+1):nrow(combi),]

#Let us predictively remove NA from Cat2
#Oops! Missforest is not working becaue of too many levels.
#Let us replace it with Mean.
combi$Category_2[is.na(combi$Category_2)] <- mean(na.omit(train$Category_2))

#Ok. Now we have some modeling.
train<-combi[1:nrowtrain,]
test<-combi[(nrowtrain+1):nrow(combi),]

#So we have do multiple target predictions, Price & Sales.
#Let us just order the data.
train <- train[order(train$Item_ID, train$Datetime),]
test <- test[order(test$Item_ID, test$Datetime),]

#Also found that test & train has same levels
#Now let us add some more columns like weekday, month, quarter
#Let us combine test & train now.
combi<-rbind(train,test)

#Feature Engineering
#First let us make Datetime column data type as date using Lubridate package.
library(lubridate)
combi$Datetime<-lubridate::ymd(combi$Datetime)

#Added Weekday column.
#Hypothesis is the trade of certain goods might be dependent on day of the week.
combi$Wday<-wday(combi$Datetime)

#Added Quarter Day column
#Hypothesis is the trade of certain goods might be dependent on day of quarter.
combi$Qday<-qday(combi$Datetime)

#I could have added month day to check improvement in model data.
# But not added as I believe might except begining and end of the month, rest of the data patern might be explained better in Weekday variable.



#Ok. Now we have some modeling.
train<-combi[1:nrowtrain,]
test<-combi[(nrowtrain+1):nrow(combi),]

summary(train)

summary(test)

#First we will predict volume. Then we will predict the price.
#Hypothesis is: Find the trade volume first. Prices are typically driven by trade volume 
#Yes it can happen other way around also. I might have done both the ways and ensembled the results maybe!
#That is the work for further improvement section.

#Using H2o because of huge data and I joined 30 minutes before stop clock to competetion!
library("h2o")
h2o.init(nthreads = -1,max_mem_size = "15g")
y<-c("Number_Of_Sales","Price")
features=names(train)[!names(train) %in% c("Number_Of_Sales","Price","Datetime")]

train.hex<-as.h2o(train[,c(features,y)])

#Models
gbmF_model_sales = h2o.gbm( x=features,
                            y = y[1],
                            training_frame =train.hex
                            ,nfolds = 3 #Commented for nfolds for saving time.
                            ,nbins=50
                            ,learn_rate = 0.001
                            ,ntrees = 800
                            ,distribution="AUTO"
                            ,max_depth=10
                            ,min_rows=20
                            ,col_sample_rate_per_tree=0.6
                            ,col_sample_rate=0.8
                            ,sample_rate=0.6
                            ,nbins_cats = 2200#Ideally I would have went for number of unique levels in Item ID. Whatever I had was greater than that. So kept it as is.
                            ,seed=1234
)

gbmF_model_sales

gbmF_model_price = h2o.gbm( x=c(features,y[1]),
                            y = y[2],
                            training_frame =train.hex
                            ,nfolds = 3 #Commented for nfolds for saving time.
                            ,nbins=50
                            ,learn_rate = 0.001
                            ,ntrees = 800
                            ,distribution="AUTO"
                            ,max_depth=10
                            ,min_rows=20
                            ,col_sample_rate_per_tree=0.6
                            ,col_sample_rate=0.8
                            ,sample_rate=0.6
                            ,nbins_cats = 2200#Ideally I would have went for number of unique levels in Item ID. Whatever I had was greater than that. So kept it as is.
                            ,seed=1234
)

gbmF_model_price



test.hex<-as.h2o(test[,c(features)])

#Prediction

#We will start by predicting sales volume then price
predict_gbm_sales = h2o.predict(gbmF_model_sales, newdata = test.hex)

#Merge the prediction with test data to further predict sales.
test.hex$Number_Of_Sales<-predict_gbm_sales$predict

#Now we will predict price.
predict_gbm_price = h2o.predict(gbmF_model_price, newdata = test.hex)
test.hex$Price<-predict_gbm_price$predict

#Let us put submission script in place.
test.frame<-as.data.frame(test.hex)
#Oops! I lost test ID in the process. Let me bring it back properly. Not a great way to code!
test <- read.csv("test.csv")
test <- test[order(test$Item_ID, test$Datetime),]
ID=as.character(test$ID)
submit<-(data.frame(cbind(ID=ID,test.frame$Number_Of_Sales,test.frame$Price)))
names(submit)<-c("ID","Number_Of_Sales","Price")
write.csv(submit,file = "submit_gbm.csv",row.names = F)

h2o.shutdown(prompt=F)

#What could have been done to improve the result.

#Addition of Lagging Features
#Merging past and present features at row level itemwise & daywise & predicting the Price & Volume
#Ensemble of following:
# Uncorelated Algorithm Results.
# Price Driving Volume & Volume Driving Price.

