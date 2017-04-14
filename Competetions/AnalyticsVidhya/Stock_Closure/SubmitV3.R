#Tried Some Serious Feature Engineering.

# Variable	                      Definition
# ID	                            Unique ID for each observation
# Timestamp	                      Unique value representing one day
# Stock_ID	                      Unique ID representing one stock
# Volume	                        Normalized values of volume traded of given stock ID on that timestamp
# Three_Day_Moving_Average	      Normalized values of three days moving average of Closing price for given stock ID (Including Current day)
# Five_Day_Moving_Average       	Normalized values of five days moving average of Closing price for given stock ID (Including Current day)
# Ten_Day_Moving_Average	        Normalized values of ten days moving average of Closing price for given stock ID (Including Current day)
# Twenty_Day_Moving_Average	      Normalized values of twenty days moving average of Closing price for given stock ID (Including Current day)
# True_Range	                    Normalized values of true range for given stock ID
# Average_True_Range	            Normalized values of average true range for given stock ID
# Positive_Directional_Movement	  Normalized values of positive directional movement for given stock ID
# Negative_Directional_Movement	  Normalized values of negative directional movement for given stock ID
# Outcome	                        Binary outcome variable representing whether price for one particular stock at the tomorrow’s market close is higher(1) or lower(0) compared to the price at today’s market close

#All right around 8 GB available.
temp <- tempfile()
download.file('https://github.com/meethariprasad/trikaal/raw/master/Competetions/AnalyticsVidhya/Stock_Closure/test_6lvBXoI.zip',temp)
test <- read.csv(unz(temp, "test.csv"))
unlink(temp)


temp <- tempfile()
download.file('https://github.com/meethariprasad/trikaal/raw/master/Competetions/AnalyticsVidhya/Stock_Closure/train_xup5Mf8.zip',temp)
#Please wait for 60 Mb file to load.
train <- read.csv(unz(temp, "train.csv"))
unlink(temp)

#Let us Order it by Stock ID and Time Stamp.

test<-test[ order(test$Stock_ID,test$timestamp), ]

train<-train[ order(train$Stock_ID,train$timestamp), ]

save(test,train,file = "base.RData")

train$Stock_ID
test$Stock_ID

length(unique(train$Stock_ID))
length(unique(test$Stock_ID))

load("base.RData")
#Create Empty Response SalePrice
test$Outcome<-NA
ID<-test[,1]

combi<-rbind(train,test)

save(ID,combi,file = "combi.RData")

setwd("D:/DataScience/Kaggle/houseowner")

load("combi.RData")

#Copy orginal dataframe to impute
combi.imp<-combi
rm(train,test)
summary(combi.imp)

#Handling Variables
combi.imp$Outcome<-as.factor(combi.imp$Outcome)
combi.imp$Stock_ID<-as.factor(combi.imp$Stock_ID)
summary(combi.imp)

#We have NA to Handle.
# #Train and Test
train.complete<-combi.imp[1:702739,]
test.complete<-combi.imp[702740:804685,]
summary(train.complete[which(train.complete$Stock_ID==2),])
summary(test.complete[which(test.complete$Stock_ID==2),])
View(train.complete[which(train.complete$Stock_ID==2),])

#See the NA present columns. 
#Case 1. It is present in Moving Averages.  See the number of NA.
#You can see that NA are genuinely not available values.
#Case 2: Volume, +/-PDM,TR,ATR all 
#Consider it is a time series based problem NA handling might be appropriate by putting
#Next available NA value in the series as we have already ordered test and train
library(zoo)
Outcome<-combi.imp$Outcome

combi.imp.test<-combi.imp[,1:3]
for(i in 4:(ncol(combi.imp)-1)){
  combi.imp.test<-cbind(combi.imp.test,na.locf(combi.imp[,i],fromLast = T))
}
combi.imp.test$Outcome<-Outcome

names(combi.imp.test)<-names(combi.imp)
combi.imp<-combi.imp.test
rm(combi.imp.test)

summary(combi.imp)

#Check Trend & Trend Strength
#http://www.investopedia.com/articles/technical/02/050602.asp
combi.imp$Trend<-ifelse((combi.imp$Positive_Directional_Movement)>(combi.imp$Negative_Directional_Movement),1,0)
combi.imp$Trend<-as.factor(combi.imp$Trend)
combi.imp$Trend_Strength<-abs((combi.imp$Positive_Directional_Movement)-(combi.imp$Negative_Directional_Movement))


#Remove ID
combi.imp<-combi.imp[,2:ncol(combi.imp)]
combi.imp$Stock_ID<-as.factor(combi.imp$Stock_ID)

#Feature engineering



#Trying Tripple or Double Cross Over Feature
#http://www.investopedia.com/terms/c/crossover.asphttp://www.investopedia.com/terms/c/crossover.asp

rank.df<-data.frame(t(apply(-combi.imp[,4:7], 1, rank, ties.method='min')))
names(rank.df)<-c("Rank3","Rank5","Rank10","Rank20")

rank.df.1020<-data.frame(t(apply(-combi.imp[,6:7], 1, rank, ties.method='min')))
names(rank.df.1020)<-c("Rank1020_10","Rank1020_20")

rank.df.510<-data.frame(t(apply(-combi.imp[,5:6], 1, rank, ties.method='min')))
names(rank.df.510)<-c("Rank510_5","Rank510_10")

rank.df.35<-data.frame(t(apply(-combi.imp[,4:5], 1, rank, ties.method='min')))
names(rank.df.35)<-c("Rank35_3","Rank35_5")

rank.df.310<-data.frame(t(apply(-combi.imp[,c(4,6)], 1, rank, ties.method='min')))
names(rank.df.310)<-c("Rank310_3","Rank310_10")

rank.df.320<-data.frame(t(apply(-combi.imp[,c(4,7)], 1, rank, ties.method='min')))
names(rank.df.320)<-c("Rank320_3","Rank320_20")

rank.df<-cbind(rank.df,
               rank.df.1020$Rank1020_10,rank.df.510$Rank510_5,rank.df.35$Rank35_3,
               rank.df.310$Rank310_3,rank.df.320$Rank320_3)
names(rank.df)<-c("Rank3_all","Rank5_all","Rank10_all","Rank20_all"
                  ,"Rank1020_10","Rank510_5","Rank35_3","Rank310_3","Rank320_3")

save(rank.df,file = "rank.RData")

#MACD Feature: http://www.investopedia.com/terms/m/macd.asp
#MACD>Signal, High Price, Low Price
combi.imp$MACD_3510<-combi.imp$Ten_Day_Moving_Average-combi.imp$Five_Day_Moving_Average
combi.imp$MACD_3510_Crossover<-ifelse(combi.imp$MACD_3510>combi.imp$Three_Day_Moving_Average,1,0)

combi.imp$MACD_3520<-combi.imp$Twenty_Day_Moving_Average-combi.imp$Five_Day_Moving_Average
combi.imp$MACD_3520_Crossover<-ifelse(combi.imp$MACD_3520>combi.imp$Three_Day_Moving_Average,1,0)


combi.imp$MACD_31020<-combi.imp$Twenty_Day_Moving_Average-combi.imp$Ten_Day_Moving_Average
combi.imp$MACD_31020_Crossover<-ifelse(combi.imp$MACD_31020>combi.imp$Three_Day_Moving_Average,1,0)

combi.imp$MACD_51020<-combi.imp$Twenty_Day_Moving_Average-combi.imp$Ten_Day_Moving_Average
combi.imp$MACD_51020_Crossover<-ifelse(combi.imp$MACD_51020>combi.imp$Five_Day_Moving_Average,1,0)

#Volume Size Feature:Volume Categorization. Less than First QR,Greater than 3 QR, In between
#http://www.investopedia.com/terms/a/averagedailytradingvolume.asp
summary(combi.imp$Volume)
combi.imp$Volume_Size<-ifelse(combi.imp$Volume<=quantile(combi.imp$Volume)[2],"low",combi.imp$Volume)
combi.imp$Volume_Size<-ifelse(combi.imp$Volume_Size>=quantile(combi.imp$Volume)[4] & combi.imp$Volume_Size!="low","high",combi.imp$Volume_Size)
combi.imp$Volume_Size<-ifelse(!combi.imp$Volume_Size %in% c("low","high"),"medium",combi.imp$Volume_Size)
combi.imp$Volume_Size<-as.factor(combi.imp$Volume_Size)

combi.imp<-cbind(combi.imp,rank.df)
summary(combi.imp)
save(combi.imp,file = "combi_imp.RData")
rm(list = ls())

load("combi_imp.RData")


#Making appropriate new features as factors.
#Modeling Plan


#Step 1: Sliding Window on Every StockID and remove TimeID. 
#Every row of previous time ID and appropriate Stock ID must go to next timestamp row.
#To do this add an empty row in begining with NA for copy of Stock ID Group
#Bind it with every group.
#Step 2: Model. Stock ID as factor will ensure predictions will be based on matching Stock ID & Related Patterns.
#Step 3: Predict

#Sliding Window

#from stock ID 1 to Stock ID N (We are going to include Stock ID, as it might be only
#thing which connects to domain specific feature set and 
#also test and train has same set of Stocks). 

#UPDATE!This assumption of Test and Train has same stock ID is found wrong! 
#But would have been a good approach.
#In case of test and train stock ID match

#get the subset of stock ID DF, Say it is past data.
#Slide it by one row by adding a Row 1 again in the begining to avoid NULL.
#Have same subset of stock ID. Call it current and add a copy of last row at the end.
#Merge both Data sets columnwise. C bind.
#Then add all individual data sets rowwise.
#Repeat the same process for test set.

#So the method left is run it without sliding window without StockID and only with new features.


combi.imp$MACD_3510_Crossover<-as.factor(combi.imp$MACD_3510_Crossover)
combi.imp$MACD_3520_Crossover<-as.factor(combi.imp$MACD_3520_Crossover)
combi.imp$MACD_31020_Crossover<-as.factor(combi.imp$MACD_31020_Crossover)
combi.imp$MACD_51020_Crossover<-as.factor(combi.imp$MACD_51020_Crossover)



#Train and Test
train.complete<-combi.imp[1:702739,]
test.complete<-combi.imp[702740:804685,]

summary(train.complete$timestamp)
summary(test.complete$timestamp)

summary(train.complete$Stock_ID)
summary(test.complete$Stock_ID)
#Brurtally removing NA cases as the the number of such cases are small.
#train.complete<-train.complete[complete.cases(train.complete),]


save(combi.imp,test.complete,train.complete,file = "combi_imp.RData")
rm(combi,combi.imp,test,train)

setwd("/resources/rstudio")

load("combi_imp.RData")

save(train.complete,file = "train.RData")



load("train.RData")
summary(train.complete)
names(train.complete)

library(h2o)
y<-c("Outcome")
#Almost removing all default features and high moving average overall ranks.
#Removing source features except Timestamp,StockID,Outcome
features=names(combi.imp)[!names(combi.imp) %in% c("Three_Day_Moving_Average"
                                                   ,"Five_Day_Moving_Average"
                                                   ,"Ten_Day_Moving_Average"
                                                   ,"Twenty_Day_Moving_Average"
                                                   ,"True_Range"
                                                   ,"Average_True_Range"
                                                   ,"Positive_Directional_Movement"
                                                   ,"Negative_Directional_Movement"
                                                   ,"Rank5_all"
                                                   ,"Rank10_all"
                                                   ,"Rank20_all"
                                                   ,"Stock_ID","timestamp","Outcome"
)]
str(combi.imp[features])

#features<-c("Negative_Directional_Movement","Positive_Directional_Movement")
h2o.shutdown(prompt=F)

library(audio)
wait(10)

#Check Linux system Memory size to allocate for h2o.
as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))/1000000

h2o.init(nthreads = -1,max_mem_size = "8g")

train.hex<-as.h2o(train.complete[,c(features,y)])

test.hex<-as.h2o(test.complete[,features])


#Models
gbmF_model_1 = h2o.gbm( x=features,
                        y = y,
                        training_frame =train.hex
                        ,nfolds = 2
                        ,nbins=50
                        ,learn_rate = 0.001
                        ,ntrees = 800
                        ,distribution="AUTO"
                        
                        ## stop as soon as loss doesn't improve by more than 0.1% on the validation set,
                        ## for 2 consecutive scoring events
                        #stopping_rounds = 2,
                        ,stopping_tolerance = 1e-3
                        ,stopping_metric = "logloss"
                        ,max_depth=10
                        ,min_rows=20
                        ,col_sample_rate_per_tree=0.6
                        ,col_sample_rate=0.8
                        ,sample_rate=0.6
                        ,nbins_cats = 2199
                        ,seed=1234
)


gbmF_model_1
#0.68

pca<-h2o.prcomp(train.hex,k=20,seed=1234)
pca
pca_data<-h2o.predict(pca,train.hex)
pca_train<-h2o.cbind(pca_data[1:7],train.hex$Outcome)

features=names(pca_train)[!names(pca_train) %in% "Outcome"]

gbmF_model_1 = h2o.gbm( x=features,
                        y = y,
                        training_frame =pca_train
                        ,nfolds = 2
                        ,nbins=50
                        ,learn_rate = 0.001
                        ,ntrees = 1000
                        ,distribution="AUTO"
                        ## stop as soon as loss doesn't improve by more than 0.1% on the validation set,
                        ## for 2 consecutive scoring events
                        #stopping_rounds = 2,
                        #,stopping_tolerance = 1e-3
                        #,stopping_metric = "logloss"
                        ,max_depth=10
                        ,min_rows=20
                        ,col_sample_rate_per_tree=0.6
                        ,col_sample_rate=0.8
                        ,sample_rate=0.6
                        ,nbins_cats = 2199
                        ,seed=1234
)

gbmF_model_1

# gbmF_model_1
# Model Details:
#   ==============
#   
#   H2OBinomialModel: gbm
# Model ID:  GBM_model_R_1492108318663_67 
# Model Summary: 
#   number_of_trees number_of_internal_trees model_size_in_bytes min_depth max_depth
# 1            1000                     1000             7926160        10        10
# mean_depth min_leaves max_leaves mean_leaves
# 1   10.00000        409        770   625.95600
# 
# 
# H2OBinomialMetrics: gbm
# ** Reported on training data. **
#   
#   MSE:  0.06899376
# RMSE:  0.2626666
# LogLoss:  0.2975916
# Mean Per-Class Error:  0.007731218
# AUC:  0.9997594
# Gini:  0.9995189
# 
# Confusion Matrix (vertical: actual; across: predicted) for F1-optimal threshold:
#   0      1    Error          Rate
# 0      382396   2060 0.005358  =2060/384456
# 1        3216 315067 0.010104  =3216/318283
# Totals 385612 317127 0.007508  =5276/702739
# 
# Maximum Metrics: Maximum metrics at their respective thresholds
# metric threshold    value idx
# 1                       max f1  0.475597 0.991697 200
# 2                       max f2  0.425087 0.993699 216
# 3                 max f0point5  0.515729 0.994023 188
# 4                 max accuracy  0.475597 0.992492 200
# 5                max precision  0.793066 1.000000   0
# 6                   max recall  0.290607 1.000000 280
# 7              max specificity  0.793066 1.000000   0
# 8             max absolute_mcc  0.475597 0.984851 200
# 9   max min_per_class_accuracy  0.457132 0.992309 205
# 10 max mean_per_class_accuracy  0.457132 0.992414 205
# 
# Gains/Lift Table: Extract with `h2o.gainsLift(<model>, <data>)` or `h2o.gainsLift(<model>, valid=<T/F>, xval=<T/F>)`
# 
# H2OBinomialMetrics: gbm
# ** Reported on cross-validation data. **
#   ** 2-fold cross-validation on training data (Metrics computed for combined holdout predictions) **
#   
#   MSE:  0.06967006
# RMSE:  0.2639509
# LogLoss:  0.299142
# Mean Per-Class Error:  0.008609118
# AUC:  0.9997013
# Gini:  0.9994026
# 
# Confusion Matrix (vertical: actual; across: predicted) for F1-optimal threshold:
#   0      1    Error          Rate
# 0      381878   2578 0.006706  =2578/384456
# 1        3346 314937 0.010513  =3346/318283
# Totals 385224 317515 0.008430  =5924/702739
# 
# Maximum Metrics: Maximum metrics at their respective thresholds
# metric threshold    value idx
# 1                       max f1  0.470176 0.990683 199
# 2                       max f2  0.415583 0.992809 218
# 3                 max f0point5  0.514326 0.993303 185
# 4                 max accuracy  0.470176 0.991570 199
# 5                max precision  0.793240 1.000000   0
# 6                   max recall  0.286317 1.000000 286
# 7              max specificity  0.793240 1.000000   0
# 8             max absolute_mcc  0.470176 0.982988 199
# 9   max min_per_class_accuracy  0.457694 0.991448 203
# 10 max mean_per_class_accuracy  0.457694 0.991482 203
# 
# Gains/Lift Table: Extract with `h2o.gainsLift(<model>, <data>)` or `h2o.gainsLift(<model>, valid=<T/F>, xval=<T/F>)`
# Cross-Validation Metrics Summary: 
#   mean           sd  cv_1_valid  cv_2_valid
# accuracy                 0.99156874 1.2473343E-4   0.9917451   0.9913923
# auc                      0.99970126 1.6716719E-7   0.9997015   0.9997011
# err                     0.008431264 1.2473343E-4 0.008254864 0.008607664
# err_count                    2962.5    44.194176      2900.0      3025.0
# f0point5                 0.99065596  5.673187E-4   0.9914583   0.9898537
# f1                       0.99069345 1.1650381E-4   0.9908582  0.99052864
# f2                        0.9907317 3.3438014E-4   0.9902588   0.9912046
# lift_top_group            2.2079172 0.0033516625   2.2126572   2.2031772
# logloss                    0.299142  3.588286E-5  0.29919276  0.29909128
# max_per_class_error      0.00948346  4.644746E-4 0.010140327 0.008826595
# mcc                       0.9829888 2.4486778E-4   0.9833351   0.9826425
# mean_per_class_accuracy   0.9914972 5.8420068E-5  0.99157983  0.99141455
# mean_per_class_error    0.008502805 5.8420068E-5 0.008420186 0.008585423
# mse                      0.06967006 2.9965556E-6   0.0696743  0.06966582
# precision                0.99063146  8.678242E-4   0.9918587  0.98940414
# r2                        0.7188255 8.5560234E-5  0.71870446   0.7189465
# recall                    0.9907577  6.350084E-4   0.9898597  0.99165577
# rmse                     0.26395085 5.6763515E-6   0.2639589  0.26394284
# specificity               0.9922367  7.518485E-4  0.99329996   0.9911734
# > 

#Unbelievable Cross Validation Results which matches with train results.
# This makes me ask questions on do we really need to break head on feature engi
#Let us find out.

load("combi.RData")

#Copy orginal dataframe to impute
combi.imp<-combi
rm(train,test,combi)
summary(combi.imp)

#Handling Variables
combi.imp$Outcome<-as.factor(combi.imp$Outcome)
combi.imp$Stock_ID<-as.factor(combi.imp$Stock_ID)
summary(combi.imp)

#We have NA to Handle.
# #Train and Test
train.complete<-combi.imp[1:702739,]
summary(train.complete)

train.hex<-as.h2o(train.complete)

#No feature engineering. Brainless PCA
pca_nofe<-h2o.prcomp(train.hex,k=10)
# #
# Error: java.lang.IllegalArgumentException: 
#   Found validation errors: ERRR on field: _train: 
#   Gram matrices (one per thread) won't fit in the driver node's memory 
# (236.309 TB > 6.06 GB) - try reducing the number of columns and/or the 
# number of categorical factors.
#So we need to get rid of unneccesary categorical column. ID
train.hex<-as.h2o(train.complete[,2:ncol(train.complete)])
pca_nofe<-h2o.prcomp(train.hex,k=10,seed=1234)
#Error: Status code 502 returned
#Let us clear out some space if that was the issue and run the same code again.
train.hex<-as.h2o(train.complete[,2:ncol(train.complete)])
pca_nofe<-h2o.prcomp(train.hex,k=10,seed=1234)
#Gets struck after 40% looks like we need to remove our Stock ID Variable too.
#As I said. Mindless FE.
train.hex<-as.h2o(train.complete[,-c(1,3)])
pca_nofe<-h2o.prcomp(train.hex,k=10,seed=1234)
#Ok. It ran Fast. Let us see, variablility.
pca_nofe
#Ah! Just one PCA explains all Variability. Is it good or nonsense!
pca_nofe_train<-h2o.predict(pca_nofe,train.hex)
#Taking long time than usual in above code. Typically 1 or 2 min max
#Error 502 again. What might be the problem? Time Stamp Maybe.
#Let us figure it out.
train.hex<-as.h2o(train.complete[,-c(1,3)])
pca_nofe<-h2o.prcomp(train.hex,k=10,seed=1234)
pca_nofe
pca_nofe_train<-h2o.predict(pca_nofe,train.hex)
pca_nofe_train
#Select the PCA with Maximum Variability
pca_nofe_train_data<-h2o.cbind(pca_nofe_train[1],train.hex$Outcome)

#Let us find it out with cross validation
features=names(pca_nofe_train_data)[!names(pca_nofe_train_data) %in% "Outcome"]
y<-"Outcome"

gbmF_model_1 = h2o.gbm( x=features,
                        y = y,
                        training_frame =pca_nofe_train_data
                        ,nfolds = 2
                        ,nbins=50
                        ,learn_rate = 0.001
                        ,ntrees = 1000
                        ,distribution="AUTO"
                        ## stop as soon as loss doesn't improve by more than 0.1% on the validation set,
                        ## for 2 consecutive scoring events
                        #stopping_rounds = 2,
                        #,stopping_tolerance = 1e-3
                        #,stopping_metric = "logloss"
                        ,max_depth=10
                        ,min_rows=20
                        ,col_sample_rate_per_tree=0.6
                        ,col_sample_rate=0.8
                        ,sample_rate=0.6
                        ,nbins_cats = 2199
                        ,seed=1234
)

#You can see that by no feature engineering your CV log loss is 0.66, AUC:0.66
#With Domain Knowledge Based Feature Eng: You can achieve amazing results.

# gbmF_model_1
# 
# 
# > gbmF_model_1
# Model Details:
#   ==============
#   
#   H2OBinomialModel: gbm
# Model ID:  GBM_model_R_1492108318663_139 
# Model Summary: 
#   number_of_trees number_of_internal_trees model_size_in_bytes min_depth max_depth
# 1            1000                     1000             1752483        10        10
# mean_depth min_leaves max_leaves mean_leaves
# 1   10.00000         87        196   133.72600
# 
# 
# H2OBinomialMetrics: gbm
# ** Reported on training data. **
#   
#   MSE:  0.2345559
# RMSE:  0.4843098
# LogLoss:  0.6613084
# Mean Per-Class Error:  0.4324567
# AUC:  0.6718296
# Gini:  0.3436593
# 
# Confusion Matrix (vertical: actual; across: predicted) for F1-optimal threshold:
#   0      1    Error            Rate
# 0      74106 310350 0.807245  =310350/384456
# 1      18355 299928 0.057669   =18355/318283
# Totals 92461 610278 0.467748  =328705/702739
# 
# Maximum Metrics: Maximum metrics at their respective thresholds
# metric threshold    value idx
# 1                       max f1  0.399263 0.646006 280
# 2                       max f2  0.315427 0.810694 352
# 3                 max f0point5  0.463849 0.591919 174
# 4                 max accuracy  0.466240 0.632344 164
# 5                max precision  0.734471 1.000000   0
# 6                   max recall  0.193547 1.000000 398
# 7              max specificity  0.734471 1.000000   0
# 8             max absolute_mcc  0.463849 0.253664 174
# 9   max min_per_class_accuracy  0.461407 0.624124 181
# 10 max mean_per_class_accuracy  0.460988 0.627053 182
# 
# Gains/Lift Table: Extract with `h2o.gainsLift(<model>, <data>)` or `h2o.gainsLift(<model>, valid=<T/F>, xval=<T/F>)`
# 
# H2OBinomialMetrics: gbm
# ** Reported on cross-validation data. **
#   ** 2-fold cross-validation on training data (Metrics computed for combined holdout predictions) **
#   
#   MSE:  0.2345057
# RMSE:  0.4842579
# LogLoss:  0.661284
# Mean Per-Class Error:  0.4309657
# AUC:  0.6725824
# Gini:  0.3451649
# 
# Confusion Matrix (vertical: actual; across: predicted) for F1-optimal threshold:
#   0      1    Error            Rate
# 0      77969 306487 0.797197  =306487/384456
# 1      20604 297679 0.064735   =20604/318283
# Totals 98573 604166 0.465452  =327091/702739
# 
# Maximum Metrics: Maximum metrics at their respective thresholds
# metric threshold    value idx
# 1                       max f1  0.399942 0.645410 286
# 2                       max f2  0.324230 0.809134 352
# 3                 max f0point5  0.461261 0.591989 191
# 4                 max accuracy  0.465293 0.630887 179
# 5                max precision  0.727131 0.984305   2
# 6                   max recall  0.234081 1.000000 399
# 7              max specificity  0.732698 0.999990   0
# 8             max absolute_mcc  0.458318 0.258632 199
# 9   max min_per_class_accuracy  0.460827 0.624485 192
# 10 max mean_per_class_accuracy  0.458318 0.629522 199
# 
# Gains/Lift Table: Extract with `h2o.gainsLift(<model>, <data>)` or `h2o.gainsLift(<model>, valid=<T/F>, xval=<T/F>)`
# Cross-Validation Metrics Summary: 
#   mean           sd  cv_1_valid  cv_2_valid
# accuracy                 0.54060173  0.006176659   0.5318666  0.54933685
# auc                       0.6740856  7.194176E-4   0.6730682    0.675103
# err                      0.45939827  0.006176659   0.4681334  0.45066315
# err_count                  161418.0    2150.3118    164459.0    158377.0
# f0point5                  0.5472229 0.0032176753  0.54267246   0.5517734
# f1                        0.6467728 0.0010790895  0.64524674  0.64829886
# f2                       0.79069597 0.0034926527   0.7956353   0.7857566
# lift_top_group            1.9941392  0.007872124   1.9830064   2.0052722
# logloss                  0.66128397 1.0350387E-4  0.66113764  0.66143036
# max_per_class_error       0.7805083  0.018271388   0.8063479   0.7546686
# mcc                      0.20532833 0.0043062004  0.19923843  0.21141821
# mean_per_class_accuracy  0.57402444 0.0043811817   0.5678285  0.58022034
# mean_per_class_error      0.4259756 0.0043811817   0.4321715  0.41977966
# mse                       0.2345057 5.7656587E-5  0.23442416  0.23458724
# precision                0.49631056 0.0039875032   0.4906714   0.5019497
# r2                      0.053581726 1.4594571E-5 0.053561088 0.053602368
# recall                    0.9285571  0.009509025  0.94200486   0.9151093
# rmse                     0.48425788 5.9530874E-5   0.4841737  0.48434207
# specificity              0.21949174  0.018271388   0.1936521  0.24533139
# > 










#Search through cartesian search. Will take time!

gbm_params2 <- list(learn_rate = c(0.001,0.01, 0.1),
                    max_depth = c(5, 10, 15),
                    sample_rate = c(0.6,0.8,1.0),
                    col_sample_rate = c(0.2, 0.5, 0.8,1.0))

#In case you want Random Search run below and add it to h2o.grid in search criteria.
#search_criteria2 <- list(strategy = "RandomDiscrete", max_models = 36)

# Train and validate a grid of GBMs
gbm_grid2 <- h2o.grid("gbm", x = features, y = y,
                      training_frame = train.hex
                      ,grid_id = "gbm_grid2"
                      ,nfolds=2
                      #validation_frame = valid,
                      #,ntrees = 200,
                      ,seed = 1234
                      ,hyper_params = gbm_params2)

gbm_gridperf2 <- h2o.getGrid(grid_id = "gbm_grid2" 
                             ,sort_by = "logloss"
                             ,decreasing = TRUE)
print(gbm_gridperf2)

best_gbm_model_id <- gbm_gridperf2@model_ids[[1]]
best_gbm <- h2o.getModel(best_gbm_model_id)

#Comparision of best_gbm and earlier gbm model performance.
best_gbm

gbmF_model_1


#Variable importance selection. Any thing with overall percent > 1.
vip<-h2o.varimp(best_gbm)
vip.imp<-vip[which(vip$percentage*100>1),]$variable
features<-vip.imp
#Added some more parameters to add more power to GBM.
gbmF_model_1 = h2o.gbm( x=features,
                        y = y,
                        training_frame =train.hex,
                        nbins_cats = 2199,
                        ntrees = 5000,
                        learn_rate = 0.0001
                        ,seed=1234
)
gbmF_model_1

#Running GLM
glm_model_1 = h2o.glm( x=features,
                       y = y,
                       training_frame =train.hex
                       ,nfolds = 3
                       ,family="binomial"
                       ,seed=1234
)
h2o.performance(glm_model_1)
glm_model_1

#Running DeepLearning
dl_model_1<-h2o.deeplearning(x=features,
                             y = y,
                             ,nfolds=3
                             ,training_frame =train.hex
                             ,seed=1234)

dl_model_1


#Prediction

predict_gbm = as.data.frame(h2o.predict(gbmF_model_1, newdata = test.hex))
predict_gbm_response<-as.character(predict_gbm$predict)

predict_rf = as.data.frame(h2o.predict(rf_model_1, newdata = test.hex))
predict_rf_response<-as.character(predict_rf$predict)

predict_glm = as.data.frame(h2o.predict(glm_model_1, newdata = test.hex))
predict_glm_response<-as.character(predict_glm$predict)

predict_dl = as.data.frame(h2o.predict(dl_model_1, newdata = test.hex))
predict_dl_response<-as.character(predict_dl$predict)

#Submission
ID=as.character(ID)
submit<-(data.frame(cbind(ID=ID,Outcome=predict_gbm_response)))
names(submit)<-c("ID","Outcome")
write.csv(submit,file = "submit_gbm.csv",row.names = F)

submit<-(data.frame(cbind(ID=ID,Outcome=predict_rf_response)))
names(submit)<-c("ID","Outcome")
write.csv(submit,file = "submit_rf.csv",row.names = F)

submit<-(data.frame(cbind(ID=ID,Outcome=predict_glm_response)))
names(submit)<-c("ID","Outcome")
write.csv(submit,file = "submit_glm.csv",row.names = F)


submit<-(data.frame(cbind(ID=ID,Outcome=predict_dl_response)))
names(submit)<-c("ID","Outcome")
write.csv(submit,file = "submit_dl.csv",row.names = F)

