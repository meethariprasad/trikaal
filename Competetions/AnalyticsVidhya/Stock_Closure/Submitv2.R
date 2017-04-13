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

save(test,train,file = "base.RData")


load("base.RData")
#Create Empty Response SalePrice
test$Outcome<-NA
ID<-test[,1]

combi<-rbind(train,test)

save(ID,combi,file = "combi.RData")

load("combi.RData")

#Copy orginal dataframe to impute
combi.imp<-combi
#rm(train,test)
summary(combi.imp)

#Handling Variables
combi.imp$Outcome<-as.factor(combi.imp$Outcome)
summary(combi.imp)



combi.imp$Trend<-ifelse((combi.imp$Positive_Directional_Movement)>(combi.imp$Negative_Directional_Movement),1,0)
combi.imp$Trend<-as.factor(combi.imp$Trend)
combi.imp$Trend_Strength<-abs((combi.imp$Positive_Directional_Movement)-(combi.imp$Negative_Directional_Movement))

combi.imp$Stock_ID<-as.factor(combi.imp$Stock_ID)

#Removing ID and Timestamp
combi.imp<-combi.imp[,3:ncol(combi.imp)]

train.complete<-combi.imp[1:702739,]
#Brurtally removing NA cases as the the number of such cases are small.
train.complete<-train.complete[complete.cases(train.complete),]
test.complete<-combi.imp[702740:804685,]

save(combi.imp,test.complete,train.complete,file = "combi_imp.RData")
rm(combi,combi.imp,test,train)

setwd("/resources/rstudio")

load("combi_imp.RData")

#Trying Double Cross Over Feature
#http://www.investopedia.com/terms/c/crossover.asphttp://www.investopedia.com/terms/c/crossover.asp

rank.df<-data.frame(t(apply(-train.complete[,3:6], 1, rank, ties.method='min')))
names(rank.df)<-c("Rank3","Rank5","Rank10","Rank20")

rank.df.34<-data.frame(t(apply(-train.complete[,3:4], 1, rank, ties.method='min')))
names(rank.df.34)<-c("Rank34_3","Rank34_4")

rank.df.35<-data.frame(t(apply(-train.complete[,c(3,5)], 1, rank, ties.method='min')))
names(rank.df.35)<-c("Rank35_3","Rank35_5")

rank.df.36<-data.frame(t(apply(-train.complete[,c(3,6)], 1, rank, ties.method='min')))
names(rank.df.36)<-c("Rank36_3","Rank36_6")

rank.df<-cbind(rank.df,rank.df.34$Rank34_3,rank.df.35$Rank35_3,rank.df.36$Rank36_3)
names(rank.df)<-c("Rank3_all","Rank5_all","Rank10_all","Rank20_all"
                  ,"rank_34","rank_35","rank_36")

save(rank.df,file = "rank.RData")

#MACD Feature: http://www.investopedia.com/terms/m/macd.asp
train.complete$MACD_3510<-train.complete$Ten_Day_Moving_Average-train.complete$Five_Day_Moving_Average
train.complete$MACD_3510_Crossover<-ifelse(train.complete$MACD_3510>train.complete$Three_Day_Moving_Average,"Sell","Buy")

#Volume Size Feature:Volume Categorization. Less than First QR,Greater than 3 QR, In between
#http://www.investopedia.com/terms/a/averagedailytradingvolume.asp
train.complete$Volume_Size<-ifelse(train.complete$Volume<=-0.40550,"low",train.complete$Volume)
train.complete$Volume_Size<-ifelse(train.complete$Volume_Size>=0.05220 & train.complete$Volume_Size!="low","high",train.complete$Volume_Size)
train.complete$Volume_Size<-ifelse(!train.complete$Volume_Size %in% c("low","high"),"medium",train.complete$Volume_Size)
train.complete$Volume_Size<-as.factor(train.complete$Volume_Size)

train.complete<-cbind(train.complete,rank.df)
summary(train.complete)

#Making appropriate new features as factors.

train.complete$Rank3_all<-as.factor(train.complete$Rank3_all)
train.complete$Rank5_all<-as.factor(train.complete$Rank5_all)
train.complete$Rank10_all<-as.factor(train.complete$Rank10_all)
train.complete$Rank20_all<-as.factor(train.complete$Rank20_all)
train.complete$MACD_3510_Crossover<-as.factor(train.complete$MACD_3510_Crossover)
train.complete$rank_34<-as.factor(train.complete$rank_34)
train.complete$rank_35<-as.factor(train.complete$rank_35)
train.complete$rank_36<-as.factor(train.complete$rank_36)

save(train.complete,file = "train.RData")

load("train.RData")
summary(train.complete)
names(train.complete)

library(h2o)
y<-c("Outcome")
#Almost removing all default features and high moving average overall ranks.
features=names(train.complete)[!names(train.complete) %in% c("Three_Day_Moving_Average",
                                                             "Outcome"
                                                             ,"Five_Day_Moving_Average"
                                                             ,"Ten_Day_Moving_Average"
                                                             ,"Twenty_Day_Moving_Average"
                                                             ,"Positive_Directional_Movement"
                                                             ,"Negative_Directional_Movement"
                                                             ,"True_Range"
                                                             ,"Rank5_all"
                                                             ,"Rank10_all"
                                                             ,"Rank20_all"
                                                             ,"Average_True_Range"
                                                             ,"Volume")]
#features<-c("Negative_Directional_Movement","Positive_Directional_Movement")
h2o.shutdown(prompt=F)

library(audio)
wait(10)

#Check Linux system Memory size to allocate for h2o.
as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))/1000000

h2o.init(nthreads = -1,max_mem_size = "11g")

train.hex<-as.h2o(train.complete)

test.hex<-as.h2o(test.complete[,features])


#Models
gbmF_model_1 = h2o.gbm( x=features,
                        y = y,
                        training_frame =train.hex
                        ,nfolds = 2
                        #,nbins=30
                        #,learn_rate = 0.001
                        #,min_rows=2
                        #,ntrees = 200
                        #,nbins_cats = 2199
                        ,seed=1234
)

gbmF_model_1


#Best Parameter Search through cartesian search. Will take time!

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
                             y = y
                             ,nfolds=3
                             ,training_frame =train.hex
                             ,seed=1234)

#Following is not a code but analysis of Cross Validation.

# > dl_model_1
# Model Details:
#   ==============
#   
#   H2OBinomialModel: deeplearning
# Model ID:  DeepLearning_model_R_1491399139235_3153 
# Status of Neuron Layers: predicting Outcome, 2-class classification, bernoulli distribution, CrossEntropy loss, 433,002 weights/biases, 5.0 MB, 1,898,189 training samples, mini-batch size 1
# layer units      type dropout       l1       l2 mean_rate rate_rms momentum mean_weight
# 1     1  1961     Input  0.00 %                                                          
# 2     2   200 Rectifier  0.00 % 0.000000 0.000000  0.087287 0.234865 0.000000   -0.000073
# 3     3   200 Rectifier  0.00 % 0.000000 0.000000  0.155572 0.256949 0.000000   -0.026576
# 4     4     2   Softmax         0.000000 0.000000  0.007730 0.003318 0.000000   -0.007230
# weight_rms mean_bias bias_rms
# 1                              
# 2   0.031232 -0.225796 0.384649
# 3   0.077172  0.761039 0.184639
# 4   0.351254 -0.509729 0.107903
# 
# 
# H2OBinomialMetrics: deeplearning
# ** Reported on training data. **
#   ** Metrics reported on temporary training frame with 9915 samples **
#   
#   MSE:  0.2473464
# RMSE:  0.4973394
# LogLoss:  0.6878262
# Mean Per-Class Error:  0.4999075
# AUC:  0.5295338
# Gini:  0.05906751
# 
# Confusion Matrix (vertical: actual; across: predicted) for F1-optimal threshold:
#   0    1    Error        Rate
# 0      1 5407 0.999815  =5407/5408
# 1      0 4507 0.000000     =0/4507
# Totals 1 9914 0.545335  =5407/9915
# 
# Maximum Metrics: Maximum metrics at their respective thresholds
# metric threshold    value idx
# 1                       max f1  0.344389 0.625061 398
# 2                       max f2  0.344389 0.806492 398
# 3                 max f0point5  0.429582 0.510373 339
# 4                 max accuracy  0.525474 0.551286  75
# 5                max precision  0.594921 1.000000   0
# 6                   max recall  0.344389 1.000000 398
# 7              max specificity  0.594921 1.000000   0
# 8             max absolute_mcc  0.493988 0.049736 166
# 9   max min_per_class_accuracy  0.446191 0.375638 272
# 10 max mean_per_class_accuracy  0.477191 0.521105 210
# 
# Gains/Lift Table: Extract with `h2o.gainsLift(<model>, <data>)` or `h2o.gainsLift(<model>, valid=<T/F>, xval=<T/F>)`
# 
# 
# > glm_model_1
# Model Details:
#   ==============
#   
#   H2OBinomialModel: glm
# Model ID:  GLM_model_R_1491399139235_3166 
# GLM Model: summary
# family  link                                regularization number_of_predictors_total
# 1 binomial logit Elastic Net (alpha = 0.5, lambda = 1.939E-5 )                       1958
# number_of_active_predictors number_of_iterations training_frame
# 1                        1030                    3 train.complete
# 
# Coefficients: glm coefficients
# names coefficients standardized_coefficients
# 1     Intercept    -0.129182                 -0.140586
# 2    Stock_ID.1     0.000000                  0.000000
# 3   Stock_ID.10     0.103157                  0.103157
# 4  Stock_ID.100     0.000000                  0.000000
# 5 Stock_ID.1000     0.172038                  0.172038
# 
# ---
#   names coefficients standardized_coefficients
# 1954          Volume_Size.low    -0.015905                 -0.015905
# 1955       Volume_Size.medium    -0.029351                 -0.029351
# 1956  MACD_3510_Crossover.Buy    -0.069241                 -0.069241
# 1957 MACD_3510_Crossover.Sell     0.045819                  0.045819
# 1958           Trend_Strength    -0.015211                 -0.019706
# 1959                MACD_3510     0.280038                  0.023708
# 
# H2OBinomialMetrics: glm
# ** Reported on training data. **
#   
#   MSE:  0.242605
# RMSE:  0.4925495
# LogLoss:  0.6775036
# Mean Per-Class Error:  0.4755938
# AUC:  0.5707104
# Gini:  0.1414208
# R^2:  0.02098043
# Null Deviance:  961954.9
# Residual Deviance:  946248.9
# AIC:  948310.9
# 
# Confusion Matrix (vertical: actual; across: predicted) for F1-optimal threshold:
#   0      1    Error            Rate
# 0      27749 354143 0.927338  =354143/381892
# 1       7547 308896 0.023849    =7547/316443
# Totals 35296 663039 0.517932  =361690/698335
# 
# Maximum Metrics: Maximum metrics at their respective thresholds
# metric threshold    value idx
# 1                       max f1  0.346967 0.630733 286
# 2                       max f2  0.235288 0.806992 348
# 3                 max f0point5  0.428558 0.526973 206
# 4                 max accuracy  0.484317 0.557389  98
# 5                max precision  0.607727 0.750000   0
# 6                   max recall  0.077898 1.000000 398
# 7              max specificity  0.607727 0.999995   0
# 8             max absolute_mcc  0.414445 0.117027 224
# 9   max min_per_class_accuracy  0.468119 0.541250 135
# 10 max mean_per_class_accuracy  0.451899 0.547897 163
# 
# Gains/Lift Table: Extract with `h2o.gainsLift(<model>, <data>)` or `h2o.gainsLift(<model>, valid=<T/F>, xval=<T/F>)`
# 
# H2OBinomialMetrics: glm
# ** Reported on cross-validation data. **
#   ** 3-fold cross-validation on training data (Metrics computed for combined holdout predictions) **
#   
#   MSE:  0.2437162
# RMSE:  0.4936762
# LogLoss:  0.679873
# Mean Per-Class Error:  0.4771898
# AUC:  0.5570376
# Gini:  0.1140752
# R^2:  0.01649638
# Null Deviance:  961956.6
# Residual Deviance:  949558.2
# AIC:  951000.2
# 
# Confusion Matrix (vertical: actual; across: predicted) for F1-optimal threshold:
#   0      1    Error            Rate
# 0      25725 356167 0.932638  =356167/381892
# 1       6880 309563 0.021742    =6880/316443
# Totals 32605 665730 0.519875  =363047/698335
# 
# Maximum Metrics: Maximum metrics at their respective thresholds
# metric threshold    value idx
# 1                       max f1  0.350986 0.630363 284
# 2                       max f2  0.232193 0.806859 353
# 3                 max f0point5  0.417644 0.523510 221
# 4                 max accuracy  0.493177 0.549294  82
# 5                max precision  0.597651 0.535714   2
# 6                   max recall  0.093098 1.000000 397
# 7              max specificity  0.629943 0.999995   0
# 8             max absolute_mcc  0.369337 0.108503 269
# 9   max min_per_class_accuracy  0.465848 0.534573 137
# 10 max mean_per_class_accuracy  0.443964 0.539903 176
# 
# Gains/Lift Table: Extract with `h2o.gainsLift(<model>, <data>)` or `h2o.gainsLift(<model>, valid=<T/F>, xval=<T/F>)`
# Cross-Validation Metrics Summary: 
#   mean           sd cv_1_valid cv_2_valid cv_3_valid
# accuracy   0.4797714  7.881821E-5 0.47975266 0.47991633 0.47964525
# auc        0.5570722  7.838784E-4 0.55697733  0.5557645  0.5584749
# err       0.52022856  7.881821E-5 0.52024734 0.52008367 0.52035475
# err_count   121098.0    246.86906   121407.0   120610.0   121277.0
# f0point5   0.5194057 3.2258508E-4 0.51981324  0.5187688 0.51963514
# 
# ---
#   mean          sd  cv_1_valid  cv_2_valid  cv_3_valid
# precision           0.4648489 2.730615E-4  0.46522087  0.46431664  0.46500918
# r2                0.016493835  2.22221E-4 0.016448282 0.016133739 0.016899481
# recall              0.9790115 9.507113E-4  0.97964984  0.97714114   0.9802434
# residual_deviance    316519.4   613.55524   317380.47   315331.72   316846.03
# rmse                0.4936762 4.125186E-5   0.4937477  0.49367598   0.4936048
# specificity        0.06609014 0.001711169  0.06445539 0.069511354  0.06430366
# > gbmF_model_1
# Model Details:
#   ==============
#   
#   H2OBinomialModel: gbm
# Model ID:  GBM_model_R_1491399139235_3128 
# Model Summary: 
#   number_of_trees number_of_internal_trees model_size_in_bytes min_depth max_depth
# 1            5000                     5000            71731725         5         5
# mean_depth min_leaves max_leaves mean_leaves
# 1    5.00000         32         32    32.00000
# 
# 
# H2OBinomialMetrics: gbm
# ** Reported on training data. **
#   
#   MSE:  0.2438319
# RMSE:  0.4937934
# LogLoss:  0.6805649
# Mean Per-Class Error:  0.4751728
# AUC:  0.5835564
# Gini:  0.1671128
# 
# Confusion Matrix (vertical: actual; across: predicted) for F1-optimal threshold:
#   0      1    Error            Rate
# 0      27274 354618 0.928582  =354618/381892
# 1       6887 309556 0.021764    =6887/316443
# Totals 34161 664174 0.517667  =361505/698335
# 
# Maximum Metrics: Maximum metrics at their respective thresholds
# metric threshold    value idx
# 1                       max f1  0.396514 0.631349 321
# 2                       max f2  0.341102 0.807116 373
# 3                 max f0point5  0.442560 0.529906 212
# 4                 max accuracy  0.469483 0.563655  97
# 5                max precision  0.497360 0.600251   1
# 6                   max recall  0.293711 1.000000 399
# 7              max specificity  0.497912 0.995931   0
# 8             max absolute_mcc  0.433068 0.123422 245
# 9   max min_per_class_accuracy  0.459148 0.557424 142
# 10 max mean_per_class_accuracy  0.455429 0.558594 160
# 
# Gains/Lift Table: Extract with `h2o.gainsLift(<model>, <data>)` or `h2o.gainsLift(<model>, valid=<T/F>, xval=<T/F>)`
