#Exploratory
setwd("D:/DataScience/Kaggle/houseowner")
train<-read.csv("train.csv")
test<-read.csv("test.csv")
train<-train[,2:81]
ID<-test[,1]
test<-test[,2:80]
test$SalePrice<-NA
combi<-rbind(train,test)
combi.imp<-combi[,1:80]

combi.imp$GarageCars<-as.factor(combi.imp$GarageCars)
combi.imp$Fireplaces<-as.factor(combi.imp$Fireplaces)
combi.imp$KitchenAbvGr<-as.factor(combi.imp$KitchenAbvGr)
combi.imp$BedroomAbvGr<-as.factor(combi.imp$BedroomAbvGr)
combi.imp$HalfBath<-as.factor(combi.imp$HalfBath)
combi.imp$FullBath<-as.factor(combi.imp$FullBath)
combi.imp$BsmtFullBath<-as.factor(combi.imp$BsmtFullBath)
combi.imp$OverallQual<-as.factor(combi.imp$OverallQual)
combi.imp$MSSubClass<-as.factor(combi.imp$MSSubClass)
combi.imp$OverallCond<-as.factor(combi.imp$OverallCond)

summary(combi.imp)

#Alley
summary(combi.imp$Alley)
combi.imp$Alley<-(as.factor(ifelse(is.na(combi.imp$Alley),0,combi.imp$Alley)))

#NA No basement
summary(combi.imp$BsmtQual)
combi.imp$BsmtQual<-(as.factor(ifelse(is.na(combi.imp$BsmtQual),0,combi.imp$BsmtQual)))

#BsmtCond NA No Basement
summary(combi.imp$BsmtCond)
combi.imp$BsmtCond<-(as.factor(ifelse(is.na(combi.imp$BsmtCond),0,combi.imp$BsmtCond)))

#BsmtExposure Na No Basement
summary(combi.imp$BsmtExposure)
combi.imp$BsmtExposure<-(as.factor(ifelse(is.na(combi.imp$BsmtExposure),0,combi.imp$BsmtExposure)))

#BsmtFinType1 Na No Basement
summary(combi.imp$BsmtFinType1)
combi.imp$BsmtFinType1<-(as.factor(ifelse(is.na(combi.imp$BsmtFinType1),0,combi.imp$BsmtFinType1)))

#BsmtFinType2 Na No Basement
summary(combi.imp$BsmtFinType2)
combi.imp$BsmtFinType2<-(as.factor(ifelse(is.na(combi.imp$BsmtFinType2),0,combi.imp$BsmtFinType2)))

#FireplaceQu Na No Fireplace
summary(combi.imp$FireplaceQu)
combi.imp$FireplaceQu<-(as.factor(ifelse(is.na(combi.imp$FireplaceQu),0,combi.imp$FireplaceQu)))

#GarageType Na No GarageType
summary(combi.imp$GarageType)
combi.imp$GarageType<-(as.factor(ifelse(is.na(combi.imp$GarageType),0,combi.imp$GarageType)))

#GarageFinish Na No GarageFinish
summary(combi.imp$GarageFinish)
combi.imp$GarageFinish<-(as.factor(ifelse(is.na(combi.imp$GarageFinish),0,combi.imp$GarageFinish)))

#GarageQual Na No GarageQual
summary(combi.imp$GarageQual)
combi.imp$GarageQual<-(as.factor(ifelse(is.na(combi.imp$GarageQual),0,combi.imp$GarageQual)))

#BsmtFinType2 Na No GarageCond
summary(combi.imp$GarageCond)
combi.imp$GarageCond<-(as.factor(ifelse(is.na(combi.imp$GarageCond),0,combi.imp$GarageCond)))

#PoolQC Na No PoolQC
summary(combi.imp$PoolQC)
combi.imp$PoolQC<-(as.factor(ifelse(is.na(combi.imp$PoolQC),0,combi.imp$PoolQC)))

#Fence Na No Fence
summary(combi.imp$Fence)
combi.imp$Fence<-(as.factor(ifelse(is.na(combi.imp$Fence),0,combi.imp$Fence)))
#MiscFeature Na No MiscFeature
summary(combi.imp$MiscFeature)
combi.imp$MiscFeature<-(as.factor(ifelse(is.na(combi.imp$MiscFeature),0,combi.imp$MiscFeature)))

#Month sold as factor
combi.imp$MoSold<-as.factor(combi.imp$MoSold)

summary(combi.imp)

#Convert all factor variables to numerics
for(i in 1:ncol(combi.imp)){
  
  if(is.factor(combi.imp[,i])){
    levels(combi.imp[,i])<-levels(as.factor(as.numeric((combi.imp[,i]))))
  }
}

summary(combi.imp)

#Replace NA with NB
# vars<-c("MiscFeature","Fence","PoolQC","GarageCond","GarageQual","GarageFinish"
#         ,"GarageType","FireplaceQu","BsmtFinType2","BsmtFinType1"
#         ,"BsmtExposure","BsmtCond","BsmtQual")

library(missForest)
combi.complete<-missForest(combi.imp,ntree =100)
combi.complete<-combi.complete$ximp
summary(combi.complete)



#We will try to correct the bias removing the constant columns or balancing them

#MSZoning Analysis:Too many classes are belonging. 
# Self made rule: If 75% of once class present, then it is most impactful and rest not so much!
summary(combi.complete$MSZoning)
combi.complete$MSZoning<-(as.factor(ifelse(combi.complete$MSZoning==4,1,0)))

precent_data<-function(x){
freq_percent<-100*(summary(x)/sum(summary(x)))
return(freq_percent)
}
precent_data(combi.complete$MSZoning)

attach(combi.complete)
#Street
summary(combi.complete$Street)
precent_data(combi.complete$Street)

#GarageYrBlt
combi.complete$GarageYrBlt<-as.factor(combi.complete$GarageYrBlt)
plot(combi.complete$GarageYrBl,combi.complete$SalePrice)

#YearBuilt
combi.complete$YearBuilt<-as.factor(combi.complete$YearBuilt)
plot(combi.complete$YearBuilt,combi.complete$SalePrice)

#YearRemodAdd
combi.complete$YearRemodAdd<-as.factor(combi.complete$YearRemodAdd)
plot(combi.complete$YearRemodAdd,combi.complete$SalePrice)

#Self made rule: if the data is more than 90% on one class, it is constant column.
#Remove that column
combi.complete$Street<-NULL

#Alley
precent_data(combi.complete$Alley)
combi.complete$Alley<-NULL

#Utilities
precent_data(combi.complete$Utilities)
combi.complete$Utilities<-NULL

#LandContour
summary(combi.complete$LandContour)
precent_data(combi.complete$LandContour)
combi.complete$LandContour<-NULL

#LotConfig
precent_data(combi.complete$LotConfig)
combi.complete$LotConfig<-(as.factor(ifelse(combi.complete$LotConfig==5,1,0)))

#LandSlope
precent_data(combi.complete$LandSlope)
combi.complete$LandSlope<-NULL

#Condition1
precent_data(combi.complete$Condition1)
combi.complete$Condition1<-(as.factor(ifelse(combi.complete$Condition1==3,1,0)))

#Condition2
precent_data(combi.complete$Condition2)
combi.complete$Condition2<-NULL

#BldgType
precent_data(combi.complete$BldgType)
combi.complete$BldgType<-(as.factor(ifelse(combi.complete$BldgType==1,1,0)))

#RoofStyle
precent_data(combi.complete$RoofStyle)
combi.complete$RoofStyle<-(as.factor(ifelse(combi.complete$RoofStyle==2,1,0)))

#RoofMatl
precent_data(combi.complete$RoofMatl)
combi.complete$RoofMatl<-NULL

#ExterCond
precent_data(combi.complete$ExterCond)
combi.complete$ExterCond<-(as.factor(ifelse(combi.complete$ExterCond==5,1,0)))

#BsmtCond
precent_data(combi.complete$BsmtCond)
combi.complete$BsmtCond<-(as.factor(ifelse(combi.complete$BsmtCond==5,1,0)))

#BsmtFinType2
precent_data(combi.complete$BsmtFinType2)
combi.complete$BsmtFinType2<-(as.factor(ifelse(combi.complete$BsmtFinType2==7,1,0)))

#YearBuilt: How to treat  year variables. Treating them as factor might help
combi.complete$YearBuilt<-as.factor(YearBuilt)
plot(combi.complete$YearBuilt)

#YearRemodAdd: Do we really think that it matters to a buyer when house got remodelled?
#But it might be useful for house condition factors.
combi.complete$YearRemodAdd<-as.factor(YearRemodAdd)
summary(combi.complete$YearRemodAdd)

#Heating:  Looks like every home has heating, 98.5% of them.
precent_data(Heating)
combi.complete$Heating<-NULL

#Electrical
precent_data(Electrical)
combi.complete$Electrical<-(as.factor(ifelse(combi.complete$Electrical==5,1,0)))

#LowQualFinSF Considering Low Spread of data doesn't look itmakes sense to have it.
plot(LowQualFinSF)
combi.complete$LowQualFinSF<-NULL

#Functional
precent_data(Functional)
combi.complete$Functional<-(as.factor(ifelse(Functional==7,1,0)))
#Looks too much of one class data 93% Ignoring the column
combi.complete$Functional<-NULL

#BsmtCond
precent_data(BsmtCond)
combi.complete$BsmtCond<-NULL

#BsmtHalfBath
combi.complete$BsmtHalfBath<-as.factor(combi.complete$BsmtHalfBath)
combi.complete$BsmtHalfBath<-NULL

#KitchenAbvGr
precent_data(combi.complete$KitchenAbvGr)
combi.complete$KitchenAbvGr<-(as.factor(ifelse(combi.complete$KitchenAbvGr==2,1,0)))

#GarageYrBlt: Skewed will keep it as it is.
combi.complete$GarageYrBlt<-as.factor(GarageYrBlt)
plot(combi.complete$GarageYrBlt)

#GarageQual
precent_data(combi.complete$GarageQual)
combi.complete$GarageQual<-(as.factor(ifelse(combi.complete$GarageQual==6,1,0)))

#GarageCond
precent_data(combi.complete$GarageCond)
combi.complete$GarageCond<-(as.factor(ifelse(combi.complete$GarageCond==6,1,0)))

#PavedDrive
precent_data(combi.complete$PavedDrive)
combi.complete$PavedDrive<-(as.factor(ifelse(combi.complete$PavedDrive==3,1,0)))

#PoolQC
precent_data(combi.complete$PoolQC)
combi.complete$PoolQC<-NULL

#PoolArea
plot(PoolArea)
combi.complete$PoolArea<-NULL

#PoolQC
precent_data(combi.complete$PoolQC)
combi.complete$PoolQC<-NULL

#Fence
precent_data(combi.complete$Fence)
combi.complete$Fence<-(as.factor(ifelse(combi.complete$Fence==1,1,0)))

#MiscFeature
precent_data(combi.complete$MiscFeature)
combi.complete$MiscFeature<-NULL

#MiscFeature
plot(combi.complete$MiscVal)
combi.complete$MiscVal<-NULL

#MoSold: I don't think the month it sold matters, but year matters
combi.complete$MoSold<-NULL

#YrSold: Convert it to factor
combi.complete$YrSold<-as.factor(combi.complete$YrSold)
precent_data(combi.complete$YrSold)
#Looks all right on distribution

#SaleType
precent_data(combi.complete$SaleType)
combi.complete$SaleType<-(as.factor(ifelse(combi.complete$SaleType==9,1,0)))

#SaleCondition
precent_data(combi.complete$SaleCondition)
combi.complete$SaleCondition<-(as.factor(ifelse(combi.complete$SaleCondition==5,1,0)))

#Looks like Garage Buid Time, Year Build Time has same effect.
#I don't think Year I remodeled has any effect on sale price.
combi.complete$GarageYrBlt<-NULL
combi.complete$YearRemodAdd<-NULL

#Severely Biased or Sparse Data Removal
combi.complete$ScreenPorch<-NULL
combi.complete$X3SsnPorch<-NULL
combi.complete$EnclosedPorch<-NULL
combi.complete$WoodDeckSF<-NULL
combi.complete$BsmtFinSF2<-NULL
combi.complete$KitchenAbvGr<-NULL
combi.complete$Electrical<-NULL
combi.complete$CentralAir<-NULL
combi.complete$BsmtFinType2<-NULL
combi.complete$ExterCond<-NULL
combi.complete$BldgType<-NULL
combi.complete$Condition1<-NULL
combi.complete$MSZoning<-NULL
combi.complete$RoofStyle<-NULL
combi.complete$GarageQual<-NULL
combi.complete$GarageCond<-NULL
combi.complete$PavedDrive<-NULL
combi.complete$Fence<-NULL

#Anamoly Management is better to be done with l1,l2 penalty in algorithms
summary(combi.complete)


write.csv(combi.complete,file="combi_complete.csv")

save(combi.imp,ID,combi.complete,file = "combi.RData")

##########



load("combi.RData")

train.complete<-combi.complete[1:1460,]
test.complete<-combi.complete[1461:2919,]

#Outlier Management using brute method(Look and Feel the ResponseData!)
train.complete<-train.complete[which(train.complete$SalePrice<=450000),]
train.complete<-train.complete[which(train.complete$SalePrice>=60000),]
plot(train.complete$SalePrice)

#Linear Model
lm.model<-lm(train.complete$SalePrice~.,train.complete)
summary(lm.model)

#Glmnet
library(glmnet)
x.train<-data.matrix(train.complete[1:42])
y.train<-data.matrix(train.complete[43])

cvfit=cv.glmnet(x=x.train,y=y.train,alpha=0.5,family="gaussian",type.measure="mse",nfolds = 3)
glmnet.model<-glmnet(x.train,y.train,family="gaussian",alpha=0.5, lambda=cvfit$lambda.min,standardize=F)




# #Baseline
# submit<-(data.frame(cbind(Id=ID,SalePrice= mean(test.complete$SalePrice))))
# write.csv(submit,file = "submit_baseline.csv",row.names = F)


library(h2o)
h2o.shutdown(prompt=F)
library(audio)
wait(10)
h2o.init(nthreads = -1,max_mem_size = "4g")
train.hex<-as.h2o(train.complete)
test.hex<-as.h2o(test.complete[c(-ncol(test.complete))])
#Variable Importance
features=names(train.complete)[!names(train.complete) %in% c("SalePrice")]

gbmF_model_1 = h2o.gbm( x=features,
                        y = "SalePrice",
                        training_frame =train.hex ,
                        #validation_frame =valid_frame,
                        nfolds = 3,
                        max_depth = 30 ,
                        distribution = "gaussian",
                        ntrees =1000,
                        learn_rate = 0.01
                        ,nbins_cats = 5000
                        ,seed=1234
)
h2o.performance(gbmF_model_1)
gbmF_model_1

vip<-h2o.varimp(gbmF_model_1)
vip.imp<-vip[which(vip$percentage*100>0.3),]$variable
#Model GBM
features<-vip.imp
gbmF_model_1 = h2o.gbm( x=features,
                        y = "SalePrice",
                        training_frame =train.hex ,
                        #validation_frame =testHex ,
                        max_depth = 60 ,
                        nfolds = 3,
                        distribution = "gaussian",
                        ntrees =10000,
                        learn_rate = 0.01
                        ,nbins_cats = 6000
                        ,seed=1234
)
h2o.rmse(gbmF_model_1,train =  T)
gbmF_model_1
#Model Deep Learning
dl_model_1 = h2o.deeplearning( x=features,
                               y = "SalePrice",
                               training_frame =train.hex,
                               #validation_frame =testHex ,
                               distribution="gaussian",
                               activation="Rectifier",
                               nfolds=3,
                               hidden=c(300,300),
                               epochs=10000,
                               #autoencoder = T,
                               l1=1e-4,
                               l2=1e-3,
                               adaptive_rate =T,seed = 1234
                               
)
h2o.rmse(dl_model_1,train =  T)
dl_model_1

#Model PCA
k=21
num_pc=15
pca_data<-h2o.prcomp(training_frame=train.hex,features,k =k,transform = "STANDARDIZE",seed=1234,impute_missing=T)
train_pca <- h2o.predict(pca_data, train.hex, num_pc=num_pc)
train_pca_complete<-h2o.cbind(train_pca,train.hex$SalePrice)
test_pca <- h2o.predict(pca_data, test.hex, num_pc=num_pc)
features_pca=names(train_pca_complete)[!names(train_pca_complete) %in% c("SalePrice")]

gbmF_model_pca = h2o.gbm( x=features_pca,
                        y = "SalePrice",
                        training_frame =train_pca_complete ,
                        #validation_frame =testHex ,
                        max_depth = 60 ,
                        distribution = "gaussian",
                        ntrees =10000,
                        nfolds=3,
                        learn_rate = 0.01
                        ,nbins_cats = 6000
                        ,seed=1234
)
h2o.rmse(gbmF_model_pca,train =  T)

gbmF_model_pca



dl_model_pca = h2o.deeplearning( x=features_pca,
                               y = "SalePrice",
                               training_frame =train_pca_complete,
                               #validation_frame =testHex ,
                               distribution="AUTO",
                               nfolds=3,
                               activation="Rectifier",
                               hidden=c(250,250,250,250),
                               epochs=10000,
                               #autoencoder = T,
                               l1=1e-4,
                               l2=1e-3,
                               #max_w2=
                               adaptive_rate =T,seed = 1234
                               
)
h2o.rmse(dl_model_pca,train =  T)
dl_model_pca



gbmF_model_1
dl_model_1

gbmF_model_pca
dl_model_pca

#Prediction Perforance Evaluvation


#Making the predictions
testPurchase_gbm_1 = as.data.frame(h2o.predict(gbmF_model_1, newdata = test.hex))
testPurchase_dl_1 = as.data.frame(h2o.predict(dl_model_1, newdata = test.hex))
testPurchase_gbmF_model_pca = as.data.frame(h2o.predict(gbmF_model_pca, newdata = test_pca))
testPurchase_dl_model_pca = as.data.frame(h2o.predict(dl_model_pca, newdata = test_pca))

testPurchase_gbm_1$predict=ifelse(testPurchase_gbm_1$predict<0,min(train.complete$SalePrice),testPurchase_gbm_1$predict)
testPurchase_dl_1$predict=ifelse(testPurchase_dl_1$predict<0,min(train.complete$SalePrice),testPurchase_dl_1$predict)
testPurchase_gbmF_model_pca$predict=ifelse(testPurchase_gbmF_model_pca$predict<0,min(train.complete$SalePrice),testPurchase_gbmF_model_pca$predict)
testPurchase_dl_model_pca$predict=ifelse(testPurchase_dl_model_pca$predict<0,min(train.complete$SalePrice),testPurchase_dl_model_pca$predict)

x.test<-data.matrix(test.complete[1:42])
glmnet.predict<-predict(glmnet.model,x.test)
glmnet.predict<-as.numeric(glmnet.predict)



#Examining all rows Prediction Graph using Plotly
library(plotly)

p <- plot_ly() %>%
  add_lines(x = ID, y = testPurchase_gbm_1$predict, name = "GBM") %>%
  add_lines(x = ID, y = testPurchase_dl_1$predict, name = "Deep Learning Model") %>%
  add_lines(x = ID, y = glmnet.predict, name = "GLMNET")
p





#Ensemble
predicts<-list()
predicts$gbm<-testPurchase_gbm_1$predict
predicts$dl<-testPurchase_dl_1$predict
predicts$gbm_pca<-testPurchase_gbmF_model_pca$predict
predicts$dl_pca<-testPurchase_dl_model_pca$predict
predicts$GLM<-glmnet.predict

#GBM,DL,GBMPCA,DLPCA,GLM
ens_weights<-integer()
number_of_models<-5
#If you want average of models
ens_weights<-rep(1/number_of_models,number_of_models)
##If you want different weights of models.
#GBM & DL & GLMNET looks like a good uncorelated model combi
#GBM,DL,GBMPCA,DLPCA,GLMNET
ens_weights<-c(0.5,0,0,0,0.5)

predict_ensemble<-0
ensemble_models<-function(ens_weights,predicts,number_of_models){
  predict_ensemble<-0
  for(i in 1:number_of_models){
    if(ens_weights[i]!=0){
    predict_ensemble=(ens_weights[i]*predicts[[i]])+predict_ensemble}
  }
  return(predict_ensemble)
}

predict_ensemble<-ensemble_models(ens_weights,predicts,number_of_models)

q <- plot_ly() %>%
  add_lines(x = ID, y = testPurchase_gbm_1$predict, name = "GBM") %>%
  add_lines(x = ID, y = testPurchase_dl_1$predict, name = "Deep Learning Model") %>%
  add_lines(x = ID, y = glmnet.predict, name = "GLMNET") %>%
  add_lines(x = ID, y = predict_ensemble, name = "ENSEMBLE_PREDICT")
q



#Subission
#Ensemble
submit<-(data.frame(cbind(Id=ID,SalePrice=predict_ensemble)))
write.csv(submit,file = "submit_ENSEMBLE.csv",row.names = F)

#Stacking
#Predict final output using uncorlated modeling
#GLM,GBM,DL stack using RF
stack.frame<-data.frame(
  GBM = as.data.frame(h2o.predict(gbmF_model_1, newdata = train.hex))$predict,
  DL = as.data.frame(h2o.predict(dl_model_1, newdata = train.hex))$predict,
 GLM=as.numeric(predict(glmnet.model,x.train)),
 RESP<-y.train
  )
stack.frame.hex<-as.h2o(stack.frame)
stack.h2o<-h2o.gbm(x=h2o.names(stack.frame.hex[,c(1,3)]),
                         y="SalePrice",
                         training_frame = stack.frame.hex,
                         ntrees = 10000)
#StackFrame Test
stack.frame.test<-data.frame(
  GBM = as.data.frame(h2o.predict(gbmF_model_1, newdata = test.hex))$predict,
  DL = as.data.frame(h2o.predict(dl_model_1, newdata = test.hex))$predict,
  GLM=as.numeric(predict(glmnet.model,x.test)))

stack.frame.test.hex<-as.h2o(stack.frame.test)

stack_predict = as.data.frame(h2o.predict(stack.h2o, newdata = stack.frame.test.hex))$predict

submit<-(data.frame(cbind(Id=ID,SalePrice=stack_predict)))
write.csv(submit,file = "submit_stack.csv",row.names = F)

#GBM
submit<-(data.frame(cbind(Id=ID,SalePrice=testPurchase_gbm_1$predict)))
write.csv(submit,file = "submit_GBM.csv",row.names = F)
#DL
submit<-(data.frame(cbind(Id=ID,SalePrice=testPurchase_dl_1$predict)))
write.csv(submit,file = "submit_DL.csv",row.names = F)
#GBMPCA
submit<-(data.frame(cbind(Id=ID,SalePrice=testPurchase_gbmF_model_pca$predict)))
write.csv(submit,file = "submit_GBMPCA.csv",row.names = F)
#dl_model_pca
submit<-(data.frame(cbind(Id=ID,SalePrice=testPurchase_dl_model_pca$predict)))
write.csv(submit,file = "submit_DLPCA.csv",row.names = F)

#GLM
submit<-(data.frame(cbind(Id=ID,SalePrice=glmnet.predict)))
write.csv(submit,file = "submit_GLM.csv",row.names = F)


h2o.shutdown()
