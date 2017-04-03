setwd("D:/DataScience/Analytics Vidhya/aminihack")
train<-read.csv("train.csv")
test<-read.csv("test.csv")

#We don't want the ID of Row
train<-train[,2:ncol(train)]
#
ID<-test$ID
#Remove ID from test
test<-test[,2:ncol(test)]
#Create Empty Response SalePrice
test$Outcome<-NA
#Original
combi<-rbind(train,test)

#Copy orginal dataframe to impute
combi.imp<-combi
#rm(train,test)
summary(combi.imp)

#Handling Variables
combi.imp$Outcome<-as.factor(combi.imp$Outcome)
combi.imp$Stock_ID<-as.factor(combi.imp$Stock_ID)
combi.imp$timestamp<-as.factor(combi.imp$timestamp)

summary(combi.imp)

#NA Handling
library(missForest)
combi.complete<-missForest(combi.imp[,3:ncol(combi.imp)],ntree =100)
combi.complete<-combi.complete$ximp
summary(combi.complete)

#Base Line Prediction
# Outcome<-combi.imp$Outcome
# combi.imp<-combi.imp[,1:11]
# combi.complete<-combi.imp[complete.cases(combi.imp),]

#h2o
train.complete<-combi.imp[1:702739,]
train.complete<-train.complete[complete.cases(train.complete),]
test.complete<-combi.imp[702740:804685,]

save(combi.imp,test.complete,train.complete,file = "combi.RData")
rm(combi,combi.imp,test,train)

load("combi.RData")

library(h2o)
y<-c("Outcome")
features=names(train.complete)[!names(train.complete) %in% c("Outcome")]
h2o.shutdown(prompt=F)

library(audio)
wait(10)

h2o.init(nthreads = -1,max_mem_size = "5g")

train.hex<-as.h2o(train.complete)
test.hex<-as.h2o(test.complete[,features])

#Models
gbmF_model_1 = h2o.gbm( x=features,
                        y = y,
                        training_frame =train.hex,
                        nbins_cats = 1500,
                        ntrees = 5000,
                        learn_rate = 0.0001
                        ,seed=1234
)
h2o.performance(gbmF_model_1)

vip<-h2o.varimp(gbmF_model_1)

vip.imp<-vip[which(vip$percentage*100>1),]$variable
#Removing Suspicious
features<-vip.imp[2:length(vip.imp)]

features<-c("Volume",
            "Three_Day_Moving_Average",
            "Positive_Directional_Movement",
            "Negative_Directional_Movement",
            "True_Range"
            )

gbmF_model_1 = h2o.gbm( x=features,
                        y = y,
                        training_frame =train.hex,
                        nbins_cats = 1500,
                        ntrees = 2000,
                        learn_rate = 0.001
                        ,seed=1234
)
h2o.performance(gbmF_model_1)

rf_model_1 = h2o.randomForest( x=features,
                        y = y,
                        training_frame =train.hex ,
                        ntrees = 500
                        ,seed=1234
)
h2o.performance(rf_model_1)
rf_model_1

glm_model_1 = h2o.glm( x=features,
                               y = y,
                               training_frame =train.hex,
                       family = "binomial"
                       ,standardize = TRUE
                               ,seed=1234
                       ,remove_collinear_columns = T
)
h2o.performance(glm_model_1)
glm_model_1

dl_model_1<-h2o.deeplearning(x=features,
                             y = y,
                             training_frame =train.hex
                             ,seed=1234)
dl_model_1
#Prediction

predict_gbm = as.data.frame(h2o.predict(gbmF_model_1, newdata = test.hex))
predict_gbm<-as.character(predict_gbm$predict)

predict_rf = as.data.frame(h2o.predict(rf_model_1, newdata = test.hex)$predict)
predict_rf<-as.character(predict_rf$predict)

predict_glm = as.data.frame(h2o.predict(glm_model_1, newdata = test.hex)$predict)
predict_glm<-as.character(predict_glm$predict)

predict_dl = as.data.frame(h2o.predict(dl_model_1, newdata = test.hex))
predict_dl<-as.character(predict_dl$predict)

#Submission
ID=as.character(ID)
submit<-(data.frame(cbind(ID=ID,Outcome=predict_gbm)))
names(submit)<-c("ID","Outcome")
write.csv(submit,file = "submit_gbm.csv",row.names = F)

submit<-(data.frame(cbind(ID=ID,Outcome=predict_rf)))
names(submit)<-c("ID","Outcome")
write.csv(submit,file = "submit_rf.csv",row.names = F)

submit<-(data.frame(cbind(ID=ID,Outcome=predict_glm)))
names(submit)<-c("ID","Outcome")
write.csv(submit,file = "submit_glm.csv",row.names = F)

submit<-(data.frame(cbind(ID=ID,Outcome=predict_dl)))
names(submit)<-c("ID","Outcome")
write.csv(submit,file = "submit_dl.csv",row.names = F)

#Get better than 0.61 Logloss
