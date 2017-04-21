#There are 2 problems
# Case 1: Predict Contacts
# Case 2: Predict Responses

#Helper Function to check the factor variable percentage.
#Helper Function to get Percentage of Factor
percent_data<-function(x){ 
  freq_percent<-100*(summary(x)/sum(summary(x))) 
  return(freq_percent) 
}

setwd("/resources/rstudio/watercustomer")

# Case 1: Predict Contacts

#Step 1: Data Load

contacts_train<-read.csv("/resources/rstudio/watercustomer/train/Train/Contacts_Pre_2017.csv")
contacts_test<-read.csv("/resources/rstudio/watercustomer/test/Test/Contacts2017.csv")


#Step 2: Descriptive Analytics and Feature Engineering
#Is both are in same grain level of data.
#No Both the data is not in same grain.
#Train needs to be aggregated with respect to Contact Type per day.
contacts_train<-aggregate(contacts_train$Contacts, by=list(CDATE=contacts_train$START.DATE,CTYPE=contacts_train$CONTACT.TYPE), FUN=sum)
names(contacts_train)<-c("CDATE","CTYPE","CONTACTS")
contacts_train<-contacts_train[order(contacts_train$CDATE),]
#Since Grains are equal we can bind the data after storning and removing ID column.
ID<-contacts_test$ID
contacts_test$ID<-NULL
names(contacts_test)<-names(contacts_train)

combi<-rbind(contacts_train,contacts_test)
#Checking variable types
str(combi)


#Let us check features one by one.

#First let us see response variable.
summary(contacts_train$CONTACTS)
#Looks like after third quarter there is a huge bulge in data.
#Let us try to see plot
plot(contacts_train$CONTACTS)
#We can see here is kind of pattern here.
plot(contacts_train$CDATE[1:900],contacts_train$CONTACTS[1:900])
#Looks like for every date you will get Contacts in two bands.
#One band on an average from minimum to 400.
#Another band is on an average above 1500-2000 goes all the way up to maximum.
#It means everyday we have almost certain categories with low & certain
#categories with high contact numbers.
#Its OK as appearently via graph the distribution looks almost constant in both bands.



#CDATE
#Ok. We might be able to derive month feature out of it
#We can which month we have maximum activity on what kind of type.
library(lubridate)
#Get date of the year
combi$CDATEY<-yday(combi$CDATE)
#Month of year
combi$CDATEM<-as.factor(lubridate::month(combi$CDATE))
#Week of the year
combi$CDATEW<-as.factor(lubridate::week(combi$CDATE))
#Week Day
combi$CDATEWD<-as.factor(lubridate::wday(combi$CDATE))
#Month Day
combi$CDATEMD<-as.factor(lubridate::mday(combi$CDATE))

#CTYPE
plot(contacts_train$CTYPE,contacts_train$CONTACTS)
#Call input has unusually large count of contacts, as expected actually.

#Changing to numeric instead of test
summary(combi$CTYPE)
percent_data(combi$CTYPE)
levels(combi$CTYPE)
combi$CTYPEN<-as.factor(as.numeric(combi$CTYPE))
#Summary
summary(combi$CTYPEN)

#Contacts
plot(contacts_train$CONTACTS)

summary(combi)

#Step 3: Modeling
train.complete<-combi[1:nrow(contacts_train),]
y="CONTACTS"
features=names(combi)[!names(combi) %in% c(names(contacts_train),"CDATE","CDATEY")]

library(h2o)
h2o.shutdown(prompt=F)
library(audio)
wait(10)
h2o.init(nthreads = -1,max_mem_size = "10g")
train.hex<-as.h2o(train.complete)

h2o.summary(train.hex)
#DL NO FE
dl_nofe = h2o.deeplearning( x=features,
                    y = y,
                    training_frame =train.hex
                    ,hidden=c(1000,500,1000)
                    #,rate=0.00001
                    #,l1=0.0001
                    #,l2=0.0001
                    #,loss="Automatic"
                    #,distribution="AUTO"
                    ,epochs =1000
                    #,nfolds = 2
                    #,elastic_averaging=T
                    ,seed=1234
)

dl_nofe

#Looks like DL doing better RMSE.
#RMSE CV 249.

#No feature engineering PCA. K-20
pca_nofe<-h2o.prcomp(train.hex[features],k=100,
                     #pca_method="GLRM",
                     #use_all_factor_levels=TRUE,
                     transform="NORMALIZE",seed=1234)
pca_nofe
pca_data<-h2o.predict(pca_nofe,train.hex)
#Check number of PCA columns to be selected based on 99% cumulative frequency.
pca_train<-h2o.cbind(pca_data[1:90],train.hex$CONTACTS)

features_pca=names(pca_train)[!names(pca_train) %in% c(y)]
dl_nofe_pca = h2o.deeplearning( x=features_pca,
                    y = y,
                    training_frame =pca_train
                    ,hidden=c(400,400,400,400)
                    ,rate=0.001
                    ,l1=0.0001
                    ,l2=0.0001
                    #,loss="Automatic"
                    #,distribution="AUTO"
                    ,epochs = 40
                    ,nfolds = 2
                    ,elastic_averaging=T
                    ,seed=1234
)

dl_nofe_pca
dl_nofe

#DL with PCA has CV RMSE 278 where as without PCA has 252.


#Step 4: Predictions
test.complete<-combi[(nrow(combi)+1-nrow(contacts_test)):nrow(combi),]
test.hex<-as.h2o(test.complete)

#Making the predictions
testPurchase_gbm_1 = as.data.frame(h2o.predict(dl_nofe, newdata = test.hex))
summary(testPurchase_gbm_1$predict)
summary(train.complete$CONTACTS)
testPurchase_gbm_1$predict=ifelse(testPurchase_gbm_1$predict<min(train.complete$CONTACTS),min(train.complete$CONTACTS),testPurchase_gbm_1$predict)
summary(testPurchase_gbm_1$predict)

#ID	Contacts
submit<-(data.frame(cbind(ID=ID,Contacts=testPurchase_gbm_1$predict)))
write.csv(submit,file = "Contacts.csv",row.names = F)

#Step 5: Ensemble



#######Case2:Predict Responses
#Step 1: Data Load
setwd("/resources/rstudio/watercustomer")
resolution_train<-read.csv("/resources/rstudio/watercustomer/train/Train/Resolution_Pre_2017.csv")
head(resolution_train)
resolution_test<-read.csv("/resources/rstudio/watercustomer/test/Test/Resolution2017.csv")
head(resolution_test)
#Remember that grain level of Train and test are different.
#If you need to bring training data to test data grain level, you have to aggregare

#Step 2: Descriptive Analytics and Feature Engineering
#The test data has grain level of Date,Category,Subject,Resolution
resolution_train<-aggregate(resolution_train$Resolution, by=list(RDATE=resolution_train$Date,RCAT=resolution_train$Category,RSUB=resolution_train$Subject), FUN=sum)
names(resolution_train)<-c("RDATE","RCAT","RSUB","RES")
resolution_train<-resolution_train[order(resolution_train$RDATE),]
#Handle ID
RESID<-resolution_test$ID
resolution_test$ID<-NULL
names(resolution_test)<-c("RDATE","RCAT","RSUB","RES")
combi<-rbind(resolution_train,resolution_test)
trainrows<-nrow(resolution_train)
testrows<-nrow(resolution_test)
names(combi)

#Overall Summary
summary(combi)
str(combi)

#Response Variable
summary(resolution_train$RES)
#Again we can see a double band like pattern here.
percent_data(as.factor(resolution_train$RES))
#But considering huge numbers, better to remove all other

#Checking Date and converting it.
summary(resolution_train$RDATE)
library(lubridate)
combi$RDATEY<-yday(combi$RDATE)
#Assuming month has effect on response.
combi$RDATEM<-as.factor(lubridate::month(combi$RDATE))
#Week of the year
combi$RDATEW<-as.factor(lubridate::week(combi$RDATE))
#Week Day
combi$RDATEWD<-as.factor(lubridate::wday(combi$RDATE))
#Month Day
combi$RDATEMD<-as.factor(lubridate::mday(combi$RDATE))

#Checking Category
summary(combi$RCAT)
#Converting to numeric
combi$RCATN<-as.numeric(combi$RCAT)
combi$RCATN<-as.factor(combi$RCATN)
summary(combi$RCATN)

#Checking first response variable.
summary(combi$RSUB)
#Converting to numeric
combi$RSUBN<-as.numeric(combi$RSUB)
summary(combi$RSUBN)
combi$RSUBN<-as.factor(combi$RSUBN)

#Overall Summary
summary(combi)

#Step 3: Modeling
train.complete<-combi[1:nrow(resolution_train),]
y="RES"
features=names(combi)[!names(combi) %in% c(names(resolution_train),"RDATEY")]
str(combi)

library(h2o)
h2o.shutdown(prompt=F)
library(audio)
wait(10)
h2o.init(nthreads = -1,max_mem_size = "10g")
train.hex<-as.h2o(train.complete)

h2o.summary(train.hex)
#GBM NO FE
gbm_nofe = h2o.gbm( x=features,
                    y = y,
                    training_frame =train.hex
                    ,nfolds = 2
                    ,nbins=50
                    ,learn_rate = 0.001
                    ,ntrees = 5000
                    ,distribution="AUTO"
                    ,max_depth=10
                    ,min_rows=20
                    ,col_sample_rate_per_tree=0.6
                    ,col_sample_rate=0.8
                    ,sample_rate=0.6
                    ,nbins_cats = 2199
                    ,seed=1234
)

gbm_nofe

dl_nofe = h2o.deeplearning( x=features,
                            y = y,
                            training_frame =train.hex
                            ,hidden=c(400,400,400,400)
                            ,rate=0.001
                            ,l1=0.0001
                            ,l2=0.0001
                            #,loss="Automatic"
                            #,distribution="AUTO"
                            ,epochs = 40
                            ,nfolds = 2
                            ,elastic_averaging=T
                            ,seed=1234
)

dl_nofe



#No feature engineering PCA. K-20
pca_nofe<-h2o.prcomp(train.hex[features],k=11,pca_method="GLRM",
                     use_all_factor_levels=TRUE,transform="NORMALIZE",seed=1234)
pca_nofe
pca_data<-h2o.predict(pca_nofe,train.hex)
#Based on cumulative % 99 get PCA.
pca_train<-h2o.cbind(pca_data[1:2],train.hex$RES)

features_pca=names(pca_train)[!names(pca_train) %in% c(y)]
dl_nofe_pca = h2o.deeplearning( x=features_pca
                        ,y = y
                        ,hidden=c(400,400,400,400)
                        ,rate=0.001
                        ,l1=0.0001
                        ,l2=0.0001
                        #,loss="Automatic"
                        #,distribution="AUTO"
                        ,epochs = 40
                        ,nfolds = 2
                        ,elastic_averaging=T
                        ,seed=1234
                        )

dl_nofe_pca

#GBM without PCA has better RMSE CV.


#Step 4: Predictions
test.complete<-combi[(nrow(combi)+1-nrow(resolution_test)):nrow(combi),]
test.hex<-as.h2o(test.complete)

#Making the predictions
testPurchase_gbm_1 = as.data.frame(h2o.predict(dl_nofe, newdata = test.hex))
summary(testPurchase_gbm_1$predict)
testPurchase_gbm_1$predict=ifelse(testPurchase_gbm_1$predict<min(train.complete$RES),min(train.complete$RES),testPurchase_gbm_1$predict)
summary(testPurchase_gbm_1$predict)
summary(train.complete$RES)

#ID	Resolution
submit<-(data.frame(cbind(ID=RESID,Resolution=testPurchase_gbm_1$predict)))
write.csv(submit,file = "Resolution.csv",row.names = F)

#Zip the files and upload.
h2o.shutdown(prompt = F)

#Step 5: Ensemble
