#Step 1: Data Load
setwd("~/resources/rstudio/houseowner")
train<-read.csv("train.csv")
test<-read.csv("test.csv")
summary(train)
#We don't want the ID Column
train<-train[,2:ncol(train)]
#
ID<-test[,1]
#Remove ID from test
test<-test[,2:ncol(test)]
#Create Empty Response SalePrice
test$SalePrice<-NA
#Original
combi<-rbind(train,test)
save(combi,file="combi.RData")

load("combi.RData")
#Copy orginal dataframe to impute
combi.imp<-combi
rm(train,test,combi)

## Step 2: Data Cleansing. 
#Converting appropriate variables as factors or numerics as based on data definition.One by one

#Helper Function to get Percentage of Factors
percent_data<-function(x){ 
  freq_percent<-100*(summary(x)/sum(summary(x))) 
  return(freq_percent) 
}

#Data Analysis Questions on column
#1. What is the data type as per definition?
#2. What is Stored Data type?
#3. Is change of Data Type  is Succesful?
#4. Are there any NA?
#5. Does Data Definition Defines NA?
#6. Is NA constitute more than 80% of data.
#7. What is the distribution of the data in case of factor(Use Percent Data) and in case of numeric?
#8. Is the data showing more than 90% of same values, 
    #making it constant column hence can be ignored!
#9. Do we think this column is independent contributor?: 
    #Check to see other related columns.
#10: Do we think this column is important as per subject?: 
    #Check with Subject Matter Experts(Google)
#11: Can we think of any change in the column?


#MSSubClass: Identifies the type of dwelling involved in the sale.	
#It is a factor by definition.
summary(combi.imp$MSSubClass)
#It is in numeric form. Let us change it to factor.
combi.imp$MSSubClass<-as.factor(combi.imp$MSSubClass)
#Check if any factor is more than 80% or 90%
percent_data(combi.imp$MSSubClass)
#Looks ok.
#Is there any visible relation with SalePrice
plot(combi.imp$MSSubClass,combi.imp$SalePrice)


#MSZoning: Identifies the general zoning classification of the sale.
#What data type? It is a factor by definition.
summary(combi.imp$MSZoning)
#Do we need to convert? It is already a factor. No need to change.
#Are there NA?:: There are NA.
#How you handle NA?:: 
#There is no NA definition 
#it means it needs to be handled via maybe through Missforest.
#Percentage of factors
percent_data(combi.imp$MSZoning)
#Looks ok.
#Is there any visible relation with SalePrice
plot(combi.imp$MSZoning,combi.imp$SalePrice)

#LotFrontage: Linear feet of street connected to property
#It is a numeric by definition.
summary(combi.imp$LotFrontage)
#It is already numeric. No need to change
#There are NA. 
#But data definition has no meaning for NA. 
#Need to fill it with Miss Forest
plot(combi.imp$LotFrontage,combi.imp$SalePrice)

# LotArea: Lot size in square feet
#It is Numeric by definition.
summary(combi.imp$LotArea)
#It is already numeric
#No NA

# Street: Type of road access to property
# 
# Grvl	Gravel	
# Pave	Paved
#factor.
summary(combi.imp$Street)
#Already a factor.
#No NA.
#Percent Data
percent_data(combi.imp$Street)
plot(combi.imp$Street,combi.imp$SalePrice)
#Highly Skewed. Hence considering it as constant column ignoring it by removing.
#GBM has option ignore_const_cols to set by the way.
combi.imp$Street<-NULL



# Alley: Type of alley access to property
# 
# Grvl	Gravel
# Pave	Paved
# NA 	No alley access
summary(combi.imp$Alley)
#How to handle NA?
##NA has definition as no alley access.
summary(combi.imp$Alley)
combi.imp$Alley<-(as.factor(ifelse(is.na(combi.imp$Alley),0,combi.imp$Alley)))
summary(combi.imp$Alley)
#percent data
percent_data(combi.imp$Alley)
plot(combi.imp$Alley,combi.imp$SalePrice)
#Skewed data by more than 90%. Removing it.
combi.imp$Alley<-NULL
 
# LotShape: General shape of property
# 
# Reg	Regular	
# IR1	Slightly irregular
# IR2	Moderately Irregular
# IR3	Irregular
summary(combi.imp$LotShape)
#No NA.
#percent data
percent_data(combi.imp$LotShape)
#Looks Ok.
plot(combi.imp$LotShape,combi.imp$SalePrice)
 
# LandContour: Flatness of the property
# 
# Lvl	Near Flat/Level	
# Bnk	Banked - Quick and significant rise from street grade to building
# HLS	Hillside - Significant slope from side to side
# Low	Depression
summary(combi.imp$LandContour) 
#No NA present.
#percent data
percent_data(combi.imp$LandContour)
plot(combi.imp$LandContour,combi.imp$SalePrice)
#Almost a constant column if rounded off. Ignoring it.
combi.imp$LandContour<-NULL

# Utilities: Type of utilities available
# 
# AllPub	All public Utilities (E,G,W,& S)	
# NoSewr	Electricity, Gas, and Water (Septic Tank)
# NoSeWa	Electricity and Gas Only
# ELO	Electricity only	
summary(combi.imp$Utilities)
#There are genuine NA's. 
#No definition of NA
#Miss Forest might help.
#Percent Data
percent_data(combi.imp$Utilities)
plot(combi.imp$Utilities,combi.imp$SalePrice)
#More than 90% in one category. Ignoring the column
combi.imp$Utilities<-NULL

# LotConfig: Lot configuration
# 
# Inside	Inside lot
# Corner	Corner lot
# CulDSac	Cul-de-sac
# FR2	Frontage on 2 sides of property
# FR3	Frontage on 3 sides of property
 
summary(combi.imp$LotConfig)
#There are No NA
#Percent Data
percent_data(combi.imp$LotConfig)
#Looks Ok.
plot(combi.imp$LotConfig,combi.imp$SalePrice)

# LandSlope: Slope of property
# 
# Gtl	Gentle slope
# Mod	Moderate Slope	
# Sev	Severe Slope
summary(combi.imp$LandSlope)
#There are No NA
#Percent Data
percent_data(combi.imp$LandSlope)
plot(combi.imp$LandSlope,combi.imp$SalePrice)
#Looks Skewed. Ignoring.
combi.imp$LandSlope<-NULL

 
# Neighborhood: Physical locations within Ames city limits
# 
# Blmngtn	Bloomington Heights
# Blueste	Bluestem
# BrDale	Briardale
# BrkSide	Brookside
# ClearCr	Clear Creek
# CollgCr	College Creek
# Crawfor	Crawford
# Edwards	Edwards
# Gilbert	Gilbert
# IDOTRR	Iowa DOT and Rail Road
# MeadowV	Meadow Village
# Mitchel	Mitchell
# Names	North Ames
# NoRidge	Northridge
# NPkVill	Northpark Villa
# NridgHt	Northridge Heights
# NWAmes	Northwest Ames
# OldTown	Old Town
# SWISU	South & West of Iowa State University
# Sawyer	Sawyer
# SawyerW	Sawyer West
# Somerst	Somerset
# StoneBr	Stone Brook
# Timber	Timberland
# Veenker	Veenker
summary(combi.imp$Neighborhood)
#There are No NA
#Percent Data
percent_data(combi.imp$Neighborhood)
#Looks ok
plot(combi.imp$Neighborhood,combi.imp$SalePrice)

 
# Condition1: Proximity to various conditions
# 
# Artery	Adjacent to arterial street
# Feedr	Adjacent to feeder street	
# Norm	Normal	
# RRNn	Within 200' of North-South Railroad
# RRAn	Adjacent to North-South Railroad
# PosN	Near positive off-site feature--park, greenbelt, etc.
# PosA	Adjacent to postive off-site feature
# RRNe	Within 200' of East-West Railroad
# RRAe	Adjacent to East-West Railroad

summary(combi.imp$Condition1)
#There are No NA
#Percent Data
percent_data(combi.imp$Condition1)
#Looks ok, even though 86% still can be considered as constant.
#But going with rule of thumb of 90% cutoff.
plot(combi.imp$Condition1,combi.imp$SalePrice)
 
# Condition2: Proximity to various conditions (if more than one is present)
# 
# Artery	Adjacent to arterial street
# Feedr	Adjacent to feeder street	
# Norm	Normal	
# RRNn	Within 200' of North-South Railroad
# RRAn	Adjacent to North-South Railroad
# PosN	Near positive off-site feature--park, greenbelt, etc.
# PosA	Adjacent to postive off-site feature
# RRNe	Within 200' of East-West Railroad
# RRAe	Adjacent to East-West Railroad

summary(combi.imp$Condition2)
#There are No NA
#Percent Data
percent_data(combi.imp$Condition2)
plot(combi.imp$Condition2,combi.imp$SalePrice)
#Looks highly skewed. Removing it.
combi.imp$Condition2<-NULL
 
# BldgType: Type of dwelling
# 
# 1Fam	Single-family Detached	
# 2FmCon	Two-family Conversion; originally built as one-family dwelling
# Duplx	Duplex
# TwnhsE	Townhouse End Unit
# TwnhsI	Townhouse Inside Unit

summary(combi.imp$BldgType)
#There are No NA
#Percent Data
percent_data(combi.imp$BldgType)
#Looks ok. But there is 83% skewness.
plot(combi.imp$BldgType,combi.imp$SalePrice)

# HouseStyle: Style of dwelling
# 
# 1Story	One story
# 1.5Fin	One and one-half story: 2nd level finished
# 1.5Unf	One and one-half story: 2nd level unfinished
# 2Story	Two story
# 2.5Fin	Two and one-half story: 2nd level finished
# 2.5Unf	Two and one-half story: 2nd level unfinished
# SFoyer	Split Foyer
# SLvl	Split Level

summary(combi.imp$HouseStyle)
#There are No NA
#Percent Data
percent_data(combi.imp$HouseStyle)
#Looks ok. But there is 83% skewness.
plot(combi.imp$HouseStyle,combi.imp$SalePrice)
 
# OverallQual: Rates the overall material and finish of the house
# 
# 10	Very Excellent
# 9	Excellent
# 8	Very Good
# 7	Good
# 6	Above Average
# 5	Average
# 4	Below Average
# 3	Fair
# 2	Poor
# 1	Very Poor

summary(combi.imp$OverallQual)
#It needs to be a factor. 
#But another way to think about it is keeping it as numbers itself 
#will help to create range. Let us keep it as is.
#We will see its impact of making factor or not by comparing model performance.
#There are No NA
#Percent Data
plot(combi.imp$OverallQual)
plot(combi.imp$OverallQual,combi.imp$SalePrice)
#You can see real linear corelation.
 
# OverallCond: Rates the overall condition of the house
# 
# 10	Very Excellent
# 9	Excellent
# 8	Very Good
# 7	Good
# 6	Above Average	
# 5	Average
# 4	Below Average	
# 3	Fair
# 2	Poor
# 1	Very Poor

summary(combi.imp$OverallCond)
#It needs to be a factor. 
#But another way to think about it is keeping it as numbers itself 
#will help to create range. Let us keep it as is.
#We will see its impact of making factor or not by comparing model performance.
#There are No NA
#Percent Data
plot(combi.imp$OverallCond)
plot(combi.imp$OverallCond,combi.imp$SalePrice)
#Doesn't look like explaining variance better.

# YearBuilt: Original construction date
summary(combi.imp$YearBuilt)
#It needs to be a factor. 
#But another way to think about it is keeping it as numbers itself 
#will help to create range. Let us keep it as is.
#We will see its impact of making factor or not by comparing model performance.
#There are No NA
#Percent Data
plot(combi.imp$YearBuilt)
plot(combi.imp$YearBuilt,combi.imp$SalePrice)
#Looks like latest homes are fetching better valuvation
 
# YearRemodAdd: Remodel date (same as construction date if no remodeling or additions)

summary(combi.imp$YearRemodAdd)
#It needs to be a factor. 
#But another way to think about it is keeping it as numbers itself 
#will help to create range. Let us keep it as is.
#We will see its impact of making factor or not by comparing model performance.
#There are No NA
#Percent Data
plot(combi.imp$YearRemodAdd) 
plot(combi.imp$YearRemodAdd,combi.imp$SalePrice)
#Doesn't look like explaining lot of variance

# RoofStyle: Type of roof
# 
# Flat	Flat
# Gable	Gable
# Gambrel	Gabrel (Barn)
# Hip	Hip
# Mansard	Mansard
# Shed	Shed

summary(combi.imp$RoofStyle)
#It needs to be a factor. 
#There are No NA
#Percent Data
percent_data(combi.imp$RoofStyle) 
#Looks ok.
plot(combi.imp$RoofStyle,combi.imp$SalePrice)
#Not explaining lot of variance.
 
# RoofMatl: Roof material
# 
# ClyTile	Clay or Tile
# CompShg	Standard (Composite) Shingle
# Membran	Membrane
# Metal	Metal
# Roll	Roll
# Tar&Grv	Gravel & Tar
# WdShake	Wood Shakes
# WdShngl	Wood Shingles

summary(combi.imp$RoofMatl)
#It needs to be a factor. 
#There are No NA
#Percent Data
percent_data(combi.imp$RoofMatl) 
plot(combi.imp$RoofMatl,combi.imp$SalePrice)
#Looks biased.
combi.imp$RoofMatl<-NULL
 
# Exterior1st: Exterior covering on house
# 
# AsbShng	Asbestos Shingles
# AsphShn	Asphalt Shingles
# BrkComm	Brick Common
# BrkFace	Brick Face
# CBlock	Cinder Block
# CemntBd	Cement Board
# HdBoard	Hard Board
# ImStucc	Imitation Stucco
# MetalSd	Metal Siding
# Other	Other
# Plywood	Plywood
# PreCast	PreCast	
# Stone	Stone
# Stucco	Stucco
# VinylSd	Vinyl Siding
# Wd Sdng	Wood Siding
# WdShing	Wood Shingles

summary(combi.imp$Exterior1st)
#It needs to be a factor. 
#There are 1 NA. Missforest might help.
#Percent Data
percent_data(combi.imp$Exterior1st) 
#Looks ok.
plot(combi.imp$Exterior1st,combi.imp$SalePrice)
 
# Exterior2nd: Exterior covering on house (if more than one material)
# 
# AsbShng	Asbestos Shingles
# AsphShn	Asphalt Shingles
# BrkComm	Brick Common
# BrkFace	Brick Face
# CBlock	Cinder Block
# CemntBd	Cement Board
# HdBoard	Hard Board
# ImStucc	Imitation Stucco
# MetalSd	Metal Siding
# Other	Other
# Plywood	Plywood
# PreCast	PreCast
# Stone	Stone
# Stucco	Stucco
# VinylSd	Vinyl Siding
# Wd Sdng	Wood Siding
# WdShing	Wood Shingles

summary(combi.imp$Exterior2nd)
#It needs to be a factor. 
#There are 1 NA. Missforest might help.
#Percent Data
percent_data(combi.imp$Exterior2nd) 
#Looks ok.
plot(combi.imp$Exterior2nd,combi.imp$SalePrice)
#Not explaining the variance better.

# MasVnrType: Masonry veneer type
# 
# BrkCmn	Brick Common
# BrkFace	Brick Face
# CBlock	Cinder Block
# None	None
# Stone	Stone

summary(combi.imp$MasVnrType)
#It needs to be a factor. 
#There are 24 NA. Missforest might help.
#Percent Data
percent_data(combi.imp$MasVnrType) 
#Looks ok.
plot(combi.imp$MasVnrType,combi.imp$SalePrice)
 
# MasVnrArea: Masonry veneer area in square feet
summary(combi.imp$MasVnrArea)
#It needs to be a numeric 
#There are 23 NA. Missforest might help.
#Percent Data
plot(combi.imp$MasVnrArea) 
#There are certain outlier. Removing those rows might help generalozation.
plot(combi.imp$MasVnrArea,combi.imp$SalePrice)
#Looks like spread all over the place. Not much help

# ExterQual: Evaluates the quality of the material on the exterior 
# 
# Ex	Excellent
# Gd	Good
# TA	Average/Typical
# Fa	Fair
# Po	Poor
summary(combi.imp$ExterQual)
#It needs to be factor. 
#There are no NA
#Percent Data
percent_data(combi.imp$ExterQual) 
#Looks OK
plot(combi.imp$ExterQual,combi.imp$SalePrice)
#Looks like Excellant houses fetching well as expected


# ExterCond: Evaluates the present condition of the material on the exterior
# 
# Ex	Excellent
# Gd	Good
# TA	Average/Typical
# Fa	Fair
# Po	Poor
summary(combi.imp$ExterCond)
#It needs to be factor. 
#There are no NA
#Percent Data
percent_data(combi.imp$ExterCond) 
#Looks OK
plot(combi.imp$ExterCond,combi.imp$SalePrice)
#Not much of the variance explained here.
 
# Foundation: Type of foundation
# 
# BrkTil	Brick & Tile
# CBlock	Cinder Block
# PConc	Poured Contrete	
# Slab	Slab
# Stone	Stone
# Wood	Wood

summary(combi.imp$Foundation)
#It needs to be factor. 
#There are no NA
#Percent Data
percent_data(combi.imp$Foundation) 
#Looks OK
plot(combi.imp$Foundation,combi.imp$SalePrice)
#Not much of the variance explaned except by PConc

# BsmtQual: Evaluates the height of the basement
# 
# Ex	Excellent (100+ inches)	
# Gd	Good (90-99 inches)
# TA	Typical (80-89 inches)
# Fa	Fair (70-79 inches)
# Po	Poor (<70 inches
#          NA	No Basement

summary(combi.imp$BsmtQual)
#It needs to be factor. 
#There are NA for missforest
#Percent Data
percent_data(combi.imp$BsmtQual) 
#Looks OK
plot(combi.imp$BsmtQual,combi.imp$SalePrice)
#Again Category 1, Excellent shows better sale price.


          
#          BsmtCond: Evaluates the general condition of the basement
#          
#          Ex	Excellent
#          Gd	Good
#          TA	Typical - slight dampness allowed
#          Fa	Fair - dampness or some cracking or settling
#          Po	Poor - Severe cracking, settling, or wetness
#          NA	No Basement

summary(combi.imp$BsmtCond)
#It needs to be factor. 
#NA No basement
combi.imp$BsmtCond<-(as.factor(ifelse(is.na(combi.imp$BsmtCond),0,combi.imp$BsmtCond)))
summary(combi.imp$BsmtCond)
#Percent Data
percent_data(combi.imp$BsmtCond) 
#Looks OK
plot(combi.imp$BsmtCond,combi.imp$SalePrice)
          
#          BsmtExposure: Refers to walkout or garden level walls
#          
#          Gd	Good Exposure
#          Av	Average Exposure (split levels or foyers typically score average or above)	
#          Mn	Mimimum Exposure
#          No	No Exposure
#          NA	No Basement

summary(combi.imp$BsmtExposure)
#It needs to be factor. 
#NA No basement
#BsmtExposure Na No Basement
combi.imp$BsmtExposure<-(as.factor(ifelse(is.na(combi.imp$BsmtExposure),0,combi.imp$BsmtExposure)))
summary(combi.imp$BsmtExposure)
#Percent Data
percent_data(combi.imp$BsmtExposure) 
#Looks OK
plot(combi.imp$BsmtExposure,combi.imp$SalePrice)
#As expected Good is fetching well where as others are at average.

          
#          BsmtFinType1: Rating of basement finished area
#          
#          GLQ	Good Living Quarters
#          ALQ	Average Living Quarters
#          BLQ	Below Average Living Quarters	
#          Rec	Average Rec Room
#          LwQ	Low Quality
#          Unf	Unfinshed
#          NA	No Basement

summary(combi.imp$BsmtFinType1)
#It needs to be factor. 
#NA No basement
#BsmtExposure Na No Basement
combi.imp$BsmtFinType1<-(as.factor(ifelse(is.na(combi.imp$BsmtFinType1),0,combi.imp$BsmtFinType1)))
summary(combi.imp$BsmtFinType1)
#Percent Data
percent_data(combi.imp$BsmtFinType1) 
#Looks OK
plot(combi.imp$BsmtFinType1,combi.imp$SalePrice)
#Looks like Category 3 is fetching good while others are average.

          
#          BsmtFinSF1: Type 1 finished square feet

summary(combi.imp$BsmtFinSF1)
#It needs to be numeric.
#There is 1 NA for missforest.
#Percent Data
plot(combi.imp$BsmtFinSF1) 
#There are 2 outliers.
plot(combi.imp$BsmtFinSF1,combi.imp$SalePrice)
        
#          BsmtFinType2: Rating of basement finished area (if multiple types)
          
#          GLQ	Good Living Quarters
#          ALQ	Average Living Quarters
#          BLQ	Below Average Living Quarters	
#          Rec	Average Rec Room
#          LwQ	Low Quality
#          Unf	Unfinshed
#          NA	No Basement

#BsmtFinType2 Na No Basement
summary(combi.imp$BsmtFinType2)
combi.imp$BsmtFinType2<-(as.factor(ifelse(is.na(combi.imp$BsmtFinType2),0,combi.imp$BsmtFinType2)))
summary(combi.imp$BsmtFinType2)
#percent
percent_data(combi.imp$BsmtFinType2)
          
#          BsmtFinSF2: Type 2 finished square feet
summary(combi.imp$BsmtFinSF2)
#1 NA
plot(combi.imp$BsmtFinSF2)
          
#          BsmtUnfSF: Unfinished square feet of basement area
summary(combi.imp$BsmtUnfSF)
#1 NA
plot(combi.imp$BsmtUnfSF)
          
#          TotalBsmtSF: Total square feet of basement area

summary(combi.imp$TotalBsmtSF)
#1 NA
plot(combi.imp$TotalBsmtSF)
#Looks outliers present.
          
#          Heating: Type of heating
#          
#          Floor	Floor Furnace
#          GasA	Gas forced warm air furnace
#          GasW	Gas hot water or steam heat
#          Grav	Gravity furnace	
#          OthW	Hot water or steam heat other than gas
#          Wall	Wall furnace

summary(combi.imp$Heating)
#1 NA
percent_data(combi.imp$Heating)
#Looks like constant column
combi.imp$Heating<-NULL
          
#          HeatingQC: Heating quality and condition
#          
#          Ex	Excellent
#          Gd	Good
#          TA	Average/Typical
#          Fa	Fair
#          Po	Poor
summary(combi.imp$HeatingQC)
#No NA
percent_data(combi.imp$HeatingQC)

          
#          CentralAir: Central air conditioning
#          
#          N	No
#          Y	Yes

summary(combi.imp$CentralAir)
#No NA
percent_data(combi.imp$CentralAir)
#Constant column
combi.imp$CentralAir<-NULL
          
#          Electrical: Electrical system
#          
#          SBrkr	Standard Circuit Breakers & Romex
#          FuseA	Fuse Box over 60 AMP and all Romex wiring (Average)	
#          FuseF	60 AMP Fuse Box and mostly Romex wiring (Fair)
#          FuseP	60 AMP Fuse Box and mostly knob & tube wiring (poor)
#          Mix	Mixed

summary(combi.imp$Electrical)
#1 NA
percent_data(combi.imp$Electrical)
#Constant column
combi.imp$Electrical<-NULL
          
#          X1stFlrSF: First Floor square feet

summary(combi.imp$X1stFlrSF)
#No NA
plot(combi.imp$X1stFlrSF)
plot(combi.imp$X1stFlrSF,combi.imp$SalePrice)
#There are outliers
#Might be removing them now itself and fitting a model on removed might help!
          
#          X2ndFlrSF: Second floor square feet\
summary(combi.imp$X2ndFlrSF)
#No NA
plot(combi.imp$X2ndFlrSF)
plot(combi.imp$X2ndFlrSF,combi.imp$SalePrice)
#Looks like not all homes has second floor.
          
#          LowQualFinSF: Low quality finished square feet (all floors)
summary(combi.imp$LowQualFinSF)
#No NA
plot(combi.imp$LowQualFinSF)
#Appears to be constant column
combi.imp$LowQualFinSF<-NULL

#          GrLivArea: Above grade (ground) living area square feet
summary(combi.imp$GrLivArea)
#No NA
plot(combi.imp$GrLivArea)
plot(combi.imp$GrLivArea,combi.imp$SalePrice)
#Looks like linear relation with saleprice.
#There are outliers
#Might be removing them now itself and fitting a model on removed might help!
#Also Above Ground Living Area appears to be X1stFlrSF+X2ndFlrSF
#It means we can remove those or remove this.
          
#          BsmtFullBath: Basement full bathrooms
summary(combi.imp$BsmtFullBath)
#It is numeric
#No NA
plot(combi.imp$BsmtFullBath)
          
#          BsmtHalfBath: Basement half bathrooms
summary(combi.imp$BsmtHalfBath)
#No NA
plot(combi.imp$BsmtHalfBath)
#Looks like almost a constant column. Seriously, why I need bathroom in basement.
combi.imp$BsmtHalfBath<-NULL
          
#          FullBath: Full bathrooms above grade
summary(combi.imp$FullBath)
#No NA
plot(combi.imp$FullBath)
plot(combi.imp$FullBath,combi.imp$SalePrice)
#Not explaining variability
#Having 4 bathrooms looks too much! Looks outlier to modeled seperately.
          
#          HalfBath: Half baths above grade:
#a room in a private home that contains a toilet and washbasin but no bath or shower.
summary(combi.imp$HalfBath)
#No NA
plot(combi.imp$HalfBath)
plot(combi.imp$HalfBath,combi.imp$SalePrice)
#Again. The variation is constant.
#Looks like constant column
          
#          Bedroom: Bedrooms above grade (does NOT include basement bedrooms)
summary(combi.imp$Bedroom)
#No NA
plot(combi.imp$Bedroom)
plot(combi.imp$Bedroom,combi.imp$SalePrice)
#Not finding variation at all.
#Looks like more than 4 bedroom is overkill and outlier
          
#          KitchenAbvGr: Kitchens above grade
summary(combi.imp$KitchenAbvGr)
#No NA
plot(combi.imp$KitchenAbvGr)
plot(combi.imp$KitchenAbvGr,combi.imp$SalePrice)
#Doesn't look like explain any variation at all
#More than 2 kitchen looks like overkill.
          
#          KitchenQual: Kitchen quality
#          
#          Ex	Excellent
#          Gd	Good
#          TA	Typical/Average
#          Fa	Fair
#          Po	Poor
summary(combi.imp$KitchenQual)
#1 NA. Missforst might be a good choice.
plot(combi.imp$KitchenQual)
plot(combi.imp$KitchenQual,combi.imp$SalePrice)
#As expected excellent fetching better results.
          
#          TotRmsAbvGrd: Total rooms above grade (does not include bathrooms)
summary(combi.imp$TotRmsAbvGrd)
#No NA
plot(combi.imp$TotRmsAbvGrd)
plot(combi.imp$TotRmsAbvGrd,combi.imp$SalePrice)
#Not much of the varations explained
#More than 10 rooms look outlier.
          
#          Functional: Home functionality (Assume typical unless deductions are warranted)
#          
#          Typ	Typical Functionality
#          Min1	Minor Deductions 1
#          Min2	Minor Deductions 2
#          Mod	Moderate Deductions
#          Maj1	Major Deductions 1
#          Maj2	Major Deductions 2
#          Sev	Severely Damaged
#          Sal	Salvage only
summary(combi.imp$Functional)
#2 NA
percent_data(combi.imp$Functional)
#Looks constant
combi.imp$Functional<-NULL
          
#          Fireplaces: Number of fireplaces
summary(combi.imp$Fireplaces)
#No NA
plot(combi.imp$Fireplaces)
#More than 2 fireplaces looks outlier
          
#          FireplaceQu: Fireplace quality
#          
#          Ex	Excellent - Exceptional Masonry Fireplace
#          Gd	Good - Masonry Fireplace in main level
#          TA	Average - Prefabricated Fireplace in main living area or Masonry Fireplace in basement
#          Fa	Fair - Prefabricated Fireplace in basement
#          Po	Poor - Ben Franklin Stove
#          NA	No Fireplace
summary(combi.imp$FireplaceQu)
#1420 NA
#NA means no fireplace.
#FireplaceQu Na No Fireplace
combi.imp$FireplaceQu<-(as.factor(ifelse(is.na(combi.imp$FireplaceQu),0,combi.imp$FireplaceQu)))
summary(combi.imp$FireplaceQu)
plot(combi.imp$FireplaceQu,combi.imp$SalePrice)
#Type 1-Excellent fetch good value, as expected.
          
#          GarageType: Garage location
#          
#          2Types	More than one type of garage
#          Attchd	Attached to home
#          Basment	Basement Garage
#          BuiltIn	Built-In (Garage part of house - typically has room above garage)
#          CarPort	Car Port
#          Detchd	Detached from home
#          NA	No Garage
summary(combi.imp$GarageType)

#GarageType Na No Garage

combi.imp$GarageType<-(as.factor(ifelse(is.na(combi.imp$GarageType),0,combi.imp$GarageType)))
summary(combi.imp$GarageType)
plot(combi.imp$GarageType,combi.imp$SalePrice)
#Type 4 fetch good value


#          GarageYrBlt: Year garage was built
summary(combi.imp$GarageYrBlt)
#159 NA
#No Garage means No Garage Year Built. NA has to be kept as is. No imputation.
plot(combi.imp$GarageYrBlt)
#There is a missentry at 2207. Come on! How can be garage built in future?
#Make it as same as Year built
combi.imp[which(combi.imp$GarageYrBlt==2207),]$GarageYrBlt<-combi.imp[which(combi.imp$GarageYrBlt==2207),]$YearBuilt
#But if you see the corelation between 
# View(data.frame(combi.imp$GarageYrBlt,combi.imp$YearBuilt))
#Hardly there is any. Do we still need to keep this column?
#Let us keep an eye on this.
plot(combi.imp$GarageYrBlt,combi.imp$YearBuilt)
plot(combi.imp$GarageYrBlt,combi.imp$SalePrice)
#Not much variation explained.
          
#          GarageFinish: Interior finish of the garage
#          
#          Fin	Finished
#          RFn	Rough Finished	
#          Unf	Unfinished
#          NA	No Garage
summary(combi.imp$X2ndFlrSF)
plot(combi.imp$X2ndFlrSF,combi.imp$SalePrice)
#There is linear increase, but 0 shows single floor

#No NA
#GarageFinish Na No GarageFinish
summary(combi.imp$GarageFinish)
combi.imp$GarageFinish<-(as.factor(ifelse(is.na(combi.imp$GarageFinish),0,combi.imp$GarageFinish)))
plot(combi.imp$GarageFinish)
plot(combi.imp$GarageFinish,combi.imp$SalePrice)

          
#          GarageCars: Size of garage in car capacity
summary(combi.imp$GarageCars)
#1 NA
plot(combi.imp$GarageCars,combi.imp$SalePrice)
#Above 3 cars look super rich. We might have to model them seperately.
          
#          GarageArea: Size of garage in square feet
summary(combi.imp$GarageArea)
#1 NA
plot(combi.imp$GarageArea)
#Above 1000 Squarefeet looks too big for a garage. 
#We might have to take them and model seperately
plot(as.factor(combi.imp$GarageArea))
plot(combi.imp$GarageArea,combi.imp$SalePrice)
#There is spread out of values
#Same for Homes with No Garages might be!
          
#          GarageQual: Garage quality
#          
#          Ex	Excellent
#          Gd	Good
#          TA	Typical/Average
#          Fa	Fair
#          Po	Poor
#          NA	No Garage
summary(combi.imp$GarageQual)
#159 NA
#GarageQual Na No GarageQual
summary(combi.imp$GarageQual)
combi.imp$GarageQual<-(as.factor(ifelse(is.na(combi.imp$GarageQual),0,combi.imp$GarageQual)))
summary(combi.imp$GarageQual)
percent_data(combi.imp$GarageQual)
#We need to consider it as constant column
combi.imp$GarageQual<-NULL

#          GarageCond: Garage condition
#          
#          Ex	Excellent
#          Gd	Good
#          TA	Typical/Average
#          Fa	Fair
#          Po	Poor
#          NA	No Garage
summary(combi.imp$GarageCond)
plot(combi.imp$GarageCond,combi.imp$SalePrice)
#  159        NA	No Garage


#BsmtFinType2 Na No GarageCond

combi.imp$GarageCond<-(as.factor(ifelse(is.na(combi.imp$GarageCond),0,combi.imp$GarageCond)))
summary(combi.imp$GarageCond)
percent_data(combi.imp$GarageCond)
combi.imp$GarageCond<-NULL
          
#          PavedDrive: Paved driveway
#          
#          Y	Paved 
#          P	Partial Pavement
#          N	Dirt/Gravel
summary(combi.imp$PavedDrive)
#No NA
percent_data(combi.imp$PavedDrive)
#Constant Column
combi.imp$PavedDrive<-NULL
          
#          WoodDeckSF: Wood deck area in square feet
summary(combi.imp$WoodDeckSF)
#No NA
plot(as.factor(combi.imp$WoodDeckSF))
plot(combi.imp$WoodDeckSF,combi.imp$SalePrice)
#Too much skewed towards 0. 
#We might have to seperate and consider it as seperate category itself.
          
#          OpenPorchSF: Open porch area in square feet
summary(combi.imp$OpenPorchSF)
#No NA
plot(combi.imp$OpenPorchSF)
plot(as.factor(combi.imp$OpenPorchSF))
plot(combi.imp$OpenPorchSF,combi.imp$SalePrice)
#Again skewed towards 0
#We might have to seperate and consider it as seperate category itself.
          
#          EnclosedPorch: Enclosed porch area in square feet
summary(combi.imp$EnclosedPorch)
#No NA
plot(combi.imp$EnclosedPorch)
#Above 350 SqFt looks outlier. Might have to be modeled seperately.
plot(as.factor(combi.imp$EnclosedPorch))
#Unsually skewed towards 0.
percent_data(as.factor(combi.imp$EnclosedPorch))
plot(combi.imp$EnclosedPorch,combi.imp$SalePrice)
          
#          X3SsnPorch: Three season porch area in square feet
summary(combi.imp$X3SsnPorch)
#No NA
plot(combi.imp$X3SsnPorch)
#Looks highly skewed data to zero.
percent_data(as.factor(combi.imp$X3SsnPorch))
combi.imp$X3SsnPorch<-NULL
          
#          ScreenPorch: Screen porch area in square feet
summary(combi.imp$ScreenPorch)
#No NA
plot(combi.imp$ScreenPorch)
#Again looks highly skewed towards zero
percent_data(as.factor(combi.imp$ScreenPorch))
combi.imp$ScreenPorch<-NULL

          
#          PoolArea: Pool area in square feet
summary(combi.imp$PoolArea)
#No NA
plot(combi.imp$PoolArea)
#Looks Skewed.Let us check.
percent_data(as.factor(combi.imp$PoolArea))
plot(combi.imp$PoolArea,combi.imp$SalePrice)
#Confirms
combi.imp$PoolArea<-NULL
          
#          PoolQC: Pool quality
#          
#          Ex	Excellent
#          Gd	Good
#          TA	Average/Typical
#          Fa	Fair
#          NA	No Pool
summary(combi.imp$PoolQC)
#NA=No Pool.
combi.imp$PoolQC<-(as.factor(ifelse(is.na(combi.imp$PoolQC),0,combi.imp$PoolQC)))
summary(combi.imp$PoolQC)
percent_data(combi.imp$PoolQC)
plot(combi.imp$PoolQC,combi.imp$SalePrice)
combi.imp$PoolQC<-NULL
          
#          Fence: Fence quality
#          
#          GdPrv	Good Privacy
#          MnPrv	Minimum Privacy
#          GdWo	Good Wood
#          MnWw	Minimum Wood/Wire
#          NA	No Fence
summary(combi.imp$Fence)

#NA means No Fence

#Fence Na No Fence
summary(combi.imp$Fence)
combi.imp$Fence<-(as.factor(ifelse(is.na(combi.imp$Fence),0,combi.imp$Fence)))
percent_data(combi.imp$Fence)
plot(combi.imp$Fence,combi.imp$SalePrice)
#Keeping it as it is.
          
#          MiscFeature: Miscellaneous feature not covered in other categories
#          
#          Elev	Elevator
#          Gar2	2nd Garage (if not described in garage section)
#          Othr	Other
#          Shed	Shed (over 100 SF)
#          TenC	Tennis Court
#          NA	None
#MiscFeature Na No MiscFeature
summary(combi.imp$MiscFeature)
combi.imp$MiscFeature<-(as.factor(ifelse(is.na(combi.imp$MiscFeature),0,combi.imp$MiscFeature)))
percent_data(combi.imp$MiscFeature)
plot(combi.imp$MiscFeature,combi.imp$SalePrice)
combi.imp$MiscFeature<-NULL
          
#          MiscVal: $Value of miscellaneous feature
summary(combi.imp$MiscVal)
#No NA
plot(combi.imp$MiscVal)
percent_data(as.factor(combi.imp$MiscVal))
plot(combi.imp$MiscVal,combi.imp$SalePrice)
#Highly Skewed
combi.imp$MiscVal<-NULL
          
#          MoSold: Month Sold (MM)
summary(combi.imp$MoSold)
#No NA
plot(combi.imp$MoSold)
#Strongly Believe in which month sold has no effect on saleprice
#As you can see below plot shows not much variation over month.
plot(combi.imp$MoSold,combi.imp$SalePrice)
combi.imp$MoSold<-NULL
          
#          YrSold: Year Sold (YYYY)
summary(combi.imp$YrSold)
#No NA
plot(combi.imp$YrSold,combi.imp$SalePrice)
#Again Year sold is not putting variability.
combi.imp$YrSold<-NULL
          
#          SaleType: Type of sale
#          
#          WD 	Warranty Deed - Conventional
#          CWD	Warranty Deed - Cash
#          VWD	Warranty Deed - VA Loan
#          New	Home just constructed and sold
#          COD	Court Officer Deed/Estate
#          Con	Contract 15% Down payment regular terms
#          ConLw	Contract Low Down payment and low interest
#          ConLI	Contract Low Interest
#          ConLD	Contract Low Down
#          Oth	Other
summary(combi.imp$SaleType)
#1 NA. Leaving it for missforest
percent_data(combi.imp$SaleType)
#86% of the data is of same type. Leaving it as is.
plot(combi.imp$SaleType,combi.imp$SalePrice)
          
#          SaleCondition: Condition of sale
#          
#          Normal	Normal Sale
#          Abnorml	Abnormal Sale -  trade, foreclosure, short sale
#          AdjLand	Adjoining Land Purchase
#          Alloca	Allocation - two linked properties with separate deeds, typically condo with a garage unit	
#          Family	Sale between family members
#          Partial	Home was not completed when last assessed (associated with New Homes)
summary(combi.imp$SaleCondition)
#No NA
percent_data(combi.imp$SaleCondition)
#Looks ok Eventhough it is skewed.
plot(combi.imp$SaleCondition,combi.imp$SalePrice)


summary(combi.imp)


#Step 1: NA Treatment

#Which columns has NA
colnames(combi.imp)[colSums(is.na(combi.imp)) > 0]
#Check the summary of NA columns
summary(combi.imp[colnames(combi.imp)[colSums(is.na(combi.imp)) > 0]])

# #Convert all factor levels to numeric numbers. Assuming it is useful
# for(i in 1:ncol(combi.imp)){
#   
#   if(is.factor(combi.imp[,i])){
#     levels(combi.imp[,i])<-levels(as.factor(as.numeric((combi.imp[,i]))))
#   }
# }

summary(combi.imp)

#Replace NA with NB
# vars<-c("MiscFeature","Fence","PoolQC","GarageCond","GarageQual","GarageFinish"
#         ,"GarageType","FireplaceQu","BsmtFinType2","BsmtFinType1"
#         ,"BsmtExposure","BsmtCond","BsmtQual")

library(doParallel)
detectCores(all.tests = FALSE, logical = TRUE)
library(missForest)
registerDoParallel(cores=4)
system.time(combi.complete<-missForest(combi.imp,parallelize="variables"))
registerDoSEQ()
combi.comp<-combi.complete$ximp

#Without NA Handling
summary(combi.imp[colnames(combi.imp)[colSums(is.na(combi.imp)) > 0]])
#With NA Handling
summary(combi.comp[colnames(combi.imp)[colSums(is.na(combi.imp)) > 0]])

write.csv(combi.comp,file="combi_complete.csv")

save(combi.imp,combi.comp,file = "combi_comp.RData")

#Model 1: Without Feature Engineering, running  PCA and GBM and checking performance

#Model 2:
#Let us plan for 3 models for 3 partitioned datasets.
#Too big homes Model 1.
#Too Small homes Model 2.
#Average Homes Model 3.



###Modeling#######
#Model 1: Without Feature Engineering, running  PCA and GBM and checking performance

setwd("~/resources/rstudio/houseowner")
load("combi_comp.RData")

train.complete<-combi.comp[1:1460,]
y<-c("SalePrice")
features=names(train.complete)[!names(train.complete) %in% c("SalePrice"
                             ,"OverallCond","X2ndFlrSF","X1stFlrSF"
                             ,"YearRemodAdd","GarageYrBlt")]

library(h2o)
h2o.shutdown(prompt=F)
library(audio)
wait(10)
h2o.init(nthreads = -1,max_mem_size = "10g")
train.hex<-as.h2o(train.complete)

gbm_nofe = h2o.gbm( x=features,
                    y = y,
                    training_frame =train.hex
                    ,nfolds = 2
                    ,nbins=50
                    ,learn_rate = 0.001
                    ,ntrees = 10000
                    ,distribution="AUTO"
                    ,max_depth=20
                    ,min_rows=20
                    ,col_sample_rate_per_tree=0.7
                    ,col_sample_rate=0.8
                    ,sample_rate=0.6
                    ,nbins_cats = 3000
                    ,seed=1234
)

gbm_nofe
#CV RMSLE 0.14
varimp<-data.frame(h2o.varimp(gbm_nofe))
impvar<-varimp[which(round(varimp$percentage*100)>=1),]$variable
#RMSLE 0.17

gbm_nofe = h2o.gbm( x=impvar,
                    y = y,
                    training_frame =train.hex
                    ,nfolds = 2
                    ,nbins=50
                    ,learn_rate = 0.001
                    ,ntrees = 10000
                    ,distribution="AUTO"
                    ,max_depth=20
                    ,min_rows=20
                    ,col_sample_rate_per_tree=0.7
                    ,col_sample_rate=0.8
                    ,sample_rate=0.6
                    ,nbins_cats = 3000
                    ,seed=1234
)

gbm_nofe
#CV RMSLE 0.14

h2o.summary(train.hex)

#No feature engineering PCA. K-20
#Using GLRM for Mixed Datatype
pca_nofe<-h2o.prcomp(train.hex[impvar],k=30,pca_method="GLRM",use_all_factor_levels = T,
                     transform="STANDARDIZE",seed=1234)
pca_nofe
pca_data<-h2o.predict(pca_nofe,train.hex)
pca_train<-h2o.cbind(pca_data,train.hex$SalePrice)

features=names(pca_train)[!names(pca_train) %in% c("SalePrice")]
gbm_nofe = h2o.gbm( x=impvar,
                        y = y,
                        training_frame =train.hex
                        ,nfolds = 2
                        ,nbins=50
                        ,learn_rate = 0.001
                        ,ntrees = 5000
                        ,distribution="gaussian"
                        ,max_depth=10
                        ,min_rows=20
                        ,col_sample_rate_per_tree=0.6
                        ,col_sample_rate=0.8
                        ,sample_rate=0.6
                        ,nbins_cats = 2199
                        ,seed=1234
)

gbm_nofe
#With Variable Importance Only Feature Engineering with PCA is CV RMSLE 0.15

#Try Deep Learning with same PCA
dl_nofe = h2o.deeplearning( x=impvar
                    ,y = y
                    ,training_frame =train.hex
                    ,nfolds = 2
                    ,hidden=c(200,200,400,400)
                    ,epochs=10000
                    ,rate = 0.001
                    ,adaptive_rate=T
                    ,distribution="gaussian"
                    ,stopping_metric="RMSLE"
                    ,l1=0.0001
                    ,l2=0.0001
                    ,seed=1234)
dl_nofe
#Again the CV RMSLE is 0.14-0.15 range

#GLM Model
glm_nofe = h2o.glm( x=impvar
                            ,y = y
                            ,training_frame =train.hex
                            ,nfolds = 2
                            ,lambda_search=T
                            ,family="gaussian"
                            ,seed=1234)
glm_nofe

#Making the predictions
test.hex<-as.h2o(combi.comp[nrow(train.complete)+1:nrow(combi.comp),1:ncol(train.complete)-1])

testPurchase_gbm_1 = as.data.frame(h2o.predict(gbm_nofe, newdata = test.hex))
summary(testPurchase_gbm_1$predict)
testPurchase_dl_1 = as.data.frame(h2o.predict(dl_nofe, newdata = test.hex))
summary(testPurchase_dl_1$predict)
testPurchase_glm_model = as.data.frame(h2o.predict(glm_nofe, newdata = test.hex))
summary(testPurchase_glm_model$predict)


# testPurchase_gbm_1$predict=ifelse(testPurchase_gbm_1$predict<0,min(train.complete$SalePrice),testPurchase_gbm_1$predict)
# testPurchase_dl_1$predict=ifelse(testPurchase_dl_1$predict<0,min(train.complete$SalePrice),testPurchase_dl_1$predict)
# testPurchase_gbmF_model_pca$predict=ifelse(testPurchase_gbmF_model_pca$predict<0,min(train.complete$SalePrice),testPurchase_gbmF_model_pca$predict)
# testPurchase_dl_model_pca$predict=ifelse(testPurchase_dl_model_pca$predict<0,min(train.complete$SalePrice),testPurchase_dl_model_pca$predict)
# 

#GBM
submit<-(data.frame(cbind(Id=ID,SalePrice=predict_ensemble)))
write.csv(submit,file = "submit_ENSEMBLE.csv",row.names = F)

#DL
submit<-(data.frame(cbind(Id=ID,SalePrice=predict_ensemble)))
write.csv(submit,file = "submit_ENSEMBLE.csv",row.names = F)

#GLM
submit<-(data.frame(cbind(Id=ID,SalePrice=predict_ensemble)))
write.csv(submit,file = "submit_ENSEMBLE.csv",row.names = F)
# #Ok. Now we can fit the model to two diferent set of data.
# #Say big homes, Small Homes, Average Homes, with same parameters 
# #of GBM and DL Above.
# #Then we will ensemble
# 
# 
# 
# 
# 
# 
# #Let us see hyperparameter search.
# 
# # Construct a large Cartesian hyper-parameter space
# ntrees_opts = c(10000)       # early stopping will stop earlier
# max_depth_opts = seq(1,20)
# min_rows_opts = c(1,5,10,20,50,100)
# learn_rate_opts = seq(0.001,0.01,0.001)
# sample_rate_opts = seq(0.3,1,0.05)
# col_sample_rate_opts = seq(0.3,1,0.05)
# col_sample_rate_per_tree_opts = seq(0.3,1,0.05)
# nbins_cats_opts = seq(100,10000,100) # no categorical features
# # in this dataset
# 
# hyper_params = list( ntrees = ntrees_opts, 
#                      max_depth = max_depth_opts, 
#                      min_rows = min_rows_opts, 
#                      learn_rate = learn_rate_opts,
#                      sample_rate = sample_rate_opts,
#                      col_sample_rate = col_sample_rate_opts,
#                      col_sample_rate_per_tree = col_sample_rate_per_tree_opts
#                      ,nbins_cats = nbins_cats_opts
# )
# 
# 
# # Search a random subset of these hyper-parmameters. Max runtime 
# # and max models are enforced, and the search will stop after we 
# # don't improve much over the best 5 random models.
# search_criteria = list(strategy = "RandomDiscrete", 
#                        max_runtime_secs = 6000, 
#                        max_models = 100, 
#                        stopping_metric = "RMSLE", 
#                        stopping_tolerance = 0.00001, 
#                        stopping_rounds = 5, 
#                        seed = 1234)
# 
# gbm_grid <- h2o.grid("gbm", 
#                      x=features,
#                      y = y,
#                      training_frame =pca_train,
#                      # # faster to use a 80/20 split
#                      # training_frame = trainSplit,
#                      # validation_frame = validSplit,
#                      # nfolds = 0,
#                      
#                      # alternatively, use N-fold cross-validation:
#                      # training_frame = train,
#                      nfolds = 2,
#                      
#                      # Gaussian is best for MSE loss, but can try 
#                      # other distributions ("laplace", "quantile"):
#                      distribution="gaussian",
#                      
#                      # stop as soon as mse doesn't improve by 
#                      # more than 0.1% on the validation set, 
#                      # for 2 consecutive scoring events:
#                      stopping_rounds = 2,
#                      stopping_tolerance = 1e-3,
#                      stopping_metric = "RMSLE",
#                      
#                      # how often to score (affects early stopping):
#                      score_tree_interval = 100, 
#                      
#                      ## seed to control the sampling of the 
#                      ## Cartesian hyper-parameter space:
#                      seed = 1234,
#                      hyper_params = hyper_params,
#                      search_criteria = search_criteria)
# 
# gbm_sorted_grid <- h2o.getGrid(grid_id = "Grid_GBM_RTMP_sid_b072_11_model_R_1492435550556_6", sort_by = "RMSLE")
# print(gbm_sorted_grid)
# 
# best_model <- h2o.getModel(gbm_sorted_grid@model_ids[[1]])
# summary(best_model)
# best_model
# #Best Model in Randam Descrete Search has CV RMSLE of 0.15
# 
# #So Let us do some feature engineering
# summary(combi.comp)
# #Model 2: Feature engineering based on house size.
# #Let us plan for 3 models for 3 partitioned datasets.
# #Too big homes Model 1.
# #Too Small homes Model 2.
# #Average Homes Model 3.
# 
# #Home Size proportional to Above Ground Living Area, Number of Rooms, Number of Bed Rooms
# #Garage Size,BedroomAbvGr,KitchenAbvGr,FullBath,GrLivArea,HouseStyle,BldgType,LotArea
# #Medium
# # Lower Boundary: combi.comp$GrLivArea>=quantile(combi.comp$GrLivArea)[2] – 1.5·IQR(combi.comp$GrLivArea)
# # Higher Boundary: combi.comp$GrLivArea<=quantile(combi.comp$GrLivArea)[4] + 1.5·IQR(combi.comp$GrLivArea)
# # Inbetween: Lower Boundary<=combi.comp$GrLivArea<=Higher Boundary
# 
# #By trial and error and to get minimum 30 values under
# Lower_Boundary<-as.numeric(770)
# Higher_Boundary<-as.numeric(quantile(combi.comp$GrLivArea)[4] + (1.5*IQR(combi.comp$GrLivArea)))
# #Inbetween: Lower_Boundary<=combi.comp$GrLivArea<=Higher_Boundary
# 
# big_house<-train.complete[which(train.complete$GrLivArea>=Higher_Boundary),]
# summary(big_house$SalePrice)
# small_house<-train.complete[which(train.complete$GrLivArea<=Lower_Boundary),]
# summary(small_house$SalePrice)
# average_house<-train.complete[which(train.complete$GrLivArea>=Lower_Boundary
#               &  train.complete$GrLivArea<=Higher_Boundary ),]
# summary(average_house$SalePrice)
# 
# train.complete[which(train.complete$SalePrice==min(train.complete$SalePrice)),]
# 
# #Removing Redundant Columns, based on business knowledge or redundancy.
# exclude<-c("LotShape","LotConfig","Condition1","OverallCond","YearRemodAdd",
# "RoofStyle","MasVnrArea","MasVnrType","ExterCond","BsmtCond","BsmtFinSF1","BsmtFinType2",
# "BsmtExposure","BsmtFinType1","BsmtFinSF2","BsmtUnfSF","GrLivArea","BsmtFullBath",
# "Fireplaces","FireplaceQu","GarageType","GarageYrBlt","GarageFinish","WoodDeckSF","OpenPorchSF",
# "EnclosedPorch","Fence","SaleType","SaleCondition")
# 
# 
# #Average Home Modeling
# 
# feature_exclusion=names(train.complete)[!names(train.complete) %in% c("SalePrice",exclude)]
# train.hex<-as.h2o((average_house[c(feature_exclusion,y)]))
# 
# 
# #Looks like GLRM is better in handling mixed datatypes than
# pca_fe<-h2o.prcomp(train.hex[feature_exclusion],k=30,pca_method="GLRM",
# use_all_factor_levels=TRUE,transform="STANDARDIZE",seed=1234)
# pca_fe
# pca_data<-h2o.predict(pca_fe,train.hex[feature_exclusion])
# pca_train<-h2o.cbind(pca_data[1:22],train.hex$SalePrice)
# features=names(pca_train)[!names(pca_train) %in% c("SalePrice")]
# 
# gbm_fe = h2o.gbm( x=features,
#                     y = y,
#                     training_frame =pca_train
#                     #,nfolds = 2
#                     ,nbins=50
#                     ,learn_rate = 0.0001
#                     ,ntrees = 10000
#                     ,distribution="AUTO"
#                     ,max_depth=30
#                     ,min_rows=15
#                     ,col_sample_rate_per_tree=0.7
#                     ,col_sample_rate=0.7
#                     ,sample_rate=0.6
#                     ,nbins_cats = 40
#                     ,seed=1234
# )
# 
# gbm_fe
# 
# h2o.varimp(gbm_fe)
# 
# #Trying PCA Mix
# 
# 
# #Maybe we have to do multifactor PCA.
# library(PCAmixdata)
# 
# # data(wine)
# # X.quanti <- wine[,c(3:29)]
# # X.quali <- wine[,c(1,2)]
# # pca<-PCAmix(X.quanti,X.quali,ndim=4, graph=FALSE)
# # summary(pca)
# # pca$eig
# # pca$ind$coord
# # nrow(pca$ind$coord)
# # nrow(wine)
# y<-c("SalePrice")
# features=names(train.complete)[!names(train.complete) %in% c("SalePrice",exclude)]
# datasplit<-splitmix(combi.comp[features])
# X.quanti<-datasplit$X.quanti
# X.quali<-datasplit$X.quali
# pca<-PCAmix(X.quanti=X.quanti,X.quali=X.quali,ndim=10, rename.level=TRUE,graph=FALSE)
# pca$eig
# colnames(pca$ind$coord)
# pca<-PCAmix(X.quanti=X.quanti,X.quali=X.quali,ndim=98, rename.level=TRUE,graph=FALSE)
# colnames(pca$ind$coord)
# 
# train<-cbind(data.frame(pca$ind$coord)[1:1460,],SalePrice=train.complete$SalePrice)
# test<-data.frame(pca$ind$coord)[1460:2919,]
# names(train)
# features=names(train)[!names(train) %in% c("SalePrice")]
# train.hex<-as.h2o(train)
# 
# gbm_fe = h2o.gbm( x=features,
#                   y = y,
#                   training_frame =train.hex
#                   ,nfolds = 2
#                   ,nbins=100
#                   ,learn_rate = 0.0001
#                   ,ntrees = 10000
#                   ,distribution="AUTO"
#                   ,max_depth=30
#                   ,min_rows=20
#                   ,col_sample_rate_per_tree=0.7
#                   ,col_sample_rate=0.7
#                   ,sample_rate=0.6
#                   ,nbins_cats = 2000
#                   ,seed=1234
# )
# 
# gbm_fe
# 
# 
# 
# 
# 
# recoded<-recod(X.quanti, X.quali,rename.level=TRUE)
# numerized<-recoded$Z
# length(colnames(numerized))
# View(recoded$Z)
# colnames(numerized)<-paste(x,1:length(colnames(numerized)),sep="")
# colnames(numerized)
# overlltrain<-data.frame(cbind(numerized[1:1460,],"SalePrice"=train.complete$SalePrice))
# names(overlltrain)
# recodedtrain<-data.frame(numerized[1:1460,])
# recodedtest<-data.frame(numerized[1460:2919,])
# 
# train.hex<-as.h2o(overlltrain)
# features=names(overlltrain)[!names(overlltrain) %in% c("SalePrice")]
# 
# gbm_fe = h2o.gbm( x=features,
#                   y = y,
#                   training_frame =train.hex
#                   ,nfolds = 2
#                   ,nbins=10
#                   ,learn_rate = 0.0001
#                   ,ntrees = 10000
#                   ,distribution="AUTO"
#                   ,max_depth=20
#                   ,min_rows=15
#                   ,col_sample_rate_per_tree=0.8
#                   ,col_sample_rate=0.7
#                   ,sample_rate=0.6
#                   ,nbins_cats = 200
#                   ,seed=1234
# )
# 
# gbm_fe
# 
# #Trying PCA Mix
# 
# pca<-PCAmix(X.quanti=numerized,X.quali=NULL,ndim=10, rename.level=TRUE,graph=FALSE)
# pca$eig
# # #You can see that at 97 cummulative addition is 99%
# pca_new<-PCAmix(X.quanti=numerized,X.quali=NULL,ndim=97, rename.level=TRUE,graph=FALSE)
# pca.train.data<-predict(pca_new,X.quanti=train2,X.quali=NULL,rename.level=TRUE)
# #There are constant columns which needs to be removed.
# library(digest)
# train2 = recodedtrain[!duplicated(lapply(recodedtrain, digest))]
# names(recodedtrain[, sapply(recodedtrain, function(v) var(v, na.rm=TRUE)==0)])
# 
# # pca<-PCAmix(X.quanti=X.quanti,X.quali=X.quali,ndim=10, rename.level=TRUE,graph=FALSE)
# # pca$eig
# # #You can see that at 95 cummulative addition is 99%
# # pca_new<-PCAmix(X.quanti=X.quanti,X.quali=X.quali,ndim=95, rename.level=TRUE,graph=FALSE)
# # pca.train.data<-predict(pca_new,X.quanti=X.quanti,X.quali=X.quali,rename.level=TRUE)
#   
# 
# h2o.shutdown(prompt = F)
