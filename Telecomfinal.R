
### Checking and setting a Working Directory ##
getwd()
setwd("C:/Users/nasar/Desktop/capstone project - Copy")

## Importing a  telecomfinal Dataset into RStudio ##

pd=read.csv("telecomfinal.csv")
View(pd)

## Checking the summary and structure of our data ##
summary(pd)
str(pd)

## Checking the no of rows and columns  of our data ##
dim(pd)

## Checking the  columns names  of our telecomfinal Dataset ##

names(pd)

## #Loading important libraries ##

library(dplyr)
library(dataQualityR)

## Checking the  Data quality of  our telecomfinal Dataset ##

checkDataQuality(data =pd,out.file.num = "Numeric.csv",out.file.cat = "Categorical.csv" )

## Finding the index numbers for the categorical variables  ##

which(colnames(pd)=="income") 
which(colnames(pd)=="asl_flag")
which(colnames(pd)=="prizm_social_one")
which(colnames(pd)=="refurb_new")
which(colnames(pd)=="hnd_webcap")
which(colnames(pd)=="marital")
which(colnames(pd)=="models")
which(colnames(pd)=="hnd_price")
which(colnames(pd)=="actvsubs")
which(colnames(pd)=="uniqsubs")
which(colnames(pd)=="forgntvl")
which(colnames(pd)=="dwlltype")
which(colnames(pd)=="mtrcycle")
which(colnames(pd)=="truck")
which(colnames(pd)=="churn")
which(colnames(pd)=="car_buy")
which(colnames(pd)=="crclscod")

##Seperating numerical variables for checking  outlier  ##

num<-pd
cat<-pd
num<-num[,-c(12,32,33,35,36,37,41,42,43,44,45,46,51,54,60,65,31)]
cat<-cat[,c(12,32,33,35,36,37,41,42,43,44,45,46,51,54,60,65,31)]

str(num)

## Checking the outliers for numerical variables ##

par(mfrow=c(3,5))
list<-names(num[,c(1:15)])
for(i in 1:length(list))
{ 
  boxplot(num[,list[i]],main=list[i]) 
}


par(mfrow=c(3,5))
list<-names(num[,c(16:29)])
for(i in 1:length(list))
{ 
  boxplot(num[,list[i]],main=list[i]) 
}


## Checking the Distribution for numerical variables ##

par(mfrow=c(3,5))
list<-names(num[,c(1:15)])
for(i in 1:length(list))
{ 
  hist(num[,list[i]],main=list[i]) 
}


par(mfrow=c(3,5))
list<-names(num[,c(16:29)])
for(i in 1:length(list))
{ 
  hist(num[,list[i]],main=list[i]) 
}

dev.off()


## Creating a dummy for retdays variable  ##

pd$retdays_Dummy <- ifelse(is.na(pd$retdays) == TRUE, 0, 1)
str(pd$retdays_Dummy)
summary(pd$retdays_Dummy)
which(colnames(x = pd)=="retdays")
pd=pd[,-53]

## numbcars variable is having 48.99% of missing values so removing the variable ##

which(colnames(x = pd)=="numbcars")
pd=pd[,-52]

## Removing the rows with missing values in the income variable ##

pd=pd[complete.cases(pd$income),]

## Finding the index numbers for the categorical variables  ##

which(colnames(pd)=="income") 
which(colnames(pd)=="asl_flag")
which(colnames(pd)=="prizm_social_one")
which(colnames(pd)=="refurb_new")
which(colnames(pd)=="hnd_webcap")
which(colnames(pd)=="marital")
which(colnames(pd)=="models")
which(colnames(pd)=="hnd_price")
which(colnames(pd)=="actvsubs")
which(colnames(pd)=="uniqsubs")
which(colnames(pd)=="forgntvl")
which(colnames(pd)=="dwlltype")
which(colnames(pd)=="mtrcycle")
which(colnames(pd)=="truck")
which(colnames(pd)=="churn")
which(colnames(pd)=="car_buy")



## Replacing missing values with Mean value for avg6mou,avg6qty Variable ##
#Treating missing values#

which(is.na(pd$avg6mou))   ## Finding the index number for the avg6mou Variable ##

pd$avg6mou[is.na(pd$avg6mou)]<-mean(pd$avg6mou,na.rm = T)

pd$avg6qty[is.na(pd$avg6qty)]<-round(mean(pd$avg6qty,na.rm = T))

## Removing the rows with missing values in the hnd_price variable ##

pd=pd[complete.cases(pd$hnd_price),]


## Replacing missing cells with Mean value for change_mou Variable ##
#Treating missing values#

which(is.na(pd$change_mou))  ## Finding the index number for the change_mou Variable ##

pd$change_mou[is.na(pd$change_mou)]<-mean(pd$change_mou,na.rm = T)

## Removing the rows with missing values in the  mou_Mean variable ##

pd=pd[complete.cases(pd$mou_Mean),]

## Removing the  Categorical Variables which are having  more than 50 % of  missing values  ##

which(colnames(pd)=="solflag")
pd=pd[,-59]
which(colnames(pd)=="wrkwoman")
pd=pd[,-53]
which(colnames(pd)=="div_type")
pd=pd[,-68]
which(colnames(pd)=="occu1")
pd=pd[,-49]
which(colnames(pd)=="proptype") 
pd=pd[,-57]
which(colnames(pd)=="cartype")
pd=pd[,-58]
which(colnames(pd)=="children")
pd=pd[,-59]
which(colnames(pd)=="mailordr")
pd=pd[,-48]
which(colnames(pd)=="mailresp")
pd=pd[,-56]
which(colnames(pd)=="csa")
pd=pd[,-57]
which(colnames(pd)=="crclscod")
pd=pd[,-31]
which(colnames(pd)=="dwllsize")
pd=pd[,-46]
which(colnames(pd)=="dwlltype") 
pd=pd[,-45]

## Removing the rows with missing values in the following  variable ##

pd=pd[complete.cases(pd$prizm_social_one),]
pd=pd[complete.cases(pd$area),]
pd=pd[complete.cases(pd$hnd_webcap),]   
 

## Converting numerical variables into Factor variables ##

pd$forgntvl<-as.factor(pd$forgntvl)
pd$mtrcycle<-as.factor(pd$mtrcycle)
pd$truck<-as.factor(pd$truck)
pd$churn<-as.factor(pd$churn)
pd$retdays_Dummy<-as.factor(pd$retdays_Dummy)
pd$actvsubs<-as.factor(pd$actvsubs)
pd$income<-as.factor(pd$income)
pd$uniqsubs<-as.factor(pd$uniqsubs)
pd$models<-as.factor(pd$models)
pd$hnd_price<-as.factor(pd$hnd_price)



## Selecting the Random sampling 70% of the data as a training and 30% as a testing ## ##

set.seed(200)
sampling<-sort(sample(nrow(pd), nrow(pd)*.7, replace = FALSE) )
sampling
length(sampling)

#Selecting  training and testing  samples in the names of train and test ##

train<-pd[sampling,]
test<-pd[-sampling,]

## Checking the no of rows in training and testing  samples ##
nrow(train)
nrow(test)

#The ratio of 1/0 should be comparable to original  dataset

table(pd$churn)/1000
table(train$churn)/700
table(test$churn)/300

#Checking Churn Rate 
table(train$churn)/nrow(train)
table(test$churn)/nrow(test)

##---------Logistic Regression----------##
##----- glm = generalized linear model-----##
#Building the first model using all the variables 
myresult<-glm( churn~. , data = train, family=binomial)
summary(myresult)


## Stepwise regression ##
step(myresult,direction="both")

## Running regression with what stepwise suggested ##
myresult<- glm(formula = churn ~  mou_Mean+totmrc_Mean + mou_Range + change_mou + 
                 drop_blk_Mean + owylis_vce_Range + months + eqpdays + custcare_Mean + 
                 iwylis_vce_Mean + ovrrev_Mean + rev_Mean + comp_vce_Mean + 
                 avg3mou + avgmou + avg6qty + asl_flag + prizm_social_one + 
                 area+ refurb_new+marital + ethnic + age1 + models + hnd_price+actvsubs 
                    uniqsubs+ mou_pead_Mean + da_Mean  + datovr_Range+
                 drop_vce_Mean + adjmou + adjrev + retdays_Dummy, family = binomial, 
               data = train)
summary(myresult)

## Creating dummy variables for the  training samples ##

train$asl_flagY<-ifelse(train$asl_flag=="Y",1,0)
train$prizm_social_oneR<-ifelse(train$prizm_social_one=="R",1,0)
train$areaNORTHWEST_ROCKY_MOUNTAIN_AREA<-ifelse(train$area=="NORTHWEST/ROCKY MOUNTAIN AREA",1,0)
train$areaSOUTH_FLORIDA_AREA<-ifelse(train$area=="SOUTH FLORIDA AREA",1,0)
train$areaSOUTHWEST_AREA<-ifelse(train$area=="SOUTHWEST AREA",1,0)
train$refurb_newR<-ifelse(train$refurb_new=="R",1,0)
train$ethnicC<-ifelse(train$ethnic=="C",1,0)
train$ethnicN<-ifelse(train$ethnic=="N",1,0)
train$ethnicp<-ifelse(train$ethnic=="P",1,0)
train$ethnicU<-ifelse(train$ethnic=="U",1,0)
train$ethnicZ<-ifelse(train$ethnic=="Z",1,0)
train$models2<-ifelse(train$models=="2",1,0)
train$models3<-ifelse(train$models=="3",1,0)
train$models4<-ifelse(train$models=="4",1,0)
train$models5<-ifelse(train$models=="5",1,0)
train$models6<-ifelse(train$models=="6",1,0)
train$hnd_price129.9899902<-ifelse(train$hnd_price=="129.9899902",1,0)
train$hnd_price199.9899902<-ifelse(train$hnd_price=="199.9899902",1,0)
train$hnd_price249.9899902<-ifelse(train$hnd_price=="249.9899902",1,0)
train$uniqsubs2<-ifelse(train$uniqsubs=="2",1,0)
train$uniqsubs3<-ifelse(train$uniqsubs=="3",1,0)
train$uniqsubs4<-ifelse(train$uniqsubs=="4",1,0)
train$uniqsubs5<-ifelse(train$uniqsubs=="5",1,0)
train$uniqsubs8<-ifelse(train$uniqsubs=="8",1,0)


##After creating the dummy variables  Again Running   the model##
myresult<- glm(formula = churn ~  totmrc_Mean + mou_Range + change_mou + 
                 drop_blk_Mean + owylis_vce_Range + months + eqpdays + custcare_Mean + 
                 iwylis_vce_Mean + ovrrev_Mean + rev_Mean + comp_vce_Mean + 
                 avg3mou + avgmou + avg6qty + asl_flagY + prizm_social_oneR + 
                 areaNORTHWEST_ROCKY_MOUNTAIN_AREA +areaSOUTH_FLORIDA_AREA+areaSOUTHWEST_AREA+ refurb_newR + ethnicC+ethnicN+ethnicp+ethnicU+ethnicZ + age1 + models2+models3+models4+models5+models6 + hnd_price129.9899902 + 
                 hnd_price199.9899902 +hnd_price249.9899902  + uniqsubs2+uniqsubs3+uniqsubs4+uniqsubs5+uniqsubs8 + mou_pead_Mean + da_Mean  + 
                 drop_vce_Mean + adjmou + adjrev + retdays_Dummy, family = binomial, 
               data = train)
summary(myresult)

## Removing insignificant variables and again running the model##
myresult<- glm(formula = churn ~  totmrc_Mean + mou_Range + change_mou + 
                 drop_blk_Mean + owylis_vce_Range + months + eqpdays + custcare_Mean + 
                 iwylis_vce_Mean + ovrrev_Mean + rev_Mean + comp_vce_Mean + 
                 avg3mou + avgmou + avg6qty + asl_flagY + prizm_social_oneR + 
                 areaNORTHWEST_ROCKY_MOUNTAIN_AREA +areaSOUTH_FLORIDA_AREA+areaSOUTHWEST_AREA+ refurb_newR + ethnicC+ethnicN+ethnicU+ethnicZ + age1 + models2+models3+models4+models5+models6 + hnd_price129.9899902 + 
                 hnd_price199.9899902 +hnd_price249.9899902  + uniqsubs2+uniqsubs3+uniqsubs4 + da_Mean  + 
                 drop_vce_Mean + adjmou + adjrev + retdays_Dummy, family = binomial, 
               data = train)
summary(myresult)

## ***** Model Diagnostics ***** ##

# Checking For Multicollinearity
library(car)
vif(myresult)

##Variables => Ideally vif values should be < 5. Choosing vif cut-off value of 5##
## Removing the Multicollinearity from the model ##

myresult<- glm(formula = churn ~  totmrc_Mean + mou_Range + change_mou + 
                 drop_blk_Mean + owylis_vce_Range + months + eqpdays + custcare_Mean + 
                 iwylis_vce_Mean 
                  + asl_flagY + prizm_social_oneR + 
                 areaNORTHWEST_ROCKY_MOUNTAIN_AREA +areaSOUTH_FLORIDA_AREA+areaSOUTHWEST_AREA+ refurb_newR + ethnicC+ethnicN+ethnicU+ethnicZ + age1 + models2+models3+models4+models5+models6 + hnd_price129.9899902 + 
                 hnd_price199.9899902 +hnd_price249.9899902  + uniqsubs2+uniqsubs3+uniqsubs4 + da_Mean  + 
                 drop_vce_Mean  + retdays_Dummy, family = binomial, 
               data = train)
summary(myresult)

## After removing  the Multicollinearity from the model removing the insignificant variables ##

myresult<- glm(formula = churn ~  totmrc_Mean + mou_Range + change_mou + 
                  owylis_vce_Range + months + eqpdays + custcare_Mean + 
                 iwylis_vce_Mean 
               + asl_flagY + prizm_social_oneR + 
                 areaNORTHWEST_ROCKY_MOUNTAIN_AREA +areaSOUTH_FLORIDA_AREA+areaSOUTHWEST_AREA+ refurb_newR + ethnicC+ethnicN+ethnicU+ethnicZ + age1 + models2+models3+models4+models5+models6 +  
                 hnd_price199.9899902 +hnd_price249.9899902  + uniqsubs2+uniqsubs3+uniqsubs4   + 
                 drop_vce_Mean  + retdays_Dummy, family = binomial, 
               data = train)
summary(myresult)

vif(myresult)
##All the vif values are well below 5. Thus there is no Multicollinearity. So this model is finalised ##

#Finding Predicted Values in the train data ##

predicted<-predict(myresult,type="response",newdata=train)
head(predicted)
hist(predicted)

#Assuming cut-off probablity as per the churn rate in data set ##

table(pd$churn) / nrow(pd)

 #As per churn rate in the dataset the cutoff probablity value is 0.22 ##
predicted1<-ifelse(predicted>=0.22,1,0)
head(predicted1)
table(predicted1)

# Creating the Confusion Matrix  from library Caret ##

library(caret)
library(e1071)
confusionMatrix(table(predicted1, train$churn),positive = "1")

## Plotting the Receiver operating characteristics curve from library ROCR ##

library(ROCR)
pred<-prediction(predicted,train$churn)
perf<-performance(pred,"tpr","fpr") #TPR= TP/P , FPR = FP/N
plot(perf)


## Getting the  Area under curve values ##

auc<-performance(pred,"auc")
auc<-unlist(slot(auc, "y.values"))
round(auc,2)

## Creating dummy variables for the  testing samples ##

test$asl_flagY<-ifelse(test$asl_flag=="Y",1,0)
test$prizm_social_oneR<-ifelse(test$prizm_social_one=="R",1,0)
test$areaNORTHWEST_ROCKY_MOUNTAIN_AREA<-ifelse(test$area=="NORTHWEST/ROCKY MOUNTAIN AREA",1,0)
test$areaSOUTH_FLORIDA_AREA<-ifelse(test$area=="SOUTH FLORIDA AREA",1,0)
test$areaSOUTHWEST_AREA<-ifelse(test$area=="SOUTHWEST AREA",1,0)
test$refurb_newR<-ifelse(test$refurb_new=="R",1,0)
test$ethnicC<-ifelse(test$ethnic=="C",1,0)
test$ethnicN<-ifelse(test$ethnic=="N",1,0)
test$ethnicp<-ifelse(test$ethnic=="P",1,0)
test$ethnicU<-ifelse(test$ethnic=="U",1,0)
test$ethnicZ<-ifelse(test$ethnic=="Z",1,0)
test$models2<-ifelse(test$models=="2",1,0)
test$models3<-ifelse(test$models=="3",1,0)
test$models4<-ifelse(test$models=="4",1,0)
test$models5<-ifelse(test$models=="5",1,0)
test$models6<-ifelse(test$models=="6",1,0)
test$hnd_price129.9899902<-ifelse(test$hnd_price=="129.9899902",1,0)
test$hnd_price199.9899902<-ifelse(test$hnd_price=="199.9899902",1,0)
test$hnd_price249.9899902<-ifelse(test$hnd_price=="249.9899902",1,0)
test$uniqsubs2<-ifelse(test$uniqsubs=="2",1,0)
test$uniqsubs3<-ifelse(test$uniqsubs=="3",1,0)
test$uniqsubs4<-ifelse(test$uniqsubs=="4",1,0)
test$uniqsubs5<-ifelse(test$uniqsubs=="5",1,0)
test$uniqsubs8<-ifelse(test$uniqsubs=="8",1,0)



## Model testing ##
#Finding Predicted Values ##

predicted<-predict(myresult,type="response",newdata=test)
head(predicted)
hist(predicted)
min(predicted)
max(predicted)

#confusion matrix require a data frame with class variable

predicted1<-ifelse(predicted>=0.22,1,0)
head(predicted1)
table(predicted1)

# Creating the Confusion Matrix  from library Caret ##

library(caret)
library(e1071)
confusionMatrix(table(predicted1, test$churn),positive = "1")

## Plotting the Receiver operating characteristics curve from library ROCR ##

library(ROCR)
pred<-prediction(predicted,test$churn)
perf<-performance(pred,"tpr","fpr") #TPR= TP/P , FPR = FP/N
plot(perf)


## Getting the  Area under curve values ##

auc<-performance(pred,"auc")
auc<-unlist(slot(auc, "y.values"))
round(auc,2)



## Gains chart##
library(gains)
gains(as.numeric(test$churn), predict(myresult, type="response", newdata=test), groups = 10)



test$prob <- predict(myresult, type="response", newdata = test)
quantile(test$prob, prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))


## Answering the Following questions##
###############################################################################################
##1.  What are the top five factors driving likelihood of churn at Mobicom?##

head(sort(abs(myresult$coefficients),decreasing = T),10)
summary(myresult)
#********************************************************
# Top 5 factors are #                Beta Coefficients 

  # 1)ethnicC                              1.3415012
  # 2)retdays_Dummy1                       0.9355378
  # 3)hnd_price249.9899902                 0.6279257
  # 4)models6                              0.5566793
  # 5)models4                              0.5188259

#********************************************************
#Special plans to be given for people with Ethnicity of level C.
#Special offers should be given to customers who makes retention calls at the earliest as per their grievance.
#################################################################################################

#2. Validation of survey findings. 
#a) Whether "cost and billing" and "network and service quality" are important factors influencing churn behaviour.
#**************************************************************
#  "cost and billing" Factors:                 Beta Coefficients
     # totmrc_Mean                                0.0050121312
     # asl_flagY                                  -0.3237215678 
#***************************************************************    
#totmrc_mean which defines Monthly Recurring Charge is the base cost
#of the calling plan regardless of actual minutes used.	 As per the model if there is a unit increase in totmrc_mean then 
#it would get increase by 0.00585827/unit in churn

#asl_flagY which defines Account spending limit As per the model if there is a unit increase in asl_flagY then 
#it would get decrease by 0.3237215678/unit in churn

#we notice above mentioned beta values,A unit increase in them will have negligible impact on churn..
#SO it seems cost and billing is not very important factors here influencing churn behaviour at Mobicom.

#****************************************************************
#  "network and service quality" Factors"      Beta Coefficients
     # change_mou                                -0.0001922063
     # mou_Range                                  0.0003401358
     # owylis_vce_Range                           0.0014935356
     # iwylis_vce_Mean                            0.0053902370
     # drop_vce_Mean                              0.0076722126
     # retdays_Dummy1                             0.9355378096
     # custcare_Mean                             -0.0179113888
#***************************************************************
# IN NETWORK AND SERVICE QUALITY,variable retdays_Dummy1 (retention) is leading to a big churn.
# As customers calling for their problems facing in services and queries should be
# handled carefully with their complaints and checking their history , offers should be given to them.


#2B) Are data usage connectivity issues turning out to be costly? In other words, is it leading to churn?
 
#No,data usage connectivity issues  not turning out to be costly so we need to influence customers to use data services which will also conclude to our'
#network quality connectivity which will reduce the customer churn and allows more customer
#to get invoved in data usage activity.

####################################################################################################


#3. Would you recommend rate plan migration as a proactive retention strategy?

# Variable asl_flagY is with -0.3237215678 coEff; Its a Account spending limit
# this means that if there is a unit increase in asl_flagY (Account spending limit) then 
#it would get decreases by 0.3237215678/unit in churn. so Cannot recommend rate plan migration as a proactive retention strategy.

#####################################################################################################

#4. What would be your recommendation on how to use this churn model for prioritisation of customers for a proactive retention campaigns in the future?

gains(as.numeric(test$churn), predict(myresult, type="response", newdata = test), groups=10)

#Top 30% probability shows 32.1%, approximately 30% are more likely to churn

#High Churn Rate prediction
test$highChurnProbability <- predict(myresult,type="response", newdata = test)
quantile(test$highChurnProbability, prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

#Result shows 30% of the probability contains <from 80 to 100> - 0.2809901- 0.8604595 This notifies us it may highly likely Churn

##Predicting a Customer Who will Churn and Apply Cut-off


customerPrediction <- predict(myresult, type="response", newdata=test)
customerPrediction <- ifelse(customerPrediction >= 0.2809901 , 1, 0)
table(customerPrediction, test$churn)

#taking <from 80 to 100> as per quantile result 
#and consider churn value  then to predicting  the customer by using unique variable to identify via Customer_ID
expectedPrediction <- test[test$highChurnProbability > 0.2809901 & 
                                test$highChurnProbability <= 0.8604595 & 
                                test$churn == "1","Customer_ID"]
expectedPrediction <- as.data.frame(expectedPrediction)
nrow(expectedPrediction) 

#816 Customers expected Prediction

#Predicted Customers who likely to churn 
write.csv(expectedPrediction, "Predicted Customers who likely to churn.csv", row.names = T)


# 5. What would be the target segments for proactive retention campaigns? Falling ARPU forecast is also a
# concern and therefore, Telecom Company would like to save their high revenue customers besides managing
# churn. Given a budget constraint of a contact list of 20% of the subscriber pool, which subscribers should
# prioritized if "revenue saves" is also a priority besides controlling churn. In other words, controlling churn
# is the primary objective and revenue saves is the secondary objective.


## Getting the Probability Score and Revenue value rate

saveRevenueCustomers <- predict(myresult, type="response",newdata=test)
test$highRevenueCustomers <- predict(myresult,type="response",newdata=test)
quantile(test$highRevenueCustomers, prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

savehighRevenueCustomers <- ifelse(saveRevenueCustomers < 0.20, "Low Probability", 
                                   ifelse(saveRevenueCustomers >= 0.20 & saveRevenueCustomers < 0.30, "Medium Probability", "High Probability"))

table(savehighRevenueCustomers, test$churn)

str(test$totrev)
quantile(test$totrev, prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
revenueValueRate <-  ifelse(test$totrev < 546.461, "Low Revenue", 
                            ifelse(test$totrev >= 546.461 & test$totrev < 924.820, "Medium Revenue", "High Revenue"))

table(savehighRevenueCustomers,revenueValueRate)

## This Result can help to select the level of customer need to be targeted 

summary(savehighRevenueCustomers)
summary(revenueValueRate)

#Inserting it in testing Data Set
test$ProbabilityRange <- ifelse(saveRevenueCustomers < 0.20, "Low Probability", 
                                   ifelse(saveRevenueCustomers >= 0.20 & saveRevenueCustomers < 0.30, "Medium Probability", "High Probability"))
test$RevenueRange <-  ifelse(test$totrev < 546.461, "Low Revenue", 
                                ifelse(test$totrev >= 546.461 & test$totrev < 924.820, "Medium Revenue", "High Revenue"))


TargetedCustomersforHighRevenue <- test[test$ProbabilityRange == "High Probability" & test$RevenueRange == "High Revenue","Customer_ID"]
TargetedCustomersforHighRevenue<- as.data.frame(TargetedCustomersforHighRevenue)
nrow(TargetedCustomersforHighRevenue) #882 Customers

#Predicted Customers who likely to be targetted with offers and etc etc to retain for High Revenue 
write.csv(TargetedCustomersforHighRevenue, "Predicted Customers who likely to retain for High Revenue.csv", row.names = T)


