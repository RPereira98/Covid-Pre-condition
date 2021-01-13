###########
#Libraries#
###########
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(ada)) install.packages("ada", repos = "http://cran.us.r-project.org")
if(!require(plyr)) install.packages("plyr", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
if(!require(party)) install.packages("party", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(gridExtra)
library(ada)
library(plyr)
library(xgboost)
library(party)

####################
#Importing Data Set#
####################
#Downloading data and creating data set
url <- "https://github.com/RPereira98/Covid-Pre-condition/raw/main/covid.zip"
dl <- tempfile()
download.file(url, dl)
unzip(dl,"covid.csv")
covid_dat <- read_csv("covid.csv")
covid_dat<-data.frame(covid_dat)
#Covid_res=1 means that the patient is covid positive
ind<- which(covid_dat$covid_res==1)
covid_dat<-covid_dat[ind,]
#Dates, id of patient,intubation,pregnancy,ICU will  not be used for this algorithm
covid_dat<-covid_dat[,-c(1,4,5,6,7,10,21,22,23)]
#97,98,99 values are NAs, not useful
ind<- which((covid_dat$pneumonia%in%c(97,98,99))|(covid_dat$diabetes%in%c(97,98,99))|(covid_dat$copd%in%c(97,98,99))|(covid_dat$asthma%in%c(97,98,99))|(covid_dat$inmsupr%in%c(97,98,99))|
(covid_dat$hypertension%in%c(97,98,99))|(covid_dat$other_disease%in%c(97,98,99))|(covid_dat$cardiovascular%in%c(97,98,99))|(covid_dat$obesity%in%c(97,98,99))|
(covid_dat$renal_chronic%in%c(97,98,99))|(covid_dat$tobacco%in%c(97,98,99)))
covid_dat<-covid_dat[-ind,]

##############################
#Split train set and test set#
##############################
#The test set is 20% of the original data set
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = covid_dat$patient_type, times = 1, p = 0.2, list = FALSE)
train_set<-covid_dat[-test_index,]
test_set<-covid_dat[test_index,]
ind<-which(test_set$patient_type==1)#patients of the test set that are outpatients

###############
#Visualization#
###############
#Visual comparison of the different pre-conditions with the need of hospitalization or not
#Correlation value of the different pre-conditions with the need of hospitalization
#1:outpatient: a patient who receives medical treatment without being admitted to a hospital
#2:inpatient:a patient who's been admitted to hospital for medical treatment
#Parameter:Sex
p1<-train_set%>%filter(sex==1)%>%ggplot(aes(as.factor(patient_type))) + geom_bar()+ggtitle("Female")+
theme(plot.title = element_text(hjust = 0.5))+labs(x="Patient type")
p2<-train_set%>%filter(sex==2)%>%ggplot(aes(as.factor(patient_type))) + geom_bar()+ggtitle("Male")+
theme(plot.title = element_text(hjust = 0.5))+labs(x="Patient type")
grid.arrange(p1,p2,ncol=2)
#Parameter: Pneumonia
p1<-train_set%>%filter(pneumonia==1)%>%ggplot(aes(as.factor(patient_type))) + geom_bar()+ggtitle("Pneumonia")+
theme(plot.title = element_text(hjust = 0.5))+labs(x="Patient type")
p2<-train_set%>%filter(pneumonia==2)%>%ggplot(aes(as.factor(patient_type))) + geom_bar()+ggtitle("No pneumonia")+
theme(plot.title = element_text(hjust = 0.5))+labs(x="Patient type")
grid.arrange(p1,p2,ncol=2)
#Parameter:Diabetes
p1<-train_set%>%filter(diabetes==1)%>%ggplot(aes(as.factor(patient_type))) + geom_bar()+ggtitle("Diabetes")+
theme(plot.title = element_text(hjust = 0.5))+labs(x="Patient type")
p2<-train_set%>%filter(diabetes==2)%>%ggplot(aes(as.factor(patient_type))) + geom_bar()+ggtitle("No diabetes")+
theme(plot.title = element_text(hjust = 0.5))+labs(x="Patient type")
grid.arrange(p1,p2,ncol=2)
#Parameter:COPD
p1<-train_set%>%filter(copd==1)%>%ggplot(aes(as.factor(patient_type))) + geom_bar()+ggtitle("COPD")+
theme(plot.title = element_text(hjust = 0.5))+labs(x="Patient type")
p2<-train_set%>%filter(copd==2)%>%ggplot(aes(as.factor(patient_type))) + geom_bar()+ggtitle("No COPD")+
theme(plot.title = element_text(hjust = 0.5))+labs(x="Patient type")
grid.arrange(p1,p2,ncol=2)
#Parameter:Asthma
p1<-train_set%>%filter(asthma==1)%>%ggplot(aes(as.factor(patient_type))) + geom_bar()+ggtitle("Asthma")+
theme(plot.title = element_text(hjust = 0.5))+labs(x="Patient type")
p2<-train_set%>%filter(asthma==2)%>%ggplot(aes(as.factor(patient_type))) + geom_bar()+ggtitle("No asthma")+
theme(plot.title = element_text(hjust = 0.5))+labs(x="Patient type")
grid.arrange(p1,p2,ncol=2)
#Parameter:Immunosuppressed
p1<-train_set%>%filter(inmsupr==1)%>%ggplot(aes(as.factor(patient_type))) + geom_bar()+ggtitle("Immunosuppressed")+
theme(plot.title = element_text(hjust = 0.5))+labs(x="Patient type")
p2<-train_set%>%filter(inmsupr==2)%>%ggplot(aes(as.factor(patient_type))) + geom_bar()+ggtitle("No immunosuppressed")+
theme(plot.title = element_text(hjust = 0.5))+labs(x="Patient type")
grid.arrange(p1,p2,ncol=2)
#Parameter:Hypertension
p1<-train_set%>%filter(hypertension==1)%>%ggplot(aes(as.factor(patient_type))) + geom_bar()+ggtitle("Hypertension")+
theme(plot.title = element_text(hjust = 0.5))+labs(x="Patient type")
p2<-train_set%>%filter(hypertension==2)%>%ggplot(aes(as.factor(patient_type))) + geom_bar()+ggtitle("No hypertension")+
theme(plot.title = element_text(hjust = 0.5))+labs(x="Patient type")
grid.arrange(p1,p2,ncol=2)
#Parameter:Other disease
p1<-train_set%>%filter(other_disease==1)%>%ggplot(aes(as.factor(patient_type))) + geom_bar()+ggtitle("Other diseases")+
theme(plot.title = element_text(hjust = 0.5))+labs(x="Patient type")
p2<-train_set%>%filter(other_disease==2)%>%ggplot(aes(as.factor(patient_type))) + geom_bar()+ggtitle("No other diseases")+
theme(plot.title = element_text(hjust = 0.5))+labs(x="Patient type")
grid.arrange(p1,p2,ncol=2)
#Parameter:Cardiovascular diseases
p1<-train_set%>%filter(cardiovascular==1)%>%ggplot(aes(as.factor(patient_type))) + geom_bar()+ggtitle("Cardiovascular  diseases")+
theme(plot.title = element_text(hjust = 0.5))+labs(x="Patient type")
p2<-train_set%>%filter(cardiovascular==2)%>%ggplot(aes(as.factor(patient_type))) + geom_bar()+ggtitle("No cardiovascular diseases")+
theme(plot.title = element_text(hjust = 0.5))+labs(x="Patient type")
grid.arrange(p1,p2,ncol=2)
#Parameter:Obesity
p1<-train_set%>%filter(obesity==1)%>%ggplot(aes(as.factor(patient_type))) + geom_bar()+ggtitle("Obesity")+
theme(plot.title = element_text(hjust = 0.5))+labs(x="Patient type")
p2<-train_set%>%filter(obesity==2)%>%ggplot(aes(as.factor(patient_type))) + geom_bar()+ggtitle("No obesity")+
theme(plot.title = element_text(hjust = 0.5))+labs(x="Patient type")
grid.arrange(p1,p2,ncol=2)
#Parameter:Chronic kidney disease 
p1<-train_set%>%filter(renal_chronic==1)%>%ggplot(aes(as.factor(patient_type))) + geom_bar()+ggtitle("Chronic kidney")+
theme(plot.title = element_text(hjust = 0.5))+labs(x="Patient type")
p2<-train_set%>%filter(renal_chronic==2)%>%ggplot(aes(as.factor(patient_type))) + geom_bar()+ggtitle("No chronic kidney")+
theme(plot.title = element_text(hjust = 0.5))+labs(x="Patient type")
grid.arrange(p1,p2,ncol=2)
#Parameter:Tobacco
p1<-train_set%>%filter(tobacco==1)%>%ggplot(aes(as.factor(patient_type))) + geom_bar()+ggtitle("Smoking habit")+
theme(plot.title = element_text(hjust = 0.5))+labs(x="Patient type")
p2<-train_set%>%filter(tobacco==2)%>%ggplot(aes(as.factor(patient_type))) + geom_bar()+ggtitle("No smoking habit")+
theme(plot.title = element_text(hjust = 0.5))+labs(x="Patient type")
grid.arrange(p1,p2,ncol=2)
#Parameter:Age
train_set%>%ggplot(aes(age))+geom_histogram(color="black",fill="#FFFFCC")+ggtitle("Number of patients per age")+facet_grid(~patient_type)

################################
#Linear discriminant analysis#
################################
#Using linear discriminant analysis for the first model
train_lda<-train(as.factor(patient_type)~as.factor(sex)+as.factor(pneumonia)+age+
as.factor(diabetes)+as.factor(copd)+as.factor(asthma)+as.factor(inmsupr)+
as.factor(hypertension)+as.factor(other_disease)+as.factor(cardiovascular)+
as.factor(obesity)+as.factor(renal_chronic)+as.factor(tobacco),
data =train_set,method="lda")
lda_pred<-predict(train_lda,test_set)
cm<-confusionMatrix(table(as.numeric(lda_pred), test_set$patient_type))
lda_ac<-cm$overall[["Accuracy"]]#Accuracy 0.8618
lda_out<-mean(lda_pred[ind]==1)#Accuracy on outpatients
lda_in<-mean(lda_pred[-ind]==2)#Accuracy on inpatients

##########################
#Generalized linear model#
##########################
#Using a generalized linear model for the second model
train_glm<-train(as.factor(patient_type)~as.factor(sex)+as.factor(pneumonia)+age+
as.factor(diabetes)+as.factor(copd)+as.factor(asthma)+as.factor(inmsupr)+
as.factor(hypertension)+as.factor(other_disease)+as.factor(cardiovascular)+
as.factor(obesity)+as.factor(renal_chronic)+as.factor(tobacco),
method="glm",data =train_set,family = "binomial")
glm_pred<-predict(train_glm,test_set)
cm<-confusionMatrix(table(as.numeric(glm_pred), test_set$patient_type))
glm_ac<-cm$overall[["Accuracy"]] #Accuracy 0.8621
glm_out<-mean(glm_pred[ind]==1)#Accuracy on outpatients
glm_in<-mean(glm_pred[-ind]==2)#Accuracy on inpatients

#################################
#Quadratic discriminant analysis#
#################################
#Using quadratic discriminatory analysis for the third model
train_qda<-train(as.factor(patient_type)~as.factor(sex)+as.factor(pneumonia)+age+
as.factor(diabetes)+as.factor(copd)+as.factor(asthma)+as.factor(inmsupr)+
as.factor(hypertension)+as.factor(other_disease)+as.factor(cardiovascular)+
as.factor(obesity)+as.factor(renal_chronic)+as.factor(tobacco),
data =train_set,method="qda")
qda_pred<-predict(train_qda,test_set)
cm<-confusionMatrix(table(as.numeric(qda_pred), test_set$patient_type))
qda_ac<-cm$overall[["Accuracy"]]#Accuracy 0.8423
qda_out<-mean(qda_pred[ind]==1)#Accuracy on outpatients
qda_in<-mean(qda_pred[-ind]==2)#Accuracy on inpatients

#####################
#Classification tree#
#####################
#Using classification tree for the fourth model
train_rpart<-train(as.factor(patient_type)~as.factor(sex)+as.factor(pneumonia)+age+
as.factor(diabetes)+as.factor(copd)+as.factor(asthma)+as.factor(inmsupr)+
as.factor(hypertension)+as.factor(other_disease)+as.factor(cardiovascular)+
as.factor(obesity)+as.factor(renal_chronic)+as.factor(tobacco),
data = train_set,method="rpart")
rpart_pred<-predict(train_rpart,test_set)
cm<-confusionMatrix(table(as.numeric(rpart_pred), test_set$patient_type))
rpart_ac<-cm$overall[["Accuracy"]]#Accuracy 0.8637
rpart_out<-mean(rpart_pred[ind]==1)#Accuracy on outpatients
rpart_in<-mean(rpart_pred[-ind]==2)#Accuracy on inpatients

#############################
#Boosted Classification Tree#
#############################
#Using Boosted Classification Tree for the fifth model
train_ada<-train(as.factor(patient_type)~as.factor(sex)+as.factor(pneumonia)+age+
                   as.factor(diabetes)+as.factor(copd)+as.factor(asthma)+as.factor(inmsupr)+
                   as.factor(hypertension)+as.factor(other_disease)+as.factor(cardiovascular)+
                   as.factor(obesity)+as.factor(renal_chronic)+as.factor(tobacco),
                 method="xgbTree", trControl = trainControl("cv", number = 5),
                 data =train_set)
train_ada$bestTune
ada_pred<-predict(train_ada,test_set)
cm<-confusionMatrix(table(as.numeric(ada_pred), test_set$patient_type))
ada_ac<-cm$overall[["Accuracy"]] #Accuracy 0.8621
ada_out<-mean(ada_pred[ind]==1)#Accuracy on outpatients
ada_in<-mean(ada_pred[-ind]==2)

############################
#Conditional Inference Tree#
############################
#Using Conditional Interference Tree for the sixth model
train_cit<-train(as.factor(patient_type)~as.factor(sex)+as.factor(pneumonia)+age+
                   as.factor(diabetes)+as.factor(copd)+as.factor(asthma)+as.factor(inmsupr)+
                   as.factor(hypertension)+as.factor(other_disease)+as.factor(cardiovascular)+
                   as.factor(obesity)+as.factor(renal_chronic)+as.factor(tobacco),
                   method="ctree", data =train_set)
cit_pred<-predict(train_cit,test_set)
cm<-confusionMatrix(table(as.numeric(cit_pred), test_set$patient_type))
cit_ac<-cm$overall[["Accuracy"]] #Accuracy 0.8627
cit_out<-mean(cit_pred[ind]==1)#Accuracy on outpatients
cit_in<-mean(cit_pred[-ind]==2)#Accuracy on inpatients

##########
#Ensemble#
##########
#Using the other six models to create an ensemble for the seventh model
ensemble<-data.frame(LDA=as.numeric(lda_pred),
                     QDA=as.numeric(qda_pred),
                     GLM=as.numeric(glm_pred),
                     RPART=as.numeric(rpart_pred),
                     ADA=as.numeric(ada_pred),
                     CIT=as.numeric(cit_pred))
ensemble_pred<-ifelse(rowMeans(ensemble)<=9/6,1,2)#There are more outpatients
#than inpatients, so in case of tie, predict outpatient
cm<-confusionMatrix(table(ensemble_pred, test_set$patient_type))
ensemble_ac<-cm$overall[["Accuracy"]] #
ensemble_out<-mean(ensemble_pred[ind]==1)#Accuracy on outpatients
ensemble_in<-mean(ensemble_pred[-ind]==2)#Accuracy on inpatients

############
#Ensemble 2#
############
#From the plots could be seen that there are considerable more inpatients  with
#pneumonia,chronic kidney disease, COPD and diabetes than outpatients with the
#same preconditions
ensemble_pred2<-ifelse(rowMeans(ensemble)<=9/6,1,2)
ind2<-which(rowMeans(ensemble)==9/6)#for ties in the ensemble
ensemble_pred2[ind2]<-ifelse(test_set$pneumonia[ind2]==1,2,
                             ifelse(test_set$renal_chronic[ind2]==1,2,
                                    ifelse(test_set$copd[ind2]==1,2,
                                           ifelse(test_set$diabetes[ind2]==1,2,1))))
cm<-confusionMatrix(table(ensemble_pred2, test_set$patient_type))
ensemble2_ac<-cm$overall[["Accuracy"]]#Accuracy 
ensemble2_out<-mean(ensemble_pred2[ind]==1)#Accuracy on outpatients
ensemble2_in<-mean(ensemble_pred2[-ind]==2)#Accuracy on inpatients

############
#Ensemble 3#
############
#Similar to Ensemble 2 but using all the pre-conditions that have more inpatients
#than outpatients
ensemble_pred3<-ifelse(rowMeans(ensemble)<=9/6,1,2)
ind2<-which(rowMeans(ensemble)==9/6)#for ties in the ensemble
ensemble_pred3[ind2]<-ifelse(test_set$pneumonia[ind2]==1,2,
                             ifelse(test_set$renal_chronic[ind2]==1,2,
                              ifelse(test_set$copd[ind2]==1,2,
                              ifelse(test_set$diabetes[ind2]==1,2,
                              ifelse(test_set$inmsupr[ind2]==1,2,
                              ifelse(test_set$hypertension[ind2]==1,2,
                              ifelse(test_set$cardiovascular[ind2]==1,2,1)))))))
cm<-confusionMatrix(table(ensemble_pred3, test_set$patient_type))
ensemble3_ac<-cm$overall[["Accuracy"]]#Accuracy 
ensemble3_out<-mean(ensemble_pred3[ind]==1)#Accuracy on outpatients
ensemble3_in<-mean(ensemble_pred3[-ind]==2)#Accuracy on inpatients

##########
#Accuracy#
##########
#Creating a data frame with the overall accuracy of the seven models
accuracy<-data.frame(row.names = c("LDA","GLM","QDA","RPART","BTREE","CTREE","ENSEMBLE","ENSEMBLE2","ENSEMBLE3"),
                     Accuracy=c(lda_ac,glm_ac,qda_ac,rpart_ac,ada_ac,cit_ac,ensemble_ac,ensemble2_ac,ensemble3_ac))
accuracy
#Creating a data frame with the accuracy of the outpatients
acc_out<-data.frame(row.names = c("LDA","GLM","QDA","RPART","BTREE","CTREE","ENSEMBLE","ENSEMBLE2","ENSEMBLE3"),
                    Accuracy=c(lda_out,glm_out,qda_out,rpart_out,ada_out,cit_out,ensemble_out,ensemble2_out,ensemble3_out))
acc_out
#Creating a data frame with the accuracy of  the inpatients
acc_in<-data.frame(row.names = c("LDA","GLM","QDA","RPART","BTREE","CTREE","ENSEMBLE","ENSEMBLE2","ENSEMBLE3"),
                   Accuracy=c(lda_in,glm_in,qda_in,rpart_in,ada_in,cit_in,ensemble_in,ensemble2_in,ensemble3_in))
acc_in