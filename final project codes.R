###Qiang Fei
###Lingjue Xie
###Zheqin Li


#The data.frame we used, named testing_final and training_final are submitted with the file 
#since some adjustments on testing data are not done by coding


##############################################################TRAINING
##############CLEANING DATA  
lafdtraining <- read.csv("~/Desktop/Statistics 101C/final project/lafdtraining.csv")
training_final <- na.omit(lafdtraining)



##############ADD/DELETE VARIABLES
training_final <- training_final[,-5] #delete "Emergency Dispatch Code"

#fd and incident
training_final[,"fd"] <- substring(training_final$incident.ID,1,7) 
training_final[,"incident"] <- substring(training_final$incident.ID,9,length(training_final$incident.ID))
training_final$incident <- as.numeric(training_final$incident)


#adding the variable "dispatch count":
training_final$Dispatch.count <- table(training_final$incident.ID)[training_final$incident.ID]


#adding the variable "hour"
length(training_final$Incident.Creation.Time..GMT.)
ICHour1<-c()
for(i in 1:2315060){
  ICHour1[i]=as.factor(substr(training_final$Incident.Creation.Time..GMT.[i],start=1, stop=2))
}
training_final[,"hour"]=ICHour1


#adding the variable "ICHourDiv"
training_final$ICHourDiv=1
training_final[training_final$hour>5,]$ICHourDiv <-2
training_final[training_final$hour>11,]$ICHourDiv <-3
training_final[training_final$hour>17,]$ICHourDiv <-4


#add the fifth part for hour 9
training_final[training_final$hour==9,]$ICHourDiv =5
training_final$ICHourDiv=as.factor(training_final$ICHourDiv)

#changing hour into a factor
training_final$hour=as.factor(training_final$hour)


#adding the variable "district order":
table(factor(training_final$First.in.District))
elapsed_mean<-c()
elapsed_var<-c()
diff<-c()
diff[1]<-0
for(i in 1:112){
  elapsed_mean[i]<-mean(training_final[which(training_final$First.in.District==i),]$elapsed_time)
  elapsed_var[i]<-var(training_final[which(training_final$First.in.District==i),]$elapsed_time)
  if(i>1) diff[i]<-elapsed_mean[i]-elapsed_mean[i-1]
}
district_time<-data.frame(district=1:112,time=elapsed_mean,variance=elapsed_var)
district_time<-district_time[order(district_time$time),]
district_time<-na.omit(district_time)
district_time[,"order"]=c(1:102)
training_final$District.Order<-1
for(i in 1:112){
  training_final[which(training_final$First.in.District==i),]$District.Order<-district_time[which(district_time$district==i),]$order
}


#re-ordering 
training_final.notordered=training_final
training_final=training_final [,c(3:8,14,11,12,13,16,10,1,2,9,15)]

#adjusting data.type
training_final$year=as.factor(training_final$year)
training_final$First.in.District=as.factor(training_final$First.in.District)
training_final$Dispatch.Sequence=as.numeric(training_final$Dispatch.Sequence)
training_final$fd=as.factor(training_final$fd)
tt$Dispatch.count=as.numeric(training_final$Dispatch.count)
training_final$elapsed_time=as.numeric(training_final$Dispatch.count)







##############################################################TESTING
##############CLEANING DATA
testing_without_response <- read_csv("~/Desktop/Statistics 101C/final project/testing.without.response.txt")
testing_final <- testing_without_response
#Here we change the NA in dispach sequence with more than 2 vhicles sent by hand
#The rest NAs were adjusted by the code:
testing_final[is.na(Dispatch.Sequence),]$Dispatch.Sequence <- 1


##############ADD/DELETE VARIABLES
testing_final <- testing_final[,-5] #delete "Emergency Dispatch Code"

#fd and incident
testing_final[,"fd"] <- substring(testing_final$incident.ID,1,7) #Q SAME Q
testing_final[,"incident"] <- substring(testing_final$incident.ID,9,length(testing_final$incident.ID))
testing_final$incident=as.numeric(testing_final$incident)

#adding the variable "dispatch count":
testing_final$Dispatch.count <- table(testing_final$incident.ID)[testing_final$incident.ID]


#adding the variable "hour"
length(testing_final$`Incident Creation Time (GMT)`)
ICHour2<-c()
for(i in 1:530352){
  ICHour2[i]=as.numeric(substr(testing_final$`Incident Creation Time (GMT)`[i],start=1, stop=2))
}
testing_final[,"hour"]=ICHour2


#adding the variable "ICHourDiv"    
testing_final$ICHourDiv=1
testing_final[testing_final$hour>5,]$ICHourDiv <-2
testing_final[testing_final$hour>11,]$ICHourDiv <-3
testing_final[testing_final$hour>17,]$ICHourDiv <-4


#add the fifth part for hour 9
testing_final[testing_final$hour==9,]$ICHourDiv =5
testing_final$ICHourDiv=as.factor(testing_final$ICHourDiv)

#changing hour into a factor
testing_final$hour=as.factor(testing_final$hour)


#re-ordering 
testing_final.notordered=testing_final
testing_final=testing_final [,c(3:8,13,10,11,12,1,2,9,14)]

#adjusting data.type
testing_final$year=as.factor(testing_final$year)
testing_final$First.in.District=as.factor(testing_final$First.in.District)
testing_final$Dispatch.Sequence=as.numeric(testing_final$Dispatch.Sequence)
testing_final$fd=as.factor(testing_final$fd)
tt$Dispatch.count=as.numeric(testing_final$Dispatch.count)




##########################################  
##As some operations on the data.frame are done without codes, we submit the data.frames we have unntil this point.
##All the changes afterwards are produced by the codes below
##########################################  




##############################################################DUMMY VARIABLES
library(car) 
library(caret) 
library(RCurl)
library(lubridate)
##############################################################TRAINING
View(training_final)
try <- training_final[,c(1:6,8,16)]
dmy <- dummyVars("~.", data=try)

train_used <- data.frame(predict(dmy, newdata = try))

save(train_used,file = "train_used.rda")


##############################################################TESTING
View(testing_final)
try2 <- testing_final[,c(1:6,8,14)]
dmy2 <- dummyVars("~.", data=try2)

test_used <- data.frame(predict(dmy2, newdata = try2))

save(test_used,file = "test_used.rda")


##predictors and response
outcome <- c('elapsed_time')
predictors.train <- names(train_used)[!names(train_used) %in% outcome]
predictors.test <- names(test_used)[!names(test_used) %in% outcome]

##clean: TRAINING has more levels than TESTING in "dispatch status,"
##so we delete the levels in training that testing doesn't have.
train_used<-train_used[,-139]
train_used<-train_used[,-142]
train_used<-train_used[,-144]

save(train_used,file = "train_used.clean.rda")


#add the new variables
train_used$"Dispatch.count"=training_final$Dispatch.count
test_used[,"Dispatch.count"]=testing_final$Dispatch.count
train_used[,"elapsed_time"]=training_final$elapsed_time

predictors.train <- names(train_used)[!names(train_used) %in% outcome]
predictors.test <- names(test_used)[!names(test_used) %in% outcome]

predictors.test==predictors.train #check if they have the same levels now: YES
predictors<-predictors.test



##############################################################MODELS + PREDICTIONS
library(xgboost)
xgb.fit <- xgboost(data = data.matrix(train_used[,predictors]),
                     label = data.matrix(train_used[,outcomeName]),eta=0.3,
                     max.depth=5, nround=12, objective = "reg:linear")
#max.depth=5, nround=12, eta=0.3
pred <- predict(xgb.fit, data.matrix(test_used[,predictors]), outputmargin=TRUE)
#Here we compare the five-number summary and try the models whose distribution 
#of prediction is close to the distribution of training elapsed_time
summary(pred)
summary(train_used$elapsed_time)


fin=testing_final
fin$prediction=pred
fin=fin[,c(11,15)]
write.csv(fin,file = "pred512eta0.3.csv",row.names = F) #5,12,0.3


