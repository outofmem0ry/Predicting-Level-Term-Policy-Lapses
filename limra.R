library(car)
library(caret)
library(ROCR)
library(DMwR)
library(party)
library(rpart)
library(randomForest)


set.seed(999)
sample = read.csv("data/srsample_Team3.csv")
rownames(sample) <- NULL


# Data modification

sample = within(data=sample,rm("Replicate","billingtype","dissysused",
                               "TERMDATE","birthyear","birthmonth","ANNPREM",
                               "IssueRes","TerminationDay","TerminationMonth",
                               "TerminationYear","ModeOfPayment","IssueState" ))


# Integer to factor conversion
sample = subset(sample,Plan == "22" | Plan == "24")

tofactor = c("Sex","DOBday","DOBmonth","IssDay","IssMonth","TerminationCauseI","Plan",
             "SmokerStatus","DOBYear","IssYear")

sample[tofactor] = lapply(sample[tofactor],as.factor)

sample = subset(sample,Sex == "0" | Sex=="1" | Sex=="2")
sample$Sex <- relevel(sample$Sex, ref = "1") 


sample$TerminationCauseI<-recode(sample$TerminationCauseI,"c(1,2,3,7,14,'D','F','E')=1")
sample$TerminationCauseI<-recode(sample$TerminationCauseI,"c(4,6,9,'A','C','Z')=0")
row.names(sample) <- NULL


dataSample <- within(data=sample,rm("DOBYear","IssYear","DOBday",
                                    "DOBmonth","IssDay","IssMonth"))
row.names(sample) <- NULL
colnames(dataSample)[5] <- "Lapse"

dataSample$IssueAge[dataSample$IssueAge<=17] <- "10-17"
dataSample$IssueAge[(dataSample$IssueAge > 17 & dataSample$IssueAge <= 25 )] <- "18-25"
dataSample$IssueAge[(dataSample$IssueAge > 25 & dataSample$IssueAge <= 35 )] <- "26-35"
dataSample$IssueAge[(dataSample$IssueAge > 35 & dataSample$IssueAge <= 45 )] <- "36-45"
dataSample$IssueAge[(dataSample$IssueAge > 45 & dataSample$IssueAge <= 55 )] <- "46-55"
dataSample$IssueAge[(dataSample$IssueAge > 55 & dataSample$IssueAge <= 65 )] <- "56-65"
dataSample$IssueAge[dataSample$IssueAge > 65] <- "66+"

dataSample$IssueAge <- as.factor(dataSample$IssueAge)
# 10-17, 18-25, 26-35, 36-45, 46-55,56-65, 66+
# trainIndex <- createDataPartition(dataSample$Lapse, p = .75,list=FALSE)
# trainData <-  dataSample[ trainIndex,]
# testData  <- dataSample[-trainIndex,]

ind1 <- which(dataSample[,"Lapse"]==1)
ind0 <- which(dataSample[,"Lapse"]==0)

sampsize <- 25000

sampind1 <- sample(ind1, sampsize, replace = FALSE)
sampind0 <- sample(ind0, sampsize, replace = FALSE)

sampind <- c(sampind1,sampind0)
trainData = dataSample[sampind,-2]
testData = dataSample[-sampind,-2]
row.names(trainData) <- NULL
row.names(testData) <- NULL

############## Generalized Linear Model ############## 
modelGLM = glm(Lapse~ Duration+Plan,family=binomial,data=trainData)
plsclassesGLM = predict(modelGLM,newdata = testData,type="response")
summary(modelGLM)
plsclasses = ifelse(plsclassesGLM>0.5,1,0)
confusionMatrix(data = plsclasses, testData$Lapse,positive = "1")

############## ############## ############## 

############## Generalized Boosted Regression Models ############## 
modelGBM = train(Lapse~.,method="gbm",data=trainData)
plsclassesGBM = predict(modelGBM,newdata = testData)
confusionMatrix(data = plsclassesGBM, testData$Lapse,positive = "1")

############## ############## ############## 

############## Decision Tree using recursive partitioning ############## 
# modelCtree<- ctree(Lapse ~ ., data=trainData)
# predictTestTree = predict(modelCtree, newdata=testData)
# confusionMatrix(data=predictTestTree,testData$Lapse,positive="1")
# plot(modelCtree,type="simple")
# n <- names(trainData)
# f <- as.formula(paste("Lapse ~", paste(n[!n %in% "Lapse"], collapse = " + ")))
############## ############## ############## 

############## Decision Tree ############## 
modelDecision = rpart(Lapse ~ ., data=trainData, method="class")
predictDecisionTree = predict(modelDecision, newdata=testData, type="class")
#head(testData$Lapse)
confusionMatrix(data=predictDecisionTree,reference=testData$Lapse,positive = "1")
text(modelDecision)
post(modelDecision, file = "tree.ps", title = "Classification Tree")
############## ############## ############## ############## 

############# RandomForest ##############
modelRF = randomForest(Lapse ~ ., data=trainData)
predictRF = predict(modelRF, newdata=testData, type="class")
confusionMatrix(data=predictRF,reference=testData$Lapse,positive = "1")
print(modelRF)
importance(modelRF)
str(modelRF)
modelRF$frame
############## ############## ############## ############## 

############## Future Scope ############## 
# #### Try with logging the amount######
# trainData$ExpInitialAmt = log(trainData$ExpInitialAmt)
# Probit Model Logit Model Togit Model
############## Future Scope ############## 