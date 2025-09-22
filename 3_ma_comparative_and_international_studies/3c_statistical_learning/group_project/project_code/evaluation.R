rm(list=ls())

# Set your working directory here. 

setwd("C:\\Users\\xoxox\\polybox\\Master\\2. Semester\\Methods III\\labs\\Lab Report\\final\\m3_final resubmit\\LabReportM3")
# Template for final project #
##############################

library(caret)
library(ROCR)
library(dplyr)
library(magrittr)
library(doParallel)
library(gbm)
library(randomForest)

################################################################

# AUC FUNCTION
auc <- function(phat,y){
	pred <- prediction(phat, y)
	perf <- performance(pred,"auc")
	auc <- perf@y.values[[1]]
	return(auc)
}

# CARET SPEED UP
K <- parallel::detectCores()
cl <- makeCluster(K)
registerDoParallel(cl)

# DEFINE TRAIN CONTROL FOR CARET
set.seed(10)
fitControl <- trainControl(method = "cv", number = 10, classProbs = T, summaryFunction = twoClassSummary)


################################################################

load("AB4x_train.Rdata")
load("AB4x_eval_mock.Rdata")
# load("AB4x_eval.Rdata")


################################################################

# GET THE DATA READY: TRAIN
############################

# SUBSET NUMERIC AND CATEGORICAL VARIABLES

# subset non-categorical variables (numeric variables)
num.var <- subset(train, select = -c(country, q1, q2, q103, emig, q214, q217, q2061, q2062, q213, q264, q265, q301, q302, q403, 
                                     q4061, q4062, q409, q4113, q4114, q4116, q501, q501b, q5021, q5022,q523a, q515a1, q515a2, 
                                     q515a3, q605a, aid1a, aid31, aid32, aid41, aid42, aid5a, aid5b, aid5c, aid5d, aid5e, q707, 
                                     q709a, q713, q714, q832, q901, q1002, q1004, q1010, q1011a, q1011b, q1011c, q1012, q1017, q2008, 
                                     q511_combined, q512_combined, q513_combined, q1005_combined, q1006_combined, q1006a_combined, 
                                     edu_combined, income_categories, first_language, second_language, region))
num.var <- as.data.frame(num.var)
class(num.var)

# subset categorcial variables
# ATTENTION: q1 and q2 could be included here, but they might lead to a terrible overfit of the models -> excluded here
cat.var <- subset(train, select = c(country, q103, emig, q214, q217, q2061, q2062, q213, q264, q265, q301, q302, q403, q4061, q4062, 
                                    q409, q4113, q4114, q4116, q501, q501b, q5021, q5022,q523a, q515a1, q515a2, q515a3, q605a, aid1a, 
                                    aid31, aid32, aid41, aid42, aid5a, aid5b, aid5c, aid5d, aid5e, q707, q709a, q713, q714, q832, q901, 
                                    q1002, q1004, q1010, q1011a, q1011b, q1011c, q1012, q1017, q2008, q511_combined, q512_combined, 
                                    q513_combined, q1005_combined, q1006_combined, q1006a_combined, edu_combined, income_categories, 
                                    first_language, second_language, region))
cat.var <- as.data.frame(cat.var)
class(cat.var)

# CHANGE NON-CATEGORICAL VARIABLES TO NUMERIC
num.var %<>% mutate_if(is.factor,as.numeric)

# STANDARDIZE NUMERIC VARIABLES
standardize <- function(x){
  x <- x - mean(x, na.rm = T)
  return(x/sd(x, na.rm = T))
}

std.num <- apply(num.var, 2, standardize)
std.num <- as.data.frame(std.num)

# MERGE CATEGORCIAL (factor) AND NUMERIC VARIABLES
cat.var <- as.data.frame(as.matrix(cat.var))
std.num <- as.data.frame(as.matrix(std.num))

trainfull <- cbind(cat.var, std.num)
class(trainfull)

# doulbe check
dim(cat.var) # 7140   64    (64 respectively, if q1 and q2 are excluded)
dim(std.num) # 7140   97
dim(trainfull) # 7140  161 
dim(train)     # 7140  163  

class(trainfull$emig) # factor
class(trainfull$q2012) # numeric

################################################################

# DATA SPLIT
##############

# SPIT: TRAIN11, TEST11, TEST12 (4620:1260:1260)
set.seed(10)
n <- nrow(trainfull)
testIndex <- sample(1:n, size = round(0.1764706*n), replace = F) # 17.64706 percent go into unseen testdata (test12)
test12 <- trainfull[testIndex,]
dim(test12) # 1260  161  -> test12
set.seed(10)
train1 <- trainfull[-testIndex,] 
dim(train1) # 5880  161
n <- nrow(train1)
trainIndex <- sample(1:n, size = round(0.2142857*n), replace = F)
test11 <- train1[trainIndex,]
dim(test11) # 1260  161 -> test11
train11 <- train1[-trainIndex,]
dim(train11)  # 4620  161 -> train11


################################################################

# GET THE DATA READY: TEST
#############################

# subset non-categorical variables (numeric variables)
num.var <- subset(test, select = -c(country, q1, q2, q103, emig, q214, q217, q2061, q2062, q213, q264, q265, q301, q302, q403, q4061, 
                                    q4062, q409, q4113, q4114, q4116, q501, q501b, q5021, q5022,q523a, q515a1, q515a2, q515a3, q605a, 
                                    aid1a, aid31, aid32, aid41, aid42, aid5a, aid5b, aid5c, aid5d, aid5e, q707, q709a, q713, q714, 
                                    q832, q901, q1002, q1004, q1010, q1011a, q1011b, q1011c, q1012, q1017, q2008, q511_combined, 
                                    q512_combined, q513_combined, q1005_combined, q1006_combined, q1006a_combined, edu_combined, 
                                    income_categories, first_language, second_language, region))
num.var <- as.data.frame(num.var)
class(num.var)

# subset categorcial variables
# ATTENTION: q1 and q2 could be included here, but they might lead to a terrible overfit of the models -> excluded here
cat.var <- subset(test, select = c(country, q103, emig, q214, q217, q2061, q2062, q213, q264, q265, q301, q302, q403, q4061, q4062, 
                                   q409, q4113, q4114, q4116, q501, q501b, q5021, q5022,q523a, q515a1, q515a2, q515a3, q605a, aid1a, 
                                   aid31, aid32, aid41, aid42, aid5a, aid5b, aid5c, aid5d, aid5e, q707, q709a, q713, q714, q832, q901, 
                                   q1002, q1004, q1010, q1011a, q1011b, q1011c, q1012, q1017, q2008, q511_combined, q512_combined, 
                                   q513_combined, q1005_combined, q1006_combined, q1006a_combined, edu_combined, income_categories, 
                                   first_language, second_language, region))
cat.var <- as.data.frame(cat.var)
class(cat.var)

# CHANGE NON-CATEGORICAL VARIABLES TO NUMERIC
num.var %<>% mutate_if(is.factor,as.numeric)

# STANDARDIZE NUMERIC VARIABLES
standardize <- function(x){
  x <- x - mean(x, na.rm = T)
  return(x/sd(x, na.rm = T))
}

std.num <- apply(num.var, 2, standardize)
std.num <- as.data.frame(std.num)

# MERGE CATEGORCIAL (factor) AND NUMERIC VARIABLES
cat.var <- as.data.frame(as.matrix(cat.var))
std.num <- as.data.frame(as.matrix(std.num))

test <- cbind(cat.var, std.num)
class(test)

# doulbe check
dim(cat.var) # 7140   64    (64 respectively, if q1 and q2 are excluded)
dim(std.num) # 7140   97
dim(test) # 7140  161 

class(test$emig) # factor
class(test$q2012) # numeric

################################################################

# Load trained model 
load("final_model.Rdata")


### # BAGGING
#set.seed(10)
#rf.bagging12<- train(emig ~ ., train11, method = "rf", tuneGrid = expand.grid(mtry = c(12)), trControl = fitControl, metric = "ROC", ntree = 1000)
#rf.bagging12 # ROC: 0.9013929 OK
set.seed(10)
pred <- predict(m,newdata=test, type = "prob")


######### <<<<<

truth <- test$emig

cat("The final score:","\n") 
auc(pred$Yes,truth)
