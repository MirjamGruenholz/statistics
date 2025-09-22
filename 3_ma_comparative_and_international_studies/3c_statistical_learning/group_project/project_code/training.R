rm(list=ls())

# Set your working directory here. 

setwd("C:\\Users\\xoxox\\polybox\\Master\\2. Semester\\Methods III\\labs\\Lab Report\\final\\LabReportM3")

# Template for final project #
##############################

# Add necessary packages
# here. All packages must
# be available for install
# via install.packages()
# or devtools::install_github().

######### >>>>

library(caret)
library(ROCR)
library(dplyr)
library(magrittr)
library(gbm)
library(glmnet)
library(glmnetUtils)
library(gam)


######### <<<<<

auc <- function(phat,y){
	pred <- prediction(phat, y)
	perf <- performance(pred,"auc")
	auc <- perf@y.values[[1]]
	return(auc)
}

####################################################################

load("AB4x_train.Rdata")

####################################################################


# PREPROCESSING
################

# SUBSET NUMERIC AND CATEGORICAL VARIABLES
num.var <- subset(train, select = -c(country, q1, q2, q103, emig, q214, q217, q2061, q2062, q213, q264, q265, q301, q302, 
                                     q403, q4061, q4062, q409, q4113, q4114, q4116, q501, q501b, q5021, q5022,q523a, q515a1, 
                                     q515a2, q515a3, q605a, aid1a, aid31, aid32, aid41, aid42, aid5a, aid5b, aid5c, aid5d, 
                                     aid5e, q707, q709a, q713, q714, q832, q901, q1002, q1004, q1010, q1011a, q1011b, q1011c, 
                                     q1012, q1017, q2008, q511_combined, q512_combined, q513_combined, q1005_combined, 
                                     q1006_combined, q1006a_combined, edu_combined, income_categories, first_language, 
                                     second_language, region))
num.var <- as.data.frame(num.var)
class(num.var)

# subset categorcial variables
# ATTENTION: q1 and q2 could be included here, but they might lead to a terrible overfit of the models -> excluded here
cat.var <- subset(train, select = c(country, q103, emig, q214, q217, q2061, q2062, q213, q264, q265, q301, q302, q403, q4061, 
                                    q4062, q409, q4113, q4114, q4116, q501, q501b, q5021, q5022,q523a, q515a1, q515a2, q515a3, 
                                    q605a, aid1a, aid31, aid32, aid41, aid42, aid5a, aid5b, aid5c, aid5d, aid5e, q707, q709a, 
                                    q713, q714, q832, q901, q1002, q1004, q1010, q1011a, q1011b, q1011c, q1012, q1017, q2008, 
                                    q511_combined, q512_combined, q513_combined, q1005_combined, q1006_combined, q1006a_combined, 
                                    edu_combined, income_categories, first_language, second_language, region))
cat.var <- as.data.frame(cat.var)
class(cat.var)

# CHANGE NON-CATEGORICAL VARIABLES TO NUMERIC
num.var %<>% mutate_if(is.factor,as.numeric)

# CORRELATION CHECK (numeric variables)
# to check correlation, emig must be numeric 
emig.cor <- as.numeric(cat.var$emig)

num.cor <- num.var[,c(1:97)]
correl <- cbind(emig.cor, num.cor)
cor(correl) # RESULT: largest correlation btw. emig + age: 0.29892979 (that's still ok)

# STANDARDIZE NUMERIC VARIABLES
standardize <- function(x){
  x <- x - mean(x, na.rm = T)
  return(x/sd(x, na.rm = T))
}

std.num <- apply(num.var, 2, standardize)
std.num <- as.data.frame(std.num)


# MERGE CATEGORCIAL AND NUMERIC VARIABLES
cat.var <- as.data.frame(as.matrix(cat.var))
std.num <- as.data.frame(as.matrix(std.num))

trainfull <- cbind(cat.var, std.num)
class(trainfull)

# doulbe check
dim(cat.var) # 7140   64    
dim(std.num) # 7140   97
dim(trainfull) # 7140  161 
dim(train)     # 7140  163  => OK

class(trainfull$emig) # factor
class(trainfull$q2012) # numeric

###########################################################################################

# SPLIT DATA
#############

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

###########################################################################################

# TRAIN MODELS
###############

# CARET SPEED UP
library(doParallel)
K <- parallel::detectCores()
cl <- makeCluster(K)
registerDoParallel(cl)

set.seed(10)
fitControl <- trainControl(method = "cv", number = 10, classProbs = T, summaryFunction = twoClassSummary)


# LOGISTIC REGRESSION, LDA, QDA, GAM  AND LASSO
################################################

# LOGISTIC REGRESSION
set.seed(10)
glm.train11 <- train(emig ~ ., data = train11, method = "glm", family = binomial(link = "logit"), trControl = fitControl, metric = "ROC")
glm.train11 # ROC: 0.7954958

# LOGISTIC REGRESSION PREDICTION
set.seed(10)
glm.test11 <- predict(glm.train11, test11, type = "prob")
auc(glm.test11$Yes, test11$emig) ### ROC: 0.8370779


# LDA
set.seed(10)
lda.fit <- train(emig ~ ., data = train11, method="lda", family = binomial(link = "logit"), trControl = fitControl, 
                 metric = "ROC") # ROC: 0.796684

# LDA PREDICT
set.seed(10)
lda.pred <- predict(lda.fit, newdata = test11, type = "prob")
auc(lda.pred$Yes, test11$emig) # 0.8330359


# QDA
set.seed(10)
(qda.fit = train(emig ~ ., data=train11, method="qda",
                 trControl = fitControl, metric ="ROC") )  #ROC: 0.741851

# QDA PREDICT 
set.seed(10)
qda.pred <- predict(qda.fit, newdata = test11, type = "prob")
auc(qda.pred$Yes, test11$emig) # ROC: 0.8494916


# GAM
grid.gam <- data.frame(df = 1:15)
# can only use numeric variables
# so subset numerics for this model
f.num <- (emig ~ q101 + q101a + q102 + q102b + q103a1 + q103a2 +
            q103a3 + q103a4 + q103a5 + q103a6 + q105 + q106 +
            q1071 + q1072 + q1074 + q2011 + q2012 + q2013 + q2014 +
            q2016 + q20113 + q20120 + q202 + q204a1 + q204a2 + q2041 +
            q2042 + q2043 + q2044 + q20420 + q20421 + q20422 + q210 +
            q216 + q2181 + q2182 + q2185 + q261a1 + q261a2 + q261b1 +
            q261b2 + q263 + q303 + q404 + q514 + q5181 + q5182 + q5183 +
            q5184 + q5185 + q5186 + q523 + q6011 + q6012 + q6013 + q6014 +
            q6018 + q60118 + q6041 + q6043 + q605 + q6062 + q6063 + q6064 +
            q6071 + q6073 + q6074 + q6076 + q6077 + aid2 + q7001 + q7002 +
            q7003 + q7004 + q7007 + q7008 + q7011 + q7012 + q7013 + q7014 +
            q701b + q704 + q705 + q708 + q7114 + q7115 + q833 + q8341 +
            q8342 + q8343 + q1001c + q1010b + q609 + q6101 + q6106 + q1016 + age)

set.seed(10)
gam.train11 <- train(f.num,
                     data = train11,
                     method = "gamSpline",
                     family = "binomial",
                     tuneGrid = grid.gam,
                     trControl = fitControl) 
gam.train11 # ROC: 0.7826810

gam.test11 <- predict(gam.train11, newdata = test11, type = "prob") 
auc(gam.test11$Yes, test11$emig) # ROC: 0.8278919


# LASSO
set.seed(10)
( lasso.fit <- train(emig~ . ,
                     data = train11, 
                     method ="glmnet",
                     trControl = fitControl, 
                     preProcess = c("center", "scale"), 
                     tuneGrid = data.frame(alpha = 1, 
                                           lambda = 10^seq(-4, -1, by = 0.5))))
lasso.fit 
# The final values used for the model were alpha = 1 and lambda = 0.003162278.
# ROC: 0.8108246 

# LASSO PREDICT
lasso.pred <- predict(lasso.fit, newdata = test11, type = "prob")
auc(lasso.pred$Yes, test11$emig) # 0.852556
# COMMENT: we also ran the model including q1 and q2 in our data. The performance is only slightly better (ROC: 0.8326329) for the
# training data than withough q1 and q2 (0.8108246). The test prediction is 0.8650729 (compared to 0.852556 without q1 and q2). 
# Nevertheless, this worse than other models.



# TREEBASED MODELS
###################

# bagging: sqrt(161) = 12.68858
# boosting: mtry = 161

# OPTIMAL
gridrf <- expand.grid(mtry = c(1,2,12,13,161))

set.seed(10)
rf.model <- train(emig ~ ., train11, method = "rf", tuneGrid = gridrf, trControl = fitControl, metric = "ROC", ntree = 1000)
rf.model
# The final value used for the model was mtry = 12. => ROC: 0.9224799


# BAGGING => BEST MODEL
set.seed(10)
rf.bagging12<- train(emig ~ ., train11, method = "rf", tuneGrid = expand.grid(mtry = c(12)), trControl = fitControl, metric = "ROC", ntree = 1000)
rf.bagging12 # ROC: 0.9225021


# BAGGING PREDICTION 
set.seed(10)
rf.bagging12.pred <- predict(rf.bagging12, test11, type = "prob")
auc(rf.bagging12.pred$Yes, test11$emig)     # ROC: 0.9569943
varImp(rf.bagging12)

# BAGGING PREDICTION (test12) 
set.seed(10)
rf.bagging12.pred2 <- predict(rf.bagging12, test12, type = "prob")
auc(rf.bagging12.pred2$Yes, test12$emig)     # ROC: 0.9217498


# BOOST
set.seed(10)
rf.boost161 <- train(emig ~ ., train11, method = "rf", tuneGrid = expand.grid(mtry = c(161)), trControl = fitControl, metric = "ROC", ntree = 1000)
rf.boost161 # ROC: 0.9188178

# BOOSTING PREDICTION
set.seed(10)
rf.boost161.pred <- predict(rf.boost161, test11, type = "prob")
auc(rf.boost161.pred$Yes, test11$emig)     # ROC: 0.9521223


# BOOSTED CLASSIFICATION TREE
gbmgrid <- expand.grid(interaction.depth = c(9, 10, 11, 12),
                       n.trees = c(5000, 6000, 7000),
                       shrinkage = c(0.1, 0.01),
                       n.minobsinnode = 20)

set.seed(10)
gbmfit <- train(emig ~., data = train11, method = "gbm", trControl = fitControl, tuneGrid = gbmgrid, metric = "ROC") ## here...
# The final values used for the model were n.trees = 7000, interaction.depth = 11, shrinkage = 0.1 and n.minobsinnode = 20.
# 0.9106719
plot(gbmfit)

gbmpred <- predict(gbmfit, test11, type = "prob")
auc(gbmpred$Yes, test11$emig)  # 0.9532243



# SVM MODELS
#############

# DEFINE GRIDS
grid.linear <- expand.grid(cost = c(.01, .5, 1, 2, 5, 10))
grid.radial <- expand.grid(sigma = c(.01, .5, 1, 2, 5, 10), C= c(0:10))
grid.poly <- expand.grid(degree = c(1:4), scale = c(.1, .5), C = c(.01, .5, .75, .9, 1.25))

# SVM LINEAR 
set.seed(10)
svm.linear.train11 <- train(emig ~ .,
                            data = train11,
                            method = "svmLinear2",
                            trControl = fitControl, 
                            tuneGrid = grid.linear) 
svm.linear.train11 # ROC: 0.7894602

# SVM LINEAR PREDICTION
set.seed(10)
svm.linear.test11.best <- predict(svm.linear.train11, newdata = test11, type = "prob")
auc(svm.linear.test11.best$Yes, test11$emig) # ROC: 0.8299467


# SVM RADIAL
set.seed(10)
svm.radial.train11 <- train(emig ~ .,
                            data = train11,
                            method = "svmRadial", 
                            trControl = fitControl,
                            tuneGrid = grid.radial) 
svm.radial.train11
# The final values used for the model were sigma = 0.01 and C = 2.
# ROC: 0.9174577

# SVM RADIAL PREDICTION
set.seed(10)
svm.radial.test11.best <- predict(svm.radial.train11, test11, type = "prob")
auc(svm.radial.test11.best$Yes, test11$emig) # 0.9531034


# SVM POLY
set.seed(10)
svm.poly.train11 <- train(emig ~ .,
                          data = train11,
                          method = "svmPoly",
                          trControl = fitControl,
                          tuneGrid = grid.poly) 
svm.poly.train11 
# The final values used for the model were degree = 3, scale = 0.5 and C = 0.01.
# ROC: 0.9168660
set.seed(10)
svm.poly.test11.best <- predict(svm.poly.train11, newdata = test11, type = "prob")
auc(svm.poly.test11.best$Yes, test11$emig) # 0.9532705


###############################################################

# CONCLUSION: BEST MODEL

# BAGGING
set.seed(10)
fitControl <- trainControl(method = "cv", number = 10, classProbs = T, summaryFunction = twoClassSummary)

set.seed(10)
m <- train(emig ~ ., train11, method = "rf", 
           tuneGrid = expand.grid(mtry = c(12)), 
           trControl = fitControl, 
           metric = "ROC", ntree = 1000)
m # ROC: 0.9225021

# Save final model 
save(m, file="final_model.Rdata")


# stop caret speed up
stopCluster(cl)
registerDoSEQ()
