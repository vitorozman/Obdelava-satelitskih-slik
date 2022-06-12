# potrebno pognati uvoz_podatkov_gozd.r

require(tidyverse)
require(grid)
require(gridExtra)
require(dplyr)
require(ggplot2)
require(rpart)
library(caret)
library(doParallel)


# Ucna in testna mnozica
ucnaInd = createDataPartition(podatki_01$ref, p=0.75, list=FALSE)
ucna <- podatki_01[ucnaInd,]
testna <- podatki_01[-ucnaInd,]

ucnaX = podatki_01[ucnaInd, -181]
testnaX = podatki_01[-ucnaInd, -181]
ucnaY = podatki_01[ucnaInd, 181]
testnaY = podatki_01[-ucnaInd, 181]


# za malo hitrejse racunanje
cl <- makePSOCKcluster(4)
registerDoParallel(cl)


#############################################################
# GLM
############################################################
modelGlm <- train(ref~., data=ucna,
                  method = "glm",
                  trControl = trainControl(method="cv", number=10))
pred <- predict(modelGlm, newdata=testna)
modelGlm
cat("\n Natancnost pri izbranih", mean(pred == testna$ref))




#############################################################
# KNN
############################################################
modelKnn <- train(ref ~., data=ucna,
                  method = "knn",
                  trControl = trainControl(method="cv", number=10),
                  tuneGrid = data.frame(k=1:30))
pred <- predict(modelKnn, newdata=testna)
modelKnn
cat("\n Natancnost pri izbranih", " spremenljivk je", mean(pred == testna$ref))



#############################################################
# SVM 
############################################################
Cji <- c(0.1, 1, 10)
modelSvm <- train(ref ~., data=ucna,
                  method='svmLinear',
                  trControl = trainControl(method="cv", number=10),
                  tuneGrid=data.frame(C=Cji))
pred <- predict(modelSvm, newdata = testna)
cat("\n Natancnost pri izbranih", " spremenljivk je", mean(pred == testna$ref))
modelSvm





#############################################################
# RF
############################################################
modelRF <- train(ucnaX, ucnaY, 
                 method='rf', 
                 trControl = trainControl(method = "cv",
                                          classProbs = TRUE,
                                          savePredictions = TRUE),
                 number=10)
pred <- predict(modelRF, newdata=testna)
cat("\n Natancnost pri izbranih", " spremenljivk je", mean(pred == testna$ref))
modelRF


##############
# metrika ROC
##############

ctrlROC <- trainControl(method='cv', 
                        number=10,
                        savePredictions = T,
                        classProbs = T,
                        summaryFunction = twoClassSummary)
glmModelR <- train(ref~., data=ucna,
                   method = "glm",
                   metric = "ROC",
                   trControl = ctrlROC)
glmModelR
knnModelR <- train(ref~., data=ucna,
                   method = "knn",
                   metric = "ROC",
                   trControl = ctrlROC,
                   tuneGrid = data.frame(k=8))
knnModelR
svmodelR <- train(ref ~., data=ucna,
                  method='svmLinear',
                  trControl = ctrlROC,
                  tuneGrid=data.frame(C=0.1))
svmodelR
RFmodelR <- train(ucnaX, ucnaY, 
                  method='rf',
                  metric = "ROC",
                  trControl = ctrlROC,
                  ntree=100)
RFmodelR



#############################################################################
# Optimizacija parametrov
#############################################################################

natancnosti <- data.frame(n=1:18, testna=rep(0, 18), ucna=rep(0, 18))
#mtry <- 50
for(mtry in 1:18){
  model <- train(ucnaX, ucnaY, method='rf',
                 tuneGrid = data.frame(mtry= 10*mtry),
                 trControl = trainControl(method='cv', number=10),
                 ntree=100)
  pred <- predict(model, testnaX)
  natancnosti$testna[mtry] <- mean(pred == testnaY)
  natancnosti$ucna[mtry] <- model$results$Accuracy
  cat("\n poskus",mtry*10 ,":" ,natancnosti$testna[mtry], natancnosti$ucna[mtry])
}
# mtry = 30, ntree = 150
# mtry = 30, ntree = 100






stopCluster(cl)
