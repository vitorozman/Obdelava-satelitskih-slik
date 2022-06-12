
# potrebno pognati uvoz_podatkov_pozidano.r 


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
# podvzorcenje
ucnaDown <- downSample(ucna[, -181], ucna$ref, yname="ref")

ucnaX = podatki_01[ucnaInd, -181]
testnaX = podatki_01[-ucnaInd, -181]
ucnaY = podatki_01[ucnaInd, 181]
testnaY = podatki_01[-ucnaInd, 181]

# za malo hitrejse racunanje
cl <- makePSOCKcluster(4)
registerDoParallel(cl)


################################################################################
# Down Tree
################################################################################
modelTreeD <- train(ref ~., data=ucnaDown, 
              method="rpart", 
              tuneGrid = data.frame(cp=0.0001*c(1:100)),
              trControl = trainControl(method="cv", number=10))
pred <- predict(modelTreeD, newdata=testna)
cat("\n Natancnost pri izbranih", mean(pred == testna$ref))
cf <- confusionMatrix(pred, testna$ref, positive = "Y")
# 0.9121978
modelTreeD






################################################################################
# Down GLM
################################################################################
modelGlmD <- train(ref~., data=ucnaDown,
                  method = "glm",
                  trControl = trainControl(method="cv", number=10))
pred <- predict(modelGlmD, newdata=testna)
cat("\n Natancnost pri izbranih", mean(pred == testna$ref))
confusionMatrix(pred, testna$ref, positive = "Y")
# 0.9219228








################################################################################
# Down Knn
################################################################################
modelKnnD <- train(ref ~., data=ucnaDown,
               method = "knn",
               trControl = trainControl(method="cv", number=5),
               tuneGrid = data.frame(k=1:30))
pred <- predict(modelKnnD, newdata=testna)
cat("\n Natancnost pri izbranih", " spremenljivk je", mean(pred == testna$ref))
confusionMatrix(pred, testna$ref, positive = "Y")
#0.9021951 , k=7








################################################################################
# Down SVM
################################################################################
modelSvmD <- train(ref ~., data=ucnaDown,
                  method='svmLinear',
                  trControl = trainControl(method="cv", number=10),
                  tuneGrid=data.frame(C=Cji))
pred <- predict(modelSvmD, newdata = testna)
cat("\n Natancnost pri izbranih", " spremenljivk je", mean(pred == testna$ref))
confusionMatrix(pred, testna$ref, positive = "Y")









################################################################################
# Down RF
################################################################################
modelRFD <- train(ucnaDown[,-181], ucnaDown$ref, 
                 method='rf', 
                 trControl = trainControl(method = "cv",
                                          classProbs = TRUE,
                                          savePredictions = TRUE))
pred <- predict(modelRFD, newdata=testna)
cat("\n Natancnost pri izbranih", " spremenljivk je", mean(pred == testna$ref))
confusionMatrix(pred, testna$ref, positive = "Y")
#0.9374826







################################################################################
# Tree
################################################################################
modelTree <- train(ref ~., data=ucna, 
                   method="rpart", 
                   tuneGrid = data.frame(cp=0.0001*c(1:100)),
                   trControl = trainControl(method="cv", number=10))

pred <- predict(modelTree, newdata=testna)
cat("\n Natancnost pri izbranih", mean(pred == testna$ref))
confusionMatrix(pred, testna$ref, positive = "Y")
#0.9341484







################################################################################
# GLM
################################################################################
modelGlm <- train(ref~., data=ucna,
                  method = "glm",
                  trControl = trainControl(method="cv", number=10))

pred <- predict(modelGlm, newdata=testna)
cat("\n Natancnost pri izbranih", mean(pred == testna$ref))
confusionMatrix(pred, testna$ref, positive = "Y")
#0.9472076







################################################################################
# KNN
################################################################################
modelKnn <- train(ref ~., data=ucna,
                  method = "knn",
                  trControl = trainControl(method="cv", number=10),
                  tuneGrid = data.frame(k=1:30))
pred <- predict(modelKnn, newdata=testna)
cat("\n Natancnost pri izbranih", " spremenljivk je", mean(pred == testna$ref))
confusionMatrix(pred, testna$ref, positive = "Y")
#0.948319 , k=12








################################################################################
# SVM
################################################################################
Cji <- c(0.1, 1, 10)
modelSvm <- train(ref ~., data=ucna,
               method='svmLinear',
               trControl = trainControl(method="cv", number=10),
               tuneGrid=data.frame(C=Cji))
pred <- predict(modelSvm, newdata = testna)
cat("\n Natancnost pri izbranih", " spremenljivk je", mean(pred == testna$ref))
confusionMatrix(pred, testna$ref, positive = "Y")









################################################################################
# RF
################################################################################
modelRF <- train(ucnaX, ucnaY, 
                 method='rf', 
                 trControl = trainControl(method = "cv",
                                          classProbs = TRUE,
                                          savePredictions = TRUE))

pred <- predict(modelRF, newdata=testna)
cat("\n Natancnost pri izbranih", " spremenljivk je", mean(pred == testna$ref))
#0.9585996
confusionMatrix(pred, testna$ref, positive = "Y")









################################################################################
################################################################################






################################################################################
# ROC
################################################################################
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
                   tuneGrid = data.frame(k=5:25))
knnModelR
svmodelR <- train(ref ~., data=ucna,
                  method='svmLinear',
                  trControl = ctrlROC,
                  tuneGrid=data.frame(C=0.1))
svmodelR
RFmodelR <- train(ucnaX, ucnaY, 
                  method='rf',
                  metric = "ROC",
                  tuneGrid = data.frame(mtry=30),
                  trControl = ctrlROC,
                  ntree=100)
RFmodelR







################################################################################
# Down ROC
################################################################################
glmModelRD <- train(ref~., data=ucnaDown,
                   method = "glm",
                   metric = "ROC",
                   trControl = ctrlROC)
glmModelRD
knnModelRD <- train(ref~., data=ucnaDown,
                   method = "knn",
                   metric = "ROC",
                   trControl = ctrlROC,
                   tuneGrid = data.frame(k=5:25))
knnModelRD
svmodelRD <- train(ref ~., data=ucnaDown,
                  method='svmLinear',
                  trControl = ctrlROC,
                  tuneGrid=data.frame(C=0.1))
svmodelRD
RFmodelRD <- train(ucnaDown[,-181], ucnaDown$ref, 
                  method='rf',
                  metric = "ROC",
                  tuneGrid = data.frame(mtry=30),
                  trControl = ctrlROC,
                  ntree=100)
RFmodelRD





################################################################################
################################################################################







################################################################################
# Optimizaciaj parametrov
################################################################################
ucnaInd = createDataPartition(podatki_01$ref, p=0.75, list=FALSE)
ucna <- podatki_01[ucnaInd,]
testna <- podatki_01[-ucnaInd,]
ucnaDown <- downSample(ucna[, -181], ucna$ref, yname="ref")


ucnaX = podatki_01[ucnaInd, -181]
testnaX = podatki_01[-ucnaInd, -181]
ucnaY = podatki_01[ucnaInd, 181]
testnaY = podatki_01[-ucnaInd, 181]



natancnosti <- data.frame(n=1:18, testna=rep(0, 18), ucna=rep(0, 18))
for(mtry in 1:18){
  model <- train(ucnaDown[,-181], ucnaDown$ref, method='rf',
                 tuneGrid = data.frame(mtry= 10*mtry),
                 trControl = trainControl(method='cv', number=10),
                 ntree=150)
  pred <- predict(model, testnaX)
  natancnosti$testna[mtry] <- mean(pred == testnaY)
  natancnosti$ucna[mtry] <- model$results$Accuracy
  cat("\n poskus",mtry*10 ,":" ,natancnosti$testna[mtry], natancnosti$ucna[mtry])
}




stopCluster(cl)





