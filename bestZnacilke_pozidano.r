require(tidyverse)
require(grid)

require(gridExtra)
require(dplyr)
require(ggplot2)

require(rpart)
library(caret)




# za rasiriti stevilo pozitivnih
#ucnaUp = upSample(ucna[, -181], ucna$ref, yname="ref")

ucnaInd = createDataPartition(podatki_01$ref, p=0.75, list=FALSE)
ucna <- podatki_01[ucnaInd,]
testna <- podatki_01[-ucnaInd,]
ucnaDown <- downSample(ucna[, -181], ucna$ref, yname="ref")
#summary(ucna)
#summary(ucnaDown)

ucnaX = podatki_01[ucnaInd, -181]
testnaX = podatki_01[-ucnaInd, -181]
ucnaY = podatki_01[ucnaInd, 181]
testnaY = podatki_01[-ucnaInd, 181]



library(doParallel)



cl <- makePSOCKcluster(4)

registerDoParallel(cl)

modelTreeD <- train(ref ~., data=ucnaDown, 
              method="rpart", 
              tuneGrid = data.frame(cp=0.0001*c(1:100)),
              trControl = trainControl(method="cv", number=10))
stopCluster(cl)


pred <- predict(modelTreeD, newdata=testna)
cat("\n Natancnost pri izbranih", mean(pred == testna$ref))
cf <- confusionMatrix(pred, testna$ref, positive = "Y")
# 0.9121978
modelTreeD

cf$byClass







cl <- makePSOCKcluster(4)

registerDoParallel(cl)


modelGlmD <- train(ref~., data=ucnaDown,
                  method = "glm",
                  trControl = trainControl(method="cv", number=10))
stopCluster(cl)
pred <- predict(modelGlmD, newdata=testna)
cat("\n Natancnost pri izbranih", mean(pred == testna$ref))
confusionMatrix(pred, testna$ref, positive = "Y")
# 0.9219228









cl <- makePSOCKcluster(4)

registerDoParallel(cl)

modelKnnD <- train(ref ~., data=ucnaDown,
               method = "knn",
               trControl = trainControl(method="cv", number=5),
               tuneGrid = data.frame(k=1:30))
stopCluster(cl)
pred <- predict(modelKnnD, newdata=testna)
cat("\n Natancnost pri izbranih", " spremenljivk je", mean(pred == testna$ref))
confusionMatrix(pred, testna$ref, positive = "Y")








cl <- makePSOCKcluster(4)

registerDoParallel(cl)

#0.9021951 , k=7
modelRFD <- train(ucnaDown[,-181], ucnaDown$ref, 
                 method='rf', 
                 trControl = trainControl(method = "cv",
                                          classProbs = TRUE,
                                          savePredictions = TRUE))
stopCluster(cl)
pred <- predict(modelRFD, newdata=testna)
cat("\n Natancnost pri izbranih", " spremenljivk je", mean(pred == testna$ref))
confusionMatrix(pred, testna$ref, positive = "Y")
#0.9374826

















cl <- makePSOCKcluster(4)

registerDoParallel(cl)


modelTree <- train(ref ~., data=ucna, 
                   method="rpart", 
                   tuneGrid = data.frame(cp=0.0001*c(1:100)),
                   trControl = trainControl(method="cv", number=10))
stopCluster(cl)

pred <- predict(modelTree, newdata=testna)
cat("\n Natancnost pri izbranih", mean(pred == testna$ref))
confusionMatrix(pred, testna$ref, positive = "Y")
#0.9341484








cl <- makePSOCKcluster(4)

registerDoParallel(cl)

modelGlm <- train(ref~., data=ucna,
                  method = "glm",
                  trControl = trainControl(method="cv", number=10))
stopCluster(cl)

pred <- predict(modelGlm, newdata=testna)
cat("\n Natancnost pri izbranih", mean(pred == testna$ref))
confusionMatrix(pred, testna$ref, positive = "Y")
#0.9472076









cl <- makePSOCKcluster(4)

registerDoParallel(cl)

modelKnn <- train(ref ~., data=ucna,
                  method = "knn",
                  trControl = trainControl(method="cv", number=10),
                  tuneGrid = data.frame(k=1:30))
stopCluster(cl)
pred <- predict(modelKnn, newdata=testna)
cat("\n Natancnost pri izbranih", " spremenljivk je", mean(pred == testna$ref))
confusionMatrix(pred, testna$ref, positive = "Y")
#0.948319 , k=12





cl <- makePSOCKcluster(4)

registerDoParallel(cl)

Cji <- c(0.1, 1, 10)
modelSvm <- train(ref ~., data=ucna,
               method='svmLinear',
               trControl = trainControl(method="cv", number=10),
               tuneGrid=data.frame(C=Cji))


pred <- predict(modelSvm, newdata = testna)
cat("\n Natancnost pri izbranih", " spremenljivk je", mean(pred == testna$ref))
confusionMatrix(pred, testna$ref, positive = "Y")
stopCluster(cl)



















cl <- makePSOCKcluster(4)

registerDoParallel(cl)

modelSvmD <- train(ref ~., data=ucnaDown,
                  method='svmLinear',
                  trControl = trainControl(method="cv", number=10),
                  tuneGrid=data.frame(C=Cji))
pred <- predict(modelSvmD, newdata = testna)
cat("\n Natancnost pri izbranih", " spremenljivk je", mean(pred == testna$ref))
confusionMatrix(pred, testna$ref, positive = "Y")
stopCluster(cl)


cl <- makePSOCKcluster(4)

registerDoParallel(cl)
modelRF <- train(ucnaX, ucnaY, 
                 method='rf', 
                 trControl = trainControl(method = "cv",
                                          classProbs = TRUE,
                                          savePredictions = TRUE))

stopCluster(cl)
pred <- predict(modelRF, newdata=testna)
cat("\n Natancnost pri izbranih", " spremenljivk je", mean(pred == testna$ref))
#0.9585996
confusionMatrix(pred, testna$ref, positive = "Y")





cl <- makePSOCKcluster(4)

registerDoParallel(cl)

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

#########################################

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

stopCluster(cl)



















