#########################
# Optimizacija parametrov
########################
require(tidyverse)
require(grid)
require(gridExtra)
require(dplyr)
require(ggplot2)
require(rpart)
library(caret)





best_podatki <- ucna[top5rlf]
best_podatki$ref <- ucna$ref


indeks <-createDataPartition(best_podatki$ref, p=0.75, list=FALSE)
trainX <- best_podatki[indeks, -6]
trainY <- best_podatki[indeks, 6]
testX <- best_podatki[-indeks, -6]
testY <- best_podatki[-indeks, 6]


  
natancnosti_RF <- data.frame(n=1:60, nt=rep(0,60), testna=rep(0, 60), OOB=rep(0, 60), ucna=rep(0, 60), testna_raw=rep(0, 60))
i <- 0
for(mtry in 1:6){
  for (ntree in 1:10){
    i <- i + 1
    model <- train(trainX, trainY, method='rf',
                   tuneGrid = data.frame(mtry=mtry),
                   trControl = trainControl(method='cv', number=10),
                   ntree=100 + 25*ntree)
    pred <- predict(model, newdata=testX)
    natancnosti_RF$testna[i] <- model$results$Accuracy
    natancnosti_RF$ucna[i] <- sum(predict(model, trainX) == trainY) / length(trainY)
    natancnosti_RF$OOB[i] <- 1-model$finalModel$err.rate[100 + (25*ntree) %/% 2,1]
    natancnosti_RF$testna_raw[i] <- mean(pred == testY) 
    natancnosti_RF$nt[i] <- ntree 
    natancnosti_RF$n[i] <- mtry 
    cat( "\n poskus", mtry, 25*ntree,"acc ucna:",sum(predict(model, trainX) == trainY) / length(trainY),"testna_raw:",  mean(pred == testY))
  }
}

plot(natancnosti_RF)

#2  1 150 0.9634676 0.9624784    1  0.9655556

bestRFmodel <- train(trainX, trainY, method='rf',
               tuneGrid = data.frame(mtry=2),
               trControl = trainControl(method='cv', number=10),
               ntree=150)
bestRFmodel

modelKnn <- train(ref~., data=best_podatki,
               method = "knn",
               trControl = trainControl(method="cv", number=10),
               tuneGrid = data.frame(k=1:30))

modelKnn$finalModel$k
#best k = 16

###############################################################################
# odločitev glede na ploscino AUC pod ROc krivuljo
###############################################################################
ctrlROC <- trainControl(method='cv', 
                      number=10,
                      savePredictions = T,
                      classProbs = T,
                      summaryFunction = twoClassSummary)
glmModelR <- train(ref~., data=best_podatki,
                   method = "glm",
                   metric = "ROC",
                   trControl = ctrlROC)
knnModelR <- train(ref~., data=best_podatki,
                   method = "knn",
                   metric = "ROC",
                   trControl = ctrlROC,
                   tuneGrid = data.frame(k=16))
RFmodelR <- train(trainX, trainY, 
                  method='rf',
                  metric = "ROC",
                  tuneGrid = data.frame(mtry=2),
                  trControl = ctrlROC,
                  ntree=150)
ROC_GLM <- glmModelR$results$ROC
ROC_KNN <- max(knnModelR$results$ROC)
ROC_RF <- RFmodelR$results$ROC
df_ROC <- data.frame(ROC_GLM,ROC_KNN,ROC_RF)
rownames(df_ROC) <- "Ploscina pod ROC krivuljo"
colnames(df_ROC) <- c("GLM", "KNN", "RF")
df_ROC

###############################################################################
# odločitev glede na ploscino AUC pod PR krivuljo
###############################################################################
ctrlpr <- trainControl(method='cv', 
                       number=10,
                       savePredictions = T,
                       classProbs = T,
                       summaryFunction = prSummary)
glmModelpr <- train(ref~., data=best_podatki,
                    method = "glm",
                    trControl = ctrlpr)
knnModelpr <- train(ref~., data=best_podatki,
                    method = "knn",
                    trControl = ctrlpr,
                    tuneGrid = data.frame(k=16))
RFmodelpr <- train(trainX, trainY, 
                   method='rf',
                   tuneGrid = data.frame(mtry=2),
                   trControl = ctrlpr,
                   ntree=150)
prGLM <- glmModelpr$results[2:4]
prKNN <- knnModelpr$results[2:4]
prRF <- RFmodelpr$results[2:4]
dfAUC <- rbind(prGLM, prKNN, prRF)
rownames(dfAUC) <- c("GLM", "KNN", "RF")
colnames(dfAUC) <- c("Ploscina pod PR krivuljo", "Preciznost", "Priklic")
dfAUC



##############
# best model =  glm ali rf
##############


