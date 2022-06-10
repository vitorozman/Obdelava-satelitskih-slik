require(tidyverse)
require(reticulate)
np = import("numpy")
require(grid)
require(gridExtra)
require(dplyr)
require(ggplot2)
require(rpart)
library(caret)
library(doParallel)




ucnaInd = createDataPartition(podatki_01$ref, p=0.75, list=FALSE)
ucna <- podatki_01[ucnaInd,]
testna <- podatki_01[-ucnaInd,]
ucnaDown <- downSample(ucna[, -181], ucna$ref, yname="ref")


ucnaX = podatki_01[ucnaInd, -181]
testnaX = podatki_01[-ucnaInd, -181]
ucnaY = podatki_01[ucnaInd, 181]
testnaY = podatki_01[-ucnaInd, 181]


cl <- makePSOCKcluster(4)

registerDoParallel(cl)

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




trainD <- podatki[-indeks_obdelava,]
testD <- podatki[indeks_obdelava,]
ucnaDown <- downSample(trainD[, -181], trainD$ref, yname="ref")
trainX <- ucnaDown[,-181]
trainY <- ucnaDown[,181]

cl <- makePSOCKcluster(4)

registerDoParallel(cl)

modelRF <- train(trainX, trainY, 
               method='rf',
               tuneGrid = data.frame(mtry=40),
               trControl = trainControl(method='cv', number=10),
               ntree=150)











stopCluster(cl)








