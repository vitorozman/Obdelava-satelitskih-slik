# prvo je potrebno zagnati uvoz_podatkov.r da dobimo zmansano mnozico
# podatki_01 so podatki na katerih bom operiral

require(tidyverse)
require(grid)

require(gridExtra)
require(dplyr)
require(ggplot2)

require(rpart)
library(caret)

library(FSelector)
library(doParallel)

ucnaInd = createDataPartition(podatki_01$ref, p=0.75, list=FALSE)
ucnaX = podatki_01[ucnaInd, -181]
testnaX = podatki_01[-ucnaInd, -181]
ucnaY = podatki_01[ucnaInd, 181]
testnaY = podatki_01[-ucnaInd, 181]

ucna <- podatki_01[ucnaInd,]
testna <- podatki_01[-ucnaInd,]

# Varjanca
vars <- diag(var(ucnaX))
varRang <- sort(vars, decreasing = T)



cl <- makePSOCKcluster(4)

registerDoParallel(cl)

# t-test glede na p-vrednost
# vse p vrednosti so 0
# raje statistiko, vecja bolj pomebna
statistika <- c()
for (i in 1:length(ucnaX)){
  ttest <- t.test(ucnaX[ucnaY == "N", i], ucnaX[ucnaY == "Y", i])
  statistika <- c(statistika, ttest$statistic) 
}

names(statistika) <- names(ucnaX)
statRang <- sort(statistika, decreasing = T)
plot(statRang)


# Relief

rlf <- relief(ref ~ ., data=ucna)
df <- data.frame(rlf)
rlfRnag <- sort(t(df), index.return=T, decreasing=T)
imena <- c(names(ucna))
top5rlf <- imena[rlfRnag$ix[1:5]]
# 10 sprem je ok




# importance random-forest
modelRF <- train(ucnaX, ucnaY, 
                 method='rf', 
                 trControl = trainControl(method = "cv",
                                          classProbs = TRUE,
                                          savePredictions = TRUE),
                 ntree=100)
imporfRnag <- sort(modelRF$finalModel$importance, index.return=T, decreasing=T)
# 10 sprem je ok
top10imp <- imena[imporfRnag$ix[1:10]]

#################################################################################
#################################################################################

#  ACC modelov glede na varjanco


natacnostVAR <- c()
for (i in 1:30){
  model <- train(ucnaX[varRang[1:i]], ucnaY, 
                 method='rf', 
                 trControl = trainControl(method = "cv",
                                          number=10),
                 ntree=100)
  pred <- predict(model, newdata=testna)
  cat("\n Natancnost pri izbranih", i, " spremenljivk je", mean(pred == testna$ref))
  natacnostVAR <- c(natacnostVAR, mean(pred == testna$ref))
}



png("grafi/pozidano/varRF.png", width = 620, height = 620, units = "px")
plot(1:length(natacnostVAR), natacnostVAR, 
     xlab = "Znacilke z najvecjo var do najmanjse", 
     ylab = "Natancnost na testni mnozici",
     main = "Vpliv varjance RF")
dev.off()



################################################################################################
################################################################################################
#  ACC modelov import RF

natacnostIMP <- c()
for (i in 1:30){
  model <- train(ucnaX[imena[imporfRnag$ix[1:i]]], ucnaY, 
                 method='rf', 
                 trControl = trainControl(method = "cv",
                                          number=10),
                 ntree=100)
  pred <- predict(model, newdata=testna)
  cat("\n Natancnost pri izbranih", i, " spremenljivk je", mean(pred == testna$ref))
  natacnostIMP <- c(natacnostIMP, mean(pred == testna$ref))
}



png("grafi/pozidano/impRF.png", width = 620, height = 620, units = "px")
plot(1:length(natacnostIMP), natacnostIMP, 
     xlab = "Znacilke z najvecjo imp do najmanjse", 
     ylab = "Natancnost na testni mnozici",
     main = "Vpliv pomebnost (imp) RF")
dev.off()



##################################################################################
##################################################################################
# ACC modelov RELIEF

natacnostREL <- c()
for (i in 1:30){
  model <- train(ucnaX[imena[rlfRnag$ix[1:i]]], ucnaY, 
                 method='rf', 
                 trControl = trainControl(method = "cv",
                                          number=10),
                 ntree=100)
  pred <- predict(model, newdata=testna)
  cat("\n Natancnost pri izbranih", i, " spremenljivk je", mean(pred == testna$ref))
  natacnostREL <- c(natacnostREL, mean(pred == testna$ref))
}



png("grafi/pozidano/relRF.png", width = 620, height = 620, units = "px")
plot(1:length(natacnostREL), natacnostREL, 
     xlab = "Znacilke z najvecjo relief do najmanjse", 
     ylab = "Natancnost na testni mnozici",
     main = "Vpliv relief RF")
dev.off()




#######################################################################################
#######################################################################################
# ACC modelov t-test statistika


natacnostTTEST <- c()
for (i in 1:30){
  model <- train(ucnaX[names(statRang[1:i])], ucnaY, 
                 method='rf', 
                 trControl = trainControl(method = "cv",
                                          number=10),
                 ntree=100)
  pred <- predict(model, newdata=testna)
  cat("\n Natancnost pri izbranih", i, " spremenljivk je", mean(pred == testna$ref))
  natacnostTTEST <- c(natacnostTTEST, mean(pred == testna$ref))
}



png("grafi/pozidano/ttestRF.png", width = 620, height = 620, units = "px")
plot(1:length(natacnostTTEST), natacnostTTEST, 
     xlab = "Znacilke z najvecjo stat-ttest do najmanjse", 
     ylab = "Natancnost na testni mnozici",
     main = "Vpliv ttets RF")
dev.off()





stopCluster(cl)

