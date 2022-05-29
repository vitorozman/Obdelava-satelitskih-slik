# prvo je potrebno zagnati uvoz_podatkov.r da dobimo zmansano mnozico
# podatki_01 so podatki na katerih bom operiral

require(tidyverse)
require(grid)

require(gridExtra)
require(dplyr)
require(ggplot2)

require(rpart)
library(caret)


ucnaInd = createDataPartition(podatki_01$ref, p=0.75, list=FALSE)
ucnaX = podatki_01[ucnaInd, -181]
testnaX = podatki_01[-ucnaInd, -181]
ucnaY = podatki_01[ucnaInd, 181]
testnaY = podatki_01[-ucnaInd, 181]

# Varjanca
vars <- diag(var(ucnaX))
varRang <- sort(vars, decreasing = T)


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
library(FSelector)
ucna <- podatki_01[ucnaInd,]
testna <- podatki_01[-ucnaInd,]
rlf <- relief(ref ~ ., data=ucna)
# 10 sprem je dovolj


# importance random-forest
modelRF <- train(ucnaX, ucnaY, 
                 method='rf', 
                 trControl = trainControl(method = "cv",
                                          classProbs = TRUE,
                                          savePredictions = TRUE))
imporfRnag <- sort(modelRF$finalModel$importance, index.return=T, decreasing=T)
# 10 sprem je ok
top10imp <- imena[imporfRnag$ix[1:10]]

#################################################################################
#################################################################################

#  ACC modelov glede na varjanco
#Tree_natacnostVar <- c()
#for (i in 1:length(varRang)){
#  variancaFormula <- as.simple.formula(names(varRang[1:i]), "ref")
#  model <- train(variancaFormula, data=ucna, 
#                method="rpart", 
#                tuneGrid = data.frame(cp=0.0001*c(1:10)),
#                trControl = trainControl(method="cv", number=5))
#  pred <- predict(model, newdata=testna)
#  cat("\n Natancnost pri izbranih", i, " spremenljivk je", mean(pred == testna$ref))
#  Tree_natacnostVar <- c(Tree_natacnostVar, mean(pred == testna$ref))
#}

#glm_natacnostVar <- c()
#for (i in 1:length(varRang)){
#  variancaFormula <- as.simple.formula(names(varRang[1:i]), "ref")
#  model <- train(variancaFormula, data=ucna,
#                    method = "glm",
#                    trControl = trainControl(method="cv", number=5))
#  pred <- predict(model, newdata=testna)
#  cat("\n Natancnost pri izbranih", i, " spremenljivk je", mean(pred == testna$ref))
#  glm_natacnostVar <- c(glm_natacnostVar, mean(pred == testna$ref))
#}


#knn_natacnostVar <- c()
#for (i in 1:50){
#  variancaFormula <- as.simple.formula(names(varRang[1:i]), "ref")
#  model <- train(variancaFormula, data=ucna,
#                 method = "knn",
#                 trControl = trainControl(method="cv", number=5),
#                 tuneGrid = data.frame(k=1:30))
#  pred <- predict(model, newdata=testna)
#  cat("\n Natancnost pri izbranih", i, " spremenljivk je", mean(pred == testna$ref))
#  knn_natacnostVar <- c(knn_natacnostVar, mean(pred == testna$ref))
#  if (mean(pred == testna$ref) > 0.97){
#    break
#  }
#  #racunal 10 ur in prisel do 14-te znacilke z natacnostjo 0.97
#}


#svm_natacnostVar <- c()
#for (i in 1:50){
#  variancaFormula <- as.simple.formula(names(varRang[1:i]), "ref")
#  model <- train(variancaFormula, data=ucna,
#                 method = "svmLinear",
#                 trControl = trainControl(method="cv", number=5),
#                 tuneGrid=data.frame(C=c(0.001, 0.01, 1, 10)))
#  pred <- predict(model, newdata=testna)
#  cat("\n Natancnost pri izbranih", i, " spremenljivk je", mean(pred == testna$ref))
#  svm_natacnostVar <- c(svm_natacnostVar, mean(pred == testna$ref))
#  if (mean(pred == testna$ref) > 0.97){
#    break
#  }
#}



#png("grafi/var_Tree.png", width = 620, height = 620, units = "px")
#plot(1:length(Tree_natacnostVar), Tree_natacnostVar, 
#     xlab = "Znacilke z najvecjo var do najmanjse", 
#     ylab = "Natancnost na testni mnozici",
#     main = "Vpliv varjance Tree")
#dev.off()

#png("grafi/var_Glm.png", width = 620, height = 620, units = "px")
#plot(1:length(glm_natacnostVar), glm_natacnostVar, 
#     xlab = "Znacilke z najvecjo var do najmanjse", 
#     ylab = "Natancnost na testni mnozici",
#     main = "Vpliv varjance glm")
#dev.off()

#png("grafi/var_Knn.png", width = 620, height = 620, units = "px")
#plot(1:length(knn_natacnostVar), knn_natacnostVar, 
#     xlab = "Znacilke z najvecjo var do najmanjse", 
#     ylab = "Natancnost na testni mnozici",
#     main = "Vpliv varjance knn")
#dev.off()



################################################################################################
################################################################################################


#  ACC modelov import RF
#Tree_natacnostImp <- c()
#for (i in 1:100){
#  imena <- c(names(ucna) )
#  variancaFormula <- as.simple.formula(imena[imporfRnag$ix[1:i]], "ref")
#  model <- train(variancaFormula, data=ucna, 
#                 method="rpart", 
#                 tuneGrid = data.frame(cp=0.0001*c(1:10)),
#                 trControl = trainControl(method="cv", number=5))
#  pred <- predict(model, newdata=testna)
#  cat("\n Natancnost pri izbranih", i, " spremenljivk je", mean(pred == testna$ref))
#  Tree_natacnostImp <- c(Tree_natacnostImp, mean(pred == testna$ref))
#}# skonvergira do 0.967
#
#png("grafi/impRF_Tree.png", width = 620, height = 620, units = "px")
#plot(1:length(Tree_natacnostImp), Tree_natacnostImp, 
#     xlab = "Znacilke z najvecjo pomebnostjo-RF do najmanjse", 
#     ylab = "Natancnost na testni mnozici",
#     main = "Pomebnost-RF tree",
#     ylim = c(0,1))
#dev.off()
#
#Glm_natacnostImp <- c()
#for (i in 1:100){
#  imena <- c(names(ucna) )
#  variancaFormula <- as.simple.formula(imena[imporfRnag$ix[1:i]], "ref")
#  model <- train(variancaFormula, data=ucna,
#                                     method = "glm",
#                                     trControl = trainControl(method="cv", number=5))
#  pred <- predict(model, newdata=testna)
#  cat("\n Natancnost pri izbranih", i, " spremenljivk je", mean(pred == testna$ref))
#  Glm_natacnostImp <- c(Glm_natacnostImp, mean(pred == testna$ref))
#}# skonvergira do 0.9724
#
#png("grafi/impRF_Glm.png", width = 620, height = 620, units = "px")
#plot(1:length(Glm_natacnostImp), Glm_natacnostImp, 
#     xlab = "Znacilke z najvecjo pomebnostjo-RF do najmanjse", 
#     ylab = "Natancnost na testni mnozici",
#     main = "Pomebnost-RF Glm",
#     ylim = c(0,1))
#dev.off()

##################################################################################
##################################################################################

# ACC modelov RELIEF
#df <- data.frame(rlf)
#rlfRnag <- sort(t(df), index.return=T, decreasing=T)
## 10 sprem je ok
#
#Tree_natacnostRfe <- c()
#for (i in 1:100){
#  imena <- c(names(ucna))
#  variancaFormula <- as.simple.formula(imena[rlfRnag$ix[1:i]], "ref")
#  model <- train(variancaFormula, data=ucna, 
#                 method="rpart", 
#                 tuneGrid = data.frame(cp=0.0001*c(1:10)),
#                 trControl = trainControl(method="cv", number=5))
#  pred <- predict(model, newdata=testna)
#  cat("\n Natancnost pri izbranih", i, " spremenljivk je", mean(pred == testna$ref))
#  Tree_natacnostRfe <- c(Tree_natacnostRfe, mean(pred == testna$ref))
#}# 0.9694
#png("grafi/rlf_Tree.png", width = 620, height = 620, units = "px")
#plot(1:length(Tree_natacnostRfe), Tree_natacnostRfe, 
#     xlab = "Znacilke z najvecjo pomebnostjo-relief do najmanjse", 
#     ylab = "Natancnost na testni mnozici",
#     main = "Pomebnost-relief Tree",
#     ylim = c(0,1))
#dev.off()
#
#Glm_natacnostRfe <- c()
#for (i in 1:100){
#  imena <- c(names(ucna))
#  variancaFormula <- as.simple.formula(imena[rlfRnag$ix[1:i]], "ref")
#  model <- train(variancaFormula, data=ucna,
#                 method = "glm",
#                 trControl = trainControl(method="cv", number=5))
#  pred <- predict(model, newdata=testna)
#  cat("\n Natancnost pri izbranih", i, " spremenljivk je", mean(pred == testna$ref))
#  Glm_natacnostRfe <- c(Glm_natacnostRfe, mean(pred == testna$ref))
#}#0.9724924
#png("grafi/rlf_Glm.png", width = 620, height = 620, units = "px")
#plot(1:length(Glm_natacnostRfe), Glm_natacnostRfe, 
#     xlab = "Znacilke z najvecjo pomebnostjo-relief do najmanjse", 
#     ylab = "Natancnost na testni mnozici",
#     main = "Pomebnost-relief Glm",
#     ylim = c(0,1))
#dev.off()
#
#Knn_natacnostRfe <- c()
#for (i in 1:100){
#  imena <- c(names(ucna))
#  variancaFormula <- as.simple.formula(imena[rlfRnag$ix[1:i]], "ref")
#  model <- train(variancaFormula, data=ucna,
#                                  method = "knn",
#                                  trControl = trainControl(method="cv", number=5),
#                                  tuneGrid = data.frame(k=1:30))
#  pred <- predict(model, newdata=testna)
#  cat("\n Natancnost pri izbranih", i, " spremenljivk je", mean(pred == testna$ref))
#  Knn_natacnostRfe <- c(Knn_natacnostRfe, mean(pred == testna$ref))
#}
#png("grafi/rlf_Knn.png", width = 620, height = 620, units = "px")
#plot(1:length(Knn_natacnostRfe), Knn_natacnostRfe, 
#     xlab = "Znacilke z najvecjo pomebnostjo-relief do najmanjse", 
#     ylab = "Natancnost na testni mnozici",
#     main = "Pomebnost-relief Knn",
#     ylim = c(0,1))
#dev.off()

#######################################################################################
#######################################################################################

# ACC modelov t-test statistika
#Tree_natacnostStat <- c()
#for (i in 1:100){
#  variancaFormula <- as.simple.formula(names(statRang[1:i]), "ref")
#  model <- train(variancaFormula, data=ucna, 
#                method="rpart", 
#                tuneGrid = data.frame(cp=0.0001*c(1:10)),
#                trControl = trainControl(method="cv", number=5))
#  pred <- predict(model, newdata=testna)
#  cat("\n Natancnost pri izbranih", i, " spremenljivk je", mean(pred == testna$ref))
#  Tree_natacnostStat <- c(Tree_natacnostStat, mean(pred == testna$ref))
#}

#glm_natacnostStat <- c()
#for (i in 1:100){
#  variancaFormula <- as.simple.formula(names(statRang[1:i]), "ref")
#  model <- train(variancaFormula, data=ucna,
#                    method = "glm",
#                    trControl = trainControl(method="cv", number=5))
#  pred <- predict(model, newdata=testna)
#  cat("\n Natancnost pri izbranih", i, " spremenljivk je", mean(pred == testna$ref))
#  glm_natacnostStat <- c(glm_natacnostStat, mean(pred == testna$ref))
#}

#knn_natacnostStat <- c()
#for (i in 1:50){
#  variancaFormula <- as.simple.formula(names(statRang[1:i]), "ref")
#  model <- train(variancaFormula, data=ucna,
#                 method = "knn",
#                 trControl = trainControl(method="cv", number=5),
#                 tuneGrid = data.frame(k=1:30))
#  pred <- predict(model, newdata=testna)
#  cat("\n Natancnost pri izbranih", i, " spremenljivk je", mean(pred == testna$ref))
#  knn_natacnostStat <- c(knn_natacnostStat, mean(pred == testna$ref))
#  if (mean(pred == testna$ref) > 0.97){
#    break
#  }
#}


#png("grafi/ttest_Tree.png", width = 620, height = 620, units = "px")
#
#plot(1:length(Tree_natacnostStat), Tree_natacnostStat, 
#     xlab = "Znacilke z najvecjo pomebnostjo-ttest-stat do najmanjse", 
#     ylab = "Natancnost na testni mnozici",
#     main = "Pomebnost-ttest Tree",
#     ylim = c(0.5,1))
#dev.off()

#png("grafi/ttest_Glm.png", width = 620, height = 620, units = "px")
#plot(1:length(glm_natacnostStat), glm_natacnostStat, 
#     xlab = "Znacilke z najvecjo pomebnostjo-ttest-stat do najmanjse", 
#     ylab = "Natancnost na testni mnozici",
#     main = "Pomebnost-ttest Glm",
#     ylim = c(0,1))
#dev.off()

#png("grafi/ttest_Knn.png", width = 620, height = 620, units = "px")
#plot(1:length(knn_natacnostStat), knn_natacnostStat, 
#     xlab = "Znacilke z najvecjo pomebnostjo-ttest-stat do najmanjse", 
#     ylab = "Natancnost na testni mnozici",
#     main = "Pomebnost-ttest Knn",
#     ylim = c(0,1))
#dev.off()
