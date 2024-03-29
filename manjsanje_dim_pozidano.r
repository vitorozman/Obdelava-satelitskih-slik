# prvo je potrebno zagnati uvoz_podatkov_pozidano.r da dobimo zmansano mnozico
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

# za hitrost
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

# ucna in testna
ucnaInd = createDataPartition(podatki_01$ref, p=0.75, list=FALSE)
ucnaX = podatki_01[ucnaInd, -181]
testnaX = podatki_01[-ucnaInd, -181]
ucnaY = podatki_01[ucnaInd, 181]
testnaY = podatki_01[-ucnaInd, 181]

ucna <- podatki_01[ucnaInd,]
testna <- podatki_01[-ucnaInd,]


########################################################################
# Varjanca
#########################################################################
vars <- diag(var(ucnaX))
varRang <- sort(vars, decreasing = T)



#########################################################################
# t-test
#########################################################################
statistika <- c()
pvrednost <- c()
for (i in 1:length(ucnaX)){
  ttest <- t.test(ucnaX[ucnaY == "N", i], ucnaX[ucnaY == "Y", i])
  statistika <- c(statistika, ttest$statistic) 
  pvrednost <- c(pvrednost, ttest$p.value)
}
names(statistika) <- names(ucnaX)
names(pvrednost) <- names(ucnaX)
statRang <- sort(statistika, decreasing = T)
pvredRang <- sort(pvrednost, decreasing = F)





#########################################################################
# Relief
#########################################################################
rlf <- relief(ref ~ ., data=ucna)
df <- data.frame(rlf)
rlfRnag <- sort(t(df), index.return=T, decreasing=T)
imena <- c(names(ucna))







#########################################################################
# Importance random-forest
#########################################################################
modelRF <- train(ucnaX, ucnaY, 
                 method='rf', 
                 trControl = trainControl(method = "cv",
                                          classProbs = TRUE,
                                          savePredictions = TRUE),
                 ntree=100)
imporfRnag <- sort(modelRF$finalModel$importance, index.return=T, decreasing=T)






#################################################################################
#  ACC modelov glede na varjanco
#################################################################################

natacnostVAR <- c()
for (i in 1:30){
  model <- train(ucnaX[names(varRang[1:i])], ucnaY, 
                 method='rf', 
                 trControl = trainControl(method = "cv",
                                          number=10),
                 ntree=100)
  pred <- predict(model, newdata=testna)
  cat("\n Natancnost pri izbranih", i, " spremenljivk je", mean(pred == testna$ref))
  natacnostVAR <- c(natacnostVAR, mean(pred == testna$ref))
}
# sranimo sliko
png("grafi/pozidano/varRF.png", width = 620, height = 620, units = "px")
plot(1:length(natacnostVAR), natacnostVAR, 
     xlab = "Znacilke z najvecjo var do najmanjse", 
     ylab = "Natancnost na testni mnozici",
     main = "Vpliv varjance RF")
dev.off()






################################################################################################
#  ACC modelov import RF
################################################################################################

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
# sranimo sliko
png("grafi/pozidano/impRF.png", width = 620, height = 620, units = "px")
plot(1:length(natacnostIMP), natacnostIMP, 
     xlab = "Znacilke z najvecjo imp do najmanjse", 
     ylab = "Natancnost na testni mnozici",
     main = "Vpliv pomebnost (imp) RF")
dev.off()







##################################################################################
# ACC modelov RELIEF
##################################################################################

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
# sranimo sliko
png("grafi/pozidano/relRF.png", width = 620, height = 620, units = "px")
plot(1:length(natacnostREL), natacnostREL, 
     xlab = "Znacilke z najvecjo relief do najmanjse", 
     ylab = "Natancnost na testni mnozici",
     main = "Vpliv relief RF")
dev.off()








#######################################################################################
# ACC modelov t-test statistika
#######################################################################################

natacnostTTEST_stat <- c()
for (i in 1:30){
  model <- train(ucnaX[names(statRang[1:i])], ucnaY, 
                 method='rf', 
                 trControl = trainControl(method = "cv",
                                          number=10),
                 ntree=100)
  pred <- predict(model, newdata=testna)
  cat("\n Natancnost pri izbranih", i, " spremenljivk je", mean(pred == testna$ref))
  natacnostTTEST_stat <- c(natacnostTTEST_stat, mean(pred == testna$ref))
}
# sranimo sliko
png("grafi/pozidano/ttestRF.png", width = 620, height = 620, units = "px")
plot(1:length(natacnostTTEST_stat), natacnostTTEST_stat, 
     xlab = "Znacilke z najvecjo stat-ttest do najmanjse", 
     ylab = "Natancnost na testni mnozici",
     main = "Vpliv ttets RF")
dev.off()




##################################################################################
# Acc modelv t-test p vrednost
##################################################################################
natacnostTTEST_p <- c()
for (i in 1:30){
  model <- train(ucnaX[names(pvredRang[1:i])], ucnaY, 
                 method='rf', 
                 trControl = trainControl(method = "cv",
                                          number=10),
                 ntree=100)
  pred <- predict(model, newdata=testna)
  cat("\n Natancnost pri izbranih", i, " spremenljivk je", mean(pred == testna$ref))
  natacnostTTEST_p <- c(natacnostTTEST_p, mean(pred == testna$ref))
}
# sranimo sliko
png("grafi/pozidano/p_ttestRF.png", width = 620, height = 620, units = "px")
plot(1:length(natacnostTTEST_p), natacnostTTEST_p, 
     xlab = "Znacilke z najvecjo pvrednost-ttest do najmanjse", 
     ylab = "Natancnost na testni mnozici",
     main = "Vpliv ttets p-vrednost RF")
dev.off()



stopCluster(cl)

########################################################################################
# Najbolsi kriterij mansanja -> statistika t-test
#######################################################################################

names(statRang[1:15])
#"NDVI.MAR" "NDVI.SEP" "NDVI.AUG" "NDVI.APR" "NDVI.JAN" "NDVI.JUL" "NDVI.DEC" "NDVI.OCT" "NDVI.MAY" "NDVI.FEB" "NDVI.NOV" "NDVI.JUN" "B9.JUL"   "B8A.JUL"
# NDVI , B09, B8A













