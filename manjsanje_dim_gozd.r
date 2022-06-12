

# prvo je potrebno zagnati uvoz_podatkov_gozd.r da dobimo zmansano mnozico
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

#hitrost
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

# Ucna testna
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
  
}

names(statistika) <- names(ucnaX)
names(pvrednost) <- names(ucnaX)
statRang <- sort(statistika, decreasing = T)






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
                                          savePredictions = TRUE,
                                          number=10),
                 ntree=100)
modelRF
pred <- predict(modelRF, testnaX)
mean(pred == testnaY)
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
# shrani sliko
png("grafi/gozd/varRF.png", width = 620, height = 620, units = "px")
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
# shrani sliko
png("grafi/gozd/impRF.png", width = 620, height = 620, units = "px")
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
# shrani sliko
png("grafi/gozd/relRF.png", width = 620, height = 620, units = "px")
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
# shrani sliko
png("grafi/gozd/ttestRF.png", width = 620, height = 620, units = "px")
plot(1:length(natacnostTTEST_stat), natacnostTTEST_stat, 
     xlab = "Znacilke z najvecjo stat-ttest do najmanjse", 
     ylab = "Natancnost na testni mnozici",
     main = "Vpliv ttets RF")
dev.off()


stopCluster(cl)



########################################################################################
# Najbolsi kriterij mansanja -> relief
#######################################################################################

imena[rlfRnag$ix][1:12] #acc > 0.97

#"NDWI.FEB" "NDBI.JUN" "NDVI.JUN" "B02.FEB"  "NDBI.NOV" "B03.FEB"  "B12.FEB"  "NDVI.MAY" "B08.APR"  "NDBI.FEB" "NDBI.DEC" "NDBI.JAN"
# NDWI, NDBI, NDVI, B02, B03, B12, B08













