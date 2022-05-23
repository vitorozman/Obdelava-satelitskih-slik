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
pVrednost <- c()
for (i in 1:length(ucnaX)){
  ttest <- t.test(ucnaX[ucnaY == "N", i], ucnaX[ucnaY == "Y", i])
  pVrednost <- c(pVrednost, ttest$p.value) 
  print(ttest$p.value)
}

names(pVrednost) <- names(ucnaX)
pvalRang <- sort(pVrednost)
plot(pvalRang)
# => vse vrednosti so 0

# Relief
library(FSelector)

ucna <- podatki_01[ucnaInd,]
testna <- podatki_01[-ucnaInd,]
#rlf <- relief(ref ~ ., data=ucna)
#

#  ACC modelov
Tree_natacnostVar <- c()
for (i in 1:length(varRang)){
  variancaFormula <- as.simple.formula(names(varRang[1:i]), "ref")
  model <- train(variancaFormula, data=ucna, 
                method="rpart", 
                tuneGrid = data.frame(cp=0.0001*c(1:10)),
                trControl = trainControl(method="cv", number=5))
  pred <- predict(model, newdata=testna)
  cat("\n Natancnost pri izbranih", i, " spremenljivk je", mean(pred == testna$ref))
  Tree_natacnostVar <- c(Tree_natacnostVar, mean(pred == testna$ref))
}

glm_natacnostVar <- c()
for (i in 1:length(varRang)){
  variancaFormula <- as.simple.formula(names(varRang[1:i]), "ref")
  model <- train(variancaFormula, data=ucna,
                    method = "glm",
                    trControl = trainControl(method="cv", number=5))
  pred <- predict(model, newdata=testna)
  cat("\n Natancnost pri izbranih", i, " spremenljivk je", mean(pred == testna$ref))
  glm_natacnostVar <- c(glm_natacnostVar, mean(pred == testna$ref))
}


knn_natacnostVar <- c()
for (i in 1:50){
  variancaFormula <- as.simple.formula(names(varRang[1:i]), "ref")
  model <- train(variancaFormula, data=ucna,
                 method = "knn",
                 trControl = trainControl(method="cv", number=5),
                 tuneGrid = data.frame(k=1:30))
  pred <- predict(model, newdata=testna)
  cat("\n Natancnost pri izbranih", i, " spremenljivk je", mean(pred == testna$ref))
  knn_natacnostVar <- c(knn_natacnostVar, mean(pred == testna$ref))
  if (mean(pred == testna$ref) > 0.97){
    break
  }
}


#plot(1:length(Tree_natacnostVar), Tree_natacnostVar, 
#     xlab = "Znacilke z najvecjo var do najmanjse", 
#     ylab = "Natancnost na testni mnozici",
#     main = "Vpliv varjance Tree")

#plot(1:length(glm_natacnostVar), glm_natacnostVar, 
#     xlab = "Znacilke z najvecjo var do najmanjse", 
#     ylab = "Natancnost na testni mnozici",
#     main = "Vpliv varjance glm")


#plot(1:length(knn_natacnostVar), knn_natacnostVar, 
#     xlab = "Znacilke z najvecjo var do najmanjse", 
#     ylab = "Natancnost na testni mnozici",
#     main = "Vpliv varjance knn")


# importance random-forest
modelRF <- train(ucnaX, ucnaY, 
                 method='rf', 
                 trControl = trainControl(method = "cv",
                                          classProbs = TRUE,
                                          savePredictions = TRUE))
modelRF$finalModel$importancee

