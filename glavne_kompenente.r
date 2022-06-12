
# potrebno pognati uvoz_podatkov_gozd.r za testiranje na gozdovih
# ali pa 
# potrebno pognati uvoz_podatkov_pozidano.r za testiranje na pozidanem obmocju

require(tidyverse)
require(grid)
require(gridExtra)
require(dplyr)
require(ggplot2)
require(rpart)
library(caret)
library(doParallel)

# pospesitev
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


# PCA metoda
pca <- prcomp(~ .-ref, data = ucna, center = T, scale. = T)

ucna_PC_6 <- pca$x[, 1:6]
testna_PS_6 <- predict(pca, newdata = testna)[, 1:6]

ucna_PC <- pca$x[, 1:15]
testna_PS <- predict(pca, newdata = testna)[, 1:15]

# testeranje na modelu
model <- train(ucnaX, ucnaY, 
               method='rf', 
               trControl = trainControl(method = "cv",
                                        number=10),
               ntree=100)
pred = predict(model, newdata=testna)
cat(" Natancnost z vsemi spremenljivkami: ", mean(pred == testna$ref))

modelPC = train(x = ucna_PC, y = ucna$ref, 
              method="rf", 
              trControl = trainControl(method = "cv",
                                       number=10),
              ntree=100)
pred = predict(modelPC, newdata=testna_PS)
cat(" Natancnost z metodo glavnih komponent: ", mean(pred == testna$ref))




# natancost odvisna od stevilo komponent
acc_pc <- c()
for (i in 2:50){
  ucna_PC <- pca$x[, 1:i]
  testna_PS <- predict(pca, newdata = testna)[, 1:i]
  m = train(x = ucna_PC, y = ucna$ref, 
            method="rf", 
            trControl = trainControl(method = "cv",
                                     number=10),
            ntree=100)
  pred = predict(m, newdata=testna_PS)
  cat("\n Natancnost z metodo glavnih komponent: ", mean(pred == testna$ref))
  acc_pc <- c(acc_pc, mean(pred == testna$ref))
}
#> acc_pc[6]
#[1] 0.9649903















stopCluster(cl)









