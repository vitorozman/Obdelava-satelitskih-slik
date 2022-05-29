require(tidyverse)
require(grid)

require(gridExtra)
require(dplyr)
require(ggplot2)

require(rpart)
library(caret)


pca <- prcomp(~ .-ref, data = ucna, center = T, scale. = T)

plot((pca$sdev)^2/sum(pca$sdev^2),type="o", ylim=c(0,1),col="red")
lines(cumsum((pca$sdev)^2/sum(pca$sdev^2)),col="green", type="o")

plot(pca$x[ucna$ref=="N", 1], pca$x[ucna$ref=="N", 2], col="red", xlim=c(-5, 5), ylim=c(-5, 5))
points(pca$x[ucna$ref=="Y", 1], pca$x[ucna$ref=="Y", 2], col="green")


ucna_PC <- pca$x[, 1:6]

testna_PS <- predict(pca, newdata = testna)[, 1:6]


model = train(ref ~ ., data=ucna, 
              method="rpart", 
              tuneGrid = data.frame(cp=0.0001*c(1:10)),
              trControl = trainControl(method="cv", number=5))
pred = predict(model, newdata=testna)
cat(" Natancnost z vsemi spremenljivkami: ", mean(pred == testna$ref))
model = train(x = ucna_PC, y = ucna$ref, 
              method="rpart", 
              tuneGrid = data.frame(cp=0.0001*c(1:10)),
              trControl = trainControl(method="cv", number=5))
pred = predict(model, newdata=testna_PS)
cat(" Natancnost z metodo glavnih komponent: ", mean(pred == testna$ref))

# se drugre nelinerane metode za krcenje raseznosti
#- t-SNE
#- UMAP
#- autoencoder

acc_pc <- c()
for (i in 2:50){
  ucna_PC <- pca$x[, 1:i]
  testna_PS <- predict(pca, newdata = testna)[, 1:i]
  model = train(x = ucna_PC, y = ucna$ref, 
                method="rpart", 
                tuneGrid = data.frame(cp=0.0001*c(1:10)),
                trControl = trainControl(method="cv", number=5))
  pred = predict(model, newdata=testna_PS)
  cat("\n Natancnost z metodo glavnih komponent: ", mean(pred == testna$ref))
  acc_pc <- c(acc_pc, mean(pred == testna$ref))
}
#> acc_pc[6]
#[1] 0.9649903
plot(acc_pc)



#png("grafi/pca_Tree.png", width = 620, height = 620, units = "px")
#plot(1:length(acc_pc), acc_pc, 
#     xlab = "sprem += sprem", 
#     ylab = "Natancnost na testni mnozici",
#     main = "PCA Tree",
#     ylim = c(0.8,1))
#dev.off()


top11var <- names(varRang[1:11])
#[1] "NDVI.MAY" "NDVI.JUN" "NDVI.AUG" "NDVI.APR" "NDVI.DEC" "NDVI.JUL" "NDVI.JAN" "NDVI.NOV" "NDVI.MAR" "NDVI.OCT" "NDVI.SEP"

varucna <- ucna[top11var]
varucna$ref <- ucna$ref
vartestna <- testna[top11var]
vartestna$ref <- testna$ref
pca <- prcomp(~ .-ref, data = varucna, center = T, scale. = T)
acc_pc_var <- c()
for (i in 2:11){
  ucna_PC <- pca$x[, 1:i]
  testna_PS <- predict(pca, newdata = vartestna)[, 1:i]
  model = train(x = ucna_PC, y = varucna$ref, 
                method="rpart", 
                tuneGrid = data.frame(cp=0.0001*c(1:10)),
                trControl = trainControl(method="cv", number=5))
  pred = predict(model, newdata=testna_PS)
  cat("\n Natancnost z metodo glavnih komponent: ", mean(pred == vartestna$ref))
  acc_pc_var <- c(acc_pc_var, mean(pred == vartestna$ref))
}





#podobno se ostalo

top5stst <- names(statRang[1:5])
# [1] "B06.APR" "B12.OCT" "B05.OCT" "B11.OCT" "B12.JUL"


top5rlf <- imena[rlfRnag$ix[1:5]]
# [1] "B02.FEB"  "B03.FEB"  "B01.FEB"  "NDBI.JUL" "NDVI.JUL"


top5imp <- imena[imporfRnag$ix[1:5]]
#[1] "B03.JUL" "B04.JUL" "B02.JUL" "B12.JUL" "B03.OCT"














