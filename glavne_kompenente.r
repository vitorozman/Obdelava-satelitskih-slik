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


ucna_PC <- pca$x[, 1:20]

testna_PS <- predict(pca, newdata = testna)[, 1:20]


model = train(ref ~ ., data=ucna, 
              method="rpart", 
              tuneGrid = data.frame(cp=0.001*c(1:5)),
              trControl = trainControl(method="cv", number=5))
pred = predict(model, newdata=testna)
cat(" Natancnost z vsemi spremenljivkami: ", mean(pred == testna$V65))
model = train(x = ucna_PC, y = ucna$ref, 
              method="rpart", 
              tuneGrid = data.frame(cp=0.001*c(1:5)),
              trControl = trainControl(method="cv", number=5))
pred = predict(model, newdata=testna_PC)
cat(" Natancnost z metodo glavnih komponent: ", mean(pred == testna$V65))

# se drugre nelinerane metode za krcenje raseznosti
#- t-SNE
#- UMAP
#- autoencoder