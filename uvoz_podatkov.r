require(tidyverse)
require(reticulate)
np = import("numpy")
require(grid)

require(gridExtra)
require(dplyr)
require(ggplot2)

require(rpart)
library(caret)

INPUT_FOLDER = file.path(".", "podatki")

FEATURE_NAMES = c(
  "B01", "B02", "B03", "B04",
  "B05", "B06", "B07", "B08",
  "B8A", "B9", "B11", "B12",
  "NDVI", "NDWI", "NDBI"
)
TIME_POINTS = c(
  "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
  "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"
)
VAR_NAMES = expand.grid(
  fn = FEATURE_NAMES,
  ts = TIME_POINTS
) %>%
  mutate(
    vn = paste0(fn, ".", ts)
  ) %>%
  getElement("vn")
area_of_interest = c(3)
arrays = list()
references = list()
k = 1
for (n in area_of_interest) {
  arrays[[k]] = np$load(file.path(
    INPUT_FOLDER,
    sprintf("data_%d.npy", n)
  ))
  references[[k]] = np$load(file.path(
    INPUT_FOLDER,
    sprintf("reference_%d.npy", n)
  ))
  k = k + 1
}
podatki = np$concatenate(arrays) %>% as.matrix() %>% as.data.frame()
colnames(podatki) = VAR_NAMES
podatki$ref = np$concatenate(references) %>% as.vector() 

podatki$ref[podatki$ref != 2] <- 0
podatki$ref[podatki$ref == 2] <- 1
podatki$ref <- as.factor(podatki$ref)
summary(podatki)


ctrl = trainControl(method = "cv",
                    savePredictions = TRUE)
var(ucna$B01.FEB)
summary(podatki)

varjance <- data.frame(VAR_NAMES)
n <- length(VAR_NAMES)
varjance$VAR <- c(1:n)
for (i in 1:n){
  varjance[i,2] <- var(podatki[,i])  
}

varjance <- varjance[order(varjance$VAR,decreasing = TRUE),]

#podatkiX = podatki[varjance[1:20,1]!= 'ref']
#podatkiY = podatki$ref

#modelTree <- train(podatkiX, podatkiY,
#                   method='rpart', 
#                   trControl=ctrl)
#

#i <- createDataPartition(podatki$ref, p=0.75, list=FALSE)
#ucna <- podatki[i,]
#testna <- podatki[-i,]

acc <- c()
for(k in 1:n){
  imena <- varjance[1:k,1]
  podatkiX = podatki[imena]
  podatkiY = podatki$ref
  model <- train(podatkiX, podatkiY,
                 method='rpart', 
                 trControl=ctrl)
  acc <- c(acc, max(model$results$Accuracy))
}

plot(1:length(acc), acc, xlab = "sprem. z najvecjo var do najmanjse", 
     ylab = "Natancnost na podlagi cv z (0.75 data)",
     main = "Vpliv varjance")

















