# potrebno je zagnati uvoz_podatko_gozd.r
require(tidyverse)
require(reticulate)
np = import("numpy")
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

natancnosti <- data.frame(n=1:18, testna=rep(0, 18), ucna=rep(0, 18))
#mtry <- 50
for(mtry in 1:18){
  model <- train(ucnaX, ucnaY, method='rf',
                 tuneGrid = data.frame(mtry= 10*mtry),
                 trControl = trainControl(method='cv', number=10),
                 ntree=100)
  pred <- predict(model, testnaX)
  natancnosti$testna[mtry] <- mean(pred == testnaY)
  natancnosti$ucna[mtry] <- model$results$Accuracy
  cat("\n poskus",mtry*10 ,":" ,natancnosti$testna[mtry], natancnosti$ucna[mtry])
}
# mtry = 30, ntree = 150
# mtry = 30, ntree = 100





GLMModel <- train(ref~., data=podatki_50,
                  method = "glm",
                  trControl = trainControl(method='cv', number=10))

GLMModel
#0.9778404
# 50% -> 0.9777945


dataX <- podatki_20[, -181]
dataY <- podatki_20[, 181]
RFModel <- train(dataX, dataY, method='rf',
                 tuneGrid = data.frame(mtry=30),
                 trControl = trainControl(method='cv', number=10),
                 ntree=100)
# 10% -> 0.9781113


shrani_podatke <- function(model, typ){
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
  INPUT_FOLDER = file.path(".", "podatki")
  obmocja <-  c(0,5, 10,15,20:24)
  for (n in obmocja){
    pydata <- np$load(file.path(
      INPUT_FOLDER,
      sprintf("data_%d.npy", n)))
    pred_data <- pydata %>% as.matrix() %>% as.data.frame()
    colnames(pred_data) <- VAR_NAMES
    prediction <- predict(model, pred_data)
    levels(prediction)[1] <- 0
    levels(prediction)[2] <- typ
    if (typ == 2){
      fileName <- sprintf("napovedi/napovedi_gozd_%d.npy", n)
    } else if (typ == 8){
      fileName <- sprintf("napovedi/napovedi_pozidano_%d.npy", n)
    }
    np$save(fileName, prediction)
  }
}

shrani_podatke(RFModel, 2)















