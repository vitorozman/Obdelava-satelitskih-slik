require(tidyverse)
require(reticulate)
np = import("numpy")
require(grid)
require(gridExtra)
require(dplyr)
require(ggplot2)
require(rpart)
library(caret)
library(doParallel)


ucnaDown <- downSample(podatki_90[, -181], podatki_90$ref, yname="ref")
trainX <- ucnaDown[,-181]
trainY <- ucnaDown[,181]

cl <- makePSOCKcluster(4)

registerDoParallel(cl)

modelRF <- train(trainX, trainY, 
               method='rf',
               tuneGrid = data.frame(mtry=40),
               trControl = trainControl(method='cv', number=10),
               ntree=100)

#acc 0.9601195
#natacnost testna 0.9595414
#Sensitivity : 0.9624          
#Specificity : 0.9589


# funkcija ki napove in skrani podatke v .npy
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

shrani_podatke(modelRF, 8)

stopCluster(cl)


