require(tidyverse)
require(reticulate)
np = import("numpy")
require(grid)

require(gridExtra)
require(dplyr)
require(ggplot2)

library(caret)
library(doParallel)

TIME <- 12
FEATURES <- 15
FEATURE_NAMES = c(
  "B01", "B02", "B03", "B04",
  "B05", "B06", "B07", "B08",
  "B8A", "B9", "B11", "B12",
  "NDVI", "NDWI", "NDBI"
)
INPUT_FOLDER = file.path(".", "podatki")

###############################################################################
# Funkcija za krcene za gozdove
###############################################################################
apply_reduction = function(input_array) {
  tibble_array = input_array %>%
    array_reshape(c(nrow(input_array) * TIME, FEATURES)) %>%
    as_tibble()
  names(tibble_array) = FEATURE_NAMES
  tibble_array %>%
    select(
      B02, B03, B08, B12, NDVI, NDWI, NDBI 
    ) %>%
    as.matrix() %>%
    array_reshape(c(nrow(input_array), 7 * TIME))
}



# Uvoz podatkov
area_of_interest = c(1:4, 6:9, 11:14, 16:19)
arrays = list()
references = list()
k = 1
for (n in area_of_interest) {
  DATA <- np$load(file.path(INPUT_FOLDER, sprintf("data_%d.npy", n)))
  arrays[[k]] = apply_reduction(DATA)
  references[[k]] = np$load(file.path(INPUT_FOLDER, sprintf("reference_%d.npy", n)))
  k = k + 1
}

NEW_FEATURE_NAMES = c(
  "B02", "B03", "B08", "B12", 
  "NDVI", "NDWI", "NDBI"
)
TIME_POINTS = c(
  "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
  "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"
)
NEW_VAR_NAMES = expand.grid(
  fn = NEW_FEATURE_NAMES,
  ts = TIME_POINTS
) %>%
  mutate(
    vn = paste0(fn, ".", ts)
  ) %>%
  getElement("vn")

podatki = np$concatenate(arrays) %>% as.matrix() %>% as.data.frame()
colnames(podatki) = NEW_VAR_NAMES
podatki$ref = np$concatenate(references) %>% as.vector() 

podatki$ref[podatki$ref != 2] <- "N"
podatki$ref[podatki$ref == 2] <- "Y"
podatki$ref <- as.factor(podatki$ref)
podatki <- podatki %>% drop_na()
#summary(podatki)

ind_15 <- createDataPartition(podatki$ref, p=0.15, list=FALSE)
podatki_15 <- podatki[ind_15,]

cl <- makePSOCKcluster(4)

registerDoParallel(cl)


# Natreniramo model 
dataX <- podatki_15[, -85]
dataY <- podatki_15[, 85]
RFModel <- train(dataX, dataY, 
                 method='rf',
                 tuneGrid = data.frame(mtry=30),
                 trControl = trainControl(method='cv', number=10),
                 ntree=100)
#0.9796066

###############################################################################
# funkcija ki sharni skrcene podatke v .npy
###############################################################################

shrani_podatke_gozd <- function(model, typ){
  obmocja <-  c(0,5, 10,15,20:24)
  for (n in obmocja){
    rawdata <- np$load(file.path(INPUT_FOLDER,sprintf("data_%d.npy", n)))
    pydata <- apply_reduction(rawdata)
    pred_data <- pydata %>% as.matrix() %>% as.data.frame()
    colnames(pred_data) <- NEW_VAR_NAMES
    prediction <- predict(model, pred_data)
    levels(prediction)[1] <- 0
    levels(prediction)[2] <- typ
    if (typ == 2){
      fileName <- sprintf("napovedi/skrcene_napovedi_gozd_%d.npy", n)
    } else if (typ == 8){
      fileName <- sprintf("napovedi/skrcene_napovedi_pozidano_%d.npy", n)
    }
    np$save(fileName, prediction)
  }
}

shrani_podatke_gozd(RFModel, 2)



stopCluster(cl)















