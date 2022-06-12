require(tidyverse)
require(reticulate)
np = import("numpy")
require(grid)
require(gridExtra)
require(dplyr)
require(ggplot2)
require(rpart)
library(caret)


###########################################################################
# Namenjeno uvoz podatkov za napovedovanje pozidanega obmocja
###########################################################################

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
area_of_interest = c(1:4, 6:9, 11:14, 16:19)
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

podatki$ref[podatki$ref != 8] <- "N"
podatki$ref[podatki$ref == 8] <- "Y"
podatki$ref <- as.factor(podatki$ref)
podatki <- podatki %>% drop_na() # ce je slucajno kaksen NA


# vzel 1% vseh podatkov iz območja z referenco
indeks_obdelava <- createDataPartition(podatki$ref, p=0.01, list=FALSE)

podatki_01 <- podatki[indeks_obdelava,]

# podatki na katerih sem natreniral koncni model
ind_90 <- createDataPartition(podatki$ref, p=0.9, list=FALSE)
podatki_90 <- podatki[ind_90,]

