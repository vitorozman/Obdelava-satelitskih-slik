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
area_of_interest = c(3) #vzel sem polje 3
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

podatki$ref[podatki$ref != 2] <- "NE"
podatki$ref[podatki$ref == 2] <- "DA"
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


#acc <- c()
#for(k in 1:n){
#  imena <- varjance[1:k,1]
#  podatkiX = podatki[imena]
#  podatkiY = podatki$ref
#  model <- train(podatkiX, podatkiY,
#                 method='rpart', 
#                 trControl=ctrl)
#  acc <- c(acc, max(model$results$Accuracy))
#}

plot(1:length(acc), acc, xlab = "sprem. z najvecjo var do najmanjse", 
     ylab = "Natancnost na podlagi cv vseh podatkov",
     main = "Vpliv varjance")

##############################################
# Pobral 20 najbolsiih sprememenlivk glede na vrjanco
##############################################
var_podatki <- podatki[varjance[1:20,1]]
var_podatki$ref <- podatki$ref
i <- createDataPartition(var_podatki$ref, p=0.75, list=FALSE)
ucna <- var_podatki[i,]
testna <- var_podatki[-i,]





ctrlROC <- trainControl(method='cv', 
                      number=10,
                      savePredictions = T,
                      classProbs = T,
                      summaryFunction = twoClassSummary)

glmROC <- train(ref~., data=ucna,
                   method = "glm",
                   metric = "ROC",
                   trControl = ctrlROC)
#Error: At least one of the class levels is not a valid R variable name; 
#This will cause errors when class probabilities are generated because the variables names will
#be converted to  X0, X1 . Please use factor levels that can be used as 
#valid R variable names  (see ?make.names for help).
knnROC <- train(ref~., data=ucna,
                   method = "knn",
                   metric = "ROC",
                   trControl = ctrlROC,
                   tuneGrid = data.frame(k=1:30))
SvmROC <- train(ref~., data=ucna,
                   method='svmLinear',
                   metric = "ROC",
                   trControl = ctrlROC,
                   tuneGrid=data.frame(C=10))


ucnaX <- ucna[varjance[1:20,1]!= 'ref']
ucnaY <- ucna$ref
TreeROC <- train(ucnaX, ucnaY,
                    method='rpart',
                    metric = "ROC",
                    trControl=ctrlROC,
                    tuneGrid = data.frame(cp = 0))

napakaUcnaTree <- sum(TreeROC$pred$pred == ucna$ref) / length(ucna$ref)
napakaUcnaTree
#[1] 0.5106963
treeacctest <- sum(predict(TreeROC, testna) == testna$ref)/ length(testna$ref)
treeacctest
# [1] 1 ...cudno +0+
rfROC <- train(ucnaX, ucnaY, 
                  method='rf',
                  metric = "ROC",
                  tuneGrid = data.frame(mtry=20),
                  trControl = ctrlROC)

ROC_GLM <- glmROC$results$ROC
ROC_KNN <- max(knnROC$results$ROC)
ROC_SVM <- SvmROC$results$ROC
ROC_TREE <- TreeROC$results$ROC
ROC_RF <- rfROC$results$ROC
df_ROC <- data.frame(ROC_GLM,ROC_KNN,ROC_SVM,ROC_TREE,ROC_RF)
rownames(df_ROC) <- "Ploscina pod ROC krivuljo"
colnames(df_ROC) <- c("GLM", "KNN", "SVM", "TREE", "RF")
df_ROC















