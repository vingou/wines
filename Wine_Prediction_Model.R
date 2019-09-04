## ----include=FALSE, echo=FALSE-------------------------------------------
# checks if the packages have been previously installed. if not, install them. 

if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org") 
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org") 
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org") 
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org") 
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org") 
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(mlbench)) install.packages("mlbench", repos = "http://cran.us.r-project.org")
if(!require(flextable)) install.packages("flextable", repos = "http://cran.us.r-project.org")

# sure that packages are available to R
library(dplyr)
library(caret)
library(ggplot2)
library(lubridate)
library(tidyr)
library(tidyverse)
library(mlbench)
library(flextable)


## ----import, echo=FALSE, include=FALSE-----------------------------------
#import dataset
Wines <- read.csv(file="https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", header=TRUE, sep=";")


## ----prepare, echo=FALSE, include=FALSE----------------------------------
# change variables character . to _ in order to avoid R names incompatibility
# also use short names to prevent break legends
Wines <- Wines %>% 
  rename(
  fix_acidity = fixed.acidity,
  vol_acidity = volatile.acidity,
  cit_acid  = citric.acid ,
  res_sugar = residual.sugar ,
  free_dioxide =  free.sulfur.dioxide,
  total_dioxide = total.sulfur.dioxide
)



## ----data_transform, echo=FALSE, include=FALSE---------------------------
## Three categories
# create new column CatQuality with 3 factors
## create dataframe from transformation of original dataset
dataset <- mutate(Wines, CatQuality = ifelse(quality %in% 0:4, 0,
                                     ifelse(quality %in% 5:7, 1,
                                            ifelse(quality %in% 8:10, 2,99))))



dataset$CatQuality <- as.factor(dataset$CatQuality)      # turn CatQuality to factor  
dataset$quality = NULL # erase older variable quality


# Rename all levels, by name then numbers
levels(dataset$CatQuality) <- list(bad=0, normal=1, best=2)


## ----data_partition , echo=FALSE, include=TRUE---------------------------
# 80% training , 20% testing

set.seed(16784568) # to turn reproducible

trainIndex <- createDataPartition(dataset$CatQuality, p = .8, 
                                  list = FALSE, 
                                  times = 1)

winesTrain <- dataset[ trainIndex,]
winesTest  <- dataset[-trainIndex,]




## ----SVM_model, echo=FALSE, include=T------------------------------------

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3) # parameters
set.seed(57453) # to turn reproducible
 
svm_model <- train(CatQuality ~., data = winesTrain, method = "svmLinear",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
svm_fit <- predict(svm_model, newdata = winesTest)
svm_cm <- confusionMatrix(svm_fit, winesTest$CatQuality )

svm_cm # show retults from confusion matrix


## ----gbm_model, echo=FALSE-----------------------------------------------

set.seed(6580349)
fitControl <- trainControl(## 10-fold CV
                           method = "repeatedcv",
                           number = 10,
                           ## repeated ten times
                           repeats = 10)

gbm_model <- train(CatQuality ~ ., data = winesTrain, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)


gbm_fit <- predict(gbm_model,winesTest)
gbm_cm <- confusionMatrix(gbm_fit, winesTest$CatQuality)

gbm_cm #show retults from confusion matrix



## ----random_forest, echo=FALSE, include=TRUE-----------------------------
set.seed(13131)
rf_model <- train(CatQuality ~ ., 
                data = winesTrain, 
                method = "rf")

rf_fit <- predict(rf_model,winesTest)
rf_cm <- confusionMatrix(rf_fit, winesTest$CatQuality)

rf_cm #show retults from confusion matrix



## ----xgb, echo=FALSE, include=TRUE---------------------------------------

set.seed(4757656)
ControlParamteres <- trainControl(method = "cv",
                                  number = 5,
                                  savePredictions = TRUE,
                                  classProbs = TRUE
)

parametersGrid <-  expand.grid(eta = 0.1, 
                            colsample_bytree=c(0.5,0.7),
                            max_depth=c(1,3,6,9),
                            nrounds=100,
                            gamma=1,
                            min_child_weight=2,
                            subsample=1
                            )


xg_model <- train(CatQuality~., 
                  data = winesTrain,
                  method = "xgbTree",
                  trControl = ControlParamteres,
                  tuneGrid=parametersGrid,
                  grid=parametersGrid)

xgb_fit <- predict(xg_model,winesTest)
xgb_cm <- confusionMatrix(xgb_fit, winesTest$CatQuality)

xgb_cm  # #show retults from confusion matrix



## Summary of Results

## ----best_bench,  echo=FALSE---------------------------------------------
best_sens <- c(svm_cm[["byClass"]][3], gbm_cm[["byClass"]][3], rf_cm[["byClass"]][3],xgb_cm[["byClass"]][3])
best_spec <- c(svm_cm[["byClass"]][6], gbm_cm[["byClass"]][6], rf_cm[["byClass"]][6],xgb_cm[["byClass"]][6])
best_balance <- c(svm_cm[["byClass"]][33], gbm_cm[["byClass"]][33], rf_cm[["byClass"]][33],xgb_cm[["byClass"]][33])
best_prec <- c(svm_cm[["byClass"]][15], gbm_cm[["byClass"]][15], rf_cm[["byClass"]][15],xgb_cm[["byClass"]][15])

models_name <- c("SVM","GBM",
                 "RF","XGB")
tb <- tibble( Model = models_name, Sensitivity = best_sens, 
              Specificity = best_spec , Balanced = best_balance, Precision = best_prec)
ft <- flextable(tb)
ft <- add_header_lines(ft, 
  values = "Best Wines predictor Model Benchmark")
ft


## ----bad_bench,  echo=FALSE----------------------------------------------
bad_sens <- c(svm_cm[["byClass"]][1], gbm_cm[["byClass"]][1], rf_cm[["byClass"]][1],xgb_cm[["byClass"]][1])
bad_spec <- c(svm_cm[["byClass"]][4], gbm_cm[["byClass"]][4], rf_cm[["byClass"]][4],xgb_cm[["byClass"]][4])
bad_balance <- c(svm_cm[["byClass"]][31], gbm_cm[["byClass"]][31], rf_cm[["byClass"]][31],xgb_cm[["byClass"]][31])
bad_prec <- c(svm_cm[["byClass"]][13], gbm_cm[["byClass"]][13], rf_cm[["byClass"]][13],xgb_cm[["byClass"]][13])


tb2 <- tibble( Model = models_name, Sensitivity = bad_sens, Specificity = bad_spec , 
                Balanced = bad_balance,  Precision = bad_prec)
ft2 <- flextable(tb2)
ft2 <- add_header_lines(ft2, 
  values = "Bad Wines predictor Model Benchmark")
ft2


## ----echo=FALSE- ---------------------------------------------------------
importance_rf <- varImp(rf_model)
importance_rf # plot variable importance from Random Forest Model


