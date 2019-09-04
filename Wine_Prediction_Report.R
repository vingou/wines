## ----include=FALSE, echo=FALSE-------------------------------------------
# checks if the packages have been previously installed. if not, install them. 

if(!require(devtools)) install.packages("devtools", repos = "http://cran.us.r-project.org") 
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org") 
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org") 
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org") 
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org") 
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org") 
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org") 
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org") 
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(factoextra)) install.packages("factoextra", repos = "http://cran.us.r-project.org")
if(!require(mlbench)) install.packages("mlbench", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(flextable)) install.packages("flextable", repos = "http://cran.us.r-project.org")
if(!require(PerformanceAnalytics)) install.packages("PerformanceAnalytics", repos = "http://cran.us.r-project.org")


# sure that packages are available to R
library(magrittr)
library(flextable)
library(devtools)
library(caret)
library(dplyr)
library(ggplot2)
library(lubridate)
library(corrplot)
library(tidyr)
library(GGally)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(factoextra)
library(mlbench)
library(kableExtra)
library(magrittr)
library(flextable)
library(PerformanceAnalytics)






## ----import, echo=FALSE, include=FALSE-----------------------------------
#import dataset
Wines <- read.csv(file="https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", header=TRUE, sep=";")



## ----global_settings, echo=FALSE-----------------------------------------
options(warn=-1) # supress warning to sure that will be not printing warnings at finished output doc 


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

# create INPUT and OUTPUT variables sub datasets
INPUT <- Wines[1:11] # first 11 columns
OUTPUT <- Wines[12] # last column quality


## ----echo=FALSE, include=T-----------------------------------------------
summary(Wines )


## ----echo=FALSE----------------------------------------------------------
g <- Wines[,1:11] %>%
  keep(is.numeric) %>%                     # Keep only numeric columns
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) + 
     
    facet_wrap(~key, scales = "free") +   # In separate panels
    geom_density()
  
  plot(g)
  





## ----bivariate_correlation, echo=FALSE , include=T-----------------------


chart.Correlation(Wines,  histogram=TRUE, pch=19)





## ----echo=FALSE----------------------------------------------------------



values <- c("0.44","-0.31", "-0.21")
vars <- c("alcohol","density","chlorides")
tb <- tibble( variable = vars, correlation = values)
ft <- flextable(tb)
ft <- add_header_lines(ft, 
  values = "correlations among quality ")
ft





## ----echo=FALSE ,include=TRUE--------------------------------------------
values <- c("0.84","0.62", "0.53")
vars <- c("residual sugar - density","free sulfur dioxide - total sulfur dioxide","total sulfur dioxide - density")
tb <- tibble( variables = vars, correlation = values)
ft <- flextable(tb)
ft <- add_header_lines(ft, 
  values = "high correlated independent variables ")
ft


## ----box_features, echo=FALSE--------------------------------------------
grey_theme <- theme(axis.text.x = element_text(colour = "grey20", size = 10, angle = 90, hjust = 0.5, vjust = 0.5),
                          axis.text.y = element_text(colour = "grey20", size = 8),
                          text = element_text(size = 10),
                    legend.position="none")

Wines%>%
  gather(-quality, key = "var", value = "value") %>% 
  ggplot(aes(x = quality, y = value, group = quality)) +
    geom_boxplot() +
    facet_wrap(~ var, scales = "free", ncol = 4)+
   grey_theme


## ----echo=FALSE, include=TRUE--------------------------------------------
ggplot(data = Wines, aes(x=as.factor(quality), y=alcohol)) + geom_boxplot() +
   labs(x= 'Quality', y= 'Alcohol (% by vol)',
       title= 'Quality Vs. Alcohol ')




## ----echO=FALSE, include=FALSE-------------------------------------------
ggscatmat(Wines, columns = 1:11, alpha=0.8)


## ----data_transform, echo=FALSE, include=FALSE---------------------------

# create new column CatQuality with 3 factors
## create dataframe from transformation of original dataset
dataset <- mutate(Wines, CatQuality = ifelse(quality %in% 0:4, 0,
                                     ifelse(quality %in% 5:7, 1,
                                            ifelse(quality %in% 8:10, 2,99))))



dataset$CatQuality <- as.factor(dataset$CatQuality)      # turn CatQuality to factor  
dataset$quality = NULL # erase variable quality


# Rename all levels, by name then numbers
levels(dataset$CatQuality) <- list(bad=0, normal=1, best=2)

# use variable names as standard and i never use this names to other propose
X <- colnames(dataset)[1:11] # define independent variable names
Y <- "CatQuality" # dependent variable




## ----plot_new_dataset, echo=FALSE, include=TRUE--------------------------
dist <- table(dataset$CatQuality)
barplot(dist, main="New wine quality distribution", 
        col=grey.colors(3),
         legend = dist,
   xlab="Quality", ylab = "Count")


## ----dens_plot, echo=FALSE-----------------------------------------------
p <- ggplot(dataset, aes(alcohol)) + geom_density(aes(fill=CatQuality), alpha=1/4) +
    labs( title= 'Alcohol distribution accordingly to Quality levels')

flt <- filter(dataset, CatQuality != 'normal')
plt <- ggplot(flt, aes(x=density, y=alcohol, color=CatQuality)) +
  geom_point(position='jitter',size = 1) + 
  geom_smooth(method =  'loess') +
  xlim(min(flt$density), quantile(flt$density, prob=0.99)) +
  ylim(min(flt$alcohol), quantile(flt$alcohol, prob=0.99)) +
 labs(x= 'Density (c/cm³)', y= 'Alcohol (% by vol)',
       title= 'Alcohol Vs. Density accordingly to extreme Quality levels')
 grid.arrange(p, plt)


## ----box_plot_features, echo=FALSE ,include=FALSE------------------------

featurePlot(x = dataset[, 1:11], 
            y = dataset$CatQuality, 
            plot = "box", 
          
            scales = list(y = list(relation="free"),
                          x = list(rot = 90)),  
            layout = c(4,1 ), 
           )



## ----correlation_resudual_density , echo=FALSE ,include=TRUE-------------

flt <- filter(dataset, CatQuality != 'normal')
plt <- ggplot(dataset, aes(x=density, y=res_sugar, color=CatQuality)) +
  geom_point(position='jitter',size = 1) + 
  geom_smooth(method =  'lm') +
  xlim(min(flt$density), quantile(flt$density, prob=0.97)) +
  ylim(min(flt$res_sugar), quantile(flt$res_sugar, prob=0.98)) +
 labs(x= 'Density (mg/dm³)', y= 'Residual Sugar (g/L)',
       title= 'Residual Sugar Vs. Density accordingly  Quality levels')
plt


## ----alcohol_significance, echo=FALSE------------------------------------
ggplot(dataset, aes(x=CatQuality, y=alcohol, fill=CatQuality)) + 
  geom_boxplot() + 
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", 
               color='red', size=1) +
  ylim(quantile(dataset$alcohol, prob=0.00), 
       quantile(dataset$alcohol, prob=1)) +
  ggtitle("Range of Alcohol per Quality Level") + 
  xlab("Quality") +
  ylab("Alcohol (% by vol)") +
  theme(title=element_text(size=11, face="bold"),
        axis.title=element_text(size=10), 
        axis.text=element_text(size=10),
        legend.position="none")


## ----free_significance, echo=FALSE---------------------------------------
ggplot(dataset, aes(x=CatQuality, y=free_dioxide, fill=CatQuality)) + 
  geom_boxplot() + 
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", 
               color='red', size=1) +
  ylim(quantile(dataset$free_dioxide, prob=0.00), 
       quantile(dataset$free_dioxide, prob=.95)) +
  ggtitle("Range of Free dioxide per Quality Level") + 
  xlab("Quality") +
  ylab("Free Dioxide (mg/l )") +
  theme(title=element_text(size=11, face="bold"),
        axis.title=element_text(size=10), 
        axis.text=element_text(size=10),
        legend.position="none")



## ----data_partition , echo=FALSE, include=TRUE---------------------------
set.seed(16784568)

trainIndex <- createDataPartition(dataset$CatQuality, p = .8, 
                                  list = FALSE, 
                                  times = 1)

winesTrain <- dataset[ trainIndex,]
winesTest  <- dataset[-trainIndex,]




## ----SVM_model, echo=FALSE, include=T------------------------------------

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(57453)
 
svm_model <- train(CatQuality ~., data = winesTrain, method = "svmLinear",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
svm_fit <- predict(svm_model, newdata = winesTest)
svm_cm <- confusionMatrix(svm_fit, winesTest$CatQuality )
#as.table(svm_cm) # print cm 
svm_cm


## ----SVM_Original, echo=FALSE, include=TRUE------------------------------

set.seed(9970)
Wines$quality <- as.factor(Wines$quality)
train <- createDataPartition(Wines$quality, p = .8, 
                                  list = FALSE, 
                                  times = 1)

train <- Wines[ trainIndex,]
test  <- Wines[-trainIndex,]


trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(9573453)
 
svm <- train(quality ~., data = train, method = "svmLinear",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
sv <- predict(svm, newdata = test)
sv_cm <- confusionMatrix(sv, test$quality )
#as.table(sv_cm)
sv_cm


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
#as.table(gbm_cm)
gbm_cm



## ----Neural_NN, echo=FALSE, include=FALSE--------------------------------
set.seed(5165765)
nn_model <-  train(CatQuality ~ ., 
                data = winesTrain, 
                method = "nnet")
             

nn_fit <-predict(nn_model, winesTest)
# Create confusion matrix
cmNN_cm <-confusionMatrix(nn_fit, winesTest$CatQuality)
#as.table(cmNN_cm)
cmNN_cm


## ----random_forest, echo=FALSE, include=TRUE-----------------------------
set.seed(13131)
rf_model <- train(CatQuality ~ ., 
                data = winesTrain, 
                method = "rf")

rf_fit <- predict(rf_model,winesTest)
rf_cm <- confusionMatrix(rf_fit, winesTest$CatQuality)
#as.table(rf_cm)
rf_cm 




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

#as.table(xgb_cm)
xgb_cm 





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


## ----echo=FALSE----------------------------------------------------------
importance_rf <- varImp(rf_model)
plot(importance_rf)

