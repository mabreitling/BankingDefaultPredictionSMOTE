rm(list = ls())
gc()
devAskNewPage(ask = FALSE)
library(DMwR)
library(ggplot2)
library(plyr)
library(tidyverse)
library(lubridate)
library(caret)
library(zoo)
library(pROC)
library(rlist)
library(stargazer)
library(ggROC)
library(xgboost)
library(gbm)
library(DataCombine)
setwd('/home/marius/VL_Unterlagen/DataAnaProject/Lyudmila/data/camel')

#########################
## Dataset_large:  contains all variables but only periods till 2012.4 (35)
load("Dataset_large.RData")

## Dataset_small.RData: contains all periods but NOT all variables are included.

#load("Dataset_small.RData")
Dataset_small$period_order <- as.integer(Dataset_small$period_order)
Dataset_small$period_date <- ymd(Dataset_small$period_date)
Dataset_small$year <- year(Dataset_small$period_date)
Dataset_small$quarter <- lubridate::quarter(Dataset_small$period_date)
Dataset_small$quarter_year <- lubridate::quarter(Dataset_small$period_date, with_year = TRUE)
Dataset_small$failure_date <- lubridate::ymd(Dataset_small$failure_date)

Dataset_small$failure_quarter_year <- lubridate::quarter(Dataset_small$failure_date, with_year = TRUE)


# two variable: Failure and Perform, Failure == 1: Bank fails at some point; Perform == 1 exactly at point of failure!
Dataset_small$Failure <- ifelse(is.na(Dataset_small$Failure), "NONFAILURE", "FAILURE")
table(Dataset_small$Failure)
prop.table(table(Dataset_small$Failure)) #imbalance: around 5 to 95 percent (failures vs non-failings bank over all
#periods!)

# plotting and descriptives:

# plot number of failed banks by year and quarter!
# by variable "perform", only over existing periods. 
Dataset_small %>%
  #arrange(period_order, .by_group = FALSE) %>%
  group_by(quarter_year) %>%
  summarise(count = sum(Perform, na.rm = TRUE)) %>%
  ggplot(aes(x = as.factor(quarter_year), y = count, stat = "identity")) +
  geom_bar(stat = "identity") +
  geom_vline(xintercept=19, colour="red") +
  geom_text(aes(x=15, label="Lehman\n Brothers\n Bankruptcy", y=47), colour="red", angle=0) +
  labs(title = "Ńumber of Failed Banks by Year and Quarter", x = "Period", y = "Number of Failed Banks")+
  scale_x_discrete(position = "bottom")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))

# plot variable "failure" date (includes dates which are not present backed with data in this datasets)
quarters <- as.data.frame(table(Dataset_small$failure_quarter_year))
names(quarters) <- c("quarter", "failures")
quarters$failures <- quarters$failures/36
breaks <- unique(lubridate::quarter(seq(min(Dataset_small$period_date, na.rm = TRUE), 
                             max(Dataset_small$failure_date, na.rm = TRUE), by = "2 months"), with_year =  TRUE))
breaks <- data.frame(quarter = breaks, failures = 0)
quarters <- plyr::join(quarters, breaks, by = c("quarter"), type = "full")
quarters <- quarters[order(quarters$quarter),]
quarters$Data <- factor(ifelse(quarters$quarter %in% lubridate::quarter(seq(min(Dataset_small$period_date, na.rm=TRUE), 
                                                          max(Dataset_small$period_date, na.rm = TRUE), by = "2.9 months"), with_year = TRUE)
                        , "available", "not available"))

quarters %>%
  #arrange(period_order, .by_group = FALSE) %>%
  ggplot(aes(x = as.factor(quarter), y = failures, stat = "identity", color = Data)) +
  geom_bar(stat = "identity") +
  labs(title = "Ńumber of Failed Banks by Year and Quarter", x = "Quarter", y = "Number of Failed Banks")+
  scale_x_discrete(position = "bottom")+
  geom_text(aes(x=14, label="Lehman\n Brothers\n Bankruptcy", y=40), colour="black", angle=0) +
  geom_vline(xintercept=19, colour="black") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5), legend.position = "bottom")


ggsave('/home/marius/VL_Unterlagen/DataAnaProject/Lyudmila/graphics/failures_by_quarter_complete_with_text.png', device = 'png', dpi = 300)
rm(quarters)
rm(breaks)


#preprocessing:
# filter for relevant variables!
# normalize!


## 1) Filter all observations, which don't have a valid "Perform" value, meaning every bank which went into default
##    is filtered in the periods afterwards: [0 0 0 0 0 1 NA NA NA NA] 
## 2) Select only CAMEL variables + cert + period_order + Failure + Perform
## 3) mutate Failure variable to numeric 1 - 0 (1 in case of failure, 0 if performing well over all observations)

#example: h = 2, standing in point period = 20 (2008.4), predicting two quarters ahead:

dataset_filtered <- Dataset_small %>%
  filter(!is.na(Perform)) %>% #filter observation of banks which arenot performing any more
  select(c(cert, period_order, eq, lnatres, roa, sc, bro, chbal, intan, lnreres, lnremult, lnrecons, lnrenres, lnci, lncon, 
           asset, Failure, Perform)) %>% #filter CAMEL variables
  mutate(Failure = ifelse(Failure == "FAILURE", 1, 0)) #create numerical variable from failure string

# 4) coerce variables to numeric before actual preprocessing:
dataset_filtered <- as.data.frame(sapply(dataset_filtered, function(x) as.numeric(x))) #transform every variable to numeric, transform to data frame
dataset_filtered$log_asset <- log(dataset_filtered$asset) #logged assets

# compute means for operating and for failed banks seperately
descriptics_means <- dataset_filtered %>%
  dplyr::group_by(Failure) %>%
  dplyr::summarise("eq: Bank equity capital" = mean(eq, na.rm = TRUE)/mean(asset, na.rm = TRUE),
                   "lnatres: Loan loss allowance" = mean(lnatres, na.rm = TRUE)/mean(asset, na.rm = TRUE),
                   "roa: Return on assets" = mean(roa, na.rm = TRUE),
                   "sc: Total securities" = mean(sc, na.rm = TRUE)/mean(asset, na.rm = TRUE),
                   "bro: Brokered desposits" = mean(bro, na.rm = TRUE)/mean(asset, na.rm = TRUE),
                   "ln_asset Logartihm of total assets:" = mean(log(asset), na.rm = TRUE),
                   "chbal: Cash and balances due from depository institutions" = mean(chbal, na.rm = TRUE)/mean(asset, na.rm = TRUE),
                   "intan: Goodwill and other intangibles" = mean(intan, na.rm = TRUE)/mean(asset, na.rm = TRUE),
                   "lnreres: 1-4 family residential mortgages" = mean(lnatres, na.rm = TRUE)/mean(asset, na.rm = TRUE),
                   "lnremult Real estate mulitfamily residential mortgages:" = mean(lnremult, na.rm = TRUE)/mean(asset, na.rm = TRUE),
                   "lnrecons: Construction and development loans" = mean(lnrecons, na.rm = TRUE)/mean(asset, na.rm = TRUE),
                   "lnrenres: Commercial real estate non-residential mortgages"= mean(lnrenres, na.rm = TRUE)/mean(asset, na.rm = TRUE),
                   "lnlnci: Commercial and industrial loans" = mean(lnci, na.rm = TRUE)/mean(asset, na.rm = TRUE),
                   "lncon: Loans to individuals " = mean(lncon, na.rm = TRUE)/mean(asset, na.rm = TRUE))

descriptics_means <- t(descriptics_means)
descriptics_means <- round(descriptics_means, 2)
descriptics_means <- as.data.frame(descriptics_means)

# compute std for operating and for failed banks seperately
descriptics_sd <- dataset_filtered %>%
  dplyr::group_by(Failure) %>%
  dplyr::summarise("eq: Bank equity capital" = sd(eq, na.rm = TRUE)/sd(asset, na.rm = TRUE),
                   "lnatres: Loan loss allowance" = sd(lnatres, na.rm = TRUE)/sd(asset, na.rm = TRUE),
                   "roa: Return on assets" = sd(roa, na.rm = TRUE),
                   "sc: Total securities" = sd(sc, na.rm = TRUE)/sd(asset, na.rm = TRUE),
                   "bro: Brokered desposits" = sd(bro, na.rm = TRUE)/sd(asset, na.rm = TRUE),
                   "ln_asset Logartihm of total assets:" = sd(log(asset), na.rm = TRUE),
                   "chbal: Cash and balances due from depository institutions" = sd(chbal, na.rm = TRUE)/sd(asset, na.rm = TRUE),
                   "intan: Goodwill and other intangibles" = sd(intan, na.rm = TRUE)/sd(asset, na.rm = TRUE),
                   "lnreres: 1-4 family residential mortgages" = sd(lnatres, na.rm = TRUE)/sd(asset, na.rm = TRUE),
                   "lnremult Real estate mulitfamily residential mortgages:" = sd(lnremult, na.rm = TRUE)/sd(asset, na.rm = TRUE),
                   "lnrecons: Construction and development loans" = sd(lnrecons, na.rm = TRUE)/sd(asset, na.rm = TRUE),
                   "lnrenres: Commercial real estate non-residential mortgages"= sd(lnrenres, na.rm = TRUE)/sd(asset, na.rm = TRUE),
                   "lnlnci: Commercial and industrial loans" = sd(lnci, na.rm = TRUE)/sd(asset, na.rm = TRUE),
                   "lncon: Loans to individuals " = sd(lncon, na.rm = TRUE)/sd(asset, na.rm = TRUE))

descriptics_sd <- t(descriptics_sd)
descriptics_sd <- round(descriptics_sd, 2)
descriptics_sd <- as.data.frame(descriptics_sd)

descriptics <- cbind(descriptics_means, descriptics_sd)
names(descriptics) <- c("mean_operating", "mean_failure", "sd_operating", "sd_failure")
descriptics <- descriptics[,c('mean_operating', 'sd_operating', 'mean_failure', 'sd_failure')]
write.csv(x = descriptics, file = 'descriptics.csv', row.names = TRUE) #write to csv for created of latex table
rm(descriptics, descriptics_means, descriptics_sd)

# 5) try to reshape data frame such that every bank is one observation over ALL periods! (long --> wide)

library(data.table)
camel <- c("eq", "lnatres", "roa", "sc", "bro", "log_asset", "chbal", "intan", "lnreres", "lnremult", "lnrecons", "lnrenres", "lncon", 
           "lnci", "lncon", "asset")
createLaggedDataset <- function(df, variable_set, lag, pred_horizon){
  for (i in -1:lag){
    for (var in camel){
      df <- slide(df, Var = var, TimeVar = "period_order", GroupVar = 'cert', NewVar = paste(var, '_', -i, sep = ""), slideBy = i)
    }
  }
  df <- slide(df, Var = "Perform", TimeVar = "period_order", GroupVar = 'cert', NewVar = 'Perform_2', slideBy = pred_horizon) 
  df <- df %>%
    select(-camel)
  return(df)
}

#dataset_filtered <- data.table::data.table(dataset_filtered) #transform to data table for easier gathering table to wide format

#create dataset with 6 last lagged variables and a prediction horizon of h = 2:
dataset_lagged <- createLaggedDataset(dataset_filtered, camel, lag = -6, pred_horizon = 2) #create variables with last 6 lagged observations 


#filter NAs (no advanced techniques here, because the focus is more on SMOTE!):
dataset_lagged <- dataset_lagged[complete.cases(dataset_lagged),]
dataset_lagged$id <- seq.int(nrow(dataset_lagged))

## 6) normalize variables. Careful!!! 


#You first need to split the data into training and test set (validation 
#set might also be required).
#Don't forget that testing data points represent real-world data. Feature normalization 
#(or data standardization) of the explanatory (or predictor) variables is a technique 
#used to center and normalise the data by subtracting the mean and dividing by the variance. 
#If you take the mean and variance of the whole dataset you'll be introducing future information into the training explanatory variables (i.e. the mean and variance).
#Therefore, you should perform feature normalisation over the training data. Then perform n
#ormalisation on testing instances as well, but this time using the mean and variance of training explanatory variables. 
#In this way, we can test and evaluate whether our model can generalize well to new, unseen data points.


### 1: Split into train and test set: ###

#careful: split first into training/test set before smoting or any other preprocessing! (80%-20%)
dataset_lagged <- dataset_lagged %>%
  select(-c(cert,period_order, Failure, Perform, id))

trainIndices <- createDataPartition(dataset_lagged$Perform_2, p = .8, 
                                    list = FALSE, 
                                    times = 1)
trainSet_imbalanced <- dataset_lagged[trainIndices,]
testSet <- dataset_lagged[-trainIndices,]

#for smoting and prediction: transform target to factor!
trainSet_imbalanced$Perform_2 <- as.factor(trainSet_imbalanced$Perform_2)
testSet$Perform_2 <- as.factor(testSet$Perform_2)

table(trainSet_imbalanced$Perform_2)
table(testSet$Perform_2)

round(prop.table(table(trainSet_imbalanced$Perform_2)),3) #[0.998 zu 0.002]
round(prop.table(table(testSet$Perform_2)),3)


#####################################################################################################
### 2: Normalize both training and test set with mean and standard deviation of the training set: ###
#####################################################################################################
preProcValues <-preProcess(trainSet_imbalanced, method = c("center","scale")) #save preProcValues for imbalanced trainSet
trainSet_imbalanced_pre <- predict(preProcValues, trainSet_imbalanced) #scale and center train set
testSet_pre <- predict(preProcValues, testSet) #apply same values to testSet


levels(trainSet_imbalanced_pre$Perform_2) <- c("nonfailure", "failure")
trainSet_imbalanced$Perform_2 <- as.factor(trainSet_imbalanced$Perform_2)
levels(trainSet_imbalanced$Perform_2) <- c("nonfailure", "failure")
testSet_pre$Perform_2 <- as.factor(testSet_pre$Perform_2)
levels(testSet_pre$Perform_2) <- c("nonfailure", "failure")

testSetPerform_2 <- as.factor(testSet$Perform_2)
levels(testSet$Perform_2) <- c("nonfailure", "failure")

rm(Dataset_small, dataset_lagged, dataset_filtered, preProcValues, Failures) #remove unn dataset


################################################
### 3: Define control and tuning parameters: ###
################################################
myFolds_full <- createFolds(trainSet_imbalanced_pre$Perform_2, k = 5) #create folds for cv
tuneGridgbm <- expand.grid(interaction.depth = 1:10, 
                        n.trees = c(1,2,5,c(1,2,3,10,15,30)*50),
                        shrinkage = 0.1,
                        n.minobsinnode = 10) #keep standard value)
  

ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                       classProbs = TRUE,
                       verboseIter = TRUE,
                       savePredictions = TRUE,
                       index = myFolds_full,
                       summaryFunction = twoClassSummary)


### fit on complete dataset:
modelFit_complete <- train(Perform_2 ~ ., data = trainSet_imbalanced_pre, #fit model to training set
                       method = "gbm",
                       trControl = ctrl,
                       #preProcess=c("scale", "center"), (XXX)
                       tuneGrid = tuneGridgbm,
                       metric = "ROC",
                       maximize = TRUE)


print(modelFit_complete$bestTune)
testSet_pre$prediction <- predict(modelFit_complete, newdata = testSet_pre)
table(testSet_pre$Perform_2, testSet_pre$prediction)
sum(testSet_pre$Perform_2 == testSet_pre$prediction)/nrow(testSet_pre)

# compute variable performance:
gbmImp <- varImp(modelFit_complete, scale = TRUE)
gbmImp
rm(modelFit_complete)
gc()


saveRDS(modelFit_complete, "/home/marius/VL_Unterlagen/DataAnaProject/Lyudmila/data/camel/trained_models/trainmodelFit_complete.rds")

####################################################################
### introduce preproc during cv while training on full dataset: ##
#################################################################

myFolds_full <- createFolds(trainSet_imbalanced$Perform_2, k = 5) #create folds for cv
tuneGridgbm <- expand.grid(interaction.depth = 1:10, 
                           n.trees = c(1,2,5,c(1,2,3,10,15,30)*50),
                           shrinkage = 0.1,
                           n.minobsinnode = 10) #keep standard value)


ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE,
                     verboseIter = TRUE,
                     savePredictions = TRUE,
                     index = myFolds_full,
                     summaryFunction = twoClassSummary)

modelFit_complete_imb <- train(Perform_2 ~ ., data = trainSet_imbalanced, #fit model to training set
                           method = "gbm",
                           trControl = ctrl,
                           preProcess=c("scale", "center"),  ## preprocessing within dataset
                           tuneGrid = tuneGridgbm,
                           metric = "ROC",
                           maximize = TRUE)


print(modelFit_complete$bestTune)
testSet_pre$prediction <- predict(modelFit_complete, newdata = testSet_pre)
table(testSet_pre$Perform_2, testSet_pre$prediction)
sum(testSet_pre$Perform_2 == testSet_pre$prediction)/nrow(testSet_pre)

saveRDS(modelFit_complete_imb, "/home/marius/VL_Unterlagen/DataAnaProject/Lyudmila/data/camel/trained_models/trainmodelFit_complete_imb.rds")

###################################################


# ############################################################
# ### 4. Introduce SMOTE into the model training process:  ###
# ###########################################################
# 
# ### 4.1 Smote training set !before! training, leaving it out of the cv:
# 
# ### perc.over = 500
# # perc.under = 100 to only oversample, no undersampling of the majority class:
# trainSet_smoted_500 <- trainSet_imbalanced_pre %>%
#   SMOTE(form = Perform_2 ~ ., perc.under = 0, perc.over = 500, k = 5) 
# 
# 
# # fit on smoted dataset:
# modelFit_smoted_500 <- train(Perform_2 ~ ., data = trainSet_smoted_500, #fit model to smoted training set
#                            method = "gbm",
#                            trControl = ctrl,
#                            tuneGrid = tuneGridgbm,
#                            metric = "ROC",
#                            maximize = TRUE)
# 
# saveRDS(modelFit_smoted_500, "/home/marius/VL_Unterlagen/DataAnaProject/Lyudmila/data/camel/trained_models/modelFit_smoted_500.rds")
# rm(modelFit_smoted_500)
# 
# ### perc.over = 1000
# trainSet_smoted_1000 <- trainSet_imbalanced_pre %>%
#   SMOTE(form = Perform_2 ~ ., perc.under = 100, perc.over = 1000, k = 5) 
# 
# # fit on smoted dataset:
# modelFit_smoted_1000 <- train(Perform_2 ~ ., data = trainSet_smoted_1000, #fit model to smoted training set
#                              method = "gbm",
#                              trControl = ctrl,
#                              tuneGrid = tuneGridgbm,
#                              metric = "ROC",
#                              maximize = TRUE)
# 
# saveRDS(modelFit_smoted_1000, "/home/marius/VL_Unterlagen/DataAnaProject/Lyudmila/data/camel/trained_models/modelFit_smoted_1000.rds")
# rm(modelFit_smoted_1000)
# 
# ### perc.over = 10000
# trainSet_smoted_10000 <- SMOTE(form = Perform_2 ~ ., data = trainSet_imbalanced_pre, perc.under = 100, perc.over = 300, k = 5) 
# 
# # fit on smoted dataset:
# modelFit_smoted_10000 <- train(Perform_2 ~ ., data = trainSet_smoted_1000, #fit model to smoted training set
#                              method = "gbm",
#                              trControl = ctrl,
#                              tuneGrid = tuneGridgbm,
#                              metric = "ROC",
#                              maximize = TRUE)
# 
# saveRDS(modelFit_complete, "/home/marius/VL_Unterlagen/DataAnaProject/Lyudmila/data/camel/trained_models/modelFit_smoted_10000.rds")
# rm(modelFit_smoted_10000)      


        
### 4.2: add standard caret option (sampling = "smote" to the training process)

ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE,
                     verboseIter = TRUE,
                     savePredictions = TRUE,
                     index = myFolds_full,
                     summaryFunction = twoClassSummary)

tuneGridgbm <- expand.grid(interaction.depth = 1:10, 
                           n.trees = c(1,2,5,c(1,2,3,10,15,30)*50),
                           shrinkage = 0.1,
                           n.minobsinnode = 10)

modelFit_smote_standard <- train(Perform_2 ~ ., data = trainSet_imbalanced, #fit model to training set
                           method = "gbm",
                           trControl = trainControl,
                           preProcess=c("scale", "center"),
                           tuneGrid = tuneGridgbm,
                           metric = "ROC",
                           sampling = "smote",
                           maximize = TRUE)

saveRDS(modelFit_smote_standard, "/home/marius/VL_Unterlagen/DataAnaProject/Lyudmila/data/camel/trained_models/modelFit_smote_standard.rds")
rm(modelFit_smote_standard)
gc()

print(modelFit_smote_standard)
testSet_pre$prediction_smote_standard <- predict(modelFit_smote_standard, newdata = testSet_pre)
table(testSet_pre$Perform_2, prediction_smote_standard$prediction_smote_standard)





################################################################################
### 4.2: Customize Smote Function with different perc.over/perc.under values: #
###############################################################################

myFolds_customSmote <- createFolds(trainSet_imbalanced_pre$Perform_2, k = 5) #create folds for cv
tuneGridgbm <- expand.grid(interaction.depth = 1:10, 
                           n.trees = c(1,2,5,c(1,2,3,10,15,30)*50),
                           shrinkage = 0.1,
                           n.minobsinnode = 10)

#########################################
### First customized Smote: ############
#########################################

smote_customized1 <- list(name = "SMOTE with more neighbors!",
                func = function (x, y) {
                  library(DMwR)
                  dat <- if (is.data.frame(x)) x else as.data.frame(x)
                  dat$.y <- y
                  dat <- SMOTE(.y ~ ., data = dat, perc.over = 4000, perc.under = 100, k = 5)
                  list(x = dat[, !grepl(".y", colnames(dat), fixed = TRUE)], 
                       y = dat$.y)
                },
                first = FALSE) #first = FALSE, boolean which specified if done first (before scaling) or after!

ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE,
                     verboseIter = TRUE,
                     savePredictions = TRUE,
                     index = myFolds_customSmote,
                     summaryFunction = twoClassSummary,
                     sampling = smote_customized1)


smote_model_customized1 <- train(Perform_2 ~ ., data = trainSet_imbalanced, #fit model to training set
      method = "gbm",
      trControl = ctrl,
      preProcess=c("scale", "center"),
      tuneGrid = tuneGridgbm,
      metric = "ROC",
      maximize = TRUE)

saveRDS(smote_model_customized1, "/home/marius/VL_Unterlagen/DataAnaProject/Lyudmila/data/camel/trained_models/smote_model_customized1.rds")
rm(smote_model_customized1)
gc()

#########################################
### Second customized Smote: ############
#########################################

smote_customized2 <- list(name = "SMOTE with more neighbors!",
                          func = function (x, y) {
                            library(DMwR)
                            dat <- if (is.data.frame(x)) x else as.data.frame(x)
                            dat$.y <- y
                            dat <- SMOTE(.y ~ ., data = dat, perc.over = 10000, perc.under = 100, k = 5)
                            list(x = dat[, !grepl(".y", colnames(dat), fixed = TRUE)], 
                                 y = dat$.y)
                          },
                          first = FALSE)

ctrl$sampling <- smote_customized2

smote_model_customized2 <- train(Perform_2 ~ ., data = trainSet_imbalanced, #fit model to training set
      method = "gbm",
      trControl = ctrl,
      preProcess=c("scale", "center"),
      tuneGrid = tuneGridgbm,
      metric = "ROC",
      maximize = TRUE)

saveRDS(smote_model_customized2, "/home/marius/VL_Unterlagen/DataAnaProject/Lyudmila/data/camel/trained_models/smote_model_customized2.rds")
rm(smote_model_customized2)
gc()

#########################################
### Third customized Smote: ############
#########################################

smote_customized3 <- list(name = "SMOTE with more neighbors!",
                          func = function (x, y) {
                            library(DMwR)
                            dat <- if (is.data.frame(x)) x else as.data.frame(x)
                            dat$.y <- y
                            dat <- SMOTE(.y ~ ., data = dat, perc.over = 10000, perc.under = 200, k = 5)
                            list(x = dat[, !grepl(".y", colnames(dat), fixed = TRUE)], 
                                 y = dat$.y)
                          },
                          first = FALSE)

ctrl$sampling <- smote_customized3

smote_model_customized3 <- train(Perform_2 ~ ., data = trainSet_imbalanced, #fit model to training set
      method = "gbm",
      trControl = ctrl,
      preProcess=c("scale", "center"),
      tuneGrid = tuneGridgbm,
      metric = "ROC",
      maximize = TRUE)

saveRDS(smote_model_customized3, "/home/marius/VL_Unterlagen/DataAnaProject/Lyudmila/data/camel/trained_models/smote_model_customized3.rds")
rm(smote_model_customized3)
gc()


setwd("/home/marius/VL_Unterlagen/DataAnaProject/Lyudmila/data/camel/trained_models/")

#######################################
#### Evaluation: Model Complete ##
######################################
model_complete <- readRDS('trainmodelFit_complete.rds')
model_complete_predictions <- predict(model_complete, testSet_pre)

varImportance <- varImp(model_complete)
varImportance <- varImportance$importance
varImportance %>% 
  rownames_to_column(var = "rowname") %>% 
  arrange(desc(Overall)) %>%
  filter(row_number() <= 15) %>%
  ggplot(aes(x = reorder(rowname, Overall), y = Overall)) +
  geom_bar(stat='identity') +
  coord_flip() +
  labs(title = "Scaled Variable Importance", subtitle = "15 most important variables - (Imbalanced)", 
       x = " Variable Importance", y = "Variable and Lag")
ggsave('/home/marius/VL_Unterlagen/DataAnaProject/Lyudmila/graphics/var_imp_imbalanced.png', device = 'png', dpi = 300)


bestTunesDf <- model_complete$bestTune

ggplot(model_complete)+
  #scale_x_discrete(breaks = 1:10, labels = 1:10)+
  theme(legend.position = "bottom")+
  scale_x_continuous(breaks=1:10)+
  labs(title = "ROC by different Max Tree Depths", subtitle = "15 most important variables - (Imbalanced)", 
       x = "Max Tree Depth", y = "ROC (Repeated Cross Validation)")
ggsave('/home/marius/VL_Unterlagen/DataAnaProject/Lyudmila/graphics/ROC_by_treeDepth_imb.png', device = 'png', dpi = 300)
rm(model_complete)
gc()

#######################################
#### Evaluation: SMOTE customized 1 ##
######################################
smote_model_customized1 <- readRDS('smote_model_customized1.rds')
smote_model_customized1_predictions <- predict(smote_model_customized1, testSet)
varImportance <- varImp(smote_model_customized1)
varImportance <- varImportance$importance
varImportance %>% 
  rownames_to_column(var = "rowname") %>% 
  arrange(desc(Overall)) %>%
  filter(row_number() <= 15) %>%
  ggplot(aes(x = reorder(rowname, Overall), y = Overall)) +
  geom_bar(stat='identity') +
  coord_flip() +
  labs(title = "Scaled Variable Importance (SMOTE cust. 1)", subtitle = "15 most important variables - (SMOTE: perc.over = 4000, perc.under = 100, k = 5)", 
       x = " Variable Importance", y = "Variable and Lag")
ggsave('/home/marius/VL_Unterlagen/DataAnaProject/Lyudmila/graphics/var_imp_smote_cust_1.png', device = 'png', dpi = 300)

bestTunesDf <- rbind(bestTunesDf, smote_model_customized1$bestTune)

ggplot(varImp(smote_model_customized1)) #plot variable importance
confusionMatrix(smote_model_customized1_predictions, testSet_pre$Perform_2)

ggplot(smote_model_customized1)+
  #scale_x_discrete(breaks = 1:10, labels = 1:10)+
  theme(legend.position = "bottom")+
  scale_x_continuous(breaks=1:10)+
  labs(title = "ROC by different Max Tree Depths", subtitle = "(SMOTE: perc.over = 4000, perc.under = 100, k = 5)", 
       x = "Max Tree Depth", y = "ROC (Repeated Cross Validation)")
ggsave('/home/marius/VL_Unterlagen/DataAnaProject/Lyudmila/graphics/ROC_by_treeDepth_smote1.png', device = 'png', dpi = 300)

rm(smote_model_customized1)
gc()

#######################################
#### Evaluation: SMOTE customized 2 ##
######################################

smote_model_customized2 <- readRDS('smote_model_customized2.rds')
smote_model_customized2_predictions <- predict(smote_model_customized2, testSet)

varImportance <- varImp(smote_model_customized2)
varImportance <- varImportance$importance
varImportance %>% 
  rownames_to_column(var = "rowname") %>% 
  arrange(desc(Overall)) %>%
  filter(row_number() <= 15) %>%
  ggplot(aes(x = reorder(rowname, Overall), y = Overall)) +
  geom_bar(stat='identity') +
  coord_flip() +
  labs(title = "Scaled Variable Importance (SMOTE cust. 2)", subtitle = "15 most important variables - (SMOTE: perc.over = 10000, perc.under = 100, k = 5)", 
       x = " Variable Importance", y = "Variable and Lag")
ggsave('/home/marius/VL_Unterlagen/DataAnaProject/Lyudmila/graphics/var_imp_smote_cust_2.png', device = 'png', dpi = 300)

bestTunesDf <- rbind(bestTunesDf, smote_model_customized2$bestTune)

confusionMatrix(smote_model_customized2_predictions, testSet_pre$Perform_2)

ggplot(smote_model_customized2)+
  #scale_x_discrete(breaks = 1:10, labels = 1:10)+
  theme(legend.position = "bottom")+
  scale_x_continuous(breaks=1:10)+
  labs(title = "ROC by different Max Tree Depths", subtitle = "(SMOTE: perc.over = 10000, perc.under = 100, k = 5)", 
       x = "Max Tree Depth", y = "ROC (Repeated Cross Validation)")
ggsave('/home/marius/VL_Unterlagen/DataAnaProject/Lyudmila/graphics/ROC_by_treeDepth_smote2.png', device = 'png', dpi = 300)

rm(smote_model_customized2)
gc()

#######################################
#### Evaluation: SMOTE customized 3 ##
######################################

smote_model_customized3 <- readRDS('smote_model_customized3.rds')
smote_model_customized3_predictions <- predict(smote_model_customized3, testSet)

varImportance <- varImp(smote_model_customized3)
varImportance <- varImportance$importance
varImportance %>% 
  rownames_to_column(var = "rowname") %>% 
  arrange(desc(Overall)) %>%
  filter(row_number() <= 15) %>%
  ggplot(aes(x = reorder(rowname, Overall), y = Overall)) +
  geom_bar(stat='identity') +
  coord_flip() +
  labs(title = "Scaled Variable Importance (SMOTE cust. 3)", subtitle = "15 most important variables - (SMOTE: perc.over = 10000, perc.under = 200, k = 5)", 
       x = " Variable Importance", y = "Variable and Lag")
ggsave('/home/marius/VL_Unterlagen/DataAnaProject/Lyudmila/graphics/var_imp_smote_cust_3.png', device = 'png', dpi = 300)

bestTunesDf <- rbind(bestTunesDf, smote_model_customized3$bestTune)
bestTunesDf <- cbind(model = c('complete', 'smote1', 'smote2', 'smote3'),bestTunesDf)
write_csv(bestTunesDf, 'bestTunesSmoteModels.csv')

confusionMatrix(smote_model_customized3_predictions, testSet_pre$Perform_2)

ggplot(smote_model_customized3)+
  #scale_x_discrete(breaks = 1:10, labels = 1:10)+
  theme(legend.position = "bottom")+
  scale_x_continuous(breaks=1:10)+
  labs(title = "ROC by different Max Tree Depths", subtitle = "(SMOTE: perc.over = 10000, perc.under = 200, k = 5)", 
       x = "Max Tree Depth", y = "ROC (Repeated Cross Validation)")
ggsave('/home/marius/VL_Unterlagen/DataAnaProject/Lyudmila/graphics/ROC_by_treeDepth_smote3.png', device = 'png', dpi = 300)

rm(smote_model_customized3)
gc()

############ compare four models:

results_smote_models <- resamples(list(smote1 = smote_model_customized1, smote2 = smote_model_customized2, smote3 = smote_model_customized3))
summary(results_smote_models)

bwplot(results_smote_models)
dotplot(results_smote_models)


selectIndices <- smote_model_customized1$pred$failure == 1
smote_model_customized2$pred

######################
### plot ROC curves:#
#####################

setwd("/home/marius/VL_Unterlagen/DataAnaProject/Lyudmila/data/camel/trained_models")
model_complete <- readRDS('trainmodelFit_complete.rds')
model_complete_predictions <- predict(model_complete, testSet, type = "prob")
rm(model_complete_predictions)
rm(model_complete)
smote_model_customized1 <- readRDS('smote_model_customized1.rds')
test_predictions_smote1 <- predict(smote_model_customized1, testSet, type = "prob")
rm(smote_model_customized1)
smote_model_customized2 <- readRDS('smote_model_customized2.rds')
test_predictions_smote2 <- predict(smote_model_customized2, testSet, type = "prob")
rm(smote_model_customized2)
smote_model_customized3 <- readRDS('smote_model_customized3.rds')
test_predictions_smote3 <- predict(smote_model_customized3, testSet, type = "prob")
rm(smote_model_customized3)

### model_complete:
complete.ROC <- roc(predictor=model_complete_predictions$failure,
                  response=testSet$Perform_2)
                  #levels=levels(testSet$Perform_2))
complete.ROC$auc
gCompl <- pROC::ggroc(smote1.ROC)
gCompl + labs(title = "ROC Curve (SMOTE cust. 1)", subtitle = "Imbalanced Dataset", 
          x = " Specifity", y = "Sensitivity")
ggsave('/home/marius/VL_Unterlagen/DataAnaProject/Lyudmila/graphics/ROC_imbalanced.png', device = 'png', dpi = 300)
gCompl$data$model <- "imb"
rocDF <- gCompl$data

#Area under the curve: 0.8731
plot(complete.ROC, main = "ROC Curve - Imbalance", col = "blue")

##################
### ROC smote1:##
################
smote1.ROC <- roc(predictor=test_predictions_smote1$failure,
               response=testSet$Perform_2)
plot(smote1.ROC)
               #levels=rev(levels(testSet$Perform_2)))
smote1.ROC$auc
g1 <- pROC::ggroc(smote1.ROC)
g1 + labs(title = "ROC Curve (SMOTE cust. 1)", subtitle = "(SMOTE: perc.over = 10000, perc.under = 200, k = 5)", 
         x = " Specifity", y = "Sensitivity")
ggsave('/home/marius/VL_Unterlagen/DataAnaProject/Lyudmila/graphics/ROC_smote_1.png', device = 'png', dpi = 300)
g1$data$model <- "smote1"
rocDF <- rbind(rocDF, g1$data)

##################
### ROC smote2:##
################
smote2.ROC <- roc(predictor=test_predictions_smote2$failure,
                  response=testSet$Perform_2)
                  #levels=rev(levels(testSet$Perform_2)))
smote2.ROC$auc
g2 <- pROC::ggroc(smote2.ROC)
g2 + labs(title = "ROC Curve (SMOTE cust. 2)", subtitle = "(SMOTE: perc.over = 10000, perc.under = 100, k = 5)", 
          x = " Specifity", y = "Sensitivity")
g2$data$model <- "smote2"
rocDF <- rbind(rocDF, g2$data)

#Area under the curve: 0.8731
plot(smote2.ROC, main="Smote 1")

###################
### ROC smote3: ##
#################
smote3.ROC <- roc(predictor=test_predictions_smote3$failure,
                  response=testSet$Perform_2,
                  levels=levels(testSet$Perform_2))
smote3.ROC$auc



g3 <- pROC::ggroc(smote3.ROC)
g3 + 
  labs(title = "ROC Curve (SMOTE cust. 3)", subtitle = "(SMOTE: perc.over = 10000, perc.under = 200, k = 5)", 
          x = " Specifity", y = "Sensitivity")
g3$data$model <- "smote3"
rocDF <- rbind(rocDF, g3$data)

#Area under the curve: 0.8731
plot(smote3.ROC, main="Smote 1")


rocDF %>%
  ggplot(aes(x = specificity, y = sensitivity, col = model)) +
  geom_line() +
  scale_x_reverse()



###########################
## Confusion Matrices: ###
#########################

setwd("/home/marius/VL_Unterlagen/DataAnaProject/Lyudmila/data/camel/trained_models")
model_complete <- readRDS('trainmodelFit_complete.rds')
model_complete_predictions <- predict(model_complete, testSet_pre)
rm(model_complete)
smote_model_customized1 <- readRDS('smote_model_customized1.rds')
test_predictions_smote1 <- predict(smote_model_customized1, testSet)
rm(smote_model_customized1)
smote_model_customized2 <- readRDS('smote_model_customized2.rds')
test_predictions_smote2 <- predict(smote_model_customized2, testSet)
rm(smote_model_customized2)
smote_model_customized3 <- readRDS('smote_model_customized3.rds')
test_predictions_smote3 <- predict(smote_model_customized3, testSet)
rm(smote_model_customized3)

cm1 <- confusionMatrix(model_complete_predictions, testSet_pre$Perform_2, positive = "failure")
cm2 <- confusionMatrix(test_predictions_smote1, testSet$Perform_2, positive = "failure")
cm3 <- confusionMatrix(test_predictions_smote2, testSet_pre$Perform_2, positive = "failure")
cm4 <- confusionMatrix(test_predictions_smote3, testSet_pre$Perform_2, positive = "failure")












plot(complete.ROC, col = "blue")
lines(smote1.ROC, col = "red")
lines(smote2.ROC, col = "green")
lines(smote3.ROC, col = "black")










