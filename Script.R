# import necessary packages
library(tidyverse)
library(readxl)
library(randomForest)
library(rpart)
library(caret)
library(rpart.plot)

# set working directory relative to path of the script file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# import training and test data
test <- read_csv("data/test.csv")

train <- read_csv("data/train.csv", col_types = cols(Target = col_factor(levels = c("0", "1", "2", "3", "4"))))

codebook <- read_excel("data/codebook.xlsx", 
                       col_types = c("text", "skip", "text"))


# rename columns and remove unnecessary features
colnames(test)[colnames(test) == codebook$`Variable name`] <- codebook$`English Variable Name`
colnames(train)[colnames(train) == codebook$`Variable name`] <- codebook$`English Variable Name`

####################### NA handling start

# find columns with missing values
na_test <- colSums(is.na(test))
na_train <- colSums(is.na(train))

na_test_perc <- na_test[na_test > 0] / nrow(test) * 100
na_train_perc <- na_train[na_train > 0] / nrow(train) * 100

# remove columns with too many missings and drop any observations for which meaneduc is missing
test <- test %>% select(-c(no_tablets, school_yrs_behind)) %>% drop_na(meaneduc)
train <- train %>% select(-c(no_tablets, school_yrs_behind)) %>% drop_na(meaneduc)

# clean columns with character values
train <- train %>% mutate(dependency = sqrt(dependency_sqr), ed_head_m = sqrt(ed_head_m_sqr), ed_head_f = as.numeric(if_else(ed_head_f == "yes", "1",
                                                                                                                  if_else(ed_head_f == "no", "0", ed_head_f)))) 

test <- test %>% mutate(dependency = sqrt(dependency_sqr), ed_head_m = sqrt(ed_head_m_sqr), ed_head_f = as.numeric(if_else(ed_head_f == "yes", "1",
                                                                                                                             if_else(ed_head_f == "no", "0", ed_head_f)))) 


train_final <- train %>% select(-c(Id, hh_id))

train_final <- droplevels(train_final)

####################### NA handling end

train_final_imp <- rfImpute(target ~ ., data = train_final) # impute NAs in rent with rfImpute function from randomForest package

set.seed(234) # initiate random seed so outcomes are reproducible

# split dataset

ind <- sample(2, nrow(train_final_imp), replace = TRUE, prob = c(0.8, 0.2))
training <- train_final_imp[ind == 1,]
validation <- train_final_imp[ind == 2,]

training_mtry <- training %>% select(-target)
optimal_mtry <- tuneRF(training_mtry, training$target, improve = 0.05, ntreeTry = 200, doBest = TRUE)


# random forest with all vars
rf <- randomForest(formula = target ~ ., data = training, do.trace = TRUE, ntree = 500, mtry = optimal_mtry[["mtry"]])

varImpPlot(rf, main = "Variable Importance")

# create confusion matrix and statistics

val <- validation %>% mutate(predictions = predict(rf, validation))
confusionMatrix(val$predictions, val$target)

# CART model

cart <- rpart(target ~ ., data = training, method = "class")

rpart.plot(cart)

val_cart <- validation %>% mutate(predictions = predict(cart, validation, type = "class"))
confusionMatrix(val_cart$predictions, val_cart$target)


# predict test set

test_predict <- test %>% mutate(prediction = predict(rf, test))
table(test_predict$prediction)
