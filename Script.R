# import necessary packages
library(tidyverse)
library(readxl)
library(randomForest)
library(caret)

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

train_final <- train %>% select(-c(Id, hh_id)) %>% drop_na(rent) # use when not imputing rent missings and drop NAs from rent

train_final <- droplevels(train_final)

####################### NA handling end

set.seed(234) # initiate random seed so outcomes are reproducible

# split dataset

ind <- sample(2, nrow(train_final), replace = TRUE, prob = c(0.8, 0.2))
training <- train_final[ind == 1,]
validation <- train_final[ind == 2,]

optimal_mtry <- tuneRF(training[,-139], training$target, improve = 0.05, ntreeTry = 200, doBest = TRUE)


# random forest with all vars
rf <- randomForest(formula = target ~ ., data = training, do.trace = TRUE, ntree = 500, mtry = optimal_mtry[["mtry"]])

varImpPlot(rf, main = "Variable Importance")

# create confusion matrix and statistics

val <- validation %>% mutate(predictions = predict(rf, validation))
confusionMatrix(val$predictions, val$target)

