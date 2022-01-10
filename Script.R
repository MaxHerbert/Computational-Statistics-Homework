# import necessary packages
library(tidyverse)
library(readxl)


# set working directory relative to path of the script file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# import training and test data
test <- read_csv("data/test.csv")
train <- read_csv("data/train.csv")
codebook <- read_excel("data/codebook.xlsx", 
                       col_types = c("text", "skip", "text"))


# rename columns and remove unnecessary features
colnames(test)[colnames(test) == codebook$`Variable name`] <- codebook$`English Variable Name`
colnames(train)[colnames(train) == codebook$`Variable name`] <- codebook$`English Variable Name`

test <- test %>% select(-c(tamhog, school_yrs_sqr, age_sqr, Age_sqr, hh_total_sqr, ed_head_m_sqr, hh_children_sqr, overcrowding_sqr, dependency_sqr, meaneduc_sqr))
train <- train %>% select(-c(tamhog, school_yrs_sqr, age_sqr, Age_sqr, hh_total_sqr, ed_head_m_sqr, hh_children_sqr, overcrowding_sqr, dependency_sqr, meaneduc_sqr))

# find columns with missing values
na_test <- colSums(is.na(test))
na_train <- colSums(is.na(train))

na_test <- na_test[na_test > 0]
na_train <- na_train[na_train > 0]