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


# remove redundant columns
test <- test %>% select(-c(tamhog, Id))
train <- train %>% select(-c(tamhog, Id))

colnames(test)[which(names(test) == codebook$`Variable name`)] <- codebook$`English Variable Name`