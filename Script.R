# import necessary packages
library(tidyverse)

# set working directory relative to path of the script file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# import training and test data
test <- read_csv("data/test.csv")
train <- read_csv("data/train.csv")

# remove redundant columns
test <- test %>% select(-tamhog)
train <- train %>% select(-tamhog)
