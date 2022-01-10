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

na_test_perc <- na_test[na_test > 0] / nrow(test) * 100
na_train_perc <- na_train[na_train > 0] / nrow(train) * 100

# remove columns with too many missings and drop any observations for which meaneduc is missing
test <- test %>% select(-c(no_tablets, school_yrs_behind)) %>% drop_na(meaneduc)
train <- train %>% select(-c(no_tablets, school_yrs_behind)) %>% drop_na(meaneduc)

# create new columns including regions and areas
test <- test %>% mutate(region = if_else(region1 == 1, "Central", 
                                         if_else(region2 == 1, "Chorotega",
                                                 if_else(region3 == 1, "Pacifico central",
                                                         if_else(region4 == 1, "Brunca",
                                                                 if_else(region5 == 1, "Huetar Atlantica",
                                                                         if_else(region6 == 1, "Huetar Norte", "")))))))

train <- train %>% mutate(region = if_else(region1 == 1, "Central", 
                                         if_else(region2 == 1, "Chorotega",
                                                 if_else(region3 == 1, "Pacifico central",
                                                         if_else(region4 == 1, "Brunca",
                                                                 if_else(region5 == 1, "Huetar Atlantica",
                                                                         if_else(region6 == 1, "Huetar Norte", "")))))))

test <- test %>% mutate(area = if_else(area1 == 1, "urban",
                                       if_else(area2 == 1, "rural", "")))

train <- train %>% mutate(area = if_else(area1 == 1, "urban",
                                       if_else(area2 == 1, "rural", "")))

# bind test and training data into one dataframe
df_master <- train %>% select(-target) 
df_master <- rbind(df_master, test)

# calculate average rent payment based on region and area
avg_rent <- df_master %>% group_by(region, area) %>%
  summarize(mean_rent = round(mean(rent, na.rm = TRUE)))
