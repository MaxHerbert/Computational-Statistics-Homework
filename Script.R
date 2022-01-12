# import necessary packages
library(tidyverse)
library(readxl)
library(randomForest)

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

# replace NA's in test and training dataset with avg rent calculated above

test <- test %>% mutate(rent = if_else(region == "Central" & area == "urban" & is.na(rent), avg_rent$mean_rent[which(avg_rent$region == "Central" & avg_rent$area == "urban")],
                                            if_else(region == "Central" & area == "rural" & is.na(rent), avg_rent$mean_rent[which(avg_rent$region == "Central" & avg_rent$area == "rural")],
                                                    if_else(region == "Chorotega" & area == "urban" & is.na(rent), avg_rent$mean_rent[which(avg_rent$region == "Chorotega" & avg_rent$area == "urban")],
                                                            if_else(region == "Chorotega" & area == "rural" & is.na(rent), avg_rent$mean_rent[which(avg_rent$region == "Chorotega" & avg_rent$area == "rural")],
                                                                    if_else(region == "Pacifico central" & area == "urban" & is.na(rent), avg_rent$mean_rent[which(avg_rent$region == "Pacifico central" & avg_rent$area == "urban")],
                                                                            if_else(region == "Pacifico central" & area == "rural" & is.na(rent), avg_rent$mean_rent[which(avg_rent$region == "Pacifico central" & avg_rent$area == "rural")],
                                                                                    if_else(region == "Brunca" & area == "urban" & is.na(rent), avg_rent$mean_rent[which(avg_rent$region == "Brunca" & avg_rent$area == "urban")],
                                                                                            if_else(region == "Brunca" & area == "rural" & is.na(rent), avg_rent$mean_rent[which(avg_rent$region == "Brunca" & avg_rent$area == "rural")],
                                                                                                    if_else(region == "Huetar Atlantica" & area == "urban" & is.na(rent), avg_rent$mean_rent[which(avg_rent$region == "Huetar Atlantica" & avg_rent$area == "urban")],
                                                                                                            if_else(region == "Huetar Atlantica" & area == "rural" & is.na(rent), avg_rent$mean_rent[which(avg_rent$region == "Huetar Atlantica" & avg_rent$area == "rural")],
                                                                                                                    if_else(region == "Huetar Norte" & area == "urban" & is.na(rent), avg_rent$mean_rent[which(avg_rent$region == "Huetar Norte" & avg_rent$area == "urban")],
                                                                                                                            if_else(region == "Huetar Norte" & area == "rural" & is.na(rent), avg_rent$mean_rent[which(avg_rent$region == "Huetar Norte" & avg_rent$area == "rural")], rent)))))))))))))

train <- train %>% mutate(rent = if_else(region == "Central" & area == "urban" & is.na(rent), avg_rent$mean_rent[which(avg_rent$region == "Central" & avg_rent$area == "urban")],
                                       if_else(region == "Central" & area == "rural" & is.na(rent), avg_rent$mean_rent[which(avg_rent$region == "Central" & avg_rent$area == "rural")],
                                               if_else(region == "Chorotega" & area == "urban" & is.na(rent), avg_rent$mean_rent[which(avg_rent$region == "Chorotega" & avg_rent$area == "urban")],
                                                       if_else(region == "Chorotega" & area == "rural" & is.na(rent), avg_rent$mean_rent[which(avg_rent$region == "Chorotega" & avg_rent$area == "rural")],
                                                               if_else(region == "Pacifico central" & area == "urban" & is.na(rent), avg_rent$mean_rent[which(avg_rent$region == "Pacifico central" & avg_rent$area == "urban")],
                                                                       if_else(region == "Pacifico central" & area == "rural" & is.na(rent), avg_rent$mean_rent[which(avg_rent$region == "Pacifico central" & avg_rent$area == "rural")],
                                                                               if_else(region == "Brunca" & area == "urban" & is.na(rent), avg_rent$mean_rent[which(avg_rent$region == "Brunca" & avg_rent$area == "urban")],
                                                                                       if_else(region == "Brunca" & area == "rural" & is.na(rent), avg_rent$mean_rent[which(avg_rent$region == "Brunca" & avg_rent$area == "rural")],
                                                                                               if_else(region == "Huetar Atlantica" & area == "urban" & is.na(rent), avg_rent$mean_rent[which(avg_rent$region == "Huetar Atlantica" & avg_rent$area == "urban")],
                                                                                                       if_else(region == "Huetar Atlantica" & area == "rural" & is.na(rent), avg_rent$mean_rent[which(avg_rent$region == "Huetar Atlantica" & avg_rent$area == "rural")],
                                                                                                               if_else(region == "Huetar Norte" & area == "urban" & is.na(rent), avg_rent$mean_rent[which(avg_rent$region == "Huetar Norte" & avg_rent$area == "urban")],
                                                                                                                       if_else(region == "Huetar Norte" & area == "rural" & is.na(rent), avg_rent$mean_rent[which(avg_rent$region == "Huetar Norte" & avg_rent$area == "rural")], rent)))))))))))))


# create RF

train_final <- train %>% select(-c(Id, hh_id, area, region))

rf <- randomForest(x = train_final, y = train$target, do.trace = TRUE, ntree = 100) # error message
