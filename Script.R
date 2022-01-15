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

###################### impute missings in rent with region averages #############################################################

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


###############################################################################################################################

# create RF

set.seed(234) # initiate random seed so outcomes are reproducible


####################### NA handling start

# find columns with missing values
na_test <- colSums(is.na(test))
na_train <- colSums(is.na(train))

na_test_perc <- na_test[na_test > 0] / nrow(test) * 100
na_train_perc <- na_train[na_train > 0] / nrow(train) * 100

# remove columns with too many missings and drop any observations for which meaneduc is missing
test <- test %>% select(-c(no_tablets, school_yrs_behind)) %>% drop_na(meaneduc)
train <- train %>% select(-c(no_tablets, school_yrs_behind)) %>% drop_na(meaneduc)

train_final <- train %>% select(-c(Id, hh_id, area, region)) # use when imputing rent missings

train_final <- train %>% select(-c(Id, hh_id)) %>% drop_na(rent) # use when not imputing rent missings and drop NAs from rent

train_final <- train %>% select(-c(dependency, ed_head_f, ed_head_m, Id, hh_id)) # use when not dropping NAs from rent and using the rfImpute() function for imputing missings

train_final <- droplevels(train_final)

train_final_imp <- rfImpute(target ~ ., data = train_final) # impute NAs in rent with rfImpute function from randomForest package

####################### NA handling end

# random forest with all vars
rf <- randomForest(formula = target ~ ., data = train_final, do.trace = TRUE)

varImpPlot(rf, main = "Variable Importance")

# random forest with 4 most important vars
rf_topfour <- randomForest(formula = target ~ rent + meaneduc_sqr + meaneduc + dependency_sqr, data = train_final, do.trace = TRUE)

# create confusion matrix and statistics

pred <- train %>% mutate(predictions = predict(rf, train))
confusionMatrix(pred$predictions, pred$target)

