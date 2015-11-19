# =============
# LOAD PACKAGES
# =============
library(dplyr)
library(ggplot2)
library(caret)
library(randomForest)

# =============
# UTILITIES
# =============
# Loss-function to evaluate result
# See Case Study_BI Recruiting data challenge.pdf
loss <- function(y, yHat){
  w <- log(y + 1) + 1
  loss <- sum(w * (y - yHat)^2) / sum(w)
  return(loss)
}

# =============
# LOAD DATA
# =============
filepath <- dir(".", "data_recruiting_bi_data_challenge.csv", recursive = T)

cclass <- c("NULL", "character", "character", "integer", "factor", "numeric", 
            "numeric", "integer", "numeric", "numeric", "numeric")

hoteldata <- read.table(filepath, header = T, sep=",", colClasses = cclass, 
                        quote = "\"'", na.strings = c("NA", "na", "-1","\"\""))

hoteldata <- tbl_df(hoteldata)
citycount <- hoteldata %>% group_by(city_id) %>% summarise(citycount = n())
hoteldata <- hoteldata %>% 
  left_join(citycount, by="city_id") %>% 
  ungroup() %>% 
  mutate(nacount = is.na(avg_price_hotel) + is.na(rating) + 
           is.na(nmbr_partners_index) + is.na(avg_rel_saving) + 
           is.na(avg_rank) + is.na(distance_to_center))
#hoteldata$nacount <- as.factor(hoteldata$nacount)
submission <- filter(hoteldata, is.na(clicks))
filtered <- filter(hoteldata, !is.na(clicks))

# =============
# DATA CLEAN
# =============
set.seed(777)
part1 <- filter(filtered, clicks > 0)
part2 <- filtered %>% filter(clicks < 1, nacount < 3) %>% sample_frac(0.1)
part <- bind_rows(part1, part2)
# Imputation
part[is.na(part$stars), 4] <- "0"
part[is.na(part$distance_to_center), 5] <- max(part$distance_to_center, na.rm = T)
part[is.na(part$avg_price_hotel), 6] <- median(part$avg_price_hotel, na.rm = T)
part[is.na(part$rating), 7] <- min(part$rating, na.rm = T)
part[is.na(part$nmbr_partners_index), 8] <- min(part$nmbr_partners_index, na.rm = T)
part[is.na(part$avg_rel_saving), 9] <- max(part$avg_rel_saving, na.rm = T)
part[is.na(part$avg_rank), 10] <- max(part$avg_rank, na.rm = T)
#part <- na.roughfix(filtered)

part <- select(part, -hotel_id, -city_id)
highclick <- filter(part, clicks > 1000)
lowclick <- filter(part, clicks >=0, clicks <= 1000)
smallsample <- highclick
smallsample <- lowclick
smallsample <- sample_frac(smallsample, 0.1)

smallsample <- as.data.frame(smallsample)

inTrain <- createDataPartition(y = smallsample$clicks, p = 0.7, list = F)
training <- smallsample[inTrain, -1]
v_label_train <- smallsample[inTrain, 1]
testing <- smallsample[-inTrain, -1]
v_label_test <- smallsample[-inTrain, 1]
clicks.rf2 <- randomForest(x = training, 
                          y = v_label_train, 
                          xtest = testing, 
                          ytest = v_label_test, 
                          ntree = 500,
                          do.trace = 5)
