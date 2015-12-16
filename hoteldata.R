# ===================
# LOAD PACKAGES
# ===================
library(dplyr, quietly = T)
library(ggplot2, quietly = T)
library(caret, quietly = T)
library(randomForest, quietly = T)

# ===================
# UTILITIES
# ===================
# Loss-function to evaluate result
# See Case Study_BI Recruiting data challenge.pdf
loss <- function(y, yHat){
  w <- log(y + 1) + 1
  loss <- sum(w * (y - yHat)^2) / sum(w)
  return(loss)
}

# ===================
# LOAD DATA
# ===================
filepath <- dir(".", "data_recruiting_bi_data_challenge.csv", recursive = T)

cclass <- c("NULL", "character", "character", "integer", "factor", "numeric", 
            "numeric", "integer", "numeric", "numeric", "numeric")

hoteldata <- read.table(filepath, header = T, sep=",", colClasses = cclass, 
                        quote = "\"'", na.strings = c("NA", "na", "-1","\"\""))

hoteldata <- tbl_df(hoteldata)

# ===================
# FEATURE ENGINEERING
# ===================

# Feature transformation 1. Stars -> categorical
hoteldata[is.na(hoteldata$stars), "stars"] <- "0"
hoteldata$stars <- as.factor(hoteldata$stars)

# Feature transformation 2. large-span distance, numerical -> categorical
fac_dist <- cut(hoteldata$distance_to_center, 
                breaks = c(0, 1e3, 5e3, 1e4, 5e4, 2e7))
hoteldata <- mutate(hoteldata, fac_dist = fac_dist)

# Feature transformation 3. Avg_rank (of large importance)
hoteldata <- mutate(hoteldata, ranksquare = avg_rank*avg_rank)

# Feature candidate 1. total count of hotels per city (city popularity)
citycount <- hoteldata %>% group_by(city_id) %>% summarise(citycount = n())
citycount <- mutate(citycount, 
                    fac_citycount = cut(citycount$citycount, c(0,1,10,100,10e3)))
hoteldata <- left_join(hoteldata, citycount, by="city_id")

# Feature candidate 2. Total number of missing values (missing info)
hoteldata <- mutate(hoteldata, nacount = is.na(avg_price_hotel) + is.na(rating) + 
                       is.na(nmbr_partners_index) + is.na(avg_rel_saving) + 
                       is.na(avg_rank) + is.na(fac_dist),
                    fac_nacount = cut(nacount, c(0, 1, 2, 3, 7), right = F))

# ===================
# DATA CLEAN
# ===================
# Remove outliers (<0.001% percentile)
hoteldata <- filter(hoteldata, clicks < 10000 | is.na(clicks))

# Imputation
impute <- function(df, hoteldata) {
  df[is.na(df$avg_price_hotel), "avg_price_hotel"] <- median(hoteldata$avg_price_hotel, na.rm = T)
  df[is.na(df$rating), "rating"] <- min(hoteldata$rating, na.rm = T)
  df[is.na(df$nmbr_partners_index), "nmbr_partners_index"] <- min(hoteldata$nmbr_partners_index, na.rm = T)
  df[is.na(df$avg_rel_saving), "avg_rel_saving"] <- max(hoteldata$avg_rel_saving, na.rm = T)
  df[is.na(df$ranksquare), "ranksquare"] <- mean(hoteldata$ranksquare, na.rm = T)
  df[is.na(df$fac_dist), "fac_dist"] <- names(which.max(table(hoteldata$fac_dist)))
  return(df)
}


# ===================
# DATA SAMPLE&SPLIT
# ===================
set.seed(1120)
submission <- filter(hoteldata, is.na(clicks))
submission <- impute(submission, hoteldata)
filtered <- filter(hoteldata, clicks > 0, nacount < 3)
part1 <- hoteldata %>% filter(clicks < 1) %>% sample_n(5000)
part2 <- filtered %>% filter(clicks <= 10) %>% sample_n(6000)
part3 <- filtered %>% filter(clicks > 10, clicks <= 100) %>% sample_n(7000)
part4 <- filtered %>% filter(clicks > 100, clicks <= 500) %>% sample_n(8000)
part5 <- filter(filtered, clicks >500, clicks <=1000) %>% sample_n(9000, replace = T)
part6 <- filter(filtered, clicks >1000) %>% sample_n(9000, replace=T)
part <- bind_rows(part1, part2, part3, part4, part5, part6)
part <- impute(part, hoteldata)

# Remove non-features
part <- select(part, -hotel_id, -city_id, -distance_to_center, -citycount, -avg_rank, -nacount)
submission_hotelid <- submission$hotel_id
submission <- select(submission, -clicks, -hotel_id, -city_id, -distance_to_center, -citycount, -avg_rank, -nacount)
part <- as.data.frame(part)
submission <- as.data.frame(submission)
# Splite training and testing sets
inTrain <- createDataPartition(y = part$clicks, p = 0.9, list = F)
training <- part[inTrain, -1]
v_label_train <- part[inTrain, 1]
testing <- part[-inTrain, -1]
v_label_test <- part[-inTrain, 1]

# ===================
# CROSS VALIDATION
# ===================


# ===================
# TRAINING
# ===================
clicks.rf <- randomForest(x = training, 
                          y = v_label_train, 
                          xtest = testing, 
                          ytest = v_label_test, 
                          ntree = 200,
                          mtry = 5,
                          do.trace = 5,
                          keep.forest=TRUE)

# ===================
# PREDICTION
# ===================
prediction <- predict(clicks.rf, newdata = submission)
result <- data.frame(hotel_id = submission_hotelid, clicks = prediction)
write.csv(result, "predict.csv", row.names = F)


