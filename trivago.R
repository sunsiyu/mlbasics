library(dplyr)
library(ggplot2)
library(caret)
cclass <- c("NULL", "character", "character", "integer", "factor", "numeric", 
            "numeric", "integer", "numeric", "numeric", "numeric")

hoteldata <- read.table("BIdatachallenge/data_recruiting_bi_data_challenge.csv", 
                   header = T, sep=",", colClasses = cclass, quote = "\"'",
                   na.strings = c("NA", "na", "\"\""))

hoteldata <- tbl_df(hoteldata)
group_city_hoteldata <- group_by(hoteldata, city_id)
# add a new column representing the frequency of the city id
citycount_all <- summarise(group_city_hoteldata, citycount = n())
group_city_hoteldata <- left_join(group_city_hoteldata, citycount_all)
filtered <- filter(hoteldata, !is.na(clicks))
group_city_filtered <- group_by(filtered, city_id)

sum_filtered <- summarise(group_city_filtered, 
                                citycount = n(),
                                na.price = sum(is.na(avg_price_hotel)),
                                na.rating = sum(is.na(rating)),
                                na.index = sum(is.na(nmbr_partners_index)),
                                na.saving = sum(is.na(avg_rel_saving)),
                                na.rank = sum(is.na(avg_rank)))
group_city_filtered <- left_join(group_city_filtered, sum_filtered)

###############################################################################
# check NA situation
na_all <- summarise_each(hoteldata, funs(sum(is.na(.))))
na_filtered <- summarise_each(filtered, funs(sum(is.na(.))))
# NA count of all variables
df_nacount_all <- data.frame(varnames = names(na_all), 
                             na.count = as.integer(na_all), 
                             na.percent = as.integer(na_all)/nrow(hoteldata))
df_nacount_filtered <- data.frame(varnames = names(na_filtered), 
                                  na.count = as.integer(na_filtered), 
                                  na.percent = as.integer(na_filtered)/nrow(filtered))
# Fix missing values in avg_price_hotel
MEAN_PRICE_ALL <- mean(filtered$avg_price_hotel, na.rm=T)

sum_price <- summarise(group_city_filtered, 
                       mean.price = mean(avg_price_hotel, na.rm = T), 
                       median.price = median(avg_price_hotel, na.rm = T))
sum_price <- mutate(sum_price, diff = abs(mean.price-median.price))
x1 <- group_city_filtered %>% 
  select(c(hotel_id:clicks, citycount, avg_price_hotel, na.price)) %>% 
  mutate(na.price.per = na.price/citycount) %>% 
  left_join(sum_price) %>% 
  filter(is.na(avg_price_hotel)) %>% 
  mutate(avg_price_hotel = mean.price) %>% 
  mutate(avg_price_hotel = ifelse(is.na(avg_price_hotel), MEAN_PRICE_ALL, avg_price_hotel))
x2 <- filter(group_city_filtered, !is.na(avg_price_hotel))
x <- bind_rows(x2, x1)
x <- select(x, c(hotel_id, avg_price_hotel))
# Fix missing values in rating
MEAN_RATING_ALL <- mean(filtered$rating, na.rm=T)

sum_rating <- summarise(group_city_filtered, 
                       mean.rating = mean(rating, na.rm = T), 
                       median.rating = median(rating, na.rm = T))
sum_rating <- mutate(sum_rating, diff = abs(mean.rating-median.rating))
y1 <- group_city_filtered %>% 
  select(c(hotel_id:clicks, citycount, rating, na.rating)) %>% 
  mutate(na.rating.per = na.rating/citycount) %>% 
  left_join(sum_rating) %>% 
  filter(is.na(rating)) %>% 
  mutate(rating = mean.rating) %>% 
  mutate(rating = ifelse(is.na(rating), MEAN_RATING_ALL, rating))

y2 <- filter(group_city_filtered, !is.na(rating))
y <- bind_rows(y2, y1)
y <- select(y, c(hotel_id, rating))

# Fix missing values in nmbr_partners_index
MEAN_INDEX_ALL <- mean(filtered$nmbr_partners_index, na.rm=T)

sum_index <- summarise(group_city_filtered, 
                        mean.index = mean(nmbr_partners_index, na.rm = T), 
                        median.index = median(nmbr_partners_index, na.rm = T))
sum_index <- mutate(sum_index, diff = abs(mean.index-median.index))
z1 <- group_city_filtered %>% 
  select(c(hotel_id:clicks, citycount, nmbr_partners_index, na.index)) %>% 
  mutate(na.index.per = na.index/citycount) %>% 
  left_join(sum_index) %>% 
  filter(is.na(nmbr_partners_index)) %>% 
  mutate(nmbr_partners_index = mean.index) %>% 
  mutate(nmbr_partners_index = ifelse(is.na(nmbr_partners_index), MEAN_INDEX_ALL, nmbr_partners_index))
z2 <- filter(group_city_filtered, !is.na(nmbr_partners_index))
z <- bind_rows(z2, z1)
z <- select(z, c(hotel_id, nmbr_partners_index))

# Fix missing value in avg_rel_saving
MEAN_SAVE_ALL <- mean(filtered$avg_rel_saving, na.rm=T)

sum_save <- summarise(group_city_filtered, 
                       mean.save = mean(avg_rel_saving, na.rm = T), 
                       median.save = median(avg_rel_saving, na.rm = T))
sum_save <- mutate(sum_save, diff = abs(mean.save-median.save))
v1 <- group_city_filtered %>% 
  select(c(hotel_id:clicks, citycount, avg_rel_saving, na.saving)) %>% 
  mutate(na.save.per = na.saving/citycount) %>% 
  left_join(sum_save) %>% 
  filter(is.na(avg_rel_saving)) %>% 
  mutate(avg_rel_saving = mean.save) %>% 
  mutate(avg_rel_saving = ifelse(is.na(avg_rel_saving), MEAN_SAVE_ALL, avg_rel_saving))
v2 <- filter(group_city_filtered, !is.na(avg_rel_saving))
v <- bind_rows(v2, v1)
v <- select(v, c(hotel_id, avg_rel_saving))

# Fix missing value in avg_rank
MEAN_RANK_ALL <- mean(filtered$avg_rank, na.rm=T)

sum_rank <- summarise(group_city_filtered, 
                      mean.rank = mean(avg_rank, na.rm = T), 
                      median.rank = median(avg_rank, na.rm = T))
sum_rank <- mutate(sum_rank, diff = abs(mean.rank - median.rank))
m1 <- group_city_filtered %>% 
  select(c(hotel_id:clicks, citycount, avg_rank, na.rank)) %>% 
  mutate(na.rank.per = na.rank/citycount) %>% 
  left_join(sum_rank) %>% 
  filter(is.na(avg_rank)) %>% 
  mutate(avg_rank = mean.rank) %>% 
  mutate(avg_rank = ifelse(is.na(avg_rank), MEAN_RANK_ALL, avg_rank))
m2 <- filter(group_city_filtered, !is.na(avg_rank))
m <- bind_rows(m2, m1)
m <- select(m, c(hotel_id, avg_rank))
# Integration
select(x, avg_price_hotel)
processed <- x %>% left_join(y) %>% left_join(z) %>% left_join(v) %>% left_join(m)
rest <- select(filtered, c(hotel_id:distance_to_center, citycount))
processed <- left_join(rest, processed)

###############################################################################
processed_noid <- processed[]

inTrain <- createDataPartition(y = processed, )



















