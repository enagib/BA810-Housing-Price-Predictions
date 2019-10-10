install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

## Loading our table
kc_housing <- read_csv("kc_house_data.csv")

## we viewd the id and found that some id are not unique this is because some houses are
## sold twice so we group_by id, and date to see if they are redudant, and it turned out that
## every transaction is unique
kc_housing %>% group_by(id, date) %>% summarise(count = n()) %>% filter(count>1)

## Examining table
View(kc_housing)
colnames(kc_housing)
dim(kc_houses)
str(kc_houses)

## Creating our table
kc_houses <- kc_housing %>% 
  mutate(transaction_id = row_number()) %>% 
  rename("house_id" = id) %>% 
  select(transaction_id, everything()) %>% 
  separate(date, into = c("year_sold", "month_sold", "day_sold"), sep = "-") 

kc_houses$year_sold <- as.numeric(kc_houses$year_sold)
kc_houses$month_sold <- as.numeric(kc_houses$month_sold)
kc_houses$day_sold <- as.numeric(kc_houses$day_sold)

str(kc_houses)
## Filtering by variables - looking at range and unique variables
kc_houses %>%  filter(sqft_living == 290) %>% View
kc_houses <- kc_houses %>% filter(bedrooms != 33)
range(kc_houses$yr_renovated)
sort(unique(kc_houses$sqft_basement))
View(kc_houses)

## NO NA's
nrow(na.omit(kc_houses))
nrow(kc_houses) 
nrow(kc_housing)
summary(kc_housing)

## save table as csv
write_csv(kc_houses, "kc_houses_table.csv")

## most expensive house
kc_houses %>%  filter(price == max(price)) %>%View()

ggplot(kc_housing, aes(bedrooms, price)) + 
  geom_point(positon = "jitter")+
  scale_y_continuous(breaks = seq(0,max(kc_housing$price),1000000))

options(scipen = 999)
ggplot(kc_housing, aes(sqft_living, price, col = view)) + 
  geom_point(alpha = 0.3) +
  scale_y_continuous(breaks = seq(0,max(kc_housing$price),500000))

View(kc_houses)

kc_houses <- kc_houses %>% 
  mutate(renovated = ifelse(yr_renovated >0, 1, 0)) %>% 
  select(everything(), -yr_renovated)

kc_houses %>% select(long,lat)


#### work on long and lat 
### center of king county
### 47.5480 ( lat) greater than this north , 121.9836 (long) greater than this, west 

kc_houses %>% mutate(lat_direction = ifelse(lat >=47.548,"N","S")) %>% 
  mutate(long_direction = ifelse(abs(long) >=121.9836,"W","E")) %>% 
  mutate(direction=paste(lat_direction,long_direction)) %>% 
  select(-c("long","lat","long_direction","lat_direction")) -> kc_houses

View(kc_houses)
unique(kc_houses$direction)

kc_houses %>% mutate(SW =ifelse(direction =="S W",1,0)) %>%
  mutate(NW =ifelse(direction =="N W",1,0)) %>%
  mutate(NE =ifelse(direction =="N E",1,0)) %>%
  mutate(SE =ifelse(direction =="S E",1,0)) %>% 
  select(-c(direction)) -> kc_house_new
View(kc_house_new)

kc_house_new %>% filter( (NW+NE+SE+SW)>1)


#### Map density 
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
counties <- map_data("county")
wa_county <- subset(counties, region == "washington")
king_county <- subset(wa_county, subregion == "king")
ggplot(king_county, aes(x = long, y = lat)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "gray") 

sum(kc_house_new$SW)
sum(kc_house_new$NW)
sum(kc_house_new$NE)
sum(kc_house_new$SE)



#### data separation 
train <- round(0.8 * nrow(kc_house_new))
test <- nrow(kc_house_new) - train

# spliting data into train and test
set.seed(98765)
train_index <- sample(nrow(kc_house_new), train) # assign 17290 random rows to the train set

# now split
kc_train <- kc_house_new[train_index,]
kc_test <- kc_house_new[-train_index,]


#### step wise regression 
xnames <- colnames(kc_house_new)
xnames <- xnames[!xnames %in% c("price","transaction_id","house_id")] ### remove y predictor 
xnames
kc_fw <- lm(price ~ 1, data = kc_train)


### predict based on simplest model 
yhat_train <- predict(kc_fw, kc_train)
mse_train <- mean((kc_train$price - yhat_train) ^ 2)
yhat_test <- predict(kc_fw, kc_test)
mse_test <- mean((kc_test$price - yhat_test) ^ 2)

## store outcome
library(tibble)
log_fw <- tibble(
  xname = "intercept",
  model = deparse(kc_fw$call), 
  mse_train = mse_train, 
  mse_test = mse_test
)



View(kc_house_new)


#### loop 1st 
best_mse_train <- NA 
best_mse_test <- NA 
best_fit_fw <- NA 
best_xname <- NA

for (xname in xnames) {
  kc_fw_tmp <- update(kc_fw, as.formula(paste0(" ~ ", xname)))
  yhat_train_tmp <- predict(kc_fw_tmp, kc_train)
  mse_train_tmp <- mean((kc_train$price - yhat_train_tmp) ^ 2)
  
  yhat_test_tmp <- predict(kc_fw_tmp, kc_test)
  mse_test_tmp <- mean((kc_test$price - yhat_test_tmp) ^ 2)
  
  if (is.na(best_mse_test) | mse_test_tmp < best_mse_test) {
    best_xname <- xname
    best_fit_fw <- kc_fw_tmp
    best_mse_train <- mse_train_tmp
    best_mse_test <- mse_test_tmp
  } 
}

print(best_xname)

log_fw <-
  log_fw %>% add_row(
    xname = best_xname,
    model = paste0(deparse(best_fit_fw$call), collapse = ""), mse_train = best_mse_train,
    mse_test = best_mse_test
  )

log_fw



## loop 
library(tidyverse)
xnames <- colnames(kc_house_new)
xnames <- xnames[!xnames %in% c("price","transaction_id","house_id")] ### remove y predictor 
xnames
kc_fw <- lm(price ~ 1, data = kc_train)

yhat_train <- predict(kc_fw, kc_train)
mse_train <- mean((kc_train$price - yhat_train) ^ 2)
yhat_test <- predict(kc_fw, kc_test)
mse_test <- mean((kc_test$price - yhat_test) ^ 2)
xname <- "intercept"

log_fw <- tibble(
  xname = xname,
  model = paste0(deparse(kc_fw$call), collapse = ""), 
  mse_train = mse_train,
  mse_test = mse_test
)



while (length(xnames) > 0) {
  best_mse_train <- NA 
  best_mse_test <- NA 
  best_fit_fw <- NA 
  best_xname <- NA
  
  
  
  for (xname in xnames) {
    kc_fw_tmp <- update(kc_fw, as.formula(paste0(" ~ ", xname)))
    yhat_train_tmp <- predict(kc_fw_tmp, kc_train)
    mse_train_tmp <- mean((kc_train$price - yhat_train_tmp) ^ 2)
    
    yhat_test_tmp <- predict(kc_fw_tmp, kc_test)
    mse_test_tmp <- mean((kc_test$price - yhat_test_tmp) ^ 2)
    
    if (is.na(best_mse_test) | mse_test_tmp < best_mse_test) {
      best_xname <- xname
      best_fit_fw <- kc_fw_tmp
      best_mse_train <- mse_train_tmp
      best_mse_test <- mse_test_tmp
    } 
  }
  
  
  log_fw <- log_fw %>% add_row(
    xname = best_xname,
    model = paste0(deparse(best_fit_fw$call), collapse = ""), 
    mse_train = best_mse_train,
    mse_test = best_mse_test
  )
  
  
  fit_fw <- best_fit_fw
  xnames <- xnames[xnames!=best_xname] 
}

### ggplot 
library(ggplot2)
ggplot(log_fw, aes(seq_along(xname), mse_test)) +
  geom_point() +
  geom_line() +
  geom_point(aes(y=mse_train), color="blue") +
  geom_line(aes(y=mse_train), color="blue") +
  scale_x_continuous("Variables", labels = log_fw$xname, breaks = seq_along(log_fw$xname)) +
  scale_y_continuous("MSE test") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





