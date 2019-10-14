library(tidyverse)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(glmnet)
library(magrittr) 
library(ggthemes)
library(dummy)
library(randomForest)
library(gbm)
library(readxl)

########################################################################################################
# Preparing our dataset table
########################################################################################################

kc_housing <- read_csv("kc_house_data.csv")

kc_houses <- kc_housing %>% 
  mutate(transaction_id = row_number()) %>% 
  rename("house_id" = id) %>% 
  select(transaction_id, everything()) %>% 
  separate(date, into = c("year_sold", "month_sold", "day_sold"), sep = "-") 

kc_houses$year_sold <- as.numeric(kc_houses$year_sold)
kc_houses$month_sold <- as.numeric(kc_houses$month_sold)
kc_houses$day_sold <- as.numeric(kc_houses$day_sold)

kc_houses <- kc_houses %>% filter(bedrooms != 33)

kc_houses <- kc_houses %>% 
  mutate(renovated = ifelse(yr_renovated >0, 1, 0)) %>% 
  select(everything(), -yr_renovated)

kc_houses %>%
  mutate(lat_direction = ifelse(lat >=47.548,"N","S")) %>% 
  mutate(long_direction = ifelse(abs(long) >=121.9836,"W","E")) %>% 
  mutate(direction=paste(lat_direction,long_direction)) %>% 
  select(-c("long","lat","long_direction","lat_direction")) -> kc_houses

kc_houses %>% mutate(SW =ifelse(direction =="S W",1,0)) %>%
  mutate(NW =ifelse(direction =="N W",1,0)) %>%
  mutate(NE =ifelse(direction =="N E",1,0)) %>%
  mutate(SE =ifelse(direction =="S E",1,0)) %>% 
  select(-c(direction)) -> kc_house_new

kc_house_new <- kc_house_new %>% mutate(years_old = 2019 - yr_built)

kc_house_new$price <- kc_house_new$price/1000

read_excel("Sandbox/zipcode.xlsx") -> ddzipcode
left_join(kc_house_new, ddzipcode2, by = c("zipcode" = "Zipcode")) -> kc_house_new2
kc_house_new2 %>% mutate(city = factor(City)) %>% select(-City) -> kc_house_new3
kc_house_new3 %>% select(city) -> city
dummy::dummy(city) -> city2
cbind(kc_house_new3, city2) -> kc_house_new_4

kc_house_new_4 <- kc_house_new_4 %>% filter(bedrooms != 0)

# Table with directions 
kc_direction <- kc_house_new_4 %>% 
  select(-c(transaction_id,house_id, zipcode, 
            year_sold, month_sold, day_sold, yr_built, city, city_Auburn, city_Bellevue,     
            city_Black.Diamond, city_Bothell, city_Carnation, 
            city_Duvall, city_Enumclaw, city_Fall.City,
            city_Federal.Way, city_Issaquah, city_Kenmore,    
            city_Kent, city_Kirkland, city_Maple.Valley,
            city_Medina, city_Mercer.Island, city_North.Bend,
            city_Redmond, city_Renton, city_Sammamish,   
            city_Seattle, city_Snoqualmie, city_Vashon,    
            city_Woodinville))

#Table with citites
kc_cities <- kc_house_new_4 %>% 
  select(-c(transaction_id,house_id, zipcode, 
            year_sold, month_sold, day_sold, yr_built, SW, NW, NE, SE))

########################################################################################################
# Eman - splitting our dataset into test and train
########################################################################################################

colnames(kc_direction)
colnames(kc_cities)

train <- round(0.8 * nrow(kc_direction))
test <- nrow(kc_direction) - train


# spliting data into train and test
set.seed(98765)
train_index <- sample(nrow(kc_direction), train) # assign 17290 random rows to the train set
kc_train <- kc_direction[train_index,]
nrow(kc_train)
kc_test <- kc_direction[-train_index,]
nrow(kc_test)

f <- as.formula(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + 
                  view + condition + grade + sqft_above + sqft_basement + renovated +
                  sqft_living15 + sqft_lot15 + SW + NW + NE + SE, kc_train)

########################################################################################################
# Eman - Linear Regression
########################################################################################################

x_train <- model.matrix(f, kc_train) [ , -1] #Intercept added by default
x_test <- model.matrix(f, kc_test) [ , -1]

fit_lm <- lm(f, kc_train)
fit_lm

# compute train and test MSEs for the above model:
yhat_train_lm <- predict(fit_lm)
mse_train_lm <- mean((kc_train$price - yhat_train_lm)^2)

yhat_test_lm <- predict(fit_lm, kc_test)
mse_test_lm <- mean((kc_test$price - yhat_test_lm)^2)

mse_train_lm
mse_test_lm

#inspect the coefficients of the linear regression model:
coef(fit_lm)

########################################################################################################
# Eman - Lasso
########################################################################################################

x_train <- model.matrix(f, kc_train) [ , -1] #Intercept added by default
x_test <- model.matrix(f, kc_test) [ , -1]

# Fit the lasso model
fit_lasso <- cv.glmnet(x_train, kc_train$price, alpha = 1, nfolds = 10)

# compute train/test MSEs
yhat_train_lasso <- predict(fit_lasso, x_train, s = fit_lasso$lambda.min)
mse_train_lasso <- mean((kc_train$y - yhat_train_lasso)^2)
yhat_test_lasso <- predict(fit_lasso, x_test, s = fit_lasso$lambda.min)
mse_test_lasso <- mean((kc_test$y - yhat_test_lasso)^2)
mse_train_lasso
mse_test_lasso

coef(fit_lasso)

########################################################################################################
# Eman - Ridge
########################################################################################################

x_train <- model.matrix(f, kc_train) [ , -1] #Intercept added by default
x_test <- model.matrix(f, kc_test) [ , -1]

# Fit the lasso model
fit_ridge <- cv.glmnet(x_train, kc_train$price, alpha = 0, nfolds = 10)

# compute train/test MSEs
yhat_train_ridge <- predict(fit_ridge, x_train, s = fit_ridge$lambda.min)
mse_train_ridge <- mean((kc_train$price - yhat_train_lasso)^2)

yhat_test_ridge <- predict(fit_ridge, x_test, s = fit_ridge$lambda.min)
mse_test_ridge <- mean((kc_test$price - yhat_test_lasso)^2)

mse_train_ridge
mse_test_ridge

coef(fit_ridge)

########################################################################################################
# Eman - Elastic Net
########################################################################################################

x_train <- model.matrix(f, kc_train) [ , -1] #Intercept added by default
x_test <- model.matrix(f, kc_test) [ , -1]

# Fit the lasso model
fit_en<- cv.glmnet(x_train, kc_train$price, alpha = 0.5, nfolds = 10)

# compute train/test MSEs
yhat_train_en<- predict(fit_en, x_train, s = fit_en$lambda.min)
mse_train_en <- mean((kc_train$price - yhat_train_lasso)^2)

yhat_test_en <- predict(fit_en, x_test, s = fit_en$lambda.min)
mse_test_en <- mean((kc_test$price - yhat_test_lasso)^2)

mse_train_en
mse_test_en

coef(fit_en)

#####################################################################################
# YAO - FORWARD SELECTION
######################################################################################

## loop 
xnames <- colnames(kc_train)
xnames <- xnames[!xnames %in% c("price","transaction_id","house_id", "zipcode", 
                                "year_sold", "month_sold", "day_sold", "yr_built", "city")] ### remove y predictor 

colnames(kc_cities)
xnames

fit_fw <- lm(price ~ 1, data = kc_train)

yhat_train <- predict(fit_fw, kc_train)
mse_train <- mean((kc_train$price - yhat_train) ^ 2)

yhat_test <- predict(fit_fw, kc_test)
mse_test <- mean((kc_test$price - yhat_test) ^ 2)

xname <- "intercept"

log_fw <- tibble(
  xname = xname,
  model = paste0(deparse(fit_fw$call), collapse = ""),
  mse_train = mse_train,
  mse_test = mse_test)

while (length(xnames) > 0) {
  best_mse_train <- NA
  best_mse_test <- NA
  best_fit_fw <- NA
  best_xname <- NA
  
  for (xname in xnames) {
    fit_fw_tmp <- update(fit_fw, as.formula(paste0(". ~ . + ", xname)))
    yhat_train_tmp <- predict(fit_fw_tmp, kc_train)
    mse_train_tmp <- mean((kc_train$price - yhat_train_tmp) ^ 2)
    yhat_test_tmp <- predict(fit_fw_tmp, kc_test)
    mse_test_tmp <- mean((kc_test$price - yhat_test_tmp) ^ 2)
    
    if (is.na(best_mse_test) | mse_test_tmp < best_mse_test) {
      best_xname <- xname
      best_fit_fw <- fit_fw_tmp
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
ggplot(log_fw, aes(seq_along(xname), mse_test)) +
  geom_point() +
  geom_line() +
  geom_point(aes(y=mse_train), color="blue") +
  geom_line(aes(y=mse_train), color="blue") +
  scale_x_continuous("Variables", labels = log_fw$xname, breaks = seq_along(log_fw$xname)) +
  scale_y_continuous("MSE test") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

best_mse_train
best_mse_test

sqrt(best_mse_test)

