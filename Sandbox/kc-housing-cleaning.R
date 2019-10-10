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


#########################################################################################
# Examining our data
#########################################################################################

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

## removed yr_renovated and added dummy variable for renovated
kc_houses <- kc_houses %>% 
  mutate(renovated = ifelse(yr_renovated >0, 1, 0)) %>% 
  select(everything(), -yr_renovated)

########################################################################################################
##YAO - segmenting long and lat into NW, NE, SE, SW
## center of king county
## 47.5480 ( lat) greater than this north , 121.9836 (long) greater than this, west
########################################################################################################

kc_houses %>%
  mutate(lat_direction = ifelse(lat >=47.548,"N","S")) %>% 
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

kc_house_new <- kc_house_new %>% mutate(years_old = 2019 - yr_built)

#changed house prices to k
kc_house_new$price <- kc_house_new$price/1000

#Spread zipcode into columns and made them dummy variables
kc_house_zc <- kc_house_new %>% select(zipcode) %>% mutate(zipcode = factor(zipcode))
zp <- dummy(kc_house_zc)

## FINAL TABLE NAME _ WITHOUT ZIP CODES
kc_house_new 
## FINAL TABLE NAME _ WITH ZIP CODES
kc_house_zp <- cbind(kc_house_new, zp)
dim(kc_house_zp)

########################################################################################################
# ploting king's county map
########################################################################################################
counties <- map_data("county")
wa_county <- subset(counties, region == "washington")
king_county <- subset(wa_county, subregion == "king")

ggplot(king_county, aes(x = long, y = lat)) +
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray") +
  qplot(kc_house_new, aes(x = long, y= lat))
counties <- map_data("county")


colnames(kc_house_new)
kc_house_new %>% 
  count(SW)
kc_house_new %>% 
  count(NW)
kc_house_new %>% 
  count(SW)
kc_house_new %>% 
  count(SW)

########################################################################################################
# Eman- LASSO regression
########################################################################################################

## Lasso regression
train <- round(0.8 * nrow(kc_house_new))
test <- nrow(kc_house_new) - train

# spliting data into train and test
set.seed(98765)
train_index <- sample(nrow(kc_house_new), train) # assign 17290 random rows to the train set

colnames(kc_house_new)

y_data <- kc_house_new$price
x_data <- model.matrix( ~ -1 + bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + 
                          view + condition + grade + sqft_above + sqft_basement + renovated +
                          sqft_living15 + sqft_lot15 + SW + NW + NE + SE, kc_house_new)
nrow(x_data)
View(y_train)

# now split
kc_train <- x_data[train_index,]
kc_test <- x_data[-train_index,]
y_train <- y_data[train_index]
y_test <-  y_data[-train_index]

#This will fit 100 lasso regressions for different values of lambda (chosen automatically) 
est <- glmnet(kc_train, y_train, alpha = 1, nlambda = 100)

# Examine lambda 
est$lambda

## use models to create predictions for both train and test
y_train_hat <- predict(est, newx = kc_train) 
y_test_hat <- predict(est, newx = kc_test) 

# write code to create a vector that contains MSEs estimates for the train data 
mse_train <- colMeans((y_train - y_train_hat)^2)
mse_test <- colMeans((y_test - y_test_hat)^2) 

# c3reate a tibble of train MSEs and lambdas 
kc_mse_train <- tibble( 
  lambda = est$lambda, 
  mse = mse_train, 
  dataset = "Train")

kc_mse_test <- tibble( 
  lambda = est$lambda, 
  mse = mse_test, 
  dataset = "Test")

kc_mse <- rbind(kc_mse_train, kc_mse_test)
colnames(kc_mse)

## Figure out lowest mse and the respective lambda for the train and test 
kc_mse %>% 
  group_by(dataset) %>% 
  filter(mse == min(mse)) %>% View

lambda_min_mse_train <-kc_mse_train$lambda[which.min(kc_mse_train$mse)]
lambda_min_mse_test <- kc_mse_test$lambda[which.min(kc_mse_test$mse)]
min_mse_train <- kc_mse_train$mse[which.min(kc_mse_train$mse)]
min_mse_test <- kc_mse_test$mse[which.min(kc_mse_test$mse)]

## plot the mse for train and test and mark the min points
ggplot(kc_mse, aes(lambda, mse, col = dataset)) + 
  geom_line() + 
  geom_point(aes(y = min_mse_train 	, x = lambda_min_mse_train ), size = 2, color = "skyblue") +
  geom_point(aes(y = min_mse_test	, x = lambda_min_mse_test), size = 2, color = "red") + 
  ggtitle("MSE for each Model") +
  scale_x_reverse()

coef(est , s = lambda_min_mse_test)

########################################################################################################
# Eman - CROSS VALIDATION
########################################################################################################

##cross validation
y <- kc_house_new$price

#The command loads an input matrix x and a response vector y 
#We fit the model using the most basic call to glmnet.
fit = glmnet(x_data, y)
plot(fit)

#We can visualize the coefficients by executing the plot function:
cvfit = cv.glmnet(x_data, y, type.measure = "mse", nfolds = 20)

#We can obtain the actual coefficients at one or more λ’s within the range of the sequence:
coef(fit,s=0.1)

#cv.glmnet is the main function to do cross-validation here
cvfit <- cv.glmnet(y, x_data)
plot(cvfit)

#the value of λ that gives minimum mean cross-validated error. 
cvfit$lambda.min
coef(cvfit, s = "lambda.min")

predict(cvfit, newx = kc_test, s = "lambda.min")

#####################################################################################
# YAO - FORWARD SELECTION
######################################################################################
# now split
kc_train <- kc_house_new[train_index,]
kc_test <- kc_house_new[-train_index,]

kc_houses_p <- kc_house_new 
View(kc_house_new)
kc_houses_p$price <- kc_houses_p$price
View(kc_houses_p)

## loop 
xnames <- colnames(kc_train)
xnames <- xnames[!xnames %in% c("price","transaction_id","house_id", "zipcode", 
                                "year_sold", "month_sold", "day_sold", "yr_built")] ### remove y predictor 

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

#####################################################################################
# Ted - RANDOM FOREST
######################################################################################
kc_train <- kc_house_new[train_index,]
kc_test <- kc_house_new[-train_index,]

kc_houses_p <- kc_house_new 
kc_houses_p$price <- kc_houses_p$price

# here's one simple formula -- it's up to your to add more predictors as you see fit
t1 <- as.formula(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + 
                   view + condition + grade + sqft_above + sqft_basement + renovated +
                   sqft_living15 + sqft_lot15 + SW + NW + NE + SE, kc_house_new)

# the [, -1] means take all columns of the matrix except the first column,
# which is an intercept added by default
x1_train <- model.matrix(t1, kc_train)[, -1]
y_train <- kc_train$price

x1_test <- model.matrix(t1, kc_test)[, -1]
y_test <- kc_test$price

# We will fit a random forest
fit_rf <- randomForest(t1,
                       kc_train,
                       ntree=10,
                       do.trace=F)

#We can check which variables are most predictive using a variable importance plot
varImpPlot(fit_rf)

# Making predictions and computing training MSE
yhat_rf <- predict(fit_rf, kc_train)
mse_rf <- mean((yhat_rf - y_train) ^ 2)
print(mse_rf)

#####################################################################################
# Ted - BOOSTED TREES
######################################################################################

#We will fit a boosted forest.
fit_btree <- gbm(t1,
                 data = kc_train,
                 distribution = "gaussian",
                 n.trees = 100,
                 interaction.depth = 2,
                 shrinkage = 0.001)

#We can check which variables are most predictive as follows
relative.influence(fit_btree)

# Making predictions and computing training MSE
yhat_btree <- predict(fit_btree, kc_train, n.trees = 100)
mse_btree <- mean((yhat_btree - y_train) ^ 2)
print(mse_btree)
