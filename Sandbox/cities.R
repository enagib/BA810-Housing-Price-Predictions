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
dim(kc_direction)
colnames(kc_cities)
dim(kc_cities)

train <- round(0.8 * nrow(kc_cities))
test <- nrow(kc_cities) - train


# spliting data into train and test
set.seed(98765)
train_index <- sample(nrow(kc_cities), train) # assign 17290 random rows to the train set
kc_train <- kc_cities[train_index,]
nrow(kc_train)
kc_test <- kc_cities[-train_index,]
nrow(kc_test)

f <- as.formula(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + 
                  view + condition + grade + sqft_above + sqft_basement + renovated +
                  sqft_living15 + sqft_lot15 + SW + NW + NE + SE, kc_train)


f <- as.formula(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + 
                  view + condition + grade + sqft_above + sqft_basement + renovated +
                  sqft_living15 + sqft_lot15 + city_Auburn + city_Bellevue + city_Black.Diamond +
                  city_Bothell + city_Carnation + city_Duvall + city_Enumclaw + city_Fall.City +
                  city_Federal.Way +city_Issaquah+ city_Kenmore + city_Kent + city_Kirkland + city_Maple.Valley +
                  city_Medina + city_Mercer.Island + city_North.Bend + city_Redmond + city_Renton + city_Sammamish +
                  city_Seattle + city_Snoqualmie + city_Vashon + city_Woodinville, kc_train)

########################################################################################################
# Eman - Linear Regression
########################################################################################################

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

#####################################################################################
# MAY- RANDOM FOREST - WE NEED MSE
######################################################################################

xnames <- colnames(kc_cities)
xnames <- xnames[!xnames %in% c("price", "bedrooms", "city")]

loopformula <- " price ~ bedrooms"
for (xname in xnames) {
  loopformula <- paste(loopformula, "+", xname, sep = "")
}
f <- as.formula(loopformula)
f

train <- round(0.8 * nrow(kc_cities))
test <- nrow(kc_cities) - train


# spliting data into train and test
set.seed(98765)
train_index <- sample(nrow(kc_cities), train) # assign 17290 random rows to the train set
kc_train <- kc_cities[train_index,]
kc_test <- kc_cities[-train_index,]


x1_train <- model.matrix(f, kc_train)[, -1]
x1_test <- model.matrix(f, kc_test)[, -1]

y_train <- kc_train$price
y_test <- kc_test$price

fit_rf <- randomForest(f,
                       kc_train,
                       ntree = 10,
                       do.trace = F)

varImpPlot(fit_rf)


y_hat_train_random <- predict(fit_rf, kc_train)
tree_mse_train_random <- mean((y_hat_train_random - y_train)^2)
y_hat_test_random <- predict(fit_rf, kc_test)
tree_mse_test_random <- mean((y_hat_test_random - y_test)^2)


#####################################################################################
# MAY- TREES - WE NEED MSE
######################################################################################
library(rpart)
library(rpart.plot)

fit.tree <- rpart(f,
                  kc_train,
                  control = rpart.control(cp = 0.001))
par(xpd = TRUE)
plot(fit.tree, compress=TRUE)
text(fit.tree, use.n=TRUE)

y_hat_train <- predict(fit.tree, kc_train)
tree_mse_train <- mean((y_hat_train - y_train)^2)
y_hat_test <- predict(fit.tree, kc_test)
tree_mse_test <- mean((y_hat_test - y_test)^2)

#####################################################################################
# MAY- Boosting TREES - WE NEED MSE
######################################################################################
fit_btree <- gbm(f,
                 data = kc_train,
                 distribution = "gaussian",
                 n.trees = 1000,
                 interaction.depth = 4,
                 shrinkage = 0.001)

summary(fit_btree)
relative.influence(fit_btree)

y_hat_train_boosting <- predict(fit_btree, kc_train, n.trees = 100)
tree_mse_train_boosting <- mean((y_hat_train_boosting - y_train)^2)
y_hat_test_boosting <- predict(fit_btree, kc_test, n.trees = 100)
tree_mse_test_boosting <- mean((y_hat_test_boosting - y_test)^2)

#####################################################################################
# MAY- Bagging TREES - WE NEED MSE
######################################################################################
library(ipred)
tree_bag <- bagging(f, data=kc_train, coob=TRUE)

y_hat_train <- predict(tree_bag, kc_train)
tree_mse_train <- mean((y_hat_train - y_train)^2)
y_hat_test <- predict(tree_bag, kc_test)
tree_mse_test <- mean((y_hat_test - y_test)^2)

######################################################################################

#Generating a Prediction matrix for each Tree
n.trees = seq(from=1 ,to=1000, by=10)
predmatrix<-predict(fit_btree, kc_train, n.trees = n.trees)
dim(predmatrix) #dimentions of the Prediction Matrix
View(predmatrix)

#Calculating The Mean squared Test Error
train.error <- with(kc_train, apply( (predmatrix- y_train)^2,2,mean))
head(train.error) #contains the Mean squared test error for each of the 100 trees averaged

#Plotting the test error vs number of trees

plot(n.trees , train.error , pch=19,col="blue",xlab="Number of Trees",
     ylab="Test MSE", main = "Perfomance of Boosting on Test Set", ylim = c(0, 150000))

abline(h = min(tree_mse_test_random),col="red") #test.err is the test error of a Random forest fitted on same data
legend("topright",c("Minimum Test error Line for Random Forests"),col="red",lty=1,lwd=1)
