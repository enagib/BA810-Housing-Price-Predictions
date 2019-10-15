#install.packages("tidyverse")
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

## removed yr_renovated and added dummy variable for renovated
kc_houses <- kc_houses %>% 
  mutate(renovated = ifelse(yr_renovated >0, 1, 0)) %>% 
  select(everything(), -yr_renovated)

## Create a map for king's county Washington

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
  
########################################################################################################
#YAO - segmenting long and lat into NW, NE, SE, SW
### center of king county
### 47.5480 ( lat) greater than this north , 121.9836 (long) greater than this, west
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

########################################################################################################
# ploting king's county map
########################################################################################################

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

counties <- map_data("county")
wa_county <- subset(counties, region == "washington")
king_county <- subset(wa_county, subregion == "king")

ggplot(king_county, aes(x = long, y = lat)) +
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray") +
  qplot(kc_house_new, aes(x = long, y= lat))


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
# Running LASSO regression
########################################################################################################

## Lasso regression
library(glmnet)
library(tidyverse) 
library(magrittr) 
library(ggplot2) 
library(ggthemes)

train <- round(0.8 * nrow(kc_house_new))
test <- nrow(kc_house_new) - train

# spliting data into train and test
set.seed(98765)
train_index <- sample(nrow(kc_house_new), train) # assign 17290 random rows to the train set

colnames(kc_house_new)

y_data <- kc_house_new$price / 1000
x_data <- model.matrix( ~ -1 +  year_sold + month_sold + day_sold + bedrooms +
                          bathrooms + sqft_living + sqft_lot + floors + waterfront + view +
                          condition + grade + sqft_above + sqft_basement + yr_built + renovated +
                          zipcode + sqft_living15 + sqft_lot15 + SW + NW + NE + SE, kc_house_new)
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
lambda_min_mse_train <- mse_train[which.min(mse_train)]
lambda_min_mse_test <- mse_test[which.min(mse_test)]

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

## Figure out lowest mse and the respective lambda for the train and test 
kc_mse %>% 
  group_by(dataset) %>% 
  filter(mse == min(mse)) %>% View

## plot the mse for train and test and mark the min points
ggplot(kc_mse, aes(lambda, mse, col = dataset)) + 
  geom_line() + 
  geom_point(aes(y = 38229.91	, x = 0.3485395), size = 2, color = "skyblue") +
  geom_point(aes(y = 42027.13	 , x = 0.3485395), size = 2, color = "red") + 
  ggtitle("MSE for each Model") +
  scale_x_reverse()

print(lambda_min_mse_test)

coef(est , s = lambda_min_mse_test)


########################################################################################################
# Running CROSS VALIDATION
########################################################################################################

##cross validation
y <- kc_house_new$price
x_dat <- mydata_matrix[, -ncol(kc_house_new)]


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



####################################################################################
# IGNORE
####################################################################################
# Practice
kc <- kc_houses %>% select(lat, long, zipcode)
colnames(kc_houses)
View(kc)
paste(cut(kc$lat, 5, labels=FALSE), cut(kc$long, 5, labels=FALSE))

within(kc, {
  grp.lat = cut(lat, 3, labels = FALSE)
  grp.lon = cut(long, 3, labels = FALSE)
})

#Want the minimum lon value for which grp.lon = 1 and the maximum lon value for which grp.lon=1

start_grp1_lon <- min(kc$long[kc$grp.long==1])
start_grp2_lon <- min(kc$long[kc$grp.long==2])
start_grp3_lon <- min(kc$long[kc$grp.long==3])

start_grp1_lat <- min(kc$lat[kc$grp.lat==1])
start_grp2_lat <- min(kc$lat[kc$grp.lat==2])
start_grp3_lat <- min(kc$lat[kc$grp.lat==3])



#########################################

#tree random forests

library(tidyverse) 
library(ggplot2) 
library(ggthemes) 
library(scales) 
library(rpart) 
library(rpart.plot)
theme_set(theme_bw())
View(kc_house_new)

train <- round(0.8 * nrow(kc_house_new))
test <- nrow(kc_house_new) - train

# spliting data into train and test
set.seed(98765)
train_index <- sample(nrow(kc_house_new), train) # assign 17290 random rows to the train set

# now split
kc.train <- kc_house_new[train_index,]
kc.test <- kc_house_new[-train_index,]
y_traintr <- kc.train$price
y_testtr <- kc.test$price
# Data preparation
colnames(kc_house_new)
ftree <- as.formula(price ~ bedrooms +
                      bathrooms + sqft_living + sqft_lot + floors + waterfront + view +
                      condition + grade + sqft_above + sqft_basement + yr_built + renovated +
                      sqft_living15 + sqft_lot15 + SW + NW + NE + SE, kc_house_new)

#ftree23 <- as.formula(price^0.5 ~ bedrooms +
                      #bathrooms + sqft_living + sqft_lot + floors + waterfront + view +
                      #condition + grade + sqft_above + sqft_basement + yr_built + renovated +
                      #sqft_living15 + sqft_lot15 + SW + NW + NE + SE, kc_house_new)



View(kc_house_new)
# ﬁt some trees
fit.tree1 <- rpart(ftree, kc.train, control = rpart.control(cp = 0.1)) 
fit.tree2 <- rpart(ftree, kc.train, control = rpart.control(cp = 0.01))
#fit.tree23 <- rpart(ftree23, kc.train, control = rpart.control(cp = 0.01)) 
fit.tree3 <- rpart(ftree, kc.train, control = rpart.control(cp = 0.001))
par(xpd = TRUE) 
plot(fit.tree2, compress=TRUE) 
text(fit.tree2, use.n=TRUE)

rpart.plot(fit.tree2) #price
#rpart.plot(fit.tree23) #price/1000
rpart.plot(fit.tree23, type = 1) #different style of plot

yhat.train.tree <- predict(fit.tree2, kc.train) 
rmse.train.tree <- sqrt(mean((kc.train$price - yhat.train.tree)^2))
rmse.train.tree

yhat.test.tree <- predict(fit.tree2, kc.test) 
rmse.test.tree <- sqrt(mean((kc.test$price - yhat.test.tree)^2))
rmse.test.tree


# Random Forests
#install.packages(c("randomForest", "gbm"))
library(tidyverse) 
library(ggplot2) 
library(ggthemes) 
library(randomForest) 
library(gbm)

fit_rf <- randomForest(ftree, kc.train, ntree=10, do.trace=F)
#fit_rf2 <- randomForest(ftree23, kc.train, ntree=10, do.trace=F)
#varImpPlot(fit_rf2)
varImpPlot(fit_rf)




# boosted forests
fit_btree <- gbm(ftree, data = kc.train, 
                 distribution = "gaussian", n.trees = 100, 
                 interaction.depth = 2, shrinkage = 0.001)

relative.influence(fit_btree)

yhat_btree <- predict(fit_btree, kc.train, n.trees = 100) 
rmse_btree <- sqrt(mean((yhat_btree - y_traintr) ^ 2))
print(rmse_btree)

yhat_btreets <- predict(fit_btree, kc.test, n.trees = 100) 
rmse_btreets <- sqrt(mean((yhat_btreets - y_testtr) ^ 2))
print(rmse_btreets)






#map
install.packages("DT", "caret","leaflet","corrplot")
library(tidyverse)
library(stringr)
library(lubridate)
library(DT)
library(caret)
library(leaflet)
library(corrplot)
library(boot) #for diagnostic plots
fillColor = "#FFA07A"
fillColor2 = "#F1C40F"


kc_housing$PriceBin<-cut(kc_housing$price, c(0,250e3,500e3,750e3,1e6,2e6,999e6))

center_lon = median(kc_housing$long,na.rm = TRUE)
center_lat = median(kc_housing$lat,na.rm = TRUE)

factpal <- colorFactor(c("black","blue","yellow","orange","#0B5345","red"), 
                       kc_housing$PriceBin)



leaflet(kc_housing) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircles(lng = ~long, lat = ~lat, 
             color = ~factpal(PriceBin))  %>%
  # controls
  setView(lng=center_lon, lat=center_lat,zoom = 12) %>%
  
  addLegend("bottomright", pal = factpal, values = ~PriceBin,
            title = "House Price Distribution",
            opacity = 1)
