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
  
View(kc_houses)

