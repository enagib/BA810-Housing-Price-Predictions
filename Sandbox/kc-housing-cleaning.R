install.packages("tidyverse")
library(tidyverse)

## Loading our table
kc_housing <- read_csv("kc_house_data.csv")

## Examining table
View(kc_housing)
colnames(kc_housing)
dim(kc_houses)

## Creating our table
kc_houses <- kc_housing %>% 
  mutate(transaction_id = row_number()) %>% 
  rename("house_id" = id) %>% 
  select(transaction_id, everything()) %>% 
  separate(date, into = c("year_sold", "month_sold", "day_sold"), sep = "-") 

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

## we viewd the id and found that some id are not unique this is because some houses are
## sold twice so we group_by id, and date to see if they are redudant, and it turned out that
## every transaction is unique
kc_housing %>% group_by(id, date) %>% summarise(count = n()) %>% filter(count>1)