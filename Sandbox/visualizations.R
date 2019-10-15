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
library(corrplot)

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


colnames(kc_house_new_4)












p <- ggplot(city_price) +
  geom_col(aes(city, sort(avg_price, decreasing =  T)), fill = "royalblue") +
  theme(axis.text.x = element_text(angle = 90))

plot(p) 
abline(h=40, col="blue")
###############################################################################
# avg price per month
###############################################################################

month_p <-kc_house_new_4 %>% 
  group_by(month_sold) %>% 
  summarise(avg_price = mean(price))

ggplot(month_p, aes(factor(month_sold), avg_price, fill = avg_price)) +
  geom_col()+
  ylim(0,600) 

###############################################################################
# avg price per view
###############################################################################

view_p <-kc_house_new_4 %>% 
  group_by(view) %>% 
  summarise(avg_price = mean(price))

ggplot(view_p, aes(factor(view), avg_price, fill = avg_price)) +
  geom_col()

###############################################################################
# avg price per bedrooms
###############################################################################

bed_p <-kc_house_new_4 %>% 
  group_by(bedrooms) %>% 
  summarise(avg_price = mean(price))

ggplot(bed_p, aes(factor(bedrooms), avg_price, fill = avg_price)) +
  geom_col()

###############################################################################
# avg price per bathrooms
###############################################################################

bath_p <-kc_house_new_4 %>% 
  group_by(bathrooms) %>% 
  summarise(avg_price = mean(price))

ggplot(bath_p, aes(factor(bathrooms), avg_price, fill = avg_price)) +
  geom_col()
                 
kc_house_new_4 %>% 
  filter(bathrooms == 7.5) %>% 
  View

###############################################################################
# avg price per grade
###############################################################################

grade_p <-kc_house_new_4 %>% 
  group_by(grade) %>% 
  summarise(avg_price = mean(price))

ggplot(grade_p, aes(factor(grade), avg_price, fill = avg_price)) +
  geom_col()

###############################################################################
# avg price per condition
###############################################################################

grade_p <-kc_house_new_4 %>% 
  group_by(condition) %>% 
  summarise(avg_price = mean(price))

ggplot(grade_p, aes(factor(condition), avg_price, fill = avg_price)) +
  geom_col()



##############################################################################################################################################################
##############################################################################################################################################################
##############################################################################################################################################################



###############################################################################
# city vs. price vs. grade
###############################################################################

grade_c <- kc_house_new_4 %>% 
  group_by(city) %>% 
  summarise(grade = round(mean(grade)),
            avg_price = mean(price))

ggplot(grade_c) +
  geom_col(aes(factor(city), sort(avg_price, decreasing = T) , fill = grade)) +
  theme(axis.text.x = element_text(angle = 90))

###############################################################################
# city vs. price vs. view
###############################################################################

view_c <- kc_house_new_4 %>% 
  group_by(city) %>% 
  summarise(view = round(mean(view)),
            avg_price = mean(price))

ggplot(view_c) +
  geom_col(aes(factor(city), sort(avg_price, decreasing = T) , fill = view)) +
  theme(axis.text.x = element_text(angle = 90))

###############################################################################
# city vs. price vs. conditon
###############################################################################

cond_c <- kc_house_new_4 %>% 
  group_by(city) %>% 
  summarise(condition = round(mean(condition)),
            avg_price = mean(price))

ggplot(cond_c) +
  geom_col(aes(factor(city), sort(avg_price, decreasing = T) , fill = condition)) +
  theme(axis.text.x = element_text(angle = 90))

###############################################################################
# count per month (INCLUDE)
###############################################################################

month_count <-kc_house_new_4 %>% 
  group_by(month_sold) %>% 
  summarise(count= n())

ggplot(month_count, aes(factor(month_sold), count, fill = count)) +
  geom_col()+
  ylim(0,600) 









##########################################################################################
INCLUDE
##########################################################################################
yao <- kc_house_new %>%
  group_by(renovated,grade) %>% 
  summarise(avg_house_price=mean(price)*1000)

reno <- yao[c(3:22),]
(reno)

reno$renovated <- as.factor(reno$renovated)
format(reno$avg_house_price,scientific=FALSE)
ggplot(reno, aes(factor(grade), avg_house_price, fill = renovated,label=round(avg_house_price,0))) +
  geom_bar(position = "dodge", stat = "identity")  +
  ggtitle("Average House Price Based on Renovation and Grade") +
  ylab("House Price") +
  xlab("Real Estate Apprisal Grade")



###############################################################################
# city vs. price
###############################################################################

city_price <- kc_house_new_4 %>% 
  group_by(city) %>% 
  summarize(avg_price = mean(price),
            max_price = max(price),
            min_price = min(price))

kc_house_new_4 %>% 
  summarize(avg_price = mean(price))

ggplot(city_price) +
  geom_col(aes(city, sort(max_price, decreasing =  T)), fill = "darkblue") +
  geom_col(aes(city, avg_price), fill = "royalblue") +
  geom_col(aes(city, min_price), fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(breaks = seq(0, 80, 10))



###############################################################################
# best correlations (INCLUDE)
###############################################################################

library(GGally)

ggcorr(kc_house_new_4 %>% select(-c(transaction_id,house_id, zipcode, 
                                    year_sold, month_sold, day_sold, yr_built, city_Auburn, city_Bellevue,     
                                    city_Black.Diamond, city_Bothell, city_Carnation, 
                                    city_Duvall, city_Enumclaw, city_Fall.City,
                                    city_Federal.Way, city_Issaquah, city_Kenmore,    
                                    city_Kent, city_Kirkland, city_Maple.Valley,
                                    city_Medina, city_Mercer.Island, city_North.Bend,
                                    city_Redmond, city_Renton, city_Sammamish,   
                                    city_Seattle, city_Snoqualmie, city_Vashon,    
                                    city_Woodinville)), 
       name = "corr", label = TRUE, hjust = 1, 
       abel_size = 2.5, angle = -45, size = 3)


#Linear Plot With "The Good" Predictor

p1 <- ggplot(kc_house_new_4, aes(x = bathrooms, y = price)) +
  geom_point(color = 'navajowhite4', size = 0.5) +
  geom_smooth(method="lm", color = 'black', size = 0.8) +
  theme_linedraw() + 
  labs(x = 'Total Bathrooms', y = 'Price (USD)') + 
  scale_y_continuous(labels = scales::comma)

p2 <- ggplot(kc_house_new_4, aes(x = sqft_living, y = price)) +
  geom_point(color = 'navajowhite', size = 0.5) +
  geom_smooth(method="lm", color = 'black', size = 0.5) +
  theme_linedraw() + 
  labs(x = 'Living Area (sq.ft)', y = 'Price (USD)') + 
  scale_y_continuous(labels = scales::comma)

p3 <- ggplot(kc_house_new_4, aes(x = grade, y = price)) +
  geom_point(color = 'navajowhite1', size = 0.5) +
  geom_smooth(method="lm", color = 'black', size = 0.5) +
  theme_linedraw() + 
  labs(x = 'Grade', y = 'Price (USD)') + 
  scale_y_continuous(labels = scales::comma)

p4 <- ggplot(kc_house_new_4, aes(x = sqft_above, y = price)) +
  geom_point(color = 'navajowhite2', size = 0.5) +
  geom_smooth(method="lm", color = 'black', size = 0.5) +
  theme_linedraw() + 
  labs(x = 'House Area (sq.ft)', y = 'Price (USD)') + 
  scale_y_continuous(labels = scales::comma)

p5 <- ggplot(kc_house_new_4, aes(x = sqft_living15, y = price)) +
  geom_point(color = 'navajowhite3', size = 0.5) +
  geom_smooth(method="lm", color = 'black', size = 0.5) +
  theme_linedraw() + 
  labs(x = 'Living Area 2015 (sq.ft)', y = 'Price (USD)') + 
  scale_y_continuous(labels = scales::comma)

grid.arrange(p1,p2,p3,p4,p5, nrow = 3,
             top = "House Sales in King County, USA")

