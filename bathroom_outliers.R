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
