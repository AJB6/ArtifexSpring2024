library(tidyverse)
conversions = read.csv("https://raw.githubusercontent.com/dbreynol/DS809/main/data/conversions.csv") 

view(conversions)

conversions$datestamp = ymd(conversions$datestamp)
conversions %>% filter(country_code == 'be',marketing_channel == 'Display Ads') %>% ggplot(aes(datestamp,conversions))+geom_line()

set

# 1. What are the top 5 countries in terms of Display Ad conversions?
conversions %>% filter(marketing_channel == 'Display Ads') %>% 
  group_by(country_code) %>% 
  summarise(cons=sum(conversions, na.rm = T)) %>%
  arrange(desc(cons))

# 2. What is the distribution of US conversions by day of week (i.e., Mon - Fri)? (boxplots or histogramconversions# 

conversions %>% filter(country_code == "us") %>% 
  mutate(weekday=wday(datestamp, label=T)) %>% 
  ggplot(aes(x=factor(weekday), y = conversions)) + geom_boxplot()
  
# 3. What is the distribution of US conversions by month? (boxplots or histogram)

conversions %>% filter(country_code == "us") %>% 
  mutate(monthday=month(datestamp, label=T)) %>% 
  ggplot(aes(x=factor(monthday), y = conversions)) + geom_boxplot()

# 4. Fit a linear regression model with daily US convsersions as your response (total across all marketing channels) and Day of week (i.e., Mon - Fri) as the covariate. What is the coefficient on Monday? What does this mean? 

model=lm(conversions ~ weekday, data=conversions)
view(model)
