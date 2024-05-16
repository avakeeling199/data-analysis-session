# load required packages
#
install.packages("tidytuesdayR")
install.package("janitor")
library(ggplot2)
library(tidytuesdayR)
library(tidyverse)

# load in data
#
tuesdata <- tidytuesdayR::tt_load(2023, week = 17)
winners <- tuesdata$winners
london_marathon <- tuesdata$london_marathon
#
# we will focus first on the 'winners' dataset
#
# first we take a look at the dataset and see what variables may interest us 
#
dim(winners)
str(winners)
glimpse(winners)
#
# check if there is any NAs - wether it is necessary to clean the data
#
colSums(is.na(winners))
# no NAs!
#
# we need to make the dataset useable 
#
df_winners <- winners %>%
  clean_names() %>% 
  # cleans the names - ' ' replaced with '_' and decapitalised
  mutate(across(.cols = c(category, nationality), .fns = factor))
  # changes all categorical vars to factors - this is easier to work with
#
# check that this has done what we expect
#
glimpse(df_winners)
#
# use ggplot to see if more winners from certain country
#
ggplot(df_winners, aes(x=nationality, fill=nationality)) +
  geom_bar() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = 'none') +
  labs(title= 'Number of London Marathon winners by Nationality',
       x = 'Nationality',
       y = 'Number of winners')
#
# see if times are decreasing
#
ggplot(df_winners, aes(x = year, y = time)) +
  geom_smooth(color = "blue", fill = "lightblue") +
  geom_jitter(color = "red", size = 1.5, alpha = 0.5) +
  labs(title = "Trend of Marathon Winning Times Over Years",
       x = "Year",
       y = "Winning Time (HH:MM:SS)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "none")
#
# you can see in the graph that it looks as though time is decreasing over years
# we could perform regression analysis to investigate this further
# does it look reasonable to fit a straight line through the data? 
#
ggplot(df_winners, aes(x = year, y = time)) +
  geom_smooth(color = "blue", fill = "lightblue", method = 'lm') +
  geom_jitter(color = "red", size = 1.5, alpha = 0.5) +
  labs(title = "Trend of Marathon Winning Times Over Years",
       x = "Year",
       y = "Winning Time (HH:MM:SS)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "none")
#
# yes! this looks reasonable.. now we can fit a linear model to the data
# lm() doesn't like HH:MM:SS format, so make a new variable of the time just
# in seconds
#
df_winners$time_seconds <- as.numeric(as.POSIXct(df_winners$time, format = "%H:%M:%S"))
# 
# now fit the model
#
m1 <- lm(time_seconds ~ year, df_winners)
summary(m1)
#
# p-value < 0.05 - there is enough evidence to reject h0. Year is significant
#
# now look at some boxplots
#
ggplot(df_winners, aes(x=category, y=time, fill = category)) +
  geom_boxplot() +
  labs(title = 'Distribution of times by Race Category',
       x = 'Category',
       y = "Time") +
  theme_minimal() +
  theme(legend.position = 'none') 
# 
# you can see from these boxplots that there are a lot of outliers in the 
# wheelchair races and that mens and womens are not widely distributed. you can 
# also see that the means of the times seem to differ for each category. It may
# be interesting to look closer at the 'mens' and 'womens' categories
#
# plot just 'mens' and 'womens'
# 
df_winners %>%
  filter(category %in% c("Men", "Women")) %>%
  ggplot(aes(x=category, y=time, fill=category)) +
  geom_boxplot() +
  labs(title = 'Distribution of times by Race Category',
       subtitle = 'Just Mens and Womens',
       x = 'Category',
       y = 'Time') +
  theme_minimal() +
  theme(legend.position = 'none')
#
# the difference between the times for men and women is more obvious here.
# 
# we can run a 't-test' to test whether the difference between the means is 
# statistically significant
#
# first we need two seperate objects containing the times of men and women
#
mens <- subset(df_winners, category == 'Men')
womens <- subset(df_winners, category == 'Women')
#
# to ensure the t test is correct, use the time in seconds rather than in 
# HH:MM:SS format
#
t_test_res <-t.test(mens$time_seconds, womens$time_seconds)
t_test_res
# p < 0.05 - reject h0, means are different
#
# Now we will look at the other dataset together
#


