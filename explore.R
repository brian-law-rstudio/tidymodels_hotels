# The challenge ----
# predict which hotel stays included children and/or babies
# based on the other characteristics of the stays 
# such as which hotel the guests stay at, how much they pay, etc.
# Note: filtered the data to include only the bookings that did not cancel
library(tidymodels)  
library(readr)       # for importing data
library(vip)         # for variable importance plots


# Explore, using packages from the tidyverse ----
hotels <- 
  read_csv('https://tidymodels.org/start/case-study/hotels.csv') %>%
  mutate_if(is.character, as.factor) 

dim(hotels)
glimpse(hotels)
table(is.na(hotels$arrival_date)) # make sure only bookings with NO cancellations
min(unique(hotels$arrival_date)) # COVID effect? 
max(unique(hotels$arrival_date)) # Nope.

str(hotels$children) # What do we know about our outcome variable?
hotels %>%  
  count(children) %>% 
  mutate(prop = n/sum(n))
