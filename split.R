# Split the Data, & Resampling with Package "rsample" ----
set.seed(123)
splits <- initial_split(hotels, strata = children)  # Note the stratification

hotel_other <- training(splits)
hotel_test  <- testing(splits)

hotel_other %>% 
  count(children) %>% 
  mutate(prop = n/sum(n)) # training set proportions by children

hotel_test  %>% 
  count(children) %>% 
  mutate(prop = n/sum(n)) # test set proportions by children

# Resampling: let's split the training set even further into: Validation set, and Training set
set.seed(234)
val_set <- validation_split(hotel_other, 
  strata = children, 
  prop = 0.80)
val_set
