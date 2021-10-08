# Our second model
#
# Modelling, with the package "Parsnip" ----
# Our second model uses a random forest, which is computationally expensive
cores <- parallel::detectCores()
cores <- cores - 2

# Build the model, thanks to packages "Ranger" and "Parsnip"
rf_mod <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
  set_engine("ranger", num.threads = cores) %>% 
  set_mode("classification")
# Note the 2 hyperparams to tune() later

# Create the recipe, with the package "Recipes" ----
# Let's prep the data, e.g. the date variables
rf_recipe <- 
  recipe(children ~ ., data = hotel_other) %>% 
  step_date(arrival_date) %>% 
  step_holiday(arrival_date) %>% 
  step_rm(arrival_date) 
print(rf_recipe)

# Create a SECOND Workflow, with the package "Workflows" ----
# Bundling our second recipe and model into a second Workflow
rf_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(rf_recipe)

# Create the grid for tuning, with the package "Tune" and "Ranger"  ----
# Let's train 25 models
rf_mod %>%    
  parameters()

set.seed(345)
rf_res <- 
  rf_workflow %>% 
  tune_grid(val_set,
    grid = 25,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc))

