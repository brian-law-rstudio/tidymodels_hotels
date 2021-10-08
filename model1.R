# Our first model
#
# Modelling, with the package "Parsnip" ----
# Our first model uses penalized logistic regression
# Build the model
lr_mod <- 
  logistic_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet")
# Note1 setting penalty to tune() so it's now a hyperparameter we can tweak later
# Note2 we've define the model but not run it b/c that way not have to rerun unneccesarily
# Note3 defining rather than running model now allows us to insert hyperparams with tune()

# Create the recipe, with the package "Recipes" ----
# Let's prep the data, e.g. normalize numeric vars...
holidays <- c("AllSouls", "AshWednesday", "ChristmasEve", "Easter", 
              "ChristmasDay", "GoodFriday", "NewYearsDay", "PalmSunday")

lr_recipe <- 
  recipe(children ~ ., data = hotel_other) %>% 
  step_date(arrival_date) %>% 
  step_holiday(arrival_date, holidays = holidays) %>% 
  step_rm(arrival_date) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

# Create the Workflow, with the package "Workflows" ----
# Bundling our receipe with our model into a workflow, like recipe and ingredients into a pot
lr_workflow <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(lr_recipe)
print(lr_workflow)

# Create the grid for tuning  ----
# To tune the hyperparameter we need a range of values for it to try, let's define that
lr_reg_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30))
lr_reg_grid %>% top_n(-5) # lowest penalty values

# Train and Tune the model, with the package "Tune" ----
# Let's train 30 models, saving out validation set predictions for diagnostics 
lr_res <- 
  lr_workflow %>% 
  tune_grid(val_set,
    grid = lr_reg_grid,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc))

# Let's visualize how the models did
lr_plot <- 
  lr_res %>% 
  collect_metrics() %>% 
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number())
lr_plot

top_models <-
  lr_res %>% 
  show_best("roc_auc", n = 15) %>% 
  arrange(penalty) 
top_models

# Let's choose a candidate "best" model focusing on parsimony 
lr_best <- 
  lr_res %>% 
  collect_metrics() %>% 
  arrange(penalty) %>% 
  slice(12)
lr_best

# Let's plot the ROC curve of our chosen model
lr_auc <- 
  lr_res %>% 
  collect_predictions(parameters = lr_best) %>% 
  roc_curve(children, .pred_children) %>% 
  mutate(model = "Logistic Regression")
autoplot(lr_auc)
