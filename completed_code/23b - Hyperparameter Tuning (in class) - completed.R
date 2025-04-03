library(tidyverse)
library(tidymodels)


# Setup w/ Credit Data --------------------------------------------------------

cr_data <- read_csv('https://www.dropbox.com/scl/fi/vykejw5ud9ejjvcc442gd/credit_small.csv?rlkey=zuyurxikxickgdjchh6681j91&dl=1') %>% 
  mutate(status = as.factor(status))

cr_data %>% glimpse()

# Model setup:
set.seed(42)
cr_split <- initial_split(cr_data, strata = status)
cr_training <- cr_split %>% training()
cr_testing <- cr_split %>% testing()

# Let's create a recipe that:
#    - imputes missing numeric values
#    - log transforms assets, debt, income, price, expenses
#    - normalizes all numeric predictors
#    - dummy codes all categories
#    - downsamples the bad/good status counts
cr_rec <- recipe(status ~ ., 
                 data = cr_training) %>% 
  step_impute_median(all_numeric_predictors()) %>% 
  step_log(assets, debt, income, price, expenses, offset = 1) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  themis::step_downsample(status, under_ratio = 1)

cr_training %>% count(status)


# Now let's setup a model spec (rpart decision tree), workflow, and do a cross validation.
model_spec <- decision_tree() %>% 
  set_engine('rpart') %>% 
  set_mode('classification')


cr_wkfl <- workflow() %>% 
  add_model(model_spec) %>% 
  add_recipe(cr_rec)


cr_folds <- vfold_cv(data = cr_training, strata = status)
  
fit_resamples()


# Next, a tunable model specification, recipe, and workflow:
model_spec_tunable <- decision_tree(
  tree_depth = tune(),
  min_n = tune()
) %>% 
  set_engine('rpart') %>% 
  set_mode('classification')

cr_rec_tunable <- recipe(status ~ ., 
                 data = cr_training) %>% 
  step_impute_median(all_numeric_predictors()) %>% 
  step_log(assets, debt, income, price, expenses, offset = 1) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  themis::step_downsample(status, under_ratio = tune())


cr_wkfl_tunable <- workflow() %>% 
  add_model(model_spec_tunable) %>% 
  add_recipe(cr_rec_tunable)



# Extract parameters, create a grid to search, then search:
params <- extract_parameter_set_dials(cr_wkfl_tunable)

tuning_grid <- grid_random(params, size = 50)


doParallel::registerDoParallel(cores = future::availableCores()-1)


tuning_results <- cr_wkfl_tunable %>%  
  tune_grid(grid = tuning_grid,
            resamples = cr_folds)

tuning_results %>% collect_metrics()
tuning_results %>% collect_metrics(summarize = F)


tuning_results %>% show_best()
tuning_results %>% show_best(metric = 'rsq')
best_parameters <- tuning_results %>% select_best()

# That best setup can be passed to finalize_workflow() to setup a final workflow to finish our pipeline
finalized_wkfl <- tunable_wkfl %>% 
  finalize_workflow(best_parameters)



# Now we use last_fit()
final_fit <- finalized_wkfl %>% 
  last_fit(split = housing_split)

final_fit %>% collect_metrics()
final_fit %>% collect_predictions()