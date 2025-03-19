library(tidyverse)
library(tidymodels)
library(lubridate)


# Warm Up Exercise --------------------------------------------------------

cr_data <- read_csv('https://www.dropbox.com/scl/fi/vykejw5ud9ejjvcc442gd/credit_small.csv?rlkey=zuyurxikxickgdjchh6681j91&dl=1')

cr_data <- cr_data %>% 
  mutate(status = as.factor(status))


cr_data %>% glimpse()

# Missingness:
cr_data %>% 
  summarise(across(everything(), ~sum(is.na(.)))) %>% 
  glimpse()

# Numeric distributions:
cr_data %>% 
  select(where(is.numeric)) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(x = value, fill = name)) +
  geom_histogram(alpha = .4) +
  facet_wrap(~name, scales = 'free') +
  labs(title = 'Distributions of Numeric Features',
       fill = 'Feature')

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

cr_rec <- recipe(status ~ ., 
                 data = cr_training) %>% 
  step_impute_median(all_numeric_predictors()) %>% 
  step_log(assets, debt, income, price, expenses, offset = 1) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors()) 




# Brief look at prep() and bake()
# What are these doing?
cr_rec %>% prep() 
cr_rec %>% prep() %>% bake(new_data = cr_training)
cr_rec %>% prep() %>% bake(new_data = cr_testing)



# Try it out --------------------------------------------------------------

# install.packages('fastDummies')
library(fastDummies)

flights <- read_csv('https://www.dropbox.com/scl/fi/3q5erfazwmnkig8ofxd9w/flights_recipes.csv?rlkey=ht3mv8knjmwnsi16p8gojhwd7&dl=1') %>% 
  mutate(arr_delay = factor(arr_delay))

flights

# Let's peek at numeric feature distributions:
flights %>% 
  select(dep_time, air_time, distance,dep_delay) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(x = value, fill = name)) +
  geom_histogram(alpha = .4) +
  facet_wrap(~name, scales = 'free') +
  labs(title = 'Distributions of Numeric Features',
       fill = 'Feature')

# Check missingness:
flights %>% summarise(across(everything(), ~sum(is.na(.))))
med_distance <- median(flights$distance, na.rm = T)


# New Features & Transformations that might be helpful:
#     - New Features: day_of_week, month
#     - Impute missing distance
#     - Log transform: dep_delay, air_time, distance
#     - Center/Scale: dep_delay, air_time, distance, dep_time
#     - Dummy codes: origin, carrier, holidays 

flights_fe <- flights %>% 
  mutate(day_of_week = wday(date, label=T),
         month = month(date, label = T)) %>% 
  mutate(distance = if_else(is.na(distance), med_distance, distance)) %>% 
  mutate(across(c(air_time,distance,dep_delay), ~log(.))) %>% 
  mutate(across(c(dep_time,air_time,distance,dep_delay), ~as.numeric(scale(.)))) %>% 
  mutate(origin_JFK = if_else(origin=='JFK',1,0),
         origin_LGA = if_else(origin=='LGA',1,0)) %>% 
  select(-origin) %>% 
  fastDummies::dummy_cols('carrier', remove_selected_columns=T) 

flights_fe %>% glimpse

set.seed(42)
flights_split <- initial_split(flights, strata = arr_delay)

flights_training <- flights_split %>% training()
flights_testing <- flights_split %>% testing()

# Let's create a recipe to handle everything we did for `flights_fe` above:
#   Some new steps to try: step_date(), step_range()

flights_rec <- recipe(arr_delay ~ .,
                      data = flights_training) %>% 
  step_date(date, features = c('dow','month')) %>% 
  step_impute_median(distance) %>% 
  step_range(dep_delay, min = 1, max = 2000) %>%
  step_log(air_time, distance, dep_delay, offset = 1) %>% 
  step_normalize(dep_time,air_time,distance,dep_delay) %>% 
  step_dummy(origin, carrier) 

# Look at departure delay with and without step_range()
flights_training %>% select(dep_delay) %>% summary()
flights_rec %>% prep() %>% bake(new_data = flights_training) %>% select(dep_delay) %>% summary()

# If you want to check your work as you go:
flights_rec %>% prep() %>% bake(new_data = flights_training)


# When we're done, here's our first workflow:
lr_spec <- logistic_reg() 

flights_wkfl <- workflow() %>% 
  add_model(lr_spec) %>% 
  add_recipe(flights_rec)

flights_fit <- flights_wkfl %>% 
  fit(data = flights_training)

# Fun shortcut function: augment()
flights_fit %>% 
  augment(flights_testing) %>% 
  roc_auc(truth = arr_delay,
          .pred_late) 




# With a messier dataset: -------------------------------------------------

cars <- read_csv('https://www.dropbox.com/scl/fi/xavej23qpauvx3xfdq7zh/car_sales.csv?rlkey=4mfp6tpia0uqkcoiqf9jleau3&dl=1')

cars %>% glimpse

# missingness counts:
cars %>% summarize(across(everything(), ~sum(is.na(.)))) %>% glimpse

# There are lots of makes and even more models
cars %>% count(make)
cars %>% count(model) %>% arrange(-n)

# Quick look at the distributions of numerics:
cars %>% 
  select(where(is.numeric)) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(x = value, fill = name)) +
  geom_histogram(alpha = .4) +
  facet_wrap(~name, scales = 'free') +
  labs(title = 'Distributions of Numeric Features',
       fill = 'Feature')

set.seed(42)
cars_split <- initial_split(cars, strata = sellingprice_log)
cars_training <- cars_split %>% training()
cars_testing <- cars_split %>% testing()

# Create a recipe that handles:
#     - median imputation for all numeric features
#     - YeoJohnson transformation of all numeric features
#     - missingness in make/model
#     - long-tail (i.e., uncommon) values in make/model
#     - Normalize all numeric features
#     - dummy coding for all categories
#
# New steps to try: step_YeoJohnson(), step_unknown(), step_other()
cars_rec <- recipe(sellingprice_log ~ .,
       data = cars_training) %>% 
  step_impute_median(all_numeric_predictors()) %>% 
  step_YeoJohnson(all_numeric_predictors()) %>% 
  step_unknown(c(make, model), new_level = 'this_was_missing') %>% 
  step_other(c(make, model), threshold = .05) %>%
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors())



# Look at the effect of step_unknown() and step_other()
cars_rec %>% prep() %>% bake(new_data = cars_training) %>% 
  glimpse()


cars_rec %>% prep() %>% bake(new_data = cars_training) %>% 
  count(model)


# A closer look:

cars_training_small <- cars_training %>% select(sellingprice_log, make, model)
cars_testing_small <- cars_testing %>% select(sellingprice_log, make, model)

training_makes <- cars_training_small %>% count(make) %>% pull(make)

# 2319 missing makes in test data
cars_training_small %>% count(is.na(make))
# 786 missing makes in test data
cars_testing_small %>% count(is.na(make))

# Most (79 of 87) makes training data are also in testing data:
cars_testing_small %>% count(make) %>% 
  filter(make %in% training_makes)
# But there are 3 that are not:
cars_testing_small %>% count(make) %>% 
  filter(!make %in% training_makes)

cars_rec_small <- recipe(sellingprice_log ~ .,
                   data = cars_training_small) %>% 
  step_unknown(c(make), new_level = 'missing') %>%
  step_novel(c(make)) %>%
  step_other(c(make), threshold = .05) %>%
  step_other(c(model), threshold = .015) #%>%
  # step_dummy(all_nominal_predictors())

# First, the effect of step_unknown() and step_novel():
# no more missing:
cars_rec_small %>% prep() %>% bake(new_data = cars_training_small) %>% 
  count(is.na(make)) 
cars_rec_small %>% prep() %>% bake(new_data = cars_training_small) %>% 
  count(make)
cars_rec_small %>% prep() %>% bake(new_data = cars_training_small) %>% 
  count(model)

cars_training_small %>% count(is.na(model))

# And in the test data, no more 'Lotus', for example:
cars_rec_small %>% prep() %>% bake(new_data = cars_testing_small) %>% 
  count(make) %>% 
  filter(make %in% c('Lotus','new'))

cars_testing_small %>% 
  count(make)



# adding in step_other():
cars_rec_small <- recipe(sellingprice_log ~ .,
                         data = cars_training_small) %>% 
  step_novel(c(make, model)) %>% 
  step_unknown(c(make, model), new_level = 'missing') %>% 
  step_other(c(make), threshold = .05) %>%
  step_other(c(model), threshold = .015) # %>%
# step_dummy(all_nominal_predictors())

# What was 87 models is now 9 for both train and test:
cars_rec_small %>% prep() %>% bake(new_data = cars_training_small) %>% 
  count(model)
cars_rec_small %>% prep() %>% bake(new_data = cars_testing_small) %>% 
  count(model) 






# After you're done, use this recipe in a workflow with an xgboost model spec:
# (This will create some warnings that we can investigate: make %in% c('audi','Lotus'))
xgb_spec <- boost_tree() %>% 
  set_mode('regression')



# Final Example - Zillions of Features ------------------------------------

claims_raw <- read_csv('https://www.dropbox.com/scl/fi/yak22stqfsq3aaz4qvxn1/claims.csv?rlkey=hj42vra7wpi6odnqvmrgxb797&dl=1')

# A handful of numeric features:
claims_raw %>% 
  select(where(is.numeric)) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(x = value, fill = name)) +
  geom_density(alpha = .4) +
  facet_wrap(~name, scales = 'free')

# But over 100 categoricals:
claims_raw %>% 
  select(where(is.character)) %>% glimpse()

# Note the highly skewed outcome (claim loss amount):
claims_raw %>%
  ggplot(aes(x = loss)) +
  geom_histogram(alpha = .4, fill = 'blue') +
  theme_bw() +
  labs(title = 'Distribution of Claim Loss Amount (Outcome)')

# It's recommended to handle any transformations of the outcome BEFORE entering
# the model flow. (In case you're curious, here's a brief explanation: 
# https://stackoverflow.com/questions/75762005/error-in-step-log-when-trying-to-make-predictions-with-my-model)
claims <- claims_raw %>% 
  mutate(log_loss = log(loss)) %>% 
  select(-loss)

# Model setup
set.seed(42)
claims_split <- initial_split(claims, strata = log_loss)

claims_training <- claims_split %>% training()
claims_testing <- claims_split %>% testing()

# Create a recipe that applies best-practice pre-processing operations:
claims_rec <- recipe(log_loss ~ .,
                     data = claims_training)


# And then setup and train a model:
linreg_spec <- linear_reg()

claims_wkfl <- workflow() %>% 
  add_recipe(claims_rec) %>% 
  add_model(linreg_spec)

claims_fit <- claims_wkfl %>% last_fit(split = claims_split)

claims_fit %>% collect_metrics()

claims_fit %>% 
  collect_predictions() %>% 
  ggplot(aes(x = log_loss, y = .pred)) +
  geom_point()

