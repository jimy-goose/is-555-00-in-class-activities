library(tidyverse)
library(tidymodels)

# Install these if you haven't:
# install.packages('vetiver')
# install.packages('pins')
# install.packages('plumber')
# install.packages('ranger')

library(vetiver)
library(pins)
library(plumber)

# Data and Model Setup
housing <-  read_csv('https://www.dropbox.com/scl/fi/9s2pkk23rsqokh2hymp92/housing_training.csv?rlkey=45f14zqlghrzwrmrhu47xapy9&dl=1')

set.seed(42)
housing_split <- initial_split(housing, strata = sale_price)

housing_training <- housing_split %>% training()
housing_testing <- housing_split %>% testing()

housing_rec <- recipe(sale_price ~ .,
                      data = housing_training) %>% 
  # step_YeoJohnson(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_other(ms_sub_class,ms_zoning,exterior_1st,exterior_2nd,utilities,electrical) %>% 
  step_unknown(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_nzv(all_predictors())


housing_rf_model <- rand_forest() %>% 
  set_engine('ranger') %>% 
  set_mode('regression')

housing_wkfl <- workflow() %>% 
  add_model(housing_rf_model) %>% 
  add_recipe(housing_rec)

# Assume we did all the things (crossvalidation, tuning, etc.), 
# and that we're ready to deploy. Drumroll, please.....

# What's first? (Or, I should say last?)
final_model <- housing_wkfl %>% 
  last_fit(split = housing_split)

final_model %>% collect_metrics()
final_model %>% collect_predictions()


# Now we need to extract the trained workflow from the larger final fit result:
extracted_workflow <- final_model %>% extract_workflow()

# And create from that (trained) workflow a deployable model object. 
# We'll use vetiver for this. Give it a name, too.
deployable_model <- vetiver_model(extracted_workflow, 'house_prices')


# Now let's create a plumber-based API server for local testing
# pr(), vetiver_api(), pr_run()
pr() %>%
  vetiver_api(deployable_model) %>%
  pr_run(port = 8888)



# Set your working directory somewhere reasonable
working_dir <- '~/Desktop/housing-deployment/'
fs::dir_create(working_dir)
setwd(working_dir)

# Let's look at publishing with the pins package:
# First, create a "pin-board"
pin_board <- board_folder('models', versioned = TRUE)


# Write the (deployable) vetiver model object to the board:
pin_board %>% vetiver_pin_write(deployable_model)


# Boards can hold lots of different models (and versions). These can be listed 
# and retreived. 
pin_board %>% pin_list()
pin_board %>% pin_versions('house_prices')


# Moving from a pin board to a docker-based plumber API is as simple as preparing
# the docker scripts:
vetiver_prepare_docker(pin_board, "house_prices")


# The docker output from the function above is...okay. But the dockerfile linked
# below is better and should build very quickly:
# https://www.dropbox.com/scl/fi/7a2v90ercg7l85dz2imgx/Dockerfile.better?rlkey=ubhuad30olpfdj31tyoqlfhct&dl=1


# Place the Dockerfile from the link above in your working directory, replacing (or renaming)
# the old one. Then you can go there in your terminal and run the following commands:

# > docker build -t name-of-your-image .

# > docker run -p 8000:8000 name-of-your-image


# If it's running, you should see "Running plumber API at http://0.0.0.0:8000"






# Let's go check out our API.
library(jsonlite)
library(httr)

next_months_data <- read_csv('https://www.dropbox.com/scl/fi/ztf8ipi4i4n5rfwtwcmh0/housing_testing.csv?rlkey=yecihhbp6hbf88rfr9w3kysbk&dl=1')

# Vetiver has some helpful functions to point to a model api endpoint:
local_api <- vetiver_endpoint("http://127.0.0.1:8000/predict")

# Now use the standard predict function to hit the api with our new data:
local_api_preds <- predict(local_api,
                     new_data = next_months_data)

# Look at performance over the past month:
next_months_data %>%
  bind_cols(api_preds) %>%
  rsq(truth = sale_price, estimate = .pred)


# Now let's see how a typical API call would work. Let's do this with a single
# house that was JUST LISTED for sale: 
new_house <- next_months_data %>% slice_sample(n=1)

# Convert the observation to JSON in preparation for API-based communication
new_house_json <- toJSON(new_house, pretty = T)

# Now we can send a request to the API's predict endpoint and get a response:
response <- POST('http://127.0.0.1:8000/predict',
                        body = new_house_json)

# The response is a list item, with the prediction found in the "content" item.
# However, it's encoded in a non-human-readable way:
response$content

# instead we can pull the content out and decode it like so:
response_json <- content(response,
                         as = "text",
                         encoding = "UTF-8")

# That decoded content is in json format, so the last thing would be to convert 
# it (if desired) to a tibble:
single_pred_api <- fromJSON(response_json) %>% as_tibble







