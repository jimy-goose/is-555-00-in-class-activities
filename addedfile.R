library(remotes)
library(tidyverse)
library(tidymodels)
library(janitor)
library(ggcorrplot)
library(lme4)
library(car)
library(xgboost)
library(C50)
library(tidypredict)

string_1 <- "This is a string."

numeric_2 <- c(1,2,3,4,5)

sw_data <- tribble(
  ~name,             ~species, ~homeworld,
  'Luke Skywalker',  'Human',  'Tatooine',
  'C-3PO',           'Droid',  'Tatooine',
  'R2-D2',           'Droid',  'Naboo',
  'Darth Vader',     'Human',  'Tatooine',
  'Leia Organa',     'Human',  'Alderaan',
  'Finn',            'Human',   NA,      
  'Owen Lars',       'Human',  'Tatooine',
  'Obi-Wan Kenobi',  'Human',  'Stewjon',
  'Rey',             'Human',   NA
)

sw_data %>% count(homeworld)

