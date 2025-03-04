library(tidyverse)
library(fastDummies)
# Tip: you can pass a whole tibble to fastDummies::dummy_cols(), 
# and it will automatically dummy code any factor columns.

laptops <- read_csv('https://www.dropbox.com/scl/fi/rsetl7vzy4nfd10ihmr96/laptops.csv?rlkey=kfwdief4jpwr1008z1llbx3qa&dl=1')


# Before we jump into train/test split, let's look at the data and
# see if we can come up with any new features.




# Okay now take the modified dataset into the initial_split() function (and beyond).