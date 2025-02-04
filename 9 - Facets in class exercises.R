library(tidyverse)


movies <- read_csv('https://www.dropbox.com/scl/fi/pi7nexxuoqnvviwfzwun9/movie_ratings.csv?rlkey=x419gluseq6p8e8xzu12ndfc9&dl=1')

# How do we compare ratings from different ratings sites? (Distribution, how they compare, etc.) 
# Or ratings from critics vs. users?









steak <- read_csv('https://www.dropbox.com/scl/fi/mzg5oxenh9oonbwpwgxzm/steak_data.csv?rlkey=2gbf1kfqfkln0zf2alwo32nza&dl=1') %>% 
  mutate(steak_prep = factor(steak_prep, levels = c('Rare','Medium rare','Medium','Medium Well','Well'))) %>% 
  mutate(age = factor(age, levels = c('18-29','30-44','45-60','> 60'))) %>% 
  mutate(hhold_income = factor(hhold_income, levels = c('$0 - $24,999','$25,000 - $49,999','$50,000 - $99,999','$100,000 - $149,999','$150,000+'))) 


# Here's a dumb chart:
steak %>% 
  filter(!is.na(steak_prep)) %>% 
  ggplot(aes(x = steak_prep, fill = educ)) +
  geom_bar() +
  labs(title = "Steak Preparation Preference by Education Level",
       x = "Steak Preparation",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# What else can you find? What other variables are in the dataset that might 
# shed some light on steak-eaters' preferences?
