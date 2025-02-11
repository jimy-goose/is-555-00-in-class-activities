library(tidyverse)

# Here's a loan dataset (defined with the ever-handy tribble() function)
loan_data <- tribble(
  ~applicant_id, ~stable_job,  ~outstanding_debt, ~loan_approved,
   1,             1,            0,                 'approve',
   2,             0,            0,                 'deny',
   3,             1,            1,                 'deny',
   4,             0,            1,                 'approve',
   5,             0,            0,                 'deny',
   6,             1,            0,                 'approve',
   7,             0,            1,                 'deny',
   8,             1,            0,                 'approve',
   9,             0,            1,                 'deny',
   10,            1,            1,                 'deny'
)


# This function calculates the "loss" for our model training, defined in this
# case as the percent of predictions that are incorrect.
calculate_loss <- function(predictions) {
  total_cases <- nrow(predictions)
  incorrect_predictions <- sum(predictions$predicted_loan_approval != predictions$loan_approved)
  loss <- incorrect_predictions / total_cases
  return(loss)
}


loan_data %>% 
  arrange(loan_approved)

# This is a simple function that uses a case_when() to apply case logic to decide
# whether a given loan should be approved. You can think of the "rules" inside
# of the case_when() function as the "model" we're training.
apply_rules <- function(data) {
  rules_applied <- data %>%
    mutate(predicted_loan_approval = case_when(
      stable_job == 1 ~ 'approve',  # First rule: approve if they have a stable job
      .default = 'deny'          # the .default parameter is the "else" in a case_when()
    ))
  return(rules_applied)
}



# Now apply your "model" and calculate the loss (which starts at 30%)
loan_predictions <- apply_rules(loan_data)
calculate_loss(loan_predictions)


loan_predictions







# What about next week's loans, though?

new_applications <- tribble(
  ~applicant_id,  ~stable_job,  ~outstanding_debt, ~loan_approved,
  101,             1,            0,                 'approve',
  102,             0,            0,                 'deny',
  103,             0,            1,                 'approve',
  104,             0,            0,                 'deny',
  105,             1,            1,                 'deny'
)
new_predictions <- apply_rules(new_applications)
calculate_loss(new_predictions)
