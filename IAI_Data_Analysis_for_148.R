library(tidyverse)
dataset <- read_csv("merged-w22-csc148.csv")
# We will store the relevant information in a tibble, where rows are weeks (we
# are analyzing 11 weeks) and the columns are:
analysis_tibble <- tibble(
  total_population = vector(mode = "double", length = 11),
  total_open_rate = vector(mode = "double", length = 11),
  control_group_population = vector(mode = "double", length = 11),
  control_group_open_rate = vector(mode = "double", length = 11),
  male_population_in_control = vector(mode = "double", length = 11),
  male_open_rate_in_control = vector(mode = "double", length = 11),
  female_population_in_control = vector(mode = "double", length = 11),
  female_open_rate_in_control = vector(mode = "double", length = 11),
  other_population_in_control = vector(mode = "double", length = 11),
  other_open_rate_in_control = vector(mode = "double", length = 11),
  undisclosed_population_in_control = vector(mode = "double", length = 11),
  undisclosed_open_rate_in_control = vector(mode = "double", length = 11),
  treatment_group_population = vector(mode = "double", length = 11),
  treatment_group_open_rate = vector(mode = "double", length = 11),
  male_population_in_treatment = vector(mode = "double", length = 11),
  male_open_rate_in_treatment = vector(mode = "double", length = 11),
  female_population_in_treatment = vector(mode = "double", length = 11),
  female_open_rate_in_treatment = vector(mode = "double", length = 11),
  other_population_in_treatment = vector(mode = "double", length = 11),
  other_open_rate_in_treatment = vector(mode = "double", length = 11),
  undisclosed_population_in_treatment = vector(mode = "double", length = 11),
  undisclosed_open_rate_in_treatment = vector(mode = "double", length = 11),
  
)
# Preemptively, we will initialize a counter for the total opened, and the
# opened for the control and treatment groups
total_opened_counter <- 0
total_control_opened_counter <- 0
total_treatment_opened_counter <- 0

# And we will extract the gender column as a vector
gender_column_title <- "Q769_pre"
genders <- as.vector(pull(dataset, gender_column_title))
# And parse the entries to make the analysis easier as follows:
# First we make every entry lower-cased
genders <- str_to_lower(genders)
# Next we get rid of non alphanumeric characters
genders <- str_replace_all(genders, "[^[:alnum:]]", "")

# For each week:
for (week in 1:11) {
  
  # First we will pull the group assignment column as a vector
  current_week_assignment_title <- sprintf("w%s_assignment", week)
  current_week_assignment <- 
    as.vector(pull(dataset, current_week_assignment_title))
  
  # Now, to get this weeks total population we simply get the length of the 
  # current week's assignment vector disregarding NA values
  analysis_tibble[week, "total_population"] <- 
    length(current_week_assignment[!is.na(current_week_assignment)])
  
  # Next, to get this week's open rate we repeat the same process with the 
  # opened column this time
  current_week_opened_title <- sprintf("w%s_opened", week)
  current_week_opened <- as.vector(pull(dataset, current_week_opened_title))
  # Then parse the string values into numerics
  for (index in seq_along(current_week_opened)) {
    # If the value is NA, we don't do anything
    if (is.na(current_week_opened[index])) {next}
    if (current_week_opened[index] == "EMAIL_OPENED") {
      current_week_opened[index] <- 1
    }
    else if (current_week_opened[index] == "EMAIL_SENT") {
      current_week_opened[index] <- 0
    }
  }
  current_week_opened <- as.numeric(current_week_opened)
  # And sum together the vector disregarding NA values from both it and the 
  # assignment vector
  current_week_opened_counter <- 0
  for (index in seq_along(current_week_opened)) {
    if (!(is.na(current_week_assignment[index]) || 
          is.na(current_week_opened[index]))) {
      current_week_opened_counter <- 
        current_week_opened_counter + current_week_opened[index]
    }
  }
  # Now Update the total opened counter
  total_opened_counter <- total_opened_counter + current_week_opened_counter
  # And divide by this week's total population to get the open rate
  analysis_tibble[week, "total_open_rate"] <- 
    current_week_opened_counter / analysis_tibble[week, "total_population"]
  # Next, to get the treatment group's population we simply sum this week's
  # assignment vector (since its binary) disregarding NA values
  analysis_tibble[week, "treatment_group_population"] <- 
    sum(current_week_assignment[!is.na(current_week_assignment)])
  # Conversely, to get the control group's population, we subtract this result 
  # from the total population
  analysis_tibble[week, "control_group_population"] <-
    analysis_tibble[week, "total_population"] - 
    analysis_tibble[week, "treatment_group_population"]
  
  # Then, to get the control and treatment group's open rate and the all the 
  # gendered information, we first initialize counters for this week's treatment
  # and control opened
  current_week_treatment_opened_counter <- 0
  current_week_control_opened_counter <- 0
  # As well as counters for this week's gendered populations and opened
  current_week_treatment_male_population_counter <- 0
  current_week_treatment_female_population_counter <- 0
  current_week_treatment_other_population_counter <- 0
  current_week_treatment_undisclosed_population_counter <- 0
  current_week_treatment_male_opened_counter <- 0
  current_week_treatment_female_opened_counter <- 0
  current_week_treatment_other_opened_counter <- 0
  current_week_treatment_undisclosed_opened_counter <- 0
  current_week_control_male_population_counter <- 0
  current_week_control_female_population_counter <- 0
  current_week_control_other_population_counter <- 0
  current_week_control_undisclosed_population_counter <- 0
  current_week_control_male_opened_counter <- 0
  current_week_control_female_opened_counter <- 0
  current_week_control_other_opened_counter <- 0
  current_week_control_undisclosed_opened_counter <- 0
  
  # Then, we iterate through this week's binary assignment vector
  for (index in seq_along(current_week_assignment)) {
    # If the current value is NA, we move to the next index
    if (is.na(current_week_assignment[index])) {next}
    # If the current observation was assigned to the treatment group
    else if (current_week_assignment[index]) {
      # Then we add the corresponding opened observation from the opened
      # binary vector to the counter
      current_week_treatment_opened_counter <-
        current_week_treatment_opened_counter + current_week_opened[index]
      # And for the gendered information:
      # If the current observation didn't report a gender
      if (is.na(genders[index])) {
        # We increment the undisclosed population counter
        current_week_treatment_undisclosed_population_counter <-
          current_week_treatment_undisclosed_population_counter + 1
        # And add the opened observation to the undisclosed opened counter too
        current_week_treatment_undisclosed_opened_counter <-
          current_week_treatment_undisclosed_opened_counter + 
          current_week_opened[index]
      }
      # Otherwise, if the corresponding observation is female
      ## For the comparison below, we are using str_detect to determine if the 
      ## reported gender contains the keywords female, woman, or she. Then, we
      ## are summing the logical vector, which causes it to coerce into a 
      ## numeric vector where any true value will be turned into 1. If the 
      ## summation results in a value greater than 0, then one of the keywords
      ## is contained in the observation, and thus the observation is a female 
      ## and the sum is coerced into a True value, otherwise, none of the 
      ## keywords were contained in the observation, and thus we continue.
      else if (sum(str_detect(genders[index], c("female", "woman", "she")))) {
        # We perform the same operations as above, but for female counters
        current_week_treatment_female_population_counter <-
          current_week_treatment_female_population_counter + 1
        current_week_treatment_female_opened_counter <-
          current_week_treatment_female_opened_counter + 
          current_week_opened[index]
      }
      # In another case, if the corresponding observation is male
      ## The comparison below follows the same logic as the one above, but we
      ## had to check for females first because all the male-related keywords 
      ## are contained by the female-related keywords
      else if (sum(str_detect(genders[index], c("male", "man", "he")))) {
        # Same again, but for male counters
        current_week_treatment_male_population_counter <-
          current_week_treatment_male_population_counter + 1
        current_week_treatment_male_opened_counter <-
          current_week_treatment_male_opened_counter + 
          current_week_opened[index]
      }
      # Finally, if the corresponding observation is neither (doesn't contain 
      # any of the keywords), we assume the assuming observation is other
      else {
        # And perform the same operations as in the first case
        current_week_treatment_other_population_counter <-
          current_week_treatment_other_population_counter + 1
        current_week_treatment_other_opened_counter <-
          current_week_treatment_other_opened_counter + 
          current_week_opened[index]
      }
    }
    # Otherwise, if the observation was assigned to the control group
    else {
      # We perform the same operations but to the control counters
      current_week_control_opened_counter <-
        current_week_control_opened_counter + current_week_opened[index]
      if (is.na(genders[index])) {
        current_week_control_undisclosed_population_counter <-
          current_week_control_undisclosed_population_counter + 1
        current_week_control_undisclosed_opened_counter <-
          current_week_control_undisclosed_opened_counter + 
          current_week_opened[index]
      }
      else if (sum(str_detect(genders[index], c("female", "woman", "she")))) {
        current_week_control_female_population_counter <-
          current_week_control_female_population_counter + 1
        current_week_control_female_opened_counter <-
          current_week_control_female_opened_counter + 
          current_week_opened[index]
      }
      else if (sum(str_detect(genders[index], c("male", "man", "he")))) {
        current_week_control_male_population_counter <-
          current_week_control_male_population_counter + 1
        current_week_control_male_opened_counter <-
          current_week_control_male_opened_counter + 
          current_week_opened[index]
      }
      else {
        current_week_control_other_population_counter <-
          current_week_control_other_population_counter + 1
        current_week_control_other_opened_counter <-
          current_week_control_other_opened_counter + 
          current_week_opened[index]
      }
    }
  }
  # Update the total treatment and control open counters
  total_treatment_opened_counter <- 
    total_treatment_opened_counter + current_week_treatment_opened_counter
  total_control_opened_counter <- 
    total_control_opened_counter + current_week_control_opened_counter
  # Save all the populations in the tibble
  analysis_tibble[week, "male_population_in_control"] <-
    current_week_control_male_population_counter
  analysis_tibble[week, "female_population_in_control"] <-
    current_week_control_female_population_counter
  analysis_tibble[week, "other_population_in_control"] <-
    current_week_control_other_population_counter
  analysis_tibble[week, "undisclosed_population_in_control"] <-
    current_week_control_undisclosed_population_counter
  analysis_tibble[week, "male_population_in_treatment"] <-
    current_week_treatment_male_population_counter
  analysis_tibble[week, "female_population_in_treatment"] <-
    current_week_treatment_female_population_counter
  analysis_tibble[week, "other_population_in_treatment"] <-
    current_week_treatment_other_population_counter
  analysis_tibble[week, "undisclosed_population_in_treatment"] <-
    current_week_treatment_undisclosed_population_counter
  # And finally we divide the treatment/control opened by the treatment/control 
  # populations (respectively) and store the result in our tibble
  analysis_tibble[week, "treatment_group_open_rate"] <-
    current_week_treatment_opened_counter / 
    analysis_tibble[week, "treatment_group_population"]
  analysis_tibble[week, "male_open_rate_in_treatment"] <-
    current_week_treatment_male_opened_counter/
    analysis_tibble[week, "male_population_in_treatment"]
  analysis_tibble[week, "female_open_rate_in_treatment"] <-
    current_week_treatment_female_opened_counter/
    analysis_tibble[week, "female_population_in_treatment"]
  analysis_tibble[week, "other_open_rate_in_treatment"] <-
    current_week_treatment_other_opened_counter/
    analysis_tibble[week, "other_population_in_treatment"]
  analysis_tibble[week, "undisclosed_open_rate_in_treatment"] <-
    current_week_treatment_undisclosed_opened_counter/
    analysis_tibble[week, "undisclosed_population_in_treatment"]
  analysis_tibble[week, "control_group_open_rate"] <-
    current_week_control_opened_counter/ 
    analysis_tibble[week, "control_group_population"]
  analysis_tibble[week, "male_open_rate_in_control"] <-
    current_week_control_male_opened_counter/
    analysis_tibble[week, "male_population_in_control"]
  analysis_tibble[week, "female_open_rate_in_control"] <-
    current_week_control_female_opened_counter/
    analysis_tibble[week, "female_population_in_control"]
  analysis_tibble[week, "other_open_rate_in_control"] <-
    current_week_control_other_opened_counter/
    analysis_tibble[week, "other_population_in_control"]
  analysis_tibble[week, "undisclosed_open_rate_in_control"] <-
    current_week_control_undisclosed_opened_counter/
    analysis_tibble[week, "undisclosed_population_in_control"]
}

# Now, to get the total population we simply sum the weekly population 
# column
total_population <- sum(pull(analysis_tibble, total_population))
# And for the total open rate, we simply divide the total opened counter by the 
# total population
total_open_rate <- total_opened_counter / total_population

# And finally, we do the same for the treatment and control groups
total_control_population <- sum(pull(analysis_tibble, control_group_population))
total_control_open_rate <- 
  total_control_opened_counter / total_control_population
total_treatment_population <- 
  sum(pull(analysis_tibble, treatment_group_population))
total_treatment_open_rate <- 
  total_treatment_opened_counter / total_treatment_population
