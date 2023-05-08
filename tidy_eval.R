# Load the tidyverse library
library(tidyverse)

# Load the built-in mtcars dataset
data(mtcars)

# OPTION 1 --------------------------------------------------------------

# Define a function called 'grouped_mean1' that calculates the mean of a specified variable for each group
grouped_mean1 <- function(.data, .summary_var, ...) {
  # Capture the summary variable and group variables as quosures
  .summary_var <- enquo(.summary_var)
  .group_vars <- enquos(...)
  
  # Perform the group_by and summarise operations using the quosures
  .data %>%
    group_by(!!!.group_vars) %>%
    summarise(mean = mean(!!.summary_var))
}

# Call the 'grouped_mean1' function with the mtcars dataset, specifying 'mpg' as the summary variable and 'cyl' as the group variable
grouped_mean1(mtcars, mpg, cyl)

# OPTION 2 --------------------------------------------------------------

# Define a function called 'grouped_mean2' that calculates the mean of a specified variable for each group
grouped_mean2 <- function(data, .group_var, .summarise_var) {
  data %>%
    # Group the data by the specified group variable
    group_by(across({{ .group_var }})) %>% 
    # Calculate the mean of the specified summary variable for each group
    summarise(across({{ .summarise_var }}, mean, .names = "mean_{.col}"))
}

# Call the 'grouped_mean2' function with the mtcars dataset, specifying 'cyl' as the group variable and 'mpg' as the summary variable
grouped_mean2(mtcars, cyl, mpg)

# PRINT COUNTS FOR EACH UNIQUE VALUE ------------------------------------

# Loop through each variable in the mtcars dataset
for (var in names(mtcars)) {
  # Count the number of occurrences of each unique value of the variable and print the result
  mtcars %>% count(.data[[var]]) %>% print()
}

# CUSTOM SUMMARISE FUNCTION ---------------------------------------------

# Define a function called 'my_summarise' that groups the data by user-specified expressions and calculates the mean of 'mpg' and 'cyl' for each group
my_summarise <- function(.data, ...) {
  .data %>%
    group_by(...) %>%
    summarise(mpg = mean(mpg, na.rm = TRUE), cyl = mean(cyl, na.rm = TRUE))
}

# Call the 'my_summarise' function with the mtcars dataset, specifying 'gear' as the grouping variable
mtcars %>% my_summarise(gear)
