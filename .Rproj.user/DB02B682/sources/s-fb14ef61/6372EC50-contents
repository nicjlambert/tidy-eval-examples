#see https://dplyr.tidyverse.org/articles/programming.html

#install.packages('tidyverse')
library(tidyverse)

data(mtcars)


# option 1 ----------------------------------------------------------------

grouped_mean1 <- function(.data, .summary_var, ...) {
  .summary_var <- enquo(.summary_var)
  .group_vars <- enquos(...)
  
  .data %>%
    group_by(!!!.group_vars) %>%
    summarise(mean = mean(!!.summary_var))
}

grouped_mean1(mtcars, mpg, cyl)


# option 2 ----------------------------------------------------------------


grouped_mean2 <- function(data, group_var, summarise_var) {
  data %>%
    group_by(across({{ group_var }})) %>% 
    #summarise("mean_{{summarise_var}}" := mean({{ summarise_var }})) %>% 
    summarise(across({{ summarise_var }}, mean, .names = "mean_{.col}"))
}

grouped_mean2(mtcars, cyl, mpg)

# When you have an env-variable that is a character vector, you need to index into the .data pronoun with [[, like

# Note that .data is not a data frame; it’s a special construct, a pronoun, that 
# allows you to access the current variables either directly, with .data$x or 
# indirectly with .data[[var]]. Don’t expect other functions to work with it.

for (var in names(mtcars)) {
  mtcars %>% count(.data[[var]]) %>% print()

}

# If you want to take an arbitrary number of user supplied expressions, use '...' 
# This is often useful when you want to give the user full control over a 
# single part of the pipeline, like a group_by() or a mutate().


my_summarise <- function(.data, ...) {
  .data %>%
    group_by(...) %>%
    summarise(mpg = mean(mpg, na.rm = TRUE), cyl = mean(cyl, na.rm = TRUE))
}

mtcars %>% my_summarise(gear)
