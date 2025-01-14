
# Define a function to recode values
recode_variable <- function(variable, values_mapping) {
  surveydata <<- surveydata %>%
    mutate({{ variable }} := case_when(
      # If the value of 'variable' is NA, recode it as 0
      is.na({{ variable }}) ~ 0,
      # If the value of 'variable' exists in 'values_mapping', recode it according to 'values_mapping'
      {{ variable }} %in% names(values_mapping) ~ values_mapping[{{ variable }}],
      # If none of the above conditions are met, recode the value as 0
      TRUE ~ 0
    ))
}