update_survey_estimates <- function(cache, new_values) {
  # Retrieve current estimates
  estimates <- cache$get_survey_estimates()

  # Merge new values with existing estimates
  updated_estimates <- c(estimates, new_values)

  # Save back to the cache
  cache$set_survey_estimates(updated_estimates)
}
