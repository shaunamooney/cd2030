#' Create a CacheConnection
#'
#' Constructor function for objects of class [CacheConnection].
#'
#' @param rds_path Path to the RDS file (NULL for in-memory only).
#' @param countdown_data Countdown data (class `cd_data`).
#'
#' @return An object of class [CacheConnection]
#' @export
init_CacheConnection <- function(rds_path = NULL, countdown_data) {
  CacheConnection$new(
    rds_path = rds_path,
    countdown_data = countdown_data
  )
}

#' CacheConnection R6 Class
#'
#' An R6 class for handling RDS caching for data analysis and report generation.
#'
#' @export
CacheConnection <- R6::R6Class(
  'CacheConnection',
  public = list(

    #' Initialize the CacheConnection class
    #'
    #' @param rds_path Path to the RDS file (NULL for in-memory only).
    #' @param countdown_data Countdown data (class `cd_data`).
    initialize = function(rds_path = NULL, countdown_data) {

      check_cd_data(countdown_data)
      private$rds_path <- rds_path

      # Initialize in-memory data using the template
      private$in_memory_data <- private$.data_template
      private$in_memory_data$countdown_data <- countdown_data

      if (!is.null(rds_path)) {
        if (file.exists(rds_path)) {
          self$load_from_disk()
        } else {
          self$save_to_disk()
        }
      }
    },

    #' Load data from RDS file
    #' @return None
    load_from_disk = function() {
      if(!file.exists(private$rds_path)){
        cd_abort(c('x' = 'rds file does not exist'))
      }
      loaded_data <- readRDS(private$rds_path)
      required_fields <- names(private$.data_template)
      missing_fields <- setdiff(required_fields, names(loaded_data))
      if (length(missing_fields) > 0) {
        cd_abort(c('x' = paste('The following required fields are missing in the RDS file:',
                   paste(missing_fields, collapse = ', '))))
      }
      private$in_memory_data <- loaded_data
    },

    #' Save data to disk (only if changed and RDS path is not NULL)
    #' @return None
    save_to_disk = function() {
      if (private$has_changed && !is.null(private$rds_path)) { # Key change here
        saveRDS(private$in_memory_data, private$rds_path)
        private$has_changed <- FALSE
      }
    },

    #' Append a page note (with change tracking)
    #'
    #' @param page_id ID of the page
    #' @param note Note content
    #' @param title Title of the note
    #' @param items List of items
    #' @param variables List of variables
    #'
    #' @return None
    append_page_note = function(page_id, note, title, items = list(), variables = list()) {
      new_note <- tibble::tibble(
        page_id = page_id, note = note, title = title,
        items = list(items), variables = list(variables)
      )
      private$in_memory_data$page_notes <- dplyr::bind_rows(private$in_memory_data$page_notes, new_note)
      private$has_changed <- TRUE
      self$save_to_disk() # Save immediately after adding a note
    },

    #' Reactive method for the CacheConnection object
    #' @return A Shiny reactive expression tracking changes
    reactive = function() {
      # Ensure the reactive stuff is initialized.
      if (is.null(private$reactiveDep)) {
        private$reactiveDep <- reactiveValues()  # Initialize as an empty reactiveValues
        for (field_name in names(private$.data_template)) {
          private$reactiveDep[[field_name]] <- 0  # Create a reactive tracker for each field
        }
      }
      reactive({
        private$depend_all()
        self
      })
    },

    #' Gets for count down data
    #' @return a tibble of class `cd_data`.
    get_data = function() {
      private$depend('countdown_data')
      private$in_memory_data$countdown_data
    },

    get_country = function() {
      data <- self$get_data()
      if (is.null(data)) return(NULL)

      attr(data, 'country')
    },

    get_country_iso = function() {
      data <- self$get_data()
      if (is.null(data)) return(NULL)

      attr(data, 'iso3')
    },

    get_indicator_groups = function() {
      data <- self$get_data()
      if (is.null(data)) return(NULL)

      attr(data, 'indicator_groups')
    },

    get_vaccine_indicators = function() {
      indicator_groups <- self$get_indicator_groups()
      if (is.null(indicator_groups)) return(NULL)

      indicator_groups$vacc
    },

    #' Gets for count down data with excluded years
    #' @return a tibble of class `cd_data`.
    get_data_with_excluded_years = function() {
      # private$depend('countdown_data_excluded')

      excluded_years <- self$get_excluded_years()
      self$get_data() %>%
        filter(if(length(excluded_years) > 0) !year %in% excluded_years else TRUE)
    },

    #' Gets for count down data
    #' @return a tibble of class `cd_data`.
    get_adjusted_data = function() {
      # private$depend('countdown_data')
      if (self$get_adjusted_flag() && !is.null(private$adjusted_data)) {
        return(private$adjusted_data)
      }
      self$get_data_with_excluded_years()
    },

    #' Get performance threshold
    #' @return Numeric threshold
    get_threshold = function() {
      private$depend('performance_threshold')
      private$in_memory_data$performance_threshold
    },

    #' Get excluded years
    #' @return Numeric vector of excluded years
    get_excluded_years = function() {
      private$depend('excluded_years')
      private$in_memory_data$excluded_years
    },

    #' Get k-factors
    #' @return List of k-factors
    get_k_factors = function() {
      private$depend('k_factors')
      private$in_memory_data$k_factors
    },

    #' Get adjusted flag
    #' @return Factor TRUE or FALSE
    get_adjusted_flag = function() {
      private$depend('adjusted_flag')
      private$in_memory_data$adjusted_flag
    },

    #' Get excluded years
    #' @return Numeric vector of excluded years
    get_start_survey_year = function() {
      private$depend('start_survey_year')
      private$in_memory_data$start_survey_year
    },

    #' Get survey estimates
    #' @return List of survey estimates
    get_survey_estimates = function() {
      private$depend('survey_estimates')
      private$in_memory_data$survey_estimates
    },

    #' Get national estimates
    #' @return List of national estimates
    get_national_estimates = function() {
      # private$depend('survey_estimates')
      private$depend('national_estimates')
      survey <- self$get_survey_estimates()
      c(
        private$in_memory_data$national_estimates,
        list(
          anc1 = unname(survey['anc1']),
          penta1 = unname(survey['penta1'])
        )
      )
    },

    #' Get file locations
    #' @return List of file locations
    get_file_locations = function() {
      private$depend('file_locations')
      private$in_memory_data$file_locations
    },

    #' Get mapping information
    #' @return List of mapping information
    get_mapping = function() {
      private$depend('mapping')
      private$in_memory_data$mapping
    },

    #' Get mapping information
    #' @return List of mapping information
    get_un_estimates = function() {
      private$depend('un_estimates')
      private$in_memory_data$un_estimates
    },

    #' Get mapping information
    #' @return List of mapping information
    get_wuenic_estimates = function() {
      private$depend('wuenic_estimates')
      private$in_memory_data$wuenic_estimates
    },

    #' Get mapping information
    #' @return List of mapping information
    get_national_survey = function() {
      private$depend('national_survey')
      private$in_memory_data$national_survey
    },

    #' Get mapping information
    #' @return List of mapping information
    get_regional_survey = function() {
      private$depend('regional_survey')
      private$in_memory_data$regional_survey
    },

    #' Get mapping information
    #' @return List of mapping information
    get_wiq_survey = function() {
      private$depend('wiq_survey')
      private$in_memory_data$wiq_survey
    },

    #' Get mapping information
    #' @return List of mapping information
    get_area_survey = function() {
      private$depend('area_survey')
      private$in_memory_data$area_survey
    },

    #' Get mapping information
    #' @return List of mapping information
    get_education_survey = function() {
      private$depend('education_survey')
      private$in_memory_data$education_survey
    },

    #' Get mapping information
    #' @return List of mapping information
    get_map_mapping = function() {
      private$depend('map_mapping')
      private$in_memory_data$map_mapping
    },

    #' Get mapping information
    #' @return List of mapping information
    get_survey_mapping = function() {
      private$depend('survey_mapping')
      private$in_memory_data$survey_mapping
    },

    #' Get page notes
    #' @return Tibble of page notes
    get_page_notes = function() {
      private$depend('page_notes')
      private$in_memory_data$page_notes
    },

    #' Set countdown data
    #' @param value Countdown data to set
    #' @return None
    set_data = function(value) {
      check_cd_data(value)
      if (get_)
      private$update_field('countdown_data', value)
    },

    #' Set adjusted countdown data
    #' @param value Countdown data to set
    #' @return None
    set_adjusted_data = function(value) {
      check_cd_data(value)
      private$adjusted_data <- value
      # private$update_field('countdown_data', value)
    },

    #' Set performance threshold
    #' @param value Numeric threshold to set
    #' @return None
    set_threshold = function(value) {
      if (!is.numeric(value) || length(value) != 1) {
        cd_abort(c('x' = 'Performance threshold must be a single numeric value.'))
      }
      private$update_field('performance_threshold', value)
    },

    #' Set excluded years
    #' @param value Numeric vector of years to exclude
    #' @return None
    set_excluded_years = function(value) {
      if (!is.numeric(value)) {
        cd_abort(c('x' = 'Excluded years must be numeric.'))
      }
      private$update_field('excluded_years', value)
    },

    #' Set k-factors
    #' @param value List of k-factors to set
    #' @return None
    set_k_factors = function(value) {
      if (!is.numeric(value) || !all(c('anc', 'idelv', 'vacc') %in% names(value))) {
        cd_abort(c('x' = 'K factors must be a numeric vector containing {.val anc}, {.val delivery}, and {.val vaccine}.'))
      }
      private$update_field('k_factors', value)
    },

    #' Toggle adjusted flag
    #' @param adjusted Logical value for adjusted flag
    #' @return None
    toggle_adjusted_flag = function(adjusted) {
      if (!is.logical(adjusted) || length(adjusted) != 1) {
        cd_abort(c('x' = 'Adjusted flag must be a scalar logical value.'))
      }
      private$update_field('adjusted_flag', adjusted)
    },

    #' Set excluded years
    #' @param value Numeric vector of years to exclude
    #' @return None
    set_start_survey_year = function(value) {
      if (!is.numeric(value) || length(value) != 1) {
        cd_abort(c('x' = 'Start survey years must be a scalar numeric.'))
      }
      private$update_field('start_survey_year', value)
    },

    #' Set survey estimates
    #' @param value List of survey estimates
    #' @return None
    set_survey_estimates = function(value) {
      if (!is.numeric(value) || !all(c('anc1', 'penta1', 'penta3') %in% names(value))) {
        cd_abort(c('x' = 'Survey must be a numeric vector containing {.val anc1}, {.val penta1} and {.val penta3}'))
      }
      private$update_field('survey_estimates', value)
    },

    #' Set national estimates
    #' @param value List of national estimates
    #' @return None
    set_national_estimates = function(value) {
      if (!is.list(value)) {
        cd_abort(c('x' = 'Analysis values must be a list.'))
      }
      private$update_field('national_estimates', value)
    },

    #' Set file locations
    #' @param value List of file locations
    #' @return None
    set_file_locations = function(value) {
      if (!is.list(value)) {
        cd_abort(c('x' = 'File locations must be a list.'))
      }
      private$update_field('file_locations', value)
    },

    #' Set UN estimates information
    #' @param value A data frame of mapping information
    #' @return None
    set_un_estimates = function(value) {
      check_un_estimates_data(value)
      private$update_field('un_estimates', value)
    },

    #' Set WUENIC information
    #' @param value A data frame of mapping information
    #' @return None
    set_wuenic_estimates = function(value) {
      check_wuenic_data(value)
      private$update_field('wuenic_estimates', value)
    },

    #' Set national survey information
    #' @param value A data frame of mapping information
    #' @return None
    set_national_survey = function(value) {
      check_survey_data(value)
      private$update_field('national_survey', value)
    },

    #' Set subnational survey mapping information
    #' @param value A data frame of mapping information
    #' @return None
    set_regional_survey = function(value) {
      check_survey_data(value, admin_level = 'adminlevel_1')
      private$update_field('regional_survey', value)
    },

    #' Set Wealth quintile information
    #' @param value A data frame of mapping information
    #' @return None
    set_wiq_survey = function(value) {
      check_equity_data(value)
      private$update_field('wiq_survey', value)
    },

    #' Set survey mapping information
    #' @param value A data frame of mapping information
    #' @return None
    set_area_survey = function(value) {
      check_equity_data(value)
      private$update_field('area_survey', value)
    },

    #' Set survey mapping information
    #' @param value A data frame of mapping information
    #' @return None
    set_education_survey = function(value) {
      check_equity_data(value)
      private$update_field('education_survey', value)
    },

    #' Set survey mapping information
    #' @param value A data frame of mapping information
    #' @return None
    set_survey_mapping = function(value) {
      if (!is.data.frame(value)) {
        cd_abort(c('x' = 'Mapping must be a dataframe'))
      }
      private$update_field('survey_mapping', value)
    },

    #' Set map mapping information
    #' @param value List of mapping information
    #' @return None
    set_map_mapping = function(value) {
      if (!is.data.frame(value)) {
        cd_abort(c('x' = 'Mapping must be a dataframe'))
      }
      private$update_field('map_mapping', value)
    }
  ),
  private = list(
    .data_template = list(
      countdown_data = NULL,
      performance_threshold = 90,
      excluded_years = numeric(),
      k_factors = c(anc = NA_real_, idelv = NA_real_, vacc = NA_real_),
      adjusted_flag = FALSE,
      survey_estimates = c(anc1 = 98, penta1 = 97, penta3 = 89),
      national_estimates  = list(
        nmr = NA_real_, pnmr = NA_real_, twin_rate = NA_real_,preg_loss = NA_real_,
        sbr = NA_real_, penta1_mort_rate = NA_real_
      ),
      file_locations = list(
        un_estimate = NULL, wuenic_estimate = NULL, survey_folder = NULL
      ),
      start_survey_year = NULL,
      un_estimates = NULL,
      wuenic_estimates = NULL,
      national_survey = NULL,
      regional_survey = NULL,
      wiq_survey = NULL,
      area_survey = NULL,
      education_survey = NULL,
      survey_mapping = NULL,
      map_mapping = NULL,
      page_notes = tibble::tibble(
        page_id = character(), note = character(), title = character(),
        items = list(), variables = list()
      )
    ),
    rds_path = NULL,
    in_memory_data = NULL,
    adjusted_data = NULL,
    has_changed = FALSE,
    reactiveDep = NULL,
    #' Update a field (with change tracking)
    update_field = function(field_name, value) {
      if (!identical(private$in_memory_data[[field_name]], value)) {
        private$in_memory_data[[field_name]] <- value
        private$has_changed <- TRUE
        private$trigger(field_name)
      }
    },
    depend = function(field_name) {
      if (!is.null(private$reactiveDep[[field_name]])) {
        private$reactiveDep[[field_name]]
      }
      invisible()
    },
    trigger = function(field_name) {
      if (!is.null(private$reactiveDep[[field_name]])) {
        private$reactiveDep[[field_name]] <- isolate(private$reactiveDep[[field_name]] + 1)
      }
    },
    depend_all = function() {
      for (field_name in names(private$.data_template)) {
        private$depend(field_name)  # Establish dependency for each field
      }
      invisible()
    }
  )
)
