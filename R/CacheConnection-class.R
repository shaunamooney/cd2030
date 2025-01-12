#' Create a CacheConnection
#'
#' Constructor function for objects of class [CacheConnection].
#'
#' @param rds_path Path to the RDS file (NULL for in-memory only).
#' @param countdown_data Countdown data (class `cd_data`).
#'
#' @return An object of class [CacheConnection]
#' @export
init_CacheConnection <- function(rds_path = NULL, countdown_data = NULL) {
  CacheConnection$new(
    rds_path = rds_path,
    countdown_data = countdown_data
  )
}

#' Print Notes for a Specific Page and Object
#'
#' This function retrieves notes from the cache for a given `page_id` and `object_id`,
#' filters them for those marked as `include_in_report`, and prints them to the console.
#' If no notes are found, it prompts the user to enter notes for the given `page_id` and
#' `object_id`, optionally including additional parameters.
#'
#' @param cache An object (e.g., of class `CacheConnection`) that manages cached
#'   data, including notes.
#' @param page_id A character string specifying the page identifier for which to
#'   retrieve notes.
#' @param object_id An optional character string specifying the object identifier
#'   within the page. Defaults to `NULL`.
#' @param parameters An optional named list of additional parameters to filter or
#'   describe the notes.
#'
#' @return None. The function is used for its side effects (printing notes or
#'   prompting the user).
#'
#' @keywords internal
print_notes <- function(cache, page_id, object_id = NULL, parameters = NULL) {
  include_in_report = note = NULL

  # Retrieve the notes from the cache
  page_notes <- cache$get_notes(page_id, object_id, parameters) %>%
    filter(include_in_report == TRUE, !is.null(note))

  # Check if there are any notes
  if (nrow(page_notes) > 0) {
    page_notes %>%
      pull(note) %>%
      walk(~ cat(paste0(.x, '\n\n')))
  } else {
    # Format the parameters for the message
    parameters_str <- if (!is.null(parameters)) {
      paste0(" (parameters: ", paste(names(parameters), parameters, sep = " = ", collapse = ", "), ")")
    } else {
      ""
    }

    cat(paste0("**--- Enter notes related to ", page_id, " ", object_id, parameters_str, ". ---**\n\n"))
  }
}

#' CacheConnection R6 Class
#'
#' An R6 class for handling RDS caching for data analysis and report generation.
#'
#' @keywords internal
CacheConnection <- R6::R6Class(
  'CacheConnection',
  public = list(

    #' Initialize the CacheConnection class
    #'
    #' @param rds_path Path to the RDS file (NULL for in-memory only).
    #' @param countdown_data Countdown data (class `cd_data`).
    initialize = function(rds_path = NULL, countdown_data = NULL) {

      if (is.null(rds_path) && is.null(countdown_data)) {
        cd_abort(c('x' = 'Both {.arg rds_path} and {.arg countdown_data} cannot be null.'))
      }

      if (!is.null(rds_path) && !is.null(countdown_data)) {
        cd_abort(c('x' = 'Only one can have a value: {.arg rds_path} and {.arg countdown_data}.'))
      }

      if (!is.null(countdown_data)) {
        check_cd_data(countdown_data)
      }

      # Initialize in-memory data using the template
      private$.in_memory_data <- private$.data_template
      private$.in_memory_data$countdown_data <- countdown_data
      private$.in_memory_data$rds_path <- rds_path

      if (!is.null(rds_path)) {
        self$load_from_disk()
      }
    },

    #' Load data from RDS file
    #' @return None
    load_from_disk = function() {
      check_file_path(private$.in_memory_data$rds_path)

      loaded_data <- readRDS(private$.in_memory_data$rds_path)
      required_fields <- names(private$.data_template)
      missing_fields <- setdiff(required_fields, names(loaded_data))
      if (length(missing_fields) > 0) {
        cd_abort(c('x' = paste('The following required fields are missing in the RDS file:',
                   paste(missing_fields, collapse = ', '))))
      }
      private$.in_memory_data <- loaded_data
    },

    #' Save data to disk (only if changed and RDS path is not NULL)
    #' @return None
    save_to_disk = function() {
      if (private$.has_changed && !is.null(private$.in_memory_data$rds_path)) { # Key change here
        saveRDS(private$.in_memory_data, private$.in_memory_data$rds_path)
        private$.has_changed <- FALSE
      }
    },

    append_page_note = function(page_id, object_id, note, parameters = list(), include_in_report = FALSE, include_plot_table = FALSE, single_entry = FALSE) {
      # Validate inputs
      stopifnot(is.character(page_id), is.character(object_id), is.character(note))
      stopifnot(is.list(parameters), is.logical(include_in_report))

      # Create a new note
      new_note <- tibble(
        page_id = page_id,
        object_id = object_id,
        note = note,
        parameters = list(parameters),   # Wrap in list for storage
        include_in_report = include_in_report,  # Single logical value
        include_plot_table = include_plot_table
      )

      if (single_entry) {
        filtered_notes <- private$.in_memory_data$page_notes %>%
          filter(!(page_id == !!page_id & object_id == !!object_id))
      } else {
        filtered_notes <- private$.in_memory_data$page_notes %>%
          filter(
            !(page_id == !!page_id & object_id == !!object_id &
                map_lgl(parameters, ~ identical(.x, !!parameters)))
          )
      }

      # Append the new note to the page_notes tibble
      private$.in_memory_data$page_notes <- bind_rows(filtered_notes, new_note)

      # Mark data as changed and save to disk
      private$.has_changed <- TRUE
      self$save_to_disk()
    },

    get_notes = function(page_id, object_id = NULL, parameters = NULL) {
      private$.in_memory_data$page_notes %>%
        filter(if (is.null(!!object_id)) TRUE else object_id == !!object_id) %>%
        filter(if (is.null(!!parameters)) TRUE else map_lgl(parameters, ~ identical(.x, !!parameters)))
    },

    #' Reactive method for the CacheConnection object
    #' @return A Shiny reactive expression tracking changes
    reactive = function() {
      # Ensure the reactive stuff is initialized.
      if (is.null(private$.reactiveDep)) {
        private$.reactiveDep <- reactiveValues()  # Initialize as an empty reactiveValues
        for (field_name in names(private$.data_template)) {
          private$.reactiveDep[[field_name]] <- 0  # Create a reactive tracker for each field
        }
      }
      reactive({
        private$depend_all()
        self
      })
    },

    set_cache_path = function(value) private$setter('rds_path', value, ~ !is.null(.x) && length(.x) > 0),
    set_countdown_data = function(value) private$setter('countdown_data', value, check_cd_data),

    set_adjusted_data = function(value) {
      check_cd_data(value)
      private$.adjusted_data <- value
    },

    set_performance_threshold = function(value) private$setter('performance_threshold', value, is_scalar_integerish),
    set_excluded_years = function(value) private$setter('excluded_years', value, is.numeric),
    set_k_factors = function(value) private$setter('k_factors', value, ~ is.numeric(.x) && all(c('anc', 'idelv', 'vacc') %in% names(.x))),
    set_adjusted_flag = function(value) private$setter('adjusted_flag', value, ~ is.logical(.x) && length(.x) == 1),
    set_survey_estimates = function(value) {
      if (!is.numeric(value) || !all(c('anc1', 'penta1') %in% names(value))) {
        cd_abort(c('x' = 'Survey must be a numeric vector containing {.val anc1}, {.val penta1} and {.val penta3}'))
      }
      if (!'penta3' %in% value) {
        value['penta3'] <- private$.in_memory_data$survey_estimates['penta3']
      }
      private$update_field('survey_estimates', value)
    },

    set_national_estimates = function(value) private$setter('national_estimates', value, is.list),
    set_start_survey_year = function(value) private$setter('start_survey_year', value, is_scalar_integerish),
    set_denominator = function(value) private$setter('denominator', value, is_scalar_character),
    set_selected_admin_level_1 = function(value) private$setter('selected_admin_level_1', value, is_scalar_character),
    set_selected_district = function(value) private$setter('selected_district', value, is_scalar_character),
    set_mapping_years = function(value) private$setter('selected_mapping_years', value, is_integerish),
    set_un_estimates = function(value) private$setter('un_estimates', value, check_un_estimates_data),
    set_wuenic_estimates = function(value) private$setter('wuenic_estimates', value, check_wuenic_data),
    set_national_survey = function(value) private$setter('national_survey', value, check_survey_data),
    set_regional_survey = function(value) private$setter('regional_survey', value, check_survey_data),
    set_wiq_survey = function(value) private$setter('wiq_survey', value, check_equity_data),
    set_area_survey = function(value) private$setter('area_survey', value, check_equity_data),
    set_education_survey = function(value) private$setter('education_survey', value, check_equity_data),
    set_survey_mapping = function(value) private$setter('survey_mapping', value, is.data.frame),
    set_map_mapping = function(value) private$setter('map_mapping', value, is.data.frame)
  ),

  active = list(
    cache_path = function(value) private$getter('rds_path', value),
    countdown_data = function(value) private$getter('countdown_data', value),
    country = function(value) {
      if (missing(value)) {
        if (is.null(self$countdown_data)) return(NULL)

        return(attr(self$countdown_data, 'country'))
      }

      cd_abort(c('x' = '{.field country} is readonly.'))
    },

    country_iso = function(value) {
      if (missing(value)) {
        if (is.null(self$countdown_data)) return(NULL)

        return(attr(self$countdown_data, 'iso3'))
      }

      cd_abort(c('x' = '{.field iso3} is readonly.'))
    },

    indicator_groups = function(value) {
      if (missing(value)) {
        if (is.null(self$countdown_data)) return(NULL)

        return(attr(self$countdown_data, 'indicator_groups'))
      }
      cd_abort(c('x' = '{.field indicator_groups} is readonly.'))
    },

    vaccine_indicators = function(value) {
      if (missing(value)) {
        if (is.null(self$indicator_groups)) return(NULL)

        return(self$indicator_groups$vacc)
      }

      cd_abort(c('x' = '{.field vaccine_indicators} is readonly.'))
    },

    adjusted_data = function(value) {
      if (missing(value)) {
        if (self$adjusted_flag && !is.null(private$.adjusted_data)) {
          return(private$.adjusted_data)
        }
        return(self$data_with_excluded_years)
      }
      check_cd_data(value)
      private$.adjusted_data <- value
    },

    data_with_excluded_years = function(value) {
      if (missing(value)) {
        excluded_years <- self$excluded_years
        data <- self$countdown_data %>%
          filter(if(length(excluded_years) > 0) !year %in% excluded_years else TRUE)
        return(data)
      }

      cd_abort(c('x' = '{.field data_with_excluded_years} is readonly.'))
    },

    performance_threshold = function(value) private$getter('performance_threshold', value),
    excluded_years = function(value) private$getter('excluded_years', value),
    k_factors = function(value) private$getter('k_factors', value),
    adjusted_flag = function(value) private$getter('adjusted_flag', value),
    survey_estimates = function(value) {
      if (missing(value)) {
        private$depend('survey_estimates')
        return(private$.in_memory_data$survey_estimates)
      }

      if (!is.numeric(value) || !all(c('anc1', 'penta1') %in% names(value))) {
        cd_abort(c('x' = 'Survey must be a numeric vector containing {.val anc1}, {.val penta1} and {.val penta3}'))
      }
      if (!'penta3' %in% value) {
        value['penta3'] <- private$.in_memory_data$survey_estimates['penta3']
      }
      private$update_field('survey_estimates', value)
    },

    national_estimates = function(value) {
      if (missing(value)) {
        private$depend('survey_estimates')
        private$depend('national_estimates')
        survey <- self$survey_estimates
        return(c(
          private$.in_memory_data$national_estimates,
          list(
            anc1 = unname(survey['anc1'])/100,
            penta1 = unname(survey['penta1'])/100
          )
        ))
      }

      if (!is.list(value)) {
        cd_abort(c('x' = 'Analysis values must be a list.'))
      }
      private$update_field('national_estimates', value)
    },

    start_survey_year = function(value) private$getter('start_survey_year', value),
    denominator = function(value) private$getter('denominator', value),
    selected_admin_level_1 = function(value) private$getter('selected_admin_level_1', value),
    selected_district = function(value) private$getter('selected_district', value),
    mapping_years = function(value) private$getter('selected_mapping_years', value),
    un_estimates = function(value) private$getter('un_estimates', value),
    wuenic_estimates = function(value) private$getter('wuenic_estimates', value),
    national_survey = function(value) private$getter('national_survey', value),
    regional_survey = function(value) private$getter('regional_survey', value),
    wiq_survey = function(value) private$getter('wiq_survey', value),
    area_survey = function(value) private$getter('area_survey', value),
    education_survey = function(value) private$getter('education_survey', value),
    survey_mapping = function(value) private$getter('survey_mapping', value),
    map_mapping = function(value) private$getter('map_mapping', value)
  ),

  private = list(
    .data_template = list(
      rds_path = NULL,
      countdown_data = NULL,
      performance_threshold = 90,
      excluded_years = numeric(),
      k_factors = c(anc = 0, idelv = 0, vacc = 0),
      adjusted_flag = FALSE,
      survey_estimates = c(anc1 = 98, penta1 = 97, penta3 = 89),
      national_estimates  = list(
        nmr = NA_real_, pnmr = NA_real_, twin_rate = NA_real_,preg_loss = NA_real_,
        sbr = NA_real_, penta1_mort_rate = NA_real_
      ),
      start_survey_year = NULL,
      denominator = 'dhis2',
      selected_admin_level_1 = NULL,
      selected_district = NULL,
      selected_mapping_years = NULL,
      # palette = c(coverage = 'Greens', dropout = 'Reds'),
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
        page_id = character(),
        object_id = character(),
        note = character(),
        parameters = list(),
        include_in_report = logical(),
        include_plot_table = logical()
      )
    ),
    .in_memory_data = NULL,
    .adjusted_data = NULL,
    .has_changed = FALSE,
    .reactiveDep = NULL,
    #' Update a field (with change tracking)
    update_field = function(field_name, value) {
      if (!identical(private$.in_memory_data[[field_name]], value)) {
        private$.in_memory_data[[field_name]] <<- value
        private$.has_changed <<- TRUE
        private$trigger(field_name)
      }
    },
    getter = function(field_name, value) {
      if (is_missing(value)) {
        private$depend(field_name)
        return(private$.in_memory_data[[field_name]])
      }

      cd_abort(c('x' = '{.field field_name} is readonly'))
    },

    setter = function(field_name, value, validation_exp = NULL) {
      check_required(field_name)
      check_required(value)

      validate_fn <- if (!is.null(validation_exp)) {
        if (rlang::is_formula(validation_exp)) {
          rlang::as_function(validation_exp)
        } else if (rlang::is_function(validation_exp)) {
          validation_exp
        } else {
          cd_abort(c('x' = "{.arg validation} must be a function or a formula."))
        }
      } else {
        function(x) TRUE
      }
      if (!validate_fn(value)) {
        cd_abort(c('x' = 'Invalid value for field {.field field_name}.'))
      }
      private$update_field(field_name, value)
    },
    depend = function(field_name) {
      if (!is.null(private$.reactiveDep[[field_name]])) {
        private$.reactiveDep[[field_name]]
      }
      invisible()
    },
    trigger = function(field_name) {
      if (!is.null(private$.reactiveDep[[field_name]])) {
        private$.reactiveDep[[field_name]] <- isolate(private$.reactiveDep[[field_name]] + 1)
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
