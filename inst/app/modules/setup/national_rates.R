nationalRatesUI <- function(id) {
  ns <- NS(id)

  box(
    title = 'Setup National Rates',
    status = 'success',
    solidHeader = TRUE,
    width = 12,
    fluidRow(
      column(3, offset = 1, numericInput(ns('neonatal_mortality_rate'), 'Neonatal Mortality Rate',
                                         min = 0, max = 0.05, value = 0.025, step = 0.001)),
      column(3, offset = 0, numericInput(ns('post_neonatal_mortality_rate'), 'Post Neonatal Mortality Rate',
                                         min = 0, max = 0.05, value = 0.024, step = 0.001)),
      column(3, offset = 0, numericInput(ns('twin_rate'), 'Twin Rate',
                                         min = 0, max = 0.05, value = 0.015, step = 0.001))
    ),
    fluidRow(
      column(3, offset = 1, numericInput(ns('pregnancy_loss'), 'Pregnancy Loss',
                                         min = 0, max = 0.05, value = 0.03, step = 0.001)),
      column(3, offset = 0, numericInput(ns('stillbirth_rate'), 'Still Birth Rate',
                                         min = 0, max = 0.05, value = 0.02, step = 0.001)),
      column(3, offset = 0, numericInput(ns('penta1_mortality_rate'), 'ANC1 to Penta1 Mortality Rate',
                                         min = 0, max = 0.05, value = 0.025, step = 0.001))
    ),
    fluidRow(
      column(3, offset = 1, numericInput(ns('anc1_prop'), 'ANC1 Survey',
                                         min = 0, max = 100, value = 0, step = 1)),
      column(3, offset = 0, numericInput(ns('penta1_prop'), 'Penta1 Survey',
                                         min = 0, max = 100, value = 0, step = 1)),
    )
  )
}

nationalRatesServer <- function(id, cache) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      observe({
        req(cache())

        # Retrieve the national estimates from the cache
        national_estimates <- cache()$get_national_estimates()

        # Check if any value in national_estimates is NA
        if (any(is.na(national_estimates))) {
          # Use input values (defaults already set in inputs) and save to cache
          national_estimates <- list(
            nmr = as.numeric(input$neonatal_mortality_rate),
            pnmr = as.numeric(input$post_neonatal_mortality_rate),
            twin_rate = as.numeric(input$twin_rate),
            preg_loss = as.numeric(input$pregnancy_loss),
            sbr = as.numeric(input$stillbirth_rate),
            penta1_mort_rate = as.numeric(input$penta1_mortality_rate)
          )

          # Save the updated estimates to the cache
          cache()$set_national_estimates(national_estimates)
        }

        # Update numeric inputs with the current national estimates
        updateNumericInput(session, "neonatal_mortality_rate", value = national_estimates$nmr)
        updateNumericInput(session, "post_neonatal_mortality_rate", value = national_estimates$pnmr)
        updateNumericInput(session, "twin_rate", value = national_estimates$twin_rate)
        updateNumericInput(session, "pregnancy_loss", value = national_estimates$preg_loss)
        updateNumericInput(session, "stillbirth_rate", value = national_estimates$sbr)
        updateNumericInput(session, "penta1_mortality_rate", value = national_estimates$penta1_mort_rate)
        updateNumericInput(session, 'anc1_prop', value = national_estimates$anc1)
        updateNumericInput(session, 'penta1_prop', value = national_estimates$penta1)
      })

      # observeEvent(c(input$anc1_prop, input$penta1_prop), {
      #   req(cache())
      #
      #   # Retrieve the survey estimates from the cache
      #   estimates <- cache()$get_survey_estimates()
      #
      #   # Check if cached values differ from inputs before updating the cache
      #   if (#any(is.na(estimates)) ||
      #       !identical(estimates["anc1"], input$anc1_prop) ||
      #       !identical(estimates["penta1"], input$penta1_prop)) {
      #     # Update estimates using input values
      #     estimates["anc1"] <- as.numeric(input$anc1_prop)
      #     estimates["penta1"] <- as.numeric(input$penta1_prop)
      #
      #     # Save the updated estimates to the cache
      #     cache()$set_survey_estimates(estimates)
      #   }
      # })



      # observeEvent(data(), {
      #   start_year <- min(data()$year)
      #   end_year <- max(data()$year)
      #
      #   if (!is.null(un_path())) {
      #     un <- load_un_estimates(un_path(), country_iso(), start_year, end_year)
      #     un_estimates(un)
      #   }
      #
      #   if (!is.null(wuenic_path())) {
      #     wuenic <- load_wuenic_data(wuenic_path(), country_iso())
      #     wuenic_data(wuenic)
      #   }
      # })
    }
  )
}
