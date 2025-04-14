dedupe <- function(r) {
  makeReactiveBinding("val")
  observe(val <<- r(), priority = 10)
  reactive(val)
}


nationalRatesUI <- function(id, i18n) {
  ns <- NS(id)

  box(
    title = i18n$t("title_national_rates"),
    status = 'success',
    solidHeader = TRUE,
    width = 12,
    fluidRow(
      column(3, offset = 1, numericInput(ns('neonatal_mortality_rate'), i18n$t("title_nmr"),
                                         min = 0, max = 0.05, value = 0.025, step = 0.001)),
      column(3, offset = 0, numericInput(ns('post_neonatal_mortality_rate'), i18n$t("title_pnmr"),
                                         min = 0, max = 0.05, value = 0.024, step = 0.001)),
      column(3, offset = 0, numericInput(ns('twin_rate'), i18n$t("title_twin_rate"),
                                         min = 0, max = 0.05, value = 0.015, step = 0.001))
    ),
    fluidRow(
      column(3, offset = 1, numericInput(ns('pregnancy_loss'), i18n$t("title_pregnancy_loss"),
                                         min = 0, max = 0.05, value = 0.03, step = 0.001)),
      column(3, offset = 0, numericInput(ns('stillbirth_rate'), i18n$t("title_still_birth_rate"),
                                         min = 0, max = 0.05, value = 0.02, step = 0.001)),
      column(3, offset = 0, numericInput(ns('penta1_mortality_rate'), i18n$t("title_anc1_to_penta1_mort_rate"),
                                         min = 0, max = 0.05, value = 0.025, step = 0.001))
    ),
    fluidRow(
      column(3, offset = 1, numericInput(ns('anc1_prop'), i18n$t("title_anc1_survey"),
                                         min = 0, max = 100, value = 0, step = 1)),
      column(3, offset = 0, numericInput(ns('penta1_prop'), i18n$t("title_penta1_survey"),
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
        national_estimates <- cache()$national_estimates

        # Check if any value in national_estimates is NA
        if (any(is.na(national_estimates))) {
          # Use input values (defaults already set in inputs) and save to cache
         estimates <- list(
            nmr = as.numeric(input$neonatal_mortality_rate),
            pnmr = as.numeric(input$post_neonatal_mortality_rate),
            twin_rate = as.numeric(input$twin_rate),
            preg_loss = as.numeric(input$pregnancy_loss),
            sbr = as.numeric(input$stillbirth_rate),
            penta1_mort_rate = as.numeric(input$penta1_mortality_rate)
          )

         cache()$set_national_estimates(estimates)
        }

        # Update numeric inputs with the current national estimates
        updateNumericInput(session, "neonatal_mortality_rate", value = national_estimates$nmr)
        updateNumericInput(session, "post_neonatal_mortality_rate", value = national_estimates$pnmr)
        updateNumericInput(session, "twin_rate", value = national_estimates$twin_rate)
        updateNumericInput(session, "pregnancy_loss", value = national_estimates$preg_loss)
        updateNumericInput(session, "stillbirth_rate", value = national_estimates$sbr)
        updateNumericInput(session, "penta1_mortality_rate", value = national_estimates$penta1_mort_rate)
      })

      debounced_anc1 <- debounce(reactive(input$anc1_prop), millis = 200)
      debounced_penta1 <- debounce(reactive(input$penta1_prop), millis = 200)

      observe({
        req(cache())

        estimates <- cache()$survey_estimates
        updateNumericInput(session, 'anc1_prop', value = unname(estimates['anc1']))
        updateNumericInput(session, 'penta1_prop', value = unname(estimates['penta1']))
      })

      observeEvent(c(debounced_anc1(), debounced_penta1()), {
        req(cache())

        estimates <- c(
          anc1 = as.numeric(debounced_anc1()),
          penta1 = as.numeric(debounced_penta1())
        )

        cache()$set_survey_estimates(estimates)
      })



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
