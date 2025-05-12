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
                                         min = 0, max = 0.05, value = 0, step = 0.001)),
      column(3, offset = 0, numericInput(ns('post_neonatal_mortality_rate'), i18n$t("title_pnmr"),
                                         min = 0, max = 0.05, value = 0, step = 0.001)),
      column(3, offset = 0, numericInput(ns('stillbirth_rate'), i18n$t("title_still_birth_rate"),
                                         min = 0, max = 0.05, value = 0, step = 0.001))
    ),
    fluidRow(
      column(3, offset = 1, numericInput(ns('pregnancy_loss'), i18n$t("title_pregnancy_loss"),
                                         min = 0, max = 0.05, value = 0, step = 0.001)),
      column(3, offset = 0, numericInput(ns('twin_rate'), i18n$t("title_twin_rate"),
                                         min = 0, max = 0.05, value = 0, step = 0.001))
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
        # if (any(is.na(national_estimates))) {
        #   # Use input values (defaults already set in inputs) and save to cache
        #  default_national_estimates <- cache()$default_national_estimates
        #
        #  estimates <- list(
        #    sbr = default_national_estimates$sbr,
        #     nmr = default_national_estimates$nmr,
        #     pnmr = default_national_estimates$pnmr,
        #     twin_rate = as.numeric(input$twin_rate),
        #     preg_loss = as.numeric(input$pregnancy_loss)
        #   )
        #
        #  cache()$set_national_estimates(estimates)
        # }

        # Update numeric inputs with the current national estimates
        updateNumericInput(session, "neonatal_mortality_rate", value = national_estimates$nmr)
        updateNumericInput(session, "post_neonatal_mortality_rate", value = national_estimates$pnmr)
        updateNumericInput(session, "twin_rate", value = national_estimates$twin_rate)
        updateNumericInput(session, "pregnancy_loss", value = national_estimates$preg_loss)
        updateNumericInput(session, "stillbirth_rate", value = national_estimates$sbr)

      })

      observeEvent(c(input$neonatal_mortality_rate, input$post_neonatal_mortality_rate, input$stillbirth_rate, input$pregnancy_loss, input$twin_rate), {
        req(cache())

        estimates <- list(
          sbr = input$stillbirth_rate,
          nmr = input$neonatal_mortality_rate,
          pnmr = input$post_neonatal_mortality_rate,
          twin_rate = input$twin_rate,
          preg_loss = input$pregnancy_loss
        )

         cache()$set_national_estimates(estimates)
      })
    }
  )
}
