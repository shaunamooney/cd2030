surveySetupUI <- function(id, i18n) {
  ns <- NS(id)
  box(
    title = i18n$t("title_survey_setup"),
    status = 'success',
    solidHeader = TRUE,
    width = 12,
    fluidRow(
      column(3, offset = 1, numericInput(ns('anc1_prop'), i18n$t("title_anc1_survey"),
                                         min = 0, max = 100, value = NA, step = 1)),
      column(3, offset = 0, numericInput(ns('penta1_prop'), i18n$t("title_penta1_survey"),
                                         min = 0, max = 100, value = NA, step = 1)),
      column(3, offset = 0, numericInput(ns('penta3_prop'), i18n$t("title_penta3_survey"),
                                         min = 0, max = 100, value = NA, step = 1))
    ),
    fluidRow(
      column(3, offset = 1, numericInput(ns('measles1_prop'), i18n$t("title_measles1_survey"),
                                         min = 0, max = 100, value = NA, step = 1)),
      column(3, offset = 0, numericInput(ns('bcg_prop'), i18n$t("title_bcg_survey"),
                                         min = 0, max = 100, value = NA, step = 1))
    ),
    fluidRow(
      column(3, offset = 1, numericInput(ns('survey_year'), i18n$t("title_survey_year"),
                                         min = 2015, max = 2030, value = NA, step = 1)),
      column(3, offset = 0, uiOutput(ns('survey_start_ui')))
    )
  )
}

surveySetupServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      observe({
        req(cache())
        estimates <- cache()$survey_estimates

        # if (any(is.na(estimates))) {
        #   default_estimates <- cache()$default_national_estimates
        #
        #   cache()$set_survey_estimates(
        #     c(anc1 = default_estimates$anc1 * 100,
        #       penta1 = default_estimates$penta1 * 100,
        #       penta3 = 89
        #     )
        #   )
        # }

        if (is.null(cache()$survey_source) || cache()$survey_source == 'ratios') {
          updateNumericInput(session, 'anc1_prop', value = unname(estimates['anc1']))
          updateNumericInput(session, 'penta1_prop', value = unname(estimates['penta1']))
          updateNumericInput(session, 'penta3_prop', value = unname(estimates['penta3']))
        }
        updateNumericInput(session, 'measles1_prop', value = unname(estimates['measles1']))
        updateNumericInput(session, 'bcg_prop', value = unname(estimates['bcg']))
      })

      observeEvent(c(input$anc1_prop, input$penta1_prop, input$penta3_prop, input$measles1_prop, input$bcg_prop), {
        req(cache())

        estimates <- cache()$survey_estimates
        new_estimates <- c(
          anc1 = as.numeric(input$anc1_prop),
          penta1 = as.numeric(input$penta1_prop),
          penta3 = as.numeric(input$penta3_prop),
          measles1 = as.numeric(input$measles1_prop),
          bcg = as.numeric(input$bcg_prop)
        )

        cache()$set_survey_estimates(new_estimates)
        cache()$set_survey_source('setup')
      })

      observeEvent(input$survey_start_year, {
        req(cache(), input$survey_start_year)
        cache()$set_start_survey_year(as.numeric(input$survey_start_year))
      })

      observe({
        req(cache())
        survey_year <- cache()$survey_year
        # max_year <- robust_max(cache()$countdown_data$year)
        #
        # if (is.null(survey_year)) {
        #   cache()$set_survey_year(max_year)
        # }
        #
        # value_year <- min(c(survey_year, max_year), na.rm = TRUE)
        updateNumericInput(session, 'survey_year', value = survey_year)
      })

      observeEvent(input$survey_year, {
        req(cache(), input$survey_year)
        cache()$set_survey_year(as.numeric(input$survey_year))
      })

      output$survey_start_ui <- renderUI({
        req(cache())
        years <- cache()$survey_years

        if (is.null(years) || length(years) == 0) return(NULL)

        selectizeInput(
          ns('survey_start_year'),
          label = i18n$t("title_survey_data_year"),
          choices = years,
          selected = cache()$start_survey_year
        )
      })
    }
  )
}
