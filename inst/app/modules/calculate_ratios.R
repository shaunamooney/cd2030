calculateRatiosUI <- function(id) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('ratios'), 'Ratios'),
    contentBody(
      box(
        title = 'Ratio Options',
        status = 'success',
        width = 12,
        fluidRow(
          column(3, offset = 1, numericInput(ns('anc1_coverage'),
                                            'ANC1 Coverage (%)',
                                            min = 0, max = 100, value = 98, step = 1)),
          column(3, offset = 0, numericInput(ns('penta1_coverage'),
                                            'Penta1 Coverage (%)',
                                            min = 0, max = 100, value = 97, step = 1)),
          column(3, offset = 0, numericInput(ns('penta3_coverage'),
                                            'Penta3 Coverage (%)',
                                            min = 0, max = 100, value = 89, step = 1))
        )
      ),
      box(
        title = 'Ratio Plots',
        status = 'success',
        width = 12,
        fluidRow(
          column(12, plotCustomOutput(ns('ratios_plot'))),
          column(4, downloadButtonUI(ns('ratio_plot_download')))
        )
      )
    )
  )
}

calculateRatiosServer <- function(id, cache) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      data <- reactive({
        req(cache())
        cache()$get_data()
      })

      survey_estimates <- reactive({
        req(cache())
        cache()$get_survey_estimates()
      })

      observe({
        req(cache())

        estimates <- survey_estimates()
        updateNumericInput(session, 'anc1_coverage', value = unname(estimates["anc1"]))
        updateNumericInput(session, 'penta1_coverage', value = unname(estimates["penta1"]))
        updateNumericInput(session, 'penta3_coverage', value = unname(estimates["penta3"]))
      })

      # Causing a loop the national_rates.R
      # observeEvent(c(input$anc1_coverage, input$penta1_coverage, input$penta3_coverage), {
      #   req(cache())
      #
      #     estimates <- c(
      #       anc1 = as.numeric(input$anc1_coverage),
      #       penta1 = as.numeric(input$penta1_coverage),
      #       penta3 = as.numeric(input$penta3_coverage)
      #     )
      #     cache()$set_survey_estimates(estimates)
      # })

      output$ratios_plot <- renderCustomPlot({
        req(data(), input$anc1_coverage)
        plot(calculate_ratios_summary(data(), survey_coverage = survey_estimates()))
      })

      downloadPlot(
        id = 'ratio_plot_download',
        filename = 'ratio_plot',
        data = data,
        plot_function = function() {
          plot(calculate_ratios_summary(data(), survey_coverage = survey_estimates()))
        }
      )

      contentHeaderServer(
        'ratios',
        cache = cache,
        objects = pageObjectsConfig(input),
        md_title = 'Ratios',
        md_file = '2_calculate_ratios.md'
      )
    }
  )
}
