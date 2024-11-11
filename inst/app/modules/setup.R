setupUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    box(
      title = 'Setup Survey Values',
      status = 'success',
      width = 12,
      fluidRow(
        column(2, offset = 10, helpButtonUI(ns('survey_values')))
      ),
      fluidRow(
        column(4, offset = 1, sliderInput(ns('anc1_coverage'),
                                          'ANC1 Coverage (%)',
                                          min = 0, max = 100, value = 98, step = 1)),
        column(4, offset = 0, sliderInput(ns('penta1_coverage'),
                                          'Penta1 Coverage (%)',
                                          min = 0, max = 100, value = 97, step = 1))
      ),
      fluidRow(
        column(4, offset = 1, sliderInput(ns('penta3_coverage'),
                                          'Penta3 Coverage (%)',
                                          min = 0, max = 100, value = 89, step = 1)),
        column(4, offset = 0, sliderInput(ns('opv1_coverage'),
                                          'OPV1 Coverage (%)',
                                          min = 0, max = 100, value = 97, step = 1))
      ),
      fluidRow(
        column(4, offset = 1, sliderInput(ns('opv3_coverage'),
                                          'OPV3 Coverage (%)',
                                          min = 0, max = 100, value = 78, step = 1)),
        column(4, offset = 0, sliderInput(ns('pcv1_coverage'),
                                          'PCV1 Coverage (%)',
                                          min = 0, max = 100, value = 97, step = 1))
      ),
      fluidRow(
        column(4, offset = 1, sliderInput(ns('rota1_coverage'),
                                          'Rota1 Coverage (%)',
                                          min = 0, max = 100, value = 96, step = 1))
      )
    ),

    box(
      title = 'Setup National Rates',
      status = 'success',
      width = 12,
      fluidRow(
        column(4, offset = 1, sliderInput(ns('neonatal_mortality_rate'),
                                          'Neonatal Mortality Rate (%)',
                                          min = 0, max = 5, value = 2, step = 0.1)),
        column(4, offset = 0, sliderInput(ns('post_neonatal_mortality_rate'),
                                          'Post Neonatal Mortality Rate (%)',
                                          min = 0, max = 5, value = 2.4, step = 0.1))
      ),
      fluidRow(
        column(4, offset = 1, sliderInput(ns('twin_rate'),
                                          'Twin Rate (%)',
                                          min = 0, max = 5, value = 1.5, step = 0.1)),
        column(4, offset = 0, sliderInput(ns('pregnancy_loss'),
                                          'Pregnancy Loss (%)',
                                          min = 0, max = 5, value = 3, step = 0.1))
      ),
      fluidRow(
        column(4, offset = 1, sliderInput(ns('stillbirth_rate'),
                                          'Still Birth Rate (%)',
                                          min = 0, max = 5, value = 2.5, step = 0.1)),
        column(4, offset = 0, sliderInput(ns('penta1_mortality_rate'),
                                          'ANC1 to Penta1 Mortality Rate (%)',
                                          min = 0, max = 5, value = 2.5, step = 0.1))
      )
    )
  )
}

setupServer <- function(id) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      setup_values <- reactive({

        list(
          surveys = c(anc1 = input$anc1_coverage,
                      penta1 = input$penta1_coverage,
                      penta3 = input$penta3_coverage,
                      opv1 = input$opv1_coverage,
                      opv3 = input$opv3_coverage,
                      pcv1 = input$pcv1_coverage,
                      rota1 = input$rota1_coverage),
          rates = c(nmr = input$neonatal_mortality_rate,
                    pnmr = input$post_neonatal_mortality_rate,
                    twin_rate = input$twin_rate,
                    preg_loss = input$pregnancy_loss,
                    sbr = input$stillbirth_rate,
                    penta1_mort_rate = input$penta1_mortality_rate)
        )
      })

      return(setup_values)
    }
  )
}
