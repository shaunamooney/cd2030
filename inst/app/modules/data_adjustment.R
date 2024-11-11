dataAjustmentUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    box(
      title = 'K-Factors',
      status = 'success',
      width = 12,
      fluidRow(
        column(3, sliderInput(ns('k_anc'),
                              label = 'ANC K-Factor',
                              min = 0, max = 1, value = 0.25, step = 0.01)),
        column(3, sliderInput(ns('k_delivery'),
                              label = 'Delivery K-Factor',
                              min = 0, max = 1, value = 0.25, step = 0.01)),
        column(3, sliderInput(ns('k_vaccines'),
                              label = 'Vaccines K-Factor',
                              min = 0, max = 1, value = 0.25, step = 0.01))
      )
    ),

    box(
      title = 'Adjust',
      status = 'danger',
      width = 12,
      solidHeader = TRUE,

      fluidRow(
        column(3, offset = 0, selectizeInput(ns('year_to_remove'), label = 'Select year to remove', choices = NULL)),
        column(3, actionButton(ns('remove_year'),
                               label = 'Remove year',
                               icon = icon('scissors'),style = 'background-color: #FFEB3B;font-weight: 500;width:100%;'))
      ),

      fluidRow(
        column(3, offset = 0, actionButton(ns('adjust_data'),
                                           label = 'Adjust Data',
                                           icon = icon('wrench'),
                                           style = 'background-color: #FFEB3B;font-weight: 500;width:100%; margin-top: 5px;'))
      )
    ),

    tabBox(
      title = 'Visualize effects of changes',
      id = 'visualize_changes',
      width = 12,

      tabPanel(
        title = 'Live Births',
        fluidRow(
          column(12, plotOutput(ns('live_births')))
        )
      ),

      tabPanel(
        title = 'Penta 1',
        fluidRow(
          column(12, plotOutput(ns('penta1')))
        )
      ),

      tabPanel(
        title = 'BCG',
        fluidRow(
          column(12, plotOutput(ns('bcg')))
        )
      ),

      tabPanel(
        title = 'Measles',
        fluidRow(
          column(12, plotOutput(ns('measles1')))
        )
      )
    ),

    box(
      title = 'Custom Check',
      status = 'success',
      width = 12,
      fluidRow(
        column(3, selectizeInput(ns('indicator'), label = 'Indicator', choices = NULL))
      ),
      fluidRow(
        column(12, plotOutput(ns('custom_check')))
      )
    )
  )
}

dataAdjustmentServer <- function(id, data) {
  stopifnot(is.reactive(data))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      adjustments <- reactive({
       data() %>%
          generate_adjustment_values(adjustment = 'custom', k_factors = c(anc = input$k_anc,
                                                                          idelv = input$k_delivery,
                                                                          vacc = input$k_vaccines))
      })

      observe({

        indicator_groups <- attr(data(), 'indicator_groups')
        all_indicators <- list_c(indicator_groups)

        names(all_indicators) <- all_indicators
        all_indicators <- c('Select' = '0', all_indicators)

        updateSelectInput(session, 'indicator', choices = all_indicators)
      })

      observe({
        years <- data() %>%
          select(year) %>%
          distinct(year) %>%
          pull(year)

        names(years) <- years
        years <- c('Select' = 0, years)

        updateSelectInput(session, 'year_to_remove', choices = years)
      })

      output$live_births <- renderPlot({
        plot(adjustments(),
             indicator = 'ideliv',
             title = 'Figure 1c: Comparison of number of live births before and after adjustments for completness and outliers')
      })

      output$penta1 <- renderPlot({
        plot(adjustments(),
             indicator = 'penta1',
             title = 'Figure 1d: Comparison of number of penta1 vaccination before and after adjustments for completness and outliers')
      })

      output$bcg <- renderPlot({
        plot(adjustments(),
             indicator = 'bcg',
             title = 'Figure 1e: Comparison of number of BCG vaccination before and after adjustments for completness and outliers')
      })

      output$measles1 <- renderPlot({
        plot(adjustments(),
             indicator = 'measles1',
             title = 'Figure 1f: Comparison of number of measles vaccination before and after adjustments for completness and outliers')
      })

      output$custom_check <- renderPlot({
        req(input$indicator != '0')

        plot(adjustments(),
             indicator = input$indicator)
      })

      modified_data <- reactiveVal()

      observeEvent(input$remove_year, {
        new_data <- data() %>%
          filter_out_years(as.integer(input$year_to_remove))

        modified_data(new_data)
      })

      observeEvent(input$adjust_data, {
        new_data <- data() %>%
          adjust_service_data(adjustment = 'custom', k_factors = c(anc = input$k_anc,
                                                                   idelv = input$k_delivery,
                                                                   vacc = input$k_vaccines))
        modified_data(new_data)
      })

      return(modified_data)
    }
  )
}
