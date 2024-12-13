denominatorSelectionUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    tabBox(
      title = 'Denominator Selection',
      id = 'denominator_selection',
      width = 12,
      tabPanel(
        title = 'Penta 3',
        fluidRow(
          column(12, plotOutput(ns('penta3'))),
          column(3, downloadButton(ns('penta3_plot'), label = 'Download Plot', style = 'color:#2196F3;width:100%;margin-top:10px;'))
        )
      ),

      tabPanel(
        title = 'Measles 1',
        fluidRow(
          column(12, plotOutput(ns('measles1'))),
          column(3, downloadButton(ns('measles1_plot'), label = 'Download Plot', style = 'color:#2196F3;width:100%;margin-top:10px;'))
        )
      ),

      tabPanel(
        title = 'BCG',
        fluidRow(
          column(12, plotOutput(ns('bcg'))),
          column(3, downloadButton(ns('bcg_plot'), label = 'Download Plot', style = 'color:#2196F3;width:100%;margin-top:10px;'))
        )
      ),

      tabPanel(
        title = 'Custom Checks',
        fluidRow(
          column(12, plotOutput(ns('custom'))),
          column(3, downloadButton(ns('custom_plot'), label = 'Download Plot', style = 'color:#2196F3;width:100%;margin-top:10px;'))
        )
      )
    )
  )
}

denominatorSelectionServer <- function(id, data, national_values) {
  stopifnot(is.reactive(data))
  stopifnot(is.reactive(national_values))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      national_rates <- reactive({
        national_values()$rates
      })

      un_estimates <- reactive({
        national_values()$data$un
      })

      indicator_coverage <- reactive({
        req(national_rates(), un_estimates())

        rates <- national_rates()

        data() %>%
          calculate_indicator_coverage(un_estimates = un_estimates(),
                                       sbr = rates$sbr,
                                       nmr = rates$nmr,
                                       pnmr = rates$pnmr,
                                       twin = rates$twin_rate,
                                       preg_loss = rates$preg_loss,
                                       anc1survey = rates$anc1,
                                       dpt1survey = rates$penta1)
      })

      output$penta3 <- renderPlot({
        plot_absolute_differences(indicator_coverage(), 'penta3')
      })

      output$measles1 <- renderPlot({
        plot_absolute_differences(indicator_coverage(), 'measles1')
      })

      output$bcg <- renderPlot({
        plot_absolute_differences(indicator_coverage(), 'bcg')
      })

      output$penta3_plot <- downloadHandler(
        filename = function() { paste0("penta3_plot_", Sys.Date(), ".png") },
        content = function(file) {
          plot_absolute_differences(indicator_coverage(), 'penta3')
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        }
      )

      output$measles1_plot <- downloadHandler(
        filename = function() { paste0("measles1_plot_", Sys.Date(), ".png") },
        content = function(file) {
          plot_absolute_differences(indicator_coverage(), 'measles1')
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        }
      )

      output$bcg_plot <- downloadHandler(
        filename = function() { paste0("bcg_plot_", Sys.Date(), ".png") },
        content = function(file) {
          plot_absolute_differences(indicator_coverage(), 'bcg')
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        }
      )

    }
  )
}
