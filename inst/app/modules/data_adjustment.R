dataAjustmentUI <- function(id) {
  ns <- NS(id)

  fluidRow(

    box(
      title = 'Adjust',
      status = 'danger',
      width = 6,
      solidHeader = TRUE,

      fluidRow(
        column(12, tags$p(
          "Adjusting the data applies custom corrections, such as removing specific
          years and scaling selected indicators. This process modifies the current
          dataset to align with predefined criteria for completeness and consistency.",
          style = "color: red; font-weight: bold; margin-bottom: 15px;"
        ))
      ),

      fluidRow(
        column(8, offset = 2, actionButton(ns('adjust_data'),
                                           label = 'Adjust Data',
                                           icon = icon('wrench'),
                                           style = 'background-color: #FFEB3B;font-weight: 500;width:100%; margin-top: 15px;'))
      ),

      fluidRow(
        column(12, offset = 2, uiOutput(ns("adjust_feedback")), style = 'margin-top: 10px;')
      ),

      fluidRow(
        column(8, offset = 2, downloadButton(ns('download_data'),
                                             label = "Download Adjusted Dataset",
                                             style = "color:#2196F3;width:100%;margin-top:20px;")
        )
      )
    ),

    box(
      title = 'K-Factors',
      status = 'success',
      solidHeader = TRUE,
      width = 6,
      fluidRow(
        column(4, selectizeInput(ns('k_anc'),
                                 selected = '0.25',
                                 label = 'ANC K-Factor',
                                 choices = c(0, 0.25, 0.75))),
        column(4, selectizeInput(ns('k_delivery'),
                                 label = 'Delivery K-Factor',
                                 selected = '0.25',
                                 choices = c(0, 0.25, 0.75))),
        column(4, selectizeInput(ns('k_vaccines'),
                                 label = 'Vaccines K-Factor',
                                 selected = '0.25',
                                 choices = c(0, 0.25, 0.75)))
      )
    ),

    tabBox(
      title = 'Visualize effects of changes',
      id = 'visualize_changes',
      width = 12,

      tabPanel(
        title = 'Live Births',
        fluidRow(
          column(12, plotOutput(ns('live_births'))),
          column(3, downloadButton(ns('live_births_plot'), label = 'Download Plot', style = 'color:#2196F3;width:100%;margin-top:10px;'))
        )
      ),

      tabPanel(
        title = 'Penta 1',
        fluidRow(
          column(12, plotOutput(ns('penta1'))),
          column(3, downloadButton(ns('penta1_plot'), label = 'Download Plot', style = 'color:#2196F3;width:100%;margin-top:10px;'))
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
        title = 'Measles',
        fluidRow(
          column(12, plotOutput(ns('measles1'))),
          column(3, downloadButton(ns('measles1_plot'), label = 'Download Plot', style = 'color:#2196F3;width:100%;margin-top:10px;'))
        )
      ),

      tabPanel(
        title = 'Custom Check',
        fluidRow(
          column(3, selectizeInput(ns('indicator'), label = 'Indicator', choices = NULL))
        ),
        fluidRow(
          column(12, plotOutput(ns('custom_check'))),
          column(3, downloadButton(ns('custom_check_plot'), label = 'Download Plot', style = 'color:#2196F3;width:100%;margin-top:10px;'))
        )
      ),
    )
  )
}

dataAdjustmentServer <- function(id, data) {
  stopifnot(is.reactive(data))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      adjustment_status <- reactiveVal(list(message = "Dataset not adjusted", color = "gray"))

      k_factors <- reactive({
        c(
          anc = as.integer(input$k_anc),
          idelv = as.integer(input$k_delivery),
          vacc = as.integer(input$k_vaccines)
        )
      })

      adjustments <- reactive({
        req(data())

       data() %>%
          generate_adjustment_values(adjustment = 'custom', k_factors = k_factors())
      })

      observe({
        req(data())

        indicator_groups <- attr(data(), 'indicator_groups')
        all_indicators <- list_c(indicator_groups)

        names(all_indicators) <- all_indicators
        all_indicators <- c('Select' = '0', all_indicators)

        updateSelectInput(session, 'indicator', choices = all_indicators)
      })

      output$adjust_feedback <- renderUI({
        status <- adjustment_status()
        tags$div(
          style = paste("color:", status$color, "; font-weight: bold;"),
          status$message
        )
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

      observeEvent(input$adjust_data, {

        req(data())

        new_data <- data() %>%
          adjust_service_data(adjustment = 'custom',
                              k_factors = k_factors())
        adjustment_status(list(
          message = 'Data Adjusted',
          color = "darkgreen"
        ))

        modified_data(new_data)
      })

      output$download_data <- downloadHandler(
        filename = function() { paste0("master_adj_dataset", Sys.Date(), ".dta") },
        content = function(file) {
          req(modified_data()) # Ensure data is available
          haven::write_dta(modified_data(), file)
        }
      )

      output$live_births_plot <- downloadHandler(
        filename = function() { paste0("live_births_plot_", Sys.Date(), ".png") },
        content = function(file) {
          plot(adjustments(),
               indicator = 'ideliv',
               title = 'Figure 1c: Comparison of number of live births before and after adjustments for completness and outliers')
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        }
      )

      output$penta1_plot <- downloadHandler(
        filename = function() { paste0("penta1_plot_", Sys.Date(), ".png") },
        content = function(file) {
          plot(adjustments(),
               indicator = 'penta1',
               title = 'Figure 1d: Comparison of number of penta1 vaccination before and after adjustments for completness and outliers')
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        }
      )

      output$bcg_plot <- downloadHandler(
        filename = function() { paste0("bcg_plot_", Sys.Date(), ".png") },
        content = function(file) {
          plot(adjustments(),
               indicator = 'bcg',
               title = 'Figure 1e: Comparison of number of BCG vaccination before and after adjustments for completness and outliers')
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        }
      )

      output$measles1_plot <- downloadHandler(
        filename = function() { paste0("measles1_plot_", Sys.Date(), ".png") },
        content = function(file) {
          plot(adjustments(),
               indicator = 'measles1',
               title = 'Figure 1f: Comparison of number of measles vaccination before and after adjustments for completness and outliers')
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        }
      )

      output$custom_check_plot <- downloadHandler(
        filename = function() { paste0("custom_check_plot_", Sys.Date(), ".png") },
        content = function(file) {
          plot(adjustments(),
               indicator = input$indicator)
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        }
      )

      return(reactive({
        if (is.null(modified_data())) data() else modified_data()
      }))
    }
  )
}
