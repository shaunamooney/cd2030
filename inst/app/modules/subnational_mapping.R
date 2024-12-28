subnationalMappingUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    box(
      title = 'Analysis Options',
      status = 'success',
      width = 12,
      solidHeader = TRUE,
      fluidRow(
        column(3, selectizeInput(ns('level'), label = 'Subnational Level',
                                 choices = c('Admin Level 1' = 'adminlevel_1',
                                             'District' = 'district'))),
        column(3, selectizeInput(ns('denominator'), label = 'Denominator',
                                 choices = c('DHIS2' = 'dhis2',
                                             'ANC 1' = 'anc1',
                                             'Penta 1' = 'penta1'))),
        column(3, selectizeInput(ns('palette'), label = 'Palette', choices = NULL))
      )
    ),

    tabBox(
      id = ns('coverage'),
      title = 'Coverage/Utilization Level',
      width = 12,

      tabPanel(
        'Penta 3 Coverage',
        fluidRow(
          column(12, plotOutput(ns('penta3_coverage'))),
          column(3, downloadButtonUI(ns('penta3_download'), label = 'Download Plot')),
        )
      ),

      tabPanel(
        'Measles 1 Coverage',
        fluidRow(
          column(12, plotOutput(ns('mcv1_coverage'))),
          column(3, downloadButtonUI(ns('mcv1_download'), label = 'Download Plot')),
        )
      ),

      tabPanel(
        'Penta1 - Penta3 dropout',
        fluidRow(
          column(12, plotOutput(ns('penta13_dropout'))),
          column(3, downloadButtonUI(ns('penta13_dropout_download'), label = 'Download Plot')),
        )
      ),

      tabPanel(
        'Penta3 - MCV3 dropout',
        fluidRow(
          column(12, plotOutput(ns('penta3mcv1_dropout'))),
          column(3, downloadButtonUI(ns('penta3mcv1_droput_download'), label = 'Download Plot')),
        )
      ),

      tabPanel(
        'Custom Check',
        fluidRow(
          column(3, selectizeInput(ns('indicator'), label = 'Indicator',
                                   choices = c('Select' = '0', "anc1", "bcg", "measles2", "measles3", "opv1", "opv2", "opv3",
                                               "pcv1", "pcv2", "pcv3", "penta1", "penta2", "rota1", "rota2", "instdeliveries",
                                               "ipv1", "ipv2", "undervax", "zerodose", "dropout_measles12")))
        ),
        fluidRow(
          column(12, plotOutput(ns('custom'))),
          column(3, downloadButtonUI(ns('custom_download'), label = 'Download Plot')),
        )
      )
    )
  )
}

subnationalMappingServer <- function(id, data, national_values) {
  stopifnot(is.reactive(data))
  stopifnot(is.reactive(national_values))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      country <- reactive({
        req(data())
        attr(data(), 'country')
      })

      national_rates <- reactive({
        national_values()$rates
      })

      un_estimates <- reactive({
        national_values()$data$un
      })

      subnational_map <- reactive({
        national_values()$data$map_map
      })

      dt <- reactive({
        if (!isTruthy(data()) || !isTruthy(un_estimates()) || !isTruthy(national_rates()) ||
            !isTruthy(input$denominator) || !isTruthy(input$palette)) {
          return(NULL)
        }

        get_mapping_data(data(), un_estimates(), national_rates(), subnational_map())
      })

      observe({
        req(input$coverage, input$indicator)

        palette <- if (grepl('drop', input$coverage)) {
          c("Reds", "Purples")
        } else if (grepl('Coverage', input$coverage)) {
          c("Greens", "Blues")
        } else {
          if (grepl('drop|under|zero', input$indicator)) {
            c("Reds", "Purples")
          } else {
            c("Greens", "Blues")
          }
        }

        updateSelectizeInput(session, 'palette', choices = palette)
      })

      output$penta13_dropout <- renderPlot({
        req(dt())

        title <- paste("Distribution of Penta1 to Penta3 dropout in ", country(), "by Regions")

        render_with_error_handling({
          plot(dt(), indicator = 'dropout_penta13',
               denominator = input$denominator,
               palette = input$palette,
               title = title)
        })
      })

      output$penta3mcv1_dropout <- renderPlot({
        req(dt())

        title <- paste("Distribution of Penta1 to Measles3 dropout in ", country(), "by Regions")

        render_with_error_handling({
          plot(dt(), indicator = 'dropout_penta3mcv1',
               denominator = input$denominator,
               palette = input$palette,
               title = title)
        })
      })

      output$penta3_coverage <- renderPlot({
        req(dt())

        title <- paste("Distribution of Penta3 Coverage in ", country(), "by Regions")

        render_with_error_handling({
          plot(dt(), indicator = 'penta3',
               denominator = input$denominator,
               palette = input$palette,
               title = title)
        })
      })


      output$mcv1_coverage <- renderPlot({
        req(dt())

        title <- paste("Distribution of Measles 1 Coverage in ", country(), "by Regions")

        render_with_error_handling({
          plot(dt(), indicator = 'measles1',
               denominator = input$denominator,
               palette = input$palette,
               title = title)
        })
      })

      output$custom <- renderPlot({
        req(dt(), input$indicator != '0')

        title <- paste('Distribution of ', input$indicator,' Coverage in ', country(), 'by Regions')

        render_with_error_handling({
          plot(dt(), indicator = input$indicator,
               denominator = input$denominator,
               palette = input$palette,
               title = title)
        })

        tryCatch(
          plot(dt(), indicator = input$indicator,
               denominator = input$denominator,
               palette = input$palette,
               title = title),
          error = function(e) {
            clean_message <- cli::ansi_strip(conditionMessage(e))
            plot.new() # Start a blank plot
            text(
              x = 0.5, y = 0.5,
              labels = paste("Error:", clean_message),
              cex = 1.2, col = "red"
            )
          }
        )
      })

      downloadButtonServer(
        id = 'penta3_download',
        filename = paste0('penta3_', input$level, '_map_', input$denominator),
        extension = 'png',
        content = function(file) {
          plot(dt(), indicator = 'penta3',
               denominator = input$denominator,
               palette = input$palette,
               title = paste("Distribution of Penta3 Coverage in ", country(), "by Regions"))
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        },
        data = dt
      )

      downloadButtonServer(
        id = 'mcv1_download',
        filename = paste0('mcv1_', input$level, '_map_', input$denominator),
        extension = 'png',
        content = function(file) {
          plot(dt(), indicator = 'measles1',
               denominator = input$denominator,
               palette = input$palette,
               title = paste("Distribution of Measles 1 Coverage in ", country(), "by Regions"))
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        },
        data = dt
      )

      downloadButtonServer(
        id = 'penta13_dropout_download',
        filename = paste0('penta13_dropout_', input$level, '_map_', input$denominator),
        extension = 'png',
        content = function(file) {
          plot(dt(), indicator = 'dropout_penta13',
               denominator = input$denominator,
               palette = input$palette,
               title = paste("Distribution of Penta1 to Penta3 dropout Coverage in ", country(), "by Regions"))
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        },
        data = dt
      )

      downloadButtonServer(
        id = 'penta3mcv1_droput_download',
        filename = paste0('penta3mcv1_droput_', input$level, '_map_', input$denominator),
        extension = 'png',
        content = function(file) {
          plot(dt(), indicator = 'dropout_penta3mcv1',
               denominator = input$denominator,
               palette = input$palette,
               title = paste("Distribution of Penta1 to Measles3 dropout in ", country(), "by Regions"))
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        },
        data = dt
      )

      downloadButtonServer(
        id = 'custom_download',
        filename = paste0(input$indicator, '_', input$level, '_map_', input$denominator),
        extension = 'png',
        content = function(file) {
          plot(dt(), indicator = input$indicator,
               denominator = input$denominator,
               palette = input$palette,
               title = paste('Distribution of ', input$indicator,' Coverage in ', country(), 'by Regions'))
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        },
        data = dt
      )
    }
  )
}
