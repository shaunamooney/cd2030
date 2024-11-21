subnationalCoverageUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    box(
      title = 'Level of analysis',
      status = 'success',
      width = 12,
      solidHeader = TRUE,
      fluidRow(
        column(3, selectizeInput(ns('level'), label = 'Subnational Level',
                                 choices = c('Admin Level 1' = 'admin_level_1',
                                             'District' = 'district'))),
        column(3, selectizeInput(ns('denominator'), label = 'Denominator',
                                 choices = c('DHIS2' = 'dhis2',
                                             'ANC 1' = 'anc1',
                                             'Penta 1' = 'penta1'))),
        column(3, selectizeInput(ns('year'), label = 'Year', choices = NULL)),
        column(3, selectizeInput(ns('palette'),
                                 label = 'Palette',
                                 choices = c("Reds", "Blues", "Greens", "Purples", "YlGnBu")))
      )
    ),

    tabBox(
      title = 'Coverage/Utilization Level',
      width = 12,

      tabPanel(
        'Penta1 - Penta3 Drop out',

        fluidRow(column(12, plotOutput(ns('penta13_dropout'))))
      ),

      tabPanel(
        'Penta3 - MCV3 Drop out',

        fluidRow(column(12, plotOutput(ns('penta3mcv1_dropout'))))
      ),

      tabPanel(
        'Penta 3 Coverage',

        fluidRow(column(12, plotOutput(ns('penta3_coverage'))))
      ),

      tabPanel(
        'Measles 1 Coverage',

        fluidRow(column(12, plotOutput(ns('mcv1_coverage'))))
      ),

      tabPanel(
        'Custom Check',

        fluidRow(
          column(3, selectizeInput(ns('indicator'), label = 'Indicator', choices = NULL))
        ),

        fluidRow(column(12, plotOutput(ns('custom'))))
      )
    )
  )
}

subnationalCoverageServer <- function(id, data, national_values) {
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

      country_shp <- reactive({
        req(country())

        shapefile_path <- paste0("shapefiles/", country(), "/", country(), "_admin_1.shp")
        validate(need(file.exists(shapefile_path), "Shapefile not found. Please check the country name."))
        st_read(shapefile_path)
      })

      dt <- reactive({
        req(data(), un_estimates(), national_rates(), country_shp())

        iso <-  attr(data(), 'iso3')
        rates <- national_rates()

        if (iso == 'KEN') {
          merged_data <- data() %>%
            calculate_indicator_coverage('district', un_estimates(),
                                         sbr = rates$sbr, nmr = rates$nmr,
                                         pnmr = rates$pnmr, anc1survey = rates$anc1,
                                         dpt1survey = rates$penta1, twin = rates$twin_rate,
                                         preg_loss = rates$preg_loss) %>%
            select(-adminlevel_1) %>%
            rename(
              adminlevel_1 = district
            )
        } else {
          merged_data <- data() %>%
            calculate_indicator_coverage('admin_level_1', un_estimates(),
                                         sbr = rates$sbr, nmr = rates$nmr,
                                         pnmr = rates$pnmr, anc1survey = rates$anc1,
                                         dpt1survey = rates$penta1, twin = rates$twin_rate,
                                         preg_loss = rates$preg_loss)
        }

        merged_data %>%
          left_join(country_shp(), by = c('adminlevel_1' = 'NAME_1')) %>%
          mutate(
            centroid = st_centroid(geometry),
            centroid_coords =st_coordinates(centroid)
          )
      })

      observe({
        req(data())

        indicators <-  c("bcg", "anc1", "pcv3", "opv1", "opv2", "opv3", "penta2", "pcv1", "pcv2",
                         "penta1", "rota1", "rota2", "instdeliveries", "measles2",
                         "ipv1", "ipv2", "undervax", "zerodose", "dropout_measles12")

        names(indicators) <- indicators
        indicators <- c('Select' = '0', indicators)

        years <- data() %>%
          distinct(year) %>%
          arrange(desc(year)) %>%
          pull(year)

        updateSelectizeInput(session, 'indicator', choices = indicators)
        updateSelectizeInput(session, 'year', choices = years)
      })

      output$penta13_dropout <- renderPlot({
        req(dt())

        dt() %>%
          filter(year == input$year) %>%
          st_as_sf() %>%
          ggplot() +
            geom_sf(aes(fill = !!sym(paste0('cov_dropout_penta13_', input$denominator)))) +
            scale_fill_gradientn(colors = RColorBrewer::brewer.pal(7, input$palette)) +
            labs(
              title = paste(input$year, "Distribution of Penta1 to Penta3 dropout in ", country(), "by Regions"),
              fill = paste0('cov_', 'dropout_penta13_', input$denominator),
              caption = "Data Source: DHIS-2 analysis"
            ) +
            theme_classic() +
            theme(
              legend.position = "bottom",
              plot.title = element_text(hjust = 0.5, face = "bold"),
              plot.caption = element_text(hjust = 1, face = "italic"),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              axis.line = element_blank(),
              panel.border = element_blank()
            )
      })

      output$penta3mcv1_dropout <- renderPlot({
        req(dt())

        dt() %>%
          filter(year == input$year) %>%
          st_as_sf() %>%
          ggplot() +
          geom_sf(aes(fill = !!sym(paste0('cov_dropout_penta3mcv1_', input$denominator)))) +
          scale_fill_gradientn(colors = RColorBrewer::brewer.pal(7, input$palette)) +
          labs(
            title = paste(input$year, "Distribution of Penta1 to Penta3 dropout in ", country(), "by Regions"),
            fill = paste0('cov_', 'dropout_penta3mcv1_', input$denominator),
            caption = "Data Source: DHIS-2 analysis"
          ) +
          theme_classic() +
          theme(
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.caption = element_text(hjust = 1, face = "italic"),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.line = element_blank(),
            panel.border = element_blank()
          )
      })

      output$penta3_coverage <- renderPlot({
        req(dt())

        dt() %>%
          filter(year == input$year) %>%
          st_as_sf() %>%
          ggplot() +
          geom_sf(aes(fill = !!sym(paste0('cov_penta3_', input$denominator)))) +
          scale_fill_gradientn(colors = RColorBrewer::brewer.pal(7, input$palette)) +
          labs(
            title = paste(input$year, "Distribution of Penta3 Coverage in ", country(), "by Regions"),
            fill = paste0('cov_', 'penta3_', input$denominator),
            caption = "Data Source: DHIS-2 analysis"
          ) +
          theme_classic() +
          theme(
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.caption = element_text(hjust = 1, face = "italic"),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.line = element_blank(),
            panel.border = element_blank()
          )
      })


      output$mcv1_coverage <- renderPlot({
        req(dt())

        dt() %>%
          filter(year == input$year) %>%
          st_as_sf() %>%
          ggplot() +
          geom_sf(aes(fill = !!sym(paste0('cov_measles1_', input$denominator)))) +
          scale_fill_gradientn(colors = RColorBrewer::brewer.pal(7, input$palette)) +
          labs(
            title = paste(input$year, "Distribution of Measles 1 Coverage in ", country(), "by Regions"),
            fill = paste0('cov_', 'measles1', input$denominator),
            caption = "Data Source: DHIS-2 analysis"
          ) +
          theme_classic() +
          theme(
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.caption = element_text(hjust = 1, face = "italic"),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.line = element_blank(),
            panel.border = element_blank()
          )
      })

      output$custom <- renderPlot({
        req(dt(), input$indicator != '0')

        dt() %>%
          filter(year == input$year) %>%
          st_as_sf() %>%
          ggplot() +
          geom_sf(aes(fill = !!sym(paste0('cov_', input$indicator, '_', input$denominator)))) +
          scale_fill_gradientn(colors = RColorBrewer::brewer.pal(7, input$palette)) +
          labs(
            title = paste(input$year, "Distribution of Measles 1 Coverage in ", country(), "by Regions"),
            fill = paste0('cov_', input$indicator, input$denominator),
            caption = "Data Source: DHIS-2 analysis"
          ) +
          theme_classic() +
          theme(
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.caption = element_text(hjust = 1, face = "italic"),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.line = element_blank(),
            panel.border = element_blank()
          )
      })
    }
  )
}
