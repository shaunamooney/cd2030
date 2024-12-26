# increase the uploading file size limit to 2000M, now our upload is not just about hfd file, it also include the saved data.
options(shiny.maxRequestSize = 2000*1024^2)
# options(shiny.trace = TRUE)
# options(shiny.trace = FALSE)

library(shiny)
library(shinydashboard)
library(shinyFiles)
library(cd2030)
library(cli)
library(dplyr)
# library(echarts4r)
library(gt)
library(openxlsx)
library(khisr)
library(plotly)
library(purrr)
library(forcats)
library(lubridate)
library(RColorBrewer)
library(sf)
library(shinyjs)
library(stringr)

source('modules/help_button.R')
source('modules/introduction.R')
source('modules/setup.R')
source('modules/upload_data.R')
source('modules/reporting_rate.R')
source('modules/data_completeness.R')
source('modules/consistency_check.R')
source('modules/outlier_detection.R')
source('modules/calculate_ratios.R')
source('modules/overall_score.R')
source('modules/remove_years.R')
source('modules/data_adjustment.R')
source('modules/denominator_assessment.R')
source('modules/denominator_selection.R')
source('modules/national_coverage.R')
source('modules/subnational_coverage.R')
source('modules/subnational_inequality.R')
source('modules/subnational_mapping.R')
source('modules/equity.R')
source('modules/download.R')

ui <- dashboardPage(
  skin = 'green',
  header = dashboardHeader(title = "cd2030"),
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = 'tabs',
      menuItem('Introduction', tabName = 'introduction', icon = icon('info-circle')),
      menuItem('Load Data', tabName = 'upload_data', icon = icon('upload')),
      menuItem('Quality Checks',
               tabName = 'quality_checks',
               icon = icon('check-circle'),
               startExpanded  = TRUE,
               menuSubItem('Reporting Rate',
                           tabName = 'reporting_rate',
                           icon = icon('chart-bar')),
               menuSubItem('Outlier Detection',
                           tabName = 'outlier_detection',
                           icon = icon('exclamation-triangle')),
               menuSubItem('Consistency Check',
                           tabName = 'consistency_check',
                           icon = icon('tasks')),
               menuSubItem('Data Completeness',
                           tabName = 'data_completeness',
                           icon = icon('check-square')),
               menuSubItem('Calculate Ratios',
                           tabName = 'calculate_ratios',
                           icon = icon('percent')),
               menuSubItem('Overall Score',
                           tabName = 'overall_score',
                           icon = icon('star'))
               ),
      menuItem('Remove Years', tabName = 'remove_years', icon = icon('trash')),
      menuItem('Analysis Setup', tabName = 'setup', icon = icon('sliders-h')),
      menuItem('Data Adjustment', tabName = 'data_adjustment', icon = icon('adjust')),
      menuItem('Denominator Selection',
               tabName = 'denom_assess',
               icon = icon('calculator'),
               startExpanded  = TRUE,
               menuSubItem('Denominator Assessment',
                           tabName = 'denominator_assessment',
                           icon = icon('calculator')),
               menuSubItem('Denominator Selection',
                           tabName = 'denominator_selection',
                           icon = icon('filter'))
               ),
      menuItem('National Analysis',
               tabName = 'national_analysis',
               icon = icon('flag'),
               startExpanded  = TRUE,
               menuSubItem('National Coverage',
                           tabName = 'national_coverage',
                           icon = icon('map-marked-alt'))
               ),
      menuItem('Subnational Analysis',
               tabName = 'subnational_analysis',
               icon = icon('flag'),
               startExpanded  = TRUE,
               menuSubItem('Subational Coverage',
                           tabName = 'subnational_coverage',
                           icon = icon('map-marked')),
               menuSubItem('Sub-National Inequality',
                           tabName = 'subnational_inequality',
                           icon = icon('balance-scale-right')),
               menuSubItem('Sub-National Mapping',
                           tabName = 'subnational_mapping',
                           icon = icon('map'))
               ),
      menuItem('Equity Assessment', tabName = 'equity_assessment', icon = icon('balance-scale')),
      menuItem('Download Report', tabName = 'download_report', icon = icon('download'))
    )
  ),
  body = dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    tabItems(
      tabItem(tabName = 'introduction', introductionUI('introduction')),
      tabItem(tabName = 'upload_data', uploadDataUI('upload_data')),
      tabItem(tabName = 'reporting_rate', reportingRateUI('reporting_rate')),
      tabItem(tabName = 'data_completeness', dataCompletenessUI('data_completeness')),
      tabItem(tabName = 'consistency_check', consistencyCheckUI('consistency_check')),
      tabItem(tabName = 'outlier_detection', outlierDetectionUI('outlier_detection')),
      tabItem(tabName = 'calculate_ratios', calculateRatiosUI('calculate_ratios')),
      tabItem(tabName = 'overall_score', overallScoreUI('overall_score')),
      tabItem(tabName = 'remove_years', removeYearsUI('remove_years')),
      tabItem(tabName = 'data_adjustment', dataAjustmentUI('data_adjustment')),
      tabItem(tabName = 'setup', setupUI('setup')),
      tabItem(tabName = 'denominator_assessment', denominatorAssessmentUI('denominator_assessment')),
      tabItem(tabName = 'denominator_selection', denominatorSelectionUI('denominator_selection')),
      tabItem(tabName = 'national_coverage', nationalCoverageUI('national_coverage')),
      tabItem(tabName = 'subnational_coverage', subnationalCoverageUI('subnational_coverage')),
      tabItem(tabName = 'subnational_inequality', subnationalInequalityUI('subnational_inequality')),
      tabItem(tabName = 'subnational_mapping', subnationalMappingUI('subnational_mapping')),
      tabItem(tabName = 'equity_assessment', equityUI('equity_assessment'))
    )
  )
)

server <- function(input, output, session) {

  introductionServer('introduction')
  data <- uploadDataServer('upload_data')

  observeEvent(data(), {
    req(data())
    updateTabItems(session, "tabs", "reporting_rate")
  })

  reportingRateServer('reporting_rate', data)
  dataCompletenessServer('data_completeness', data)
  consistencyCheckServer('consistency_check', data)
  outlierDetectionServer('outlier_detection', data)
  survey_data <- calculateRatiosServer('calculate_ratios', data)
  overallScoreServer('overall_score', data)
  removed_dt <- removeYearsServer('remove_years', data)
  dt <- dataAdjustmentServer('data_adjustment', removed_dt)

  national_values <- setupServer('setup', data, survey_data)
  denominatorAssessmentServer('denominator_assessment', dt, national_values)
  denominatorSelectionServer('denominator_selection', dt, national_values)
  nationalCoverageServer('national_coverage', dt, national_values)
  subnationalCoverageServer('subnational_coverage', dt, national_values)
  subnationalInequalityServer('subnational_inequality', dt, national_values)
  subnationalMappingServer('subnational_mapping', dt, national_values)
  equityServer('equity_assessment', national_values)

  observeEvent(input$tabs, {
    if (input$tabs == 'download_report') {
      if (is.null(dt()) || is.null(national_values()$data$un)) {
        # Show an error dialog if data is not available
        showModal(
          modalDialog(
            title = "Error",
            "The necessary data for generating the report is not available. Please ensure that the data is uploaded and processed correctly.",
            easyClose = TRUE,
            footer = modalButton("OK")
          )
        )
      } else {
        # Generate the report if data is available
        generate_checks_report(dt(), 'report.html', un_estimates = national_values()$data$un)
      }
    }
  })

  observe({
    # Dynamic header title or content
    req(data())

    country <- attr(data(), 'country')
    header_title <- div(
      class = "navbar-header",
      h4(HTML(paste0(country, ' &mdash; Countdown Analysis')), class = 'navbar-brand')
    )

    # Dynamically update the header
    header <- htmltools::tagQuery(dashboardHeader(title = "cd2030"))
    header <- header$
      addAttrs(style = "position: relative")$ # add some styles to the header
      find(".navbar.navbar-static-top")$      # find the header right side
      append(header_title)$                     # inject dynamic content
      allTags()

    removeUI(selector = "header.main-header", immediate = TRUE)

    # Replace the header in the UI
    insertUI(
      selector = "body",
      where = "afterBegin",
      ui = header
    )

    shinyjs::addClass(selector = ".main-sidebar", class = "custom-main-sidebar")
  })
}

shinyApp(ui = ui, server = server)
