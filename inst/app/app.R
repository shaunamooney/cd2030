# increase the uploading file size limit to 2000M, now our upload is not just about hfd file, it also include the saved data.
options(shiny.maxRequestSize = 2000*1024^2)
# options(shiny.trace = TRUE)
# options(shiny.trace = FALSE)

library(shiny)
library(shinydashboard)
library(cd2030)
library(cli)
library(dplyr)
library(echarts4r)
library(gt)
library(openxlsx)
library(plotly)
library(purrr)
library(RColorBrewer)
library(sf)
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
source('modules/data_adjustment.R')
source('modules/denominator_assessment.R')
source('modules/national_coverage.R')
source('modules/subnational_coverage.R')
source('modules/subnational_inequality.R')


ui <- dashboardPage(
  skin = 'green',
  header = dashboardHeader(
    title = 'cd2030',
    dropdownMenuOutput('messageMenu')
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = 'tabs',
      menuItem('Introduction', tabName = 'introduction', icon = icon('info-circle')),
      menuItem('Load Data', tabName = 'upload_data', icon = icon('upload')),
      menuItem('Quality Checks',
               tabName = 'quality_checks',
               icon = icon('check-circle'),
               menuSubItem('Reporting Rate',
                           tabName = 'reporting_rate',
                           icon = icon('chart-bar')),
               menuSubItem('Data Completeness',
                           tabName = 'data_completeness',
                           icon = icon('check-square')),
               menuSubItem('Consistency Check',
                           tabName = 'consistency_check',
                           icon = icon('tasks')),
               menuSubItem('Outlier Detection',
                           tabName = 'outlier_detection',
                           icon = icon('exclamation-triangle')),
               menuSubItem('Calculate Ratios',
                           tabName = 'calculate_ratios',
                           icon = icon('percent')),
               menuSubItem('Overall Score',
                           tabName = 'overall_score',
                           icon = icon('star'))
               ),
      menuItem('Data Adjustment', tabName = 'data_adjustment', icon = icon('adjust')),
      menuItem('Analysis Setup', tabName = 'setup', icon = icon('sliders-h')),
      menuItem('Denominator Assessment', tabName = 'denominator_assessment', icon = icon('calculator')),
      menuItem('National Coverage', tabName = 'national_coverage', icon = icon('map-marked-alt')),
      menuItem('Sub-National Coverage', tabName = 'subnational_coverage', icon = icon('map')),
      menuItem('Sub-National Inequality', tabName = 'subnational_inequality', icon = icon('map'))
    )
  ),
  body = dashboardBody(
    tabItems(
      tabItem(tabName = 'introduction', introductionUI('introduction')),
      tabItem(tabName = 'upload_data', uploadDataUI('upload_data')),
      tabItem(tabName = 'reporting_rate', reportingRateUI('reporting_rate')),
      tabItem(tabName = 'data_completeness', dataCompletenessUI('data_completeness')),
      tabItem(tabName = 'consistency_check', consistencyCheckUI('consistency_check')),
      tabItem(tabName = 'outlier_detection', outlierDetectionUI('outlier_detection')),
      tabItem(tabName = 'calculate_ratios', calculateRatiosUI('calculate_ratios')),
      tabItem(tabName = 'overall_score', overallScoreUI('overall_score')),
      tabItem(tabName = 'data_adjustment', dataAjustmentUI('data_adjustment')),

      tabItem(tabName = 'setup', setupUI('setup')),
      tabItem(tabName = 'denominator_assessment', denominatorAssessmentUI('denominator_assessment')),
      tabItem(tabName = 'national_coverage', nationalCoverageUI('national_coverage')),
      tabItem(tabName = 'subnational_coverage', subnationalCoverageUI('subnational_coverage')),
      tabItem(tabName = 'subnational_inequality', subnationalInequalityUI('subnational_inequality'))
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
  dt <- dataAdjustmentServer('data_adjustment', data)

  national_values <- setupServer('setup', data, survey_data)
  denominatorAssessmentServer('denominator_assessment', dt, national_values)
  nationalCoverageServer('national_coverage', dt, national_values)
  subnationalCoverageServer('subnational_coverage', dt, national_values)
  subnationalInequalityServer('subnational_inequality', dt, national_values)
}

shinyApp(ui = ui, server = server)
