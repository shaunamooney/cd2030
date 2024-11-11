# increase the uploading file size limit to 2000M, now our upload is not just about movebank file, it also include the saved data.
options(shiny.maxRequestSize = 2000*1024^2)
# options(shiny.trace = TRUE)
# options(shiny.trace = FALSE)

library(shiny)
library(shinydashboard)
library(dplyr)
library(echarts4r)
library(gt)
library(purrr)

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


ui <- dashboardPage(
  skin = 'green',
  header = dashboardHeader(
    title = 'cd2030',
    dropdownMenuOutput('messageMenu')
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = 'tabs',
      menuItem('Introduction', tabName = 'introduction', icon = icon('question-circle')),
      menuItem('Setup', tabName = 'setup', icon = icon('cog')),
      menuItem('Load Data', tabName = 'upload_data', icon = icon('folder-open')),
      menuItem('Quality Checks',
               tabName = 'quality_checks',
               icon = icon('globe'),
               menuSubItem('Reporting Rate',
                           tabName = 'reporting_rate',
                           icon = icon('chart-area')),
               menuSubItem('Data Completeness',
                           tabName = 'data_completeness',
                           icon = icon('clone')),
               menuSubItem('Consistency Check',
                           tabName = 'consistency_check',
                           icon = icon('hourglass-start')),
               menuSubItem('Outlier Detection',
                           tabName = 'outlier_detection',
                           icon = icon('paw')),
               menuSubItem('Calculate Ratios',
                           tabName = 'calculate_ratios',
                           icon = icon('chart-pie')),
               menuSubItem('Overall Score',
                           tabName = 'overall_score',
                           icon = icon('exchange-alt'))
               ),
      menuItem('Data Adjustment', tabName = 'data_adjustment', icon = icon('filter')),
      menuItem('Denominator Assessment', tabName = 'denominator_assessment', icon = icon('tasks')),
      menuItem('National Coverage', tabName = 'national_coverage', icon = icon('globe')),
      menuItem('Sub-National Coverage', tabName = 'subnational_coverage', icon = icon('globe'))
    )
  ),
  body = dashboardBody(
    tabItems(
      tabItem(tabName = 'introduction', introductionUI('introduction')),
      tabItem(tabName = 'setup', setupUI('setup')),
      tabItem(tabName = 'upload_data', uploadDataUI('upload_data')),
      tabItem(tabName = 'reporting_rate', reportingRateUI('reporting_rate')),
      tabItem(tabName = 'data_completeness', dataCompletenessUI('data_completeness')),
      tabItem(tabName = 'consistency_check', consistencyCheckUI('consistency_check')),
      tabItem(tabName = 'outlier_detection', outlierDetectionUI('outlier_detection')),
      tabItem(tabName = 'calculate_ratios', calculateRatiosUI('calculate_ratios')),
      tabItem(tabName = 'overall_score', overallScoreUI('overall_score')),
      tabItem(tabName = 'data_adjustment', dataAjustmentUI('data_adjustment')),
      tabItem(tabName = 'denominator_assessment', denominatorAssessmentUI('denominator_assessment')),
      tabItem(tabName = 'national_coverage', nationalCoverageUI('national_coverage')),
      tabItem(tabName = 'subnational_coverage', subnationalCoverageUI('subnational_coverage'))
    )
  )
)

server <- function(input, output, session) {

  introductionServer('introduction')
  national_values <- setupServer('setup')
  data <- uploadDataServer('upload_data')

  observeEvent(data(), {
    updateTabItems(session, "tabs", "reporting_rate")
  })

  reportingRateServer('reporting_rate', data)
  dataCompletenessServer('data_completeness', data)
  consistencyCheckServer('consistency_check', data)
  outlierDetectionServer('outlier_detection', data)
  calculateRatiosServer('calculate_ratios', data, reactive(national_values()[['surveys']]))
  overallScoreServer('overall_score', data)
  dt <- dataAdjustmentServer('data_adjustment', data)
  denominatorAssessmentServer('denominator_assessment', data, reactive(national_values()[['rates']]), reactive(national_values()[['surveys']]))
  nationalCoverageServer('national_coverage', data)
  subnationalCoverageServer('subnational_coverage', data)
}

shinyApp(ui = ui, server = server)
