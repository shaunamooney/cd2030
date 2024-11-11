introductionUI <- function(id) {
  ns <- NS(id)

  box(title = 'Introduction: Countdown to 2030',
      status = 'success',
      solidHeader = TRUE,
      width = 12,
      fluidPage(includeMarkdown('help/0_intro.md'))
  )
}

introductionServer <- function(id) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

    }
  )
}
