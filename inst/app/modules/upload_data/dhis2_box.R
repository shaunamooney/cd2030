dhis2BoxUI <- function(id) {
  ns <- NS(id)

  box(title = 'Import from DHIS2',
      status = 'info',
      solidHeader = TRUE,
      width = 12,
      fluidRow(
        column(4, textInput(ns('username'), label = NULL, placeholder = 'DHIS2 User Name')),
        column(3, passwordInput(ns('password'), label = NULL, placeholder = 'DHIS2 Password')),
        column(2, offset = 1, actionButton(ns('login'), 'Login',
                                           icon = icon('sign-in-alt'),
                                           style = 'background-color: #FFEB3B;font-weight: 500;width:100%;')),
        column(2, helpButtonUI(ns('dhis2_download')))
      )
  )
}

dhis2BoxServer <- function(id) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      helpButtonServer('dhis2_download', 'DHIS2 Download', 'l', '1_dhis2_login.md')
    }
  )
}
