messageBoxUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns('message_box'))
}

messageBoxServer <- function(id, default_message = 'Awaiting file upload...') {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      status_data <- reactiveVal(list(message = default_message, status = 'info'))

      # Define colors based on status
      status_colors <- list(
        info = 'gray',       # Default state
        success = 'darkgreen',
        error = 'red',
        warning = 'orange'
      )

      # Render the message box UI based on current status
      output$message_box <- renderUI({
        status <- status_data()
        message <- status$message
        color <- status_colors[[status$status]]  # Get color based on status

        tags$div(
          style = paste(
            'color:', color, '; font-weight: bold;',
            'border: 1px solid ', color, ';',
            'padding: 10px; margin-top: 10px; border-radius: 5px; border-color', color, ';'
          ),
          message
        )
      })

      update_message <- function(message, status) {
        if (!status %in% names(status_colors)) {
          stop("Invalid status. Choose from 'info', 'success', 'error', or 'warning'.")
        }

        status_data(list(message = message, status = status))
      }

      return(list(
        update_message = update_message
      ))
    })
}
