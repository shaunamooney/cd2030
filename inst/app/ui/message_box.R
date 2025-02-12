messageBoxUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns('message_box'))
}

messageBoxServer <- function(id, default_message = 'Awaiting file upload...', use_pre = FALSE) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      # Define colors based on status
      status_colors <- list(
        info = 'gray',       # Default state
        success = 'darkgreen',
        error = 'red',
        warning = 'orange'
      )

      # Store messages as a list. Each message is a list with "text" and "status".
      # The default is a single message.
      messages_data <- reactiveVal(list(list(text = default_message, status = "info")))

      # Render the message box UI based on current status
      output$message_box <- renderUI({
        msgs <- messages_data()

        tagList(map(msgs, ~ {
          colour <- status_colors[[.x$status]]
          tag_fun <- if (use_pre) tags$pre else tags$div
          tag_fun(
            style = paste(
              'color: ', colour, ';',
              'font-weight: bold;',
              'border: 1px solid', colour, ';',
              'padding: 10px;',
              'margin-top: 10px;',
              'border-radius: 5px'
            ),
            .x$text
          )
        }))
      })

      update_message <- function(message, status) {
        if (!status %in% names(status_colors)) {
          stop("Invalid status. Choose from 'info', 'success', 'error', or 'warning'.")
        }

        messages_data(list(list(text = message, status = status)))
      }

      add_message <- function(message, status) {
        if (!status %in% names(status_colors)) {
          stop("Invalid status. Choose from 'info', 'success', 'error', or 'warning'.")
        }
        current <- messages_data()
        new_message <- list(text = message, status = status)
        messages_data(c(current, list(new_message)))
      }

      return(list(
        update_message = update_message,
        add_message = add_message
      ))
    })
}
