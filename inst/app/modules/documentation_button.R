documentationButtonUI <- function(id) {
  ns <- NS(id)
  actionButton(
    inputId = ns('document_page'),
    label = 'Add Notes',
    icon = icon('edit'),
    class ='btn-sm',
    style = 'color: white; background-color: #007bff;'
  )
}

documentationButtonServer <- function(id, page_name, parameters_reactive) {#, save_to_db_function) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Show modal dialog for documentation
    observeEvent(input$document_page, {
      showModal(
        modalDialog(
          title = paste('Document Page:', page_name),
          textAreaInput(
            inputId = ns('documentation_text'),
            label = 'Enter your notes or observations:',
            placeholder = 'Add documentation for this page...',
            width = '100%',
            height = '150px'
          ),
          footer = tagList(
            modalButton('Cancel'),
            actionButton(ns('save_documentation'), 'Save', class = 'btn-primary')
          ),
          size = 'm'
        )
      )
    })

    # Save documentation to database
    observeEvent(input$save_documentation, {
      req(input$documentation_text)

      # Collect parameters from the reactive source
      # parameters <- parameters_reactive()

      # Save documentation and parameters
      # save_to_db_function(list(
      #   page_name = page_name,
      #   documentation = input$documentation_text,
      #   parameters = parameters,
      #   timestamp = Sys.time()
      # ))

      # Close modal and show a success notification
      removeModal()
      showNotification('Documentation saved successfully!', type = 'message')
    })
  })
}
