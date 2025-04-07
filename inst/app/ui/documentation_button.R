documentationButtonUI <- function(id) {
  ns <- NS(id)
  actionButton(
    inputId = ns('document_page'),
    label = 'Add Notes',
    icon = icon('edit'),
    class ='btn bg-purple btn-flat btn-sm',
    style = 'margin-left:4px;'
  )
}

documentationButtonServer <- function(id, cache, document_objects, page_id, page_name) {
  stopifnot(is.reactive(cache))

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$document_page, {
      req(document_objects)

      objects <- names(document_objects)

      showModal(
        modalDialog(
          title = paste('Document Page:', page_name),
          fluidPage(
            fluidRow(
              column(
                6,
                selectInput(
                  inputId = ns('object_select'),
                  label = 'Select object to document:',
                  choices = c('Select Object' = '', objects)
                )
              ),
              column(
                6,
                uiOutput(ns('checkbox_container'))
              )
            ),
            tags$div(
              id = ns('pointers_display'),
              class = 'parameters-section',
              tags$h5('Interpretation Pointers:'),
              uiOutput(ns('pointers_text'))
            ),
            textAreaInput(
              inputId = ns('documentation_text'),
              label = 'Enter your notes or observations:',
              placeholder = 'Add documentation for this object...',
              width = '100%',
              height = '150px'
            ),
            tags$div(
              id = ns('parameters_display'),
              class = 'parameters-section',
              tags$h5('Associated Parameters:'),
              uiOutput(ns('parameters_text'))
            )
          ),
          footer = tagList(
            modalButton('Cancel'),
            actionButton(ns('save_documentation'), 'Save', class = 'btn-primary')
          ),
          size = 'l'
        )
      )
    })

    observeEvent(input$object_select, {
      req(input$object_select)

      selected_object <- document_objects[[input$object_select]]

      params <- map(selected_object$parameters, ~ .x())
      # params <- lapply(selected_object$parameters, function(p) p())

      output$parameters_text <- renderUI({
        tags$div(
          style = "border: 1px solid #ddd; padding: 10px; background: #f9f9f9; border-radius: 5px;",
          tags$ul(
            map(names(params), ~ {
              tags$li(
                tags$b(.x), ": ", tags$span(style = "color: #007bff;", params[[.x]])
              )
            })
          )
        )
      })

      output$pointers_text <- renderUI({
        prompts <- selected_object$prompts

        if (length(prompts) == 0) {
          tags$em("No interpretation pointers provided for this object.")
        } else {
          tags$div(
            style = "border: 1px solid #ddd; padding: 10px; background: #fefefe; border-radius: 5px;",
            tags$ul(
              map(prompts, ~ tags$li(style = "margin-bottom: 6px;", .x))
            )
          )
        }
      })

      output$checkbox_container <- renderUI({
        if (selected_object$always_include) {
          tags$div(
            tags$input(
              type = "checkbox",
              checked = "checked",
              disabled = "disabled",
              style = "margin-right: 10px;"
            ),
            tags$label("Include this note in the report (Always included)")
          )
        } else {
          tagList(
            checkboxInput(
              inputId = ns('include_in_report'),
              label = 'Include this note in the final report',
              value = TRUE
            ),
            checkboxInput(
              inputId = ns('include_plot_table'),
              label = 'Include plot/table in the page report',
              value = FALSE
            )
          )
        }
      })

      # Check if an existing note matches the parameters and pre-fill fields
      existing_notes <- cache()$get_notes(page_id, input$object_select, params)

      if (nrow(existing_notes) > 0) {
        updateTextAreaInput(session, 'documentation_text', value = existing_notes$note[1])
        updateCheckboxInput(session, 'include_in_report', value = existing_notes$include_in_report[1])
        if (!selected_object$always_include) {
          updateCheckboxInput(session, 'include_plot_table', value = existing_notes$include_plot_table[1])
        }
      } else {
        updateTextAreaInput(session, 'documentation_text', value = "")
        updateCheckboxInput(session, 'include_in_report', value = FALSE)
        if (!selected_object$always_include) {
          updateCheckboxInput(session, 'include_plot_table', value = FALSE)
        }
      }
    })

    observeEvent(input$save_documentation, {
      # Validate required inputs
      req(input$object_select, input$documentation_text)

      # Retrieve the selected object and its parameters
      selected_object <- document_objects[[input$object_select]]

      # Safely evaluate parameters
      params <- tryCatch(
        map(selected_object$parameters, ~ .x()),
        error = function(e) {
          showNotification("Error evaluating parameters. Please check your input.", type = "error")
          return(NULL)
        }
      )
      req(params)  # Stop execution if parameter evaluation fails

      # Determine whether the note should be included in the report
      include_in_report <- if (selected_object$always_include) {
        TRUE
      } else {
        input$include_in_report
      }

      include_plot_table <- if (!selected_object$always_include) {
        input$include_plot_table
      } else {
        NULL
      }

      # Save the note to the cache
      tryCatch({
        # Use append_page_note with overwrite parameter
        cache()$append_page_note(
          page_id = page_id,
          object_id = input$object_select,
          note = input$documentation_text,
          parameters = params,
          single_entry = selected_object$single_entry,  # Overwrite for single-entry objects
          include_in_report = include_in_report,
          include_plot_table = include_plot_table
        )

        # Close modal and show success notification
        removeModal()
        showNotification("Documentation saved successfully!", type = "message")
      }, error = function(e) {
        # Handle errors during the save process
        showNotification("Failed to save documentation. Please try again.", type = "error")
        message("Error saving documentation: ", clean_error_message(e))  # Log error details
      })
    })

  })
}
