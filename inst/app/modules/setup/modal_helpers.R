createMappingModal <- function(id, title, gregion, choices, type) {
  showModal(
    modalDialog(
      title = title,
      size = 'l',
      fluidPage(
        map(gregion, ~ {
          tagList(
            fluidRow(
              column(6, strong(.x)),
              column(6, selectizeInput(
                inputId = NS(id, .x),
                label = NULL,
                choices = c('Select' = "", choices),
                selected = ""
              ))
            ),
            tags$hr(style = "margin: 0 0 10px;")
          )
        })
      ),
      footer = tagList(
        actionButton(NS(id, type), "Save Mapping"),
        modalButton("Cancel")
      )
    ))
}
