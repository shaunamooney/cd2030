#' Create an Excel Sheet with Custom Styles and Data
#'
#' @param wb A workbook object created using `createWorkbook()`.
#' @param .data A data frame containing the data to be written to the Excel sheet.
#' @param sheet_name A string specifying the name of the sheet.
#' @param hfd_ids A character vector of column headers (IDs) for the sheet.
#' @param hfd_titles A character vector of column titles corresponding to `hfd_ids`.
#' @param hfd_subtitles (Optional) A character vector of subtitles for the sheet's columns.
#' @param instruction (Optional) A string specifying instructions for the sheet, written in the second row.
#' @param freeze_col (Optional) The number of columns to freeze, default is 4.
#' @param instruction_row_height (Optional) Height of the instruction row, default is 60.
#'
#' @details This function writes data to an Excel sheet with custom styles, header, and optional instructions.
#'          It supports grouped titles for specific sheets like `Reporting_completeness`.
#'          The function ensures uniform formatting and allows freezing of specific columns.
#'
#' @return Writes data to the workbook in the specified sheet.
#'
#' @noRd
create_excel_sheet <- function(wb, .data, sheet_name, hfd_ids, hfd_titles,
                               hfd_subtitles = NULL, instruction = NULL,
                               freeze_col = 4, instruction_row_height = 60) {

  check_required(wb)
  check_required(.data)
  check_required(sheet_name)
  check_required(hfd_ids)
  check_required(hfd_titles)

  if (!is.null(hfd_subtitles) && !is.null(instruction)) {
    cd_abort(
      c('x' = 'hfd_subtitle and instruction should not be provided at the same time')
    )
  }

  headerStyle <- create_custom_style(fontColour = 'red', fgFill = 'khaki1', textDecoration = 'bold',
                                     border = 'TopBottomLeftRight', borderColour = 'black')
  subHeaderStyle <- create_custom_style(fontColour = 'white', fgFill = 'darkcyan',
                                        textDecoration = 'bold', border = 'TopBottomLeftRight', borderColour = 'white')
  contentStyle <- create_custom_style(fgFill = 'gray', border = 'TopBottomLeftRight', borderColour = 'white',
                                      halign = NULL)

  # Add worksheet
  addWorksheet(wb, sheet_name)

  writeData(wb, sheet_name, t(hfd_ids), startRow = 1, colNames = FALSE)
  if (!is.null(instruction)) writeData(wb, sheet_name, instruction, startRow = 2, startCol = freeze_col)
  if (!is.null(hfd_subtitles)) writeData(wb, sheet_name, t(hfd_subtitles), startRow = 2, colNames = FALSE)
  writeData(wb, sheet_name, t(hfd_titles), startRow = 3, colNames = FALSE)
  writeData(wb, sheet_name, .data, startRow = 4, colNames = FALSE)

  # Merge cells for grouped titles (if applicable)
  if (sheet_name == 'Reporting_completeness' && !is.null(hfd_subtitles)) {
    unique_titles <- unique(hfd_subtitles[-(1:3)]) # Skip the first 3 empty cells
    pwalk(list(unique_titles), ~ {
      col_range <- which(hfd_subtitles == .x) # Get the column range for the title
      mergeCells(wb, sheet_name, cols = col_range, rows = 2) # Merge cells for the title
    })
    mergeCells(wb, sheet_name, cols = 1, rows = 2:3)
    mergeCells(wb, sheet_name, cols = 2, rows = 2:3)
    mergeCells(wb, sheet_name, cols = 3, rows = 2:3)
  }

  # Apply styles
  addStyle(wb, sheet_name, style = headerStyle, rows = 1, cols = 1:length(hfd_ids), gridExpand = TRUE)
  if (!is.null(instruction)) addStyle(wb, sheet_name, style = headerStyle, rows = 2, cols = freeze_col:length(hfd_ids), gridExpand = TRUE)
  if (!is.null(hfd_subtitles)) addStyle(wb, sheet_name, style = subHeaderStyle, rows = 2, cols = 1:length(hfd_ids), gridExpand = TRUE)
  addStyle(wb, sheet_name, style = subHeaderStyle, rows = 3, cols = 1:length(hfd_ids), gridExpand = TRUE)
  addStyle(wb, sheet_name, style = contentStyle, rows = 4:(nrow(.data) + 3), cols = 1:length(hfd_ids), gridExpand = TRUE)

  # Adjust column widths and freeze panes
  setColWidths(wb, sheet_name, cols = 1:length(hfd_ids), widths = 17.58)
  groupRows(wb, sheet_name, rows = 1, hidden = TRUE)
  freezePane(wb, sheet_name, firstActiveRow = 4, firstActiveCol = freeze_col)

  if (!is.null(instruction)) {
    mergeCells(wb, sheet_name, cols = freeze_col:length(hfd_ids), rows = 2)
    setRowHeights(wb, sheet_name, rows = 2, heights = instruction_row_height)
  }
}

#' Create a Custom Style for Excel Sheets
#'
#' @param fontSize Font size, default is 11.
#' @param fontColour Font color, default is `"black"`.
#' @param fgFill Background fill color, default is `"white"`.
#' @param halign Horizontal alignment, default is `"center"`.
#' @param valign Vertical alignment, default is `"center"`.
#' @param wrapText Boolean indicating whether text should wrap, default is `TRUE`.
#' @param textDecoration Text decoration (e.g., `"bold"`), default is `NULL`.
#' @param border Border style, default is `NULL`.
#' @param borderColour Border color, default is `NULL`.
#'
#' @return A style object created using `createStyle`.
#'
#' @noRd
create_custom_style <- function(fontSize = 11, fontColour = "black", fgFill = "white",
                                halign = "center", valign = "center", wrapText = TRUE,
                                textDecoration = NULL, border = NULL, borderColour = NULL) {
  createStyle(
    fontSize = fontSize,
    fontColour = fontColour,
    fgFill = fgFill,
    halign = halign,
    valign = valign,
    wrapText = wrapText,
    textDecoration = textDecoration,
    border = border,
    borderColour = borderColour
  )
}

#' Create Excel Sheets for Data Groups
#'
#' @param wb A workbook object created using `createWorkbook()`.
#' @param .data A data frame containing grouped data for each sheet to be created.
#' @param header_rows A character vector specifying the columns to be used as
#'   headers (e.g., `c("district", "year", "month")`).
#' @param freeze_col (Optional) The number of columns to freeze in the Excel sheet.
#'    Default is 4.
#' @param instruction (Optional) Instructions to be written in the second row of
#'   the Excel sheet.
#' @param instruction_row_height (Optional) The height of the row containing the
#'   instruction. Default is 60.
#'
#' @details
#' This function creates multiple Excel sheets based on grouped data, with each
#' group written to a separate sheet. It handles wide-format transformations,
#' dynamic header and title generation, and applies consistent styles. Special
#' handling is provided for `Reporting_completeness` sheets, where grouped titles
#' and subtitles are generated.
#'
#' @return Modifies the workbook object by adding formatted sheets for each group.
#'
#' @examples
#' \dontrun{
#'   wb <- createWorkbook()
#'   data <- your_data_frame
#'   create_sheets(wb, data, header_rows = c("district", "year", "month"))
#' }
#'
#' @noRd
create_sheets <- function(wb, .data, header_rows, freeze_col = 4,
                          instruction = NULL, instruction_row_height = 60) {

  hfd_sheet = NULL

  .data %>%
    group_by(hfd_sheet) %>%
    group_split() %>%
    walk(~ {
      # Extract sheet name
      sheet_name <- unique(.x$hfd_sheet)

      aggr_fun <- if (sheet_name == 'Admin_data') NULL else sum

      # Prepare wide-format data
      sheet_data <- .x %>%
        arrange(hfd_id) %>%
        select(all_of(header_rows), hfd_id, value) %>%
        pivot_wider(names_from = hfd_id, values_from = value, values_fn = aggr_fun, values_fill = NA) %>%
        arrange(across(all_of(header_rows)))

      column_order <- names(sheet_data)[(length(header_rows) + 1):ncol(sheet_data)]

      # Generate hfd_ids and titles
      hfd_ids <- c(header_rows, column_order)
      hfd_titles <- c(str_to_title(header_rows),
                      .x %>%
                        filter(hfd_id %in% column_order) %>%
                        distinct(hfd_id, hfd_title) %>%
                        arrange(match(hfd_id, column_order)) %>%
                        pull(hfd_title))
      if (sheet_name == 'Reporting_completeness') {
        hfd_subtitles <- hfd_titles
        hfd_titles <- c('', '', '',
                        .x %>%
                          filter(hfd_id %in% column_order) %>%
                          distinct(hfd_id, hfd_subtitle) %>%
                          arrange(match(hfd_id, column_order)) %>%
                          pull(hfd_subtitle))
      } else {
        hfd_subtitles <- NULL
      }

      # Write to Excel using the utility function
      create_excel_sheet(
        wb = wb,
        .data = sheet_data,
        sheet_name = sheet_name,
        hfd_ids = hfd_ids,
        hfd_titles = hfd_titles,
        hfd_subtitles = hfd_subtitles,
        instruction = instruction,
        freeze_col = freeze_col,
        instruction_row_height = instruction_row_height
      )
    })
}

#' Create an Administrative Data Sheet in Excel
#'
#' @description
#' This function generates a formatted Excel sheet for administrative data,
#' including health districts and their corresponding administrative units.
#' The sheet includes:
#'
#' - Column headers (`hfd_ids`).
#' - Column titles (`hfd_titles`).
#' - An instruction row to guide users on how to populate the sheet.
#'
#' @param wb A `Workbook` object created using the `openxlsx` package.
#' @param .data A data frame containing the administrative data. Expected columns include:
#'   - `district`: Health district names.
#'   - Additional columns representing administrative data.
#' @param header_rows A character vector of columns that define the structure of
#'   the data (e.g., `district`).
#' @param instruction A string containing the instruction text to be displayed in
#'   the second row of the sheet.
#'
#' @details
#' The function applies custom styles to headers, titles, and data cells for
#' consistent formatting. The `instruction` row is styled, merged across relevant
#' columns, and has an adjustable height.
#'
#' @return Adds the administrative sheet to the provided Excel workbook (`wb`).
#'
#' @examples
#' \dontrun{
#' wb <- createWorkbook()
#' admin_data <- data.frame(
#'   district = c("District A", "District B"),
#'   region = c("Region 1", "Region 2"),
#'   country = c("Country X", "Country X")
#' )
#' instruction <- "Please fill in the information for each district..."
#' create_admin_sheet(wb, admin_data, header_rows = c("district"), instruction = instruction)
#' saveWorkbook(wb, "admin_data.xlsx", overwrite = TRUE)
#' }
#'
#' @noRd
create_admin_sheet <- function(wb, .data, header_rows, instruction) {

  iso3 = district = hfd_id = hfd_title = hfd_sheet = NULL

  headerStyle <- create_custom_style(fontColour = 'red', fgFill = 'khaki1', textDecoration = 'bold',
                                     border = 'TopBottomLeftRight', borderColour = 'black')
  subHeaderStyle <- create_custom_style(fontColour = 'white', fgFill = 'darkcyan',
                                        textDecoration = 'bold', border = 'TopBottomLeftRight', borderColour = 'white')
  contentStyle <- create_custom_style(fgFill = 'gray', border = 'TopBottomLeftRight', borderColour = 'white',
                                      halign = NULL)

  sheet_name <- unique(.data$hfd_sheet)
  admin_data <- admin_data %>%
    select(-iso3) %>%
    relocate(district)

  column_order <- names(admin_data)[1:ncol(admin_data)]

  # Generate hfd_ids and titles
  hfd_ids <- c(header_rows, column_order)
  hfd_titles <- c(str_to_title(header_rows),
                  .data %>%
                    filter(hfd_id %in% column_order) %>%
                    distinct(hfd_id, hfd_title) %>%
                    arrange(match(hfd_id, column_order)) %>%
                    pull(hfd_title))

  # Add worksheet
  addWorksheet(wb, sheet_name)

  writeData(wb, sheet_name, t(hfd_ids), startRow = 1, colNames = FALSE)
  writeData(wb, sheet_name, instruction, startRow = 2, startCol = 2)
  writeData(wb, sheet_name, t(hfd_titles), startRow = 3, colNames = FALSE)
  writeData(wb, sheet_name, admin_data, startRow = 4, colNames = FALSE)

  addStyle(wb, sheet_name, style = headerStyle, rows = 1, cols = 1:length(hfd_ids), gridExpand = TRUE)
  addStyle(wb, sheet_name, style = headerStyle, rows = 2, cols = 2:length(hfd_ids), gridExpand = TRUE)
  addStyle(wb, sheet_name, style = subHeaderStyle, rows = 3, cols = 1:length(hfd_ids), gridExpand = TRUE)
  addStyle(wb, sheet_name, style = contentStyle, rows = 4:(nrow(admin_data) + 3), cols = 1:length(hfd_ids), gridExpand = TRUE)

  setColWidths(wb, sheet_name, cols = 1:length(hfd_ids), widths = 17.58)
  groupRows(wb, sheet_name, rows = 1, hidden = TRUE)
  freezePane(wb, sheet_name, firstActiveRow = 4, firstActiveCol = 2)

  if (!is.null(instruction)) {
    mergeCells(wb, sheet_name, cols = 2:length(hfd_ids), rows = 2)
    setRowHeights(wb, sheet_name, rows = 2, heights = 160)
  }
}
