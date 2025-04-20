regionInputUI <- function(id, i18n) {
  ns <- NS(id)
  selectizeInput(
    ns('region'),
    label = i18n$t('opt_admin_level_1'),
    choices = NULL
  )
}

regionInputServer <- function(id, cache, admin_level, i18n, selected_region = NULL) {
  stopifnot(is.reactive(cache))
  stopifnot(is.reactive(admin_level))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      data <- reactive({
        req(cache())
        cache()$countdown_data
      })

      region <- reactive({
        region <- if (admin_level() == 'national') {
          NULL
        } else {
          input$region
        }
      })

      observe({
        req(data(), admin_level() %in% c('adminlevel_1', 'district'))

        admin_col <- admin_level()

        # Determine optgroup column conditionally
        use_optgroup <- admin_col == 'district'
        group_col <- if (use_optgroup) 'adminlevel_1' else NULL

        # Prepare data with value, label, and optional optgroup
        region_data <- data() %>%
          distinct(!!sym(admin_col), .keep_all = TRUE) %>%
          arrange(!!sym(admin_col)) %>%
          mutate(
            value = !!sym(admin_col),
            label = !!sym(admin_col),
            optgroup = if (use_optgroup) adminlevel_1 else NA
          ) %>%
          select(value, label, optgroup)

        # Convert to list of lists (rows)
        options_list <- region_data %>% pmap(~ list(...))

        # Unique optgroup labels (if used)
        optgroups_list <- if (use_optgroup) {
          region_data %>%
            distinct(optgroup) %>%
            drop_na() %>%
            transmute(value = optgroup, label = optgroup) %>%
            pmap(~ list(...))
        } else NULL

        # UI label
        label <- if (admin_col == 'district') i18n$t('opt_district') else i18n$t('opt_admin_level_1')

        # Update selectize input
        updateSelectizeInput(
          session,
          inputId = 'region',
          choices = NULL,
          selected = options_list[[1]]$value,
          label = label,
          options = list(
            options = options_list,
            optgroups = optgroups_list,
            valueField = 'value',
            labelField = 'label',
            optgroupField = if (use_optgroup) 'optgroup' else NULL,
            placeholder = i18n$t('msg_select_region')
          )
        )
      })

      return(region)
    }
  )
}
