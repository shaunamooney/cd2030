pageObjectsConfig <- function(input) {
  list(
    reporting_rate = list(
      'Reporting Rate' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE
      )
    ),
    outlier_detection = list(
      'Outlier Detection' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE
      )
    ),
    consistency_checks = list(
      'ANC1 and Penta1' = list(
        parameters = list(),
        always_include = FALSE,
        single_entry = TRUE
      ),
      'Penta1 and Penta3' = list(
        parameters = list(),
        always_include = FALSE,
        single_entry = TRUE
      ),
      'OPV1 and OPV3' = list(
        parameters = list(),
        always_include = FALSE,
        single_entry = TRUE
      ),
      'Custom Check' = list(
        parameters = list(
          x_axis = reactive({ input$x_axis }),
          y_axis = reactive({ input$y_axis })
        ),
        always_include = FALSE,
        single_entry = FALSE
      )
    ),
    data_completeness = list(
      'Data Completeness' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE
      )
    ),
    ratios = list(
      'Ratio Plots' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE
      )
    ),
    adjustment_changes = list(
      'Live Births' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE
      ),
      'Penta1' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE
      ),
      'BCG' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE
      ),
      'Measles 1' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE
      ),
      'Custom Check' = list(
        parameters = list(
          indicator = reactive({ input$indicator })
        ),
        always_include = FALSE,
        single_entry = FALSE
      )
    ),
    denominator_assessment = list(
      'Total Population' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE
      ),
      'Births' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE
      )
    ),
    denominator_selection = list(
      'Penta 3' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE
      ),
      'Measles 1' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE
      ),
      'BCG' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE
      )
    ),
    national_coverage = list(
      'Measles 1' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE
      ),
      'Penta 3' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE
      ),
      'Penta1 to Penta3 Dropout' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE
      ),
      'Penta3 to Measles 1 Dropout' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE
      ),
      'Custom Checks' = list(
        parameters = list(
          indicator = reactive({ input$indicator })
        ),
        always_include = FALSE,
        single_entry = FALSE
      )
    ),
    subnational_coverage = list(
      'Measles 1' = list(
        parameters = list(
          admin_level = reactive({ input$admin_level })
        ),
        always_include = TRUE,
        single_entry = FALSE
      ),
      'Penta 3' = list(
        parameters = list(
          admin_level = reactive({ input$admin_level })
        ),
        always_include = TRUE,
        single_entry = FALSE
      ),
      'Penta1 to Penta3 Dropout' = list(
        parameters = list(
          admin_level = reactive({ input$admin_level })
        ),
        always_include = TRUE,
        single_entry = FALSE
      ),
      'Penta3 to Measles 1 Dropout' = list(
        parameters = list(
          admin_level = reactive({ input$admin_level })
        ),
        always_include = TRUE,
        single_entry = FALSE
      ),
      'Custom Checks' = list(
        parameters = list(
          admin_level = reactive({ input$admin_level }),
          indicator = reactive({ input$indicator })
        ),
        always_include = FALSE,
        single_entry = FALSE
      )
    ),
    subnational_inequality = list(
      'Measles 1' = list(
        parameters = list(
          level = reactive({ input$level })
        ),
        always_include = TRUE,
        single_entry = FALSE
      ),
      'Penta 3' = list(
        parameters = list(
          level = reactive({ input$level })
        ),
        always_include = TRUE,
        single_entry = FALSE
      ),
      'Penta1 to Penta3 Dropout' = list(
        parameters = list(
          level = reactive({ input$level })
        ),
        always_include = TRUE,
        single_entry = FALSE
      ),
      'Penta3 to Measles 1 Dropout' = list(
        parameters = list(
          level = reactive({ input$level })
        ),
        always_include = TRUE,
        single_entry = FALSE
      ),
      'Custom Checks' = list(
        parameters = list(
          level = reactive({ input$level }),
          indicator = reactive({ input$indicator })
        ),
        always_include = FALSE,
        single_entry = FALSE
      )
    ),
    subnational_mapping = list(
      'Measles 1' = list(
        parameters = list(
          level = reactive({ input$level })
        ),
        always_include = TRUE,
        single_entry = FALSE
      ),
      'Penta 3' = list(
        parameters = list(
          level = reactive({ input$level })
        ),
        always_include = TRUE,
        single_entry = FALSE
      ),
      'Penta1 to Penta3 Dropout' = list(
        parameters = list(
          level = reactive({ input$level })
        ),
        always_include = TRUE,
        single_entry = FALSE
      ),
      'Penta3 to Measles 1 Dropout' = list(
        parameters = list(
          level = reactive({ input$level })
        ),
        always_include = TRUE,
        single_entry = FALSE
      ),
      'Custom Checks' = list(
        parameters = list(
          level = reactive({ input$level }),
          indicator = reactive({ input$indicator })
        ),
        always_include = FALSE,
        single_entry = FALSE
      )
    ),
    equity_assessment = list(
      'Measles 1' = list(
        parameters = list(
          type = reactive({ input$type })
        ),
        always_include = TRUE,
        single_entry = FALSE
      ),
      'Penta 3' = list(
        parameters = list(
          type = reactive({ input$type })
        ),
        always_include = TRUE,
        single_entry = FALSE
      ),
      'Custom Checks' = list(
        parameters = list(
          type = reactive({ input$type }),
          indicator = reactive({ input$indicator })
        ),
        always_include = FALSE,
        single_entry = FALSE
      )
    )
  )
}
