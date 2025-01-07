pageObjectsConfig <- function(input) {
  list(
    reporting_rate = list(
      'Average Report Rate' = list(
        parameters = list(
          threshold = reactive({ input$threshold })
        ),
        always_include = TRUE,
        single_entry = TRUE
      ),
      'Low Reporting DIstricts' = list(
        parameters = list(
          threshold = reactive({ input$threshold }),
          indicator = reactive({ input$indicator }),
          year = reactive({ input$year })
        ),
        always_include = FALSE,
        single_entry = FALSE
      ),
      'District Trend' = list(
        parameters = list(
          threshold = reactive({ input$threshold }),
          indicator = reactive({ input$indicator }),
          district = reactive({ input$district })
        ),
        always_include = FALSE,
        single_entry = FALSE
      )
    ),
    outlier_detection = list(
      'Indicator with Outlier' = list(
        parameters = list(
          year = reactive({ input$year })
        ),
        always_include = FALSE,
        single_entry = FALSE
      ),
      'District with Outlier' = list(
        parameters = list(
          year = reactive({ input$year }),
          indicator = reactive({ input$indicator })
        ),
        always_include = FALSE,
        single_entry = FALSE
      ),
      'District Trends' = list(
        parameters = list(
          indicator = reactive({ input$indicator }),
          district = reactive({ input$district })
        ),
        always_include = FALSE,
        single_entry = FALSE
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
      'Indicators with Missing Data' = list(
        parameters = list(
          year = reactive({ input$year })
        ),
        always_include = FALSE,
        single_entry = FALSE
      ),
      'Districts with Missing Data' = list(
        parameters = list(
          year = reactive({ input$year }),
          indicator = reactive({ input$indicator })
        ),
        always_include = FALSE,
        single_entry = FALSE
      )
    ),
    ratios = list(
      'Ratio Plots' = list(
        parameters = list(
          anc1_coverage = reactive({ input$anc1_coverage }),
          penta1_coverage = reactive({ input$penta1_coverage }),
          penta3_coverage = reactive({ input$penta3_coverage })
        ),
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
        parameters = list(
          denominator = reactive({ input$denominator }),
          year = reactive({ input$year })
        ),
        always_include = TRUE,
        single_entry = TRUE
      ),
      'Penta 3' = list(
        parameters = list(
          denominator = reactive({ input$denominator }),
          year = reactive({ input$year })
        ),
        always_include = TRUE,
        single_entry = TRUE
      ),
      'Penta1 to Penta3 Dropout' = list(
        parameters = list(
          denominator = reactive({ input$denominator }),
          year = reactive({ input$year })
        ),
        always_include = TRUE,
        single_entry = TRUE
      ),
      'Penta3 to Measles 1 Dropout' = list(
        parameters = list(
          denominator = reactive({ input$denominator }),
          year = reactive({ input$year })
        ),
        always_include = TRUE,
        single_entry = TRUE
      ),
      'Custom Checks' = list(
        parameters = list(
          denominator = reactive({ input$denominator }),
          year = reactive({ input$year }),
          indicator = reactive({ input$indicator })
        ),
        always_include = FALSE,
        single_entry = FALSE
      )
    ),
    subnational_coverage = list(
      'Measles 1' = list(
        parameters = list(
          admin_level = reactive({ input$admin_level }),
          region = reactive({ input$region }),
          denominator = reactive({ input$denominator }),
          year = reactive({ input$year })
        ),
        always_include = TRUE,
        single_entry = TRUE
      ),
      'Penta 3' = list(
        parameters = list(
          admin_level = reactive({ input$admin_level }),
          region = reactive({ input$region }),
          denominator = reactive({ input$denominator }),
          year = reactive({ input$year })
        ),
        always_include = TRUE,
        single_entry = TRUE
      ),
      'Penta1 to Penta3 Dropout' = list(
        parameters = list(
          admin_level = reactive({ input$admin_level }),
          region = reactive({ input$region }),
          denominator = reactive({ input$denominator }),
          year = reactive({ input$year })
        ),
        always_include = TRUE,
        single_entry = TRUE
      ),
      'Penta3 to Measles 1 Dropout' = list(
        parameters = list(
          admin_level = reactive({ input$admin_level }),
          region = reactive({ input$region }),
          denominator = reactive({ input$denominator }),
          year = reactive({ input$year })
        ),
        always_include = TRUE,
        single_entry = TRUE
      ),
      'Custom Checks' = list(
        parameters = list(
          admin_level = reactive({ input$admin_level }),
          region = reactive({ input$region }),
          denominator = reactive({ input$denominator }),
          year = reactive({ input$year }),
          indicator = reactive({ input$indicator })
        ),
        always_include = FALSE,
        single_entry = FALSE
      )
    ),
    subnational_inequality = list(
      'Measles 1' = list(
        parameters = list(
          denominator = reactive({ input$denominator }),
          level = reactive({ input$level })
        ),
        always_include = TRUE,
        single_entry = TRUE
      ),
      'Penta 3' = list(
        parameters = list(
          denominator = reactive({ input$denominator }),
          level = reactive({ input$level })
        ),
        always_include = TRUE,
        single_entry = TRUE
      ),
      'Custom Checks' = list(
        parameters = list(
          denominator = reactive({ input$denominator }),
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
          denominator = reactive({ input$denominator }),
          level = reactive({ input$level }),
          years = reactive({ input$years }),
          palette = reactive({ input$palette })
        ),
        always_include = TRUE,
        single_entry = TRUE
      ),
      'Penta 3' = list(
        parameters = list(
          denominator = reactive({ input$denominator }),
          level = reactive({ input$level }),
          years = reactive({ input$years }),
          palette = reactive({ input$palette })
        ),
        always_include = TRUE,
        single_entry = TRUE
      ),
      'Penta1 to Penta3 Dropout' = list(
        parameters = list(
          denominator = reactive({ input$denominator }),
          level = reactive({ input$level }),
          years = reactive({ input$years }),
          palette = reactive({ input$palette })
        ),
        always_include = TRUE,
        single_entry = TRUE
      ),
      'Penta3 to Measles 1 Dropout' = list(
        parameters = list(
          denominator = reactive({ input$denominator }),
          level = reactive({ input$level }),
          years = reactive({ input$years }),
          palette = reactive({ input$palette })
        ),
        always_include = TRUE,
        single_entry = TRUE
      ),
      'Custom Checks' = list(
        parameters = list(
          denominator = reactive({ input$denominator }),
          level = reactive({ input$level }),
          years = reactive({ input$years }),
          palette = reactive({ input$palette }),
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
