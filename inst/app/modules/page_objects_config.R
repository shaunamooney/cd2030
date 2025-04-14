pageObjectsConfig <- function(input) {
  list(
    reporting_rate = list(
      'Reporting Rate' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE,
        prompts = list(
          'Interpret the differences between the different admin1 units: are there any admin1s that stand out as more or less complete? What are some explanations?',
          'Interpret the differences between different years: are there any years that stand out as being more or less complete? What are some explanations?'
        )
      )
    ),
    outlier_detection = list(
      'Outlier Detection' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE,
        prompts = list(
          'Make a statement about the presence of outliers in the immunization data: are there many or few, is the number of outliers increasing or decreasing?',
          'Interpret the differences between the different regions and years: are there any regions that have more outliers? Are there any years that have more outliers? What are some explanations (e.g. campaigns or catch-up activities with older kids that may have been recorded incorrectly)?',
          'Interpret the differences between the outliers across different vaccines and regions: are there any vaccines that stand out as having more or less outliers? What are some explanations?',
          'Interpret the example outliers: are they likely to be errors or is there an explanation for why they may be true values?'
        )
      )
    ),
    consistency_checks = list(
      'ANC1 and Penta1' = list(
        parameters = list(),
        always_include = FALSE,
        single_entry = TRUE,
        prompts = list()
      ),
      'Penta1 and Penta3' = list(
        parameters = list(),
        always_include = FALSE,
        single_entry = TRUE,
        prompts = list()
      ),
      'OPV1 and OPV3' = list(
        parameters = list(),
        always_include = FALSE,
        single_entry = TRUE,
        prompts = list()
      ),
      'Custom Check' = list(
        parameters = list(
          x_axis = reactive({ input$x_axis }),
          y_axis = reactive({ input$y_axis })
        ),
        always_include = FALSE,
        single_entry = FALSE,
        prompts = list()
      )
    ),
    data_completeness = list(
      'Data Completeness' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE,
        prompts = list(
          'Interpret the differences between the different vaccines: are there any vaccines that stand out as more or less complete? What are some explanations?',
          'Interpret the differences between the different regions: are there any regions that stand out as more or less complete? What are some explanations (e.g. stockout)?'
        )
      )
    ),
    ratios = list(
      'Ratio Plots' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE,
        prompts = list()
      )
    ),
    adjustment_changes = list(
      'Live Births' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE,
        prompts = list()
      ),
      'Penta1' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE,
        prompts = list(
          'Consider the effect of the adjustment on the number of DTP1 doses administered, and mention the difference in the number, as well as the percent increase in 2023; highlight the year with the greatest impact if there is one; interpret if the impact of the adjustment on coverage rates is large or small'
        )
      ),
      'BCG' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE,
        prompts = list(
          'Consider the effect of the adjustment on the number of BCG doses administered, and mention the difference in the number, as well as the percent increase in 2023; highlight the year with the greatest impact if there is one; interpret if the impact of the adjustment on coverage rates is large or small'
        )
      ),
      'Measles 1' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE,
        prompts = list(
          'Consider the effect of the adjustment on the number of Measles doses administered, and mention the difference in the number, as well as the percent increase in 2023; highlight the year with the greatest impact if there is one; interpret if the impact of the adjustment on coverage rates is large or small'
        )
      ),
      'Custom Check' = list(
        parameters = list(
          indicator = reactive({ input$indicator })
        ),
        always_include = FALSE,
        single_entry = FALSE,
        prompts = list(
          'Consider the effect of the adjustment on the number of doses administered, and mention the difference in the number, as well as the percent increase in 2023; highlight the year with the greatest impact if there is one; interpret if the impact of the adjustment on coverage rates is large or small'
        )
      )
    ),
    denominator_assessment = list(
      'Total Population' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE,
        prompts = list('How do the estimates of total population differ compared to the UN projection?')
      ),
      'Births' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE,
        prompts = list('How do the estimates of births differ compared to the UN projection?')
      )
    ),
    denominator_selection = list(
      'Penta 3' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE,
        prompts = list('What is the selected denominator for the key vaccination indicators and why?  Note that one best denominator should be selected to be applied for all immunization data.')
      ),
      'Measles 1' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE,
        prompts = list('What is the selected denominator for the key vaccination indicators and why?  Note that one best denominator should be selected to be applied for all immunization data.')
      ),
      'BCG' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE,
        prompts = list('What is the selected denominator for the key vaccination indicators and why?  Note that one best denominator should be selected to be applied for all immunization data.')
      )
    ),
    national_coverage = list(
      'Measles 1' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE,
        prompts = list(
          'First address data quality: Are the levels and trends plausible? Is there good consistency between the facility and survey data?',
          'Then, interpret the data if there is sufficient confidence in the observed levels and trends. How does the coverage compare to global targets to reach 90% coverage? Is this a positive trend? Are there explanations for the observed levels and trends?'
        )
      ),
      'Penta 3' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE,
        prompts = list(
          'First address data quality: Are the levels and trends plausible? Is there good consistency between the facility and survey data?',
          'Then, interpret the data if there is sufficient confidence in the observed levels and trends. How does the coverage compare to global targets to reach 90% coverage? Is this a positive trend? Are there explanations for the observed levels and trends?'
        )
      ),
      'Penta1 to Penta3 Dropout' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE,
        prompts = list(
          'Are the dropout rates similar to the survey data? What is the dropout in surveys among those with cards vs those without?',
          'Is dropout improving, staying the same or getting worse?'
        )
      ),
      'Penta3 to Measles 1 Dropout' = list(
        parameters = list(),
        always_include = TRUE,
        single_entry = TRUE,
        prompts = list(
          'Are the dropout rates similar to the survey data? What is the dropout in surveys among those with cards vs those without?',
          'Is dropout improving, staying the same or getting worse?'
        )
      ),
      'Custom Checks' = list(
        parameters = list(
          indicator = reactive({ input$indicator })
        ),
        always_include = FALSE,
        single_entry = FALSE,
        prompts = list(
          'First address data quality: Are the levels and trends plausible? Is there good consistency between the facility and survey data?',
          'Then, interpret the data if there is sufficient confidence in the observed levels and trends. How does the coverage compare to global targets to reach 90% coverage? Is this a positive trend? Are there explanations for the observed levels and trends?'
        )
      )
    ),
    subnational_coverage = list(
      'Measles 1' = list(
        parameters = list(
          admin_level = reactive({ input$admin_level })
        ),
        always_include = TRUE,
        single_entry = FALSE,
        prompts = list(
          'Are there regions that have especially high or especially low Measles1 coverage?',
          'Comment on the variation between highest-coverage and lowest-coverage regions and districts.'
        )
      ),
      'Penta 3' = list(
        parameters = list(
          admin_level = reactive({ input$admin_level })
        ),
        always_include = TRUE,
        single_entry = FALSE,
        prompts = list(
          'Are there regions that have especially high or especially low Penta3 coverage?',
          'Comment on the variation between highest-coverage and lowest-coverage regions and districts.'
        )
      ),
      'Penta1 to Penta3 Dropout' = list(
        parameters = list(
          admin_level = reactive({ input$admin_level })
        ),
        always_include = TRUE,
        single_entry = FALSE,
        prompts = list(
          'Are there regions that have especially high or especially low Penta dropout rates? How do they compare to regions with high or low Penta coverage?',
          'Comment on the variation between highest-dropout and lowest-dropout regions and districts.'
        )
      ),
      'Penta3 to Measles 1 Dropout' = list(
        parameters = list(
          admin_level = reactive({ input$admin_level })
        ),
        always_include = TRUE,
        single_entry = FALSE,
        prompts = list(
          'Are there regions that have especially high or especially low Penta measles dropout rates? How do they compare to regions with high or low Penta coverage?',
          'Comment on the variation between highest-dropout and lowest-dropout regions and districts.'
        )
      ),
      'Custom Checks' = list(
        parameters = list(
          admin_level = reactive({ input$admin_level }),
          indicator = reactive({ input$indicator })
        ),
        always_include = FALSE,
        single_entry = FALSE,
        prompts = list(
          'Are there regions that have especially high or especially low coverage?',
          'Comment on the variation between highest-coverage and lowest-coverage regions and districts.'
        )
      )
    ),
    subnational_inequality = list(
      'Measles 1' = list(
        parameters = list(
          level = reactive({ input$level })
        ),
        always_include = TRUE,
        single_entry = FALSE,
        prompts = list(
          'Are there regions that have especially high or especially low Measles1 coverage?',
          'Comment on the variation between highest-coverage and lowest-coverage regions and districts.'
        )
      ),
      'Penta 3' = list(
        parameters = list(
          level = reactive({ input$level })
        ),
        always_include = TRUE,
        single_entry = FALSE,
        prompts = list(
          'Are there regions that have especially high or especially low Penta3 coverage?',
          'Comment on the variation between highest-coverage and lowest-coverage regions and districts.'
        )
      ),
      'Penta1 to Penta3 Dropout' = list(
        parameters = list(
          level = reactive({ input$level })
        ),
        always_include = TRUE,
        single_entry = FALSE,
        prompts = list(
          'Are there regions that have especially high or especially low Penta dropout rates? How do they compare to regions with high or low Penta coverage?',
          'Comment on the variation between highest-dropout and lowest-dropout regions and districts.'
        )
      ),
      'Penta3 to Measles 1 Dropout' = list(
        parameters = list(
          level = reactive({ input$level })
        ),
        always_include = TRUE,
        single_entry = FALSE,
        prompts = list(
          'Are there regions that have especially high or especially low Penta measles dropout rates? How do they compare to regions with high or low Penta coverage?',
          'Comment on the variation between highest-dropout and lowest-dropout regions and districts.'
        )
      ),
      'Custom Checks' = list(
        parameters = list(
          level = reactive({ input$level }),
          indicator = reactive({ input$indicator })
        ),
        always_include = FALSE,
        single_entry = FALSE,
        prompts = list(
          'Are there regions that have especially high or especially low coverage?',
          'Comment on the variation between highest-coverage and lowest-coverage regions and districts.'
        )
      )
    ),
    subnational_mapping = list(
      'Measles 1' = list(
        parameters = list(
          level = reactive({ input$level })
        ),
        always_include = TRUE,
        single_entry = FALSE,
        prompts = list()
      ),
      'Penta 3' = list(
        parameters = list(
          level = reactive({ input$level })
        ),
        always_include = TRUE,
        single_entry = FALSE,
        prompts = list()
      ),
      'Penta1 to Penta3 Dropout' = list(
        parameters = list(
          level = reactive({ input$level })
        ),
        always_include = TRUE,
        single_entry = FALSE,
        prompts = list()
      ),
      'Penta3 to Measles 1 Dropout' = list(
        parameters = list(
          level = reactive({ input$level })
        ),
        always_include = TRUE,
        single_entry = FALSE,
        prompts = list()
      ),
      'Custom Checks' = list(
        parameters = list(
          level = reactive({ input$level }),
          indicator = reactive({ input$indicator })
        ),
        always_include = FALSE,
        single_entry = FALSE,
        prompts = list()
      )
    ),
    low_reporting = list(
      'Coverage' = list(
        parameters = list(
          level = reactive({ input$admin_level })
        ),
        always_include = TRUE,
        single_entry = FALSE,
        prompts = list(
         'Reflect on the percent of districts that are meeting performance targets for coverage: has it increased or decreased over time? Does it vary between different vaccines? ',
         'Do these results using the adjusted data and alternative denominator align with performance indicators in the DHIS2 data? If they are different, why?',
         'Then, make the same reflections for the percent of districts that are meeting performance targets for dropout.'
        )
      ),
      'Dropout' = list(
        parameters = list(
          level = reactive({ input$admin_level })
        ),
        always_include = TRUE,
        single_entry = FALSE,
        prompts = list(
          'Reflect on the percent of districts that are meeting performance targets for coverage: has it increased or decreased over time? Does it vary between different vaccines? ',
          'Do these results using the adjusted data and alternative denominator align with performance indicators in the DHIS2 data? If they are different, why?',
          'Then, make the same reflections for the percent of districts that are meeting performance targets for dropout.'
        )
      )
    ),
    equity_assessment = list(
      'Measles 1' = list(
        parameters = list(
          type = reactive({ input$type })
        ),
        always_include = TRUE,
        single_entry = FALSE,
        prompts = list(
          'Residence: are the gaps between the urban and rural areas large, have they changed over time? What pattern of inequality (bottom, linear, top)?',
          'Region:  Are the gaps between regions large? Have they changed over time?',
          'Wealth: are the gaps between the rich and poor large, have they changed over time? What pattern of inequality (bottom, linear, top)?',
          'Education: are the gaps in coverage by mother’s education large, have they changed over time?',
          'How do these results link to immunization strategies like fixed post, mobile and outreach?'
        )
      ),
      'Penta 3' = list(
        parameters = list(
          type = reactive({ input$type })
        ),
        always_include = TRUE,
        single_entry = FALSE,
        prompts = list(
          'Residence: are the gaps between the urban and rural areas large, have they changed over time? What pattern of inequality (bottom, linear, top)?',
          'Region:  Are the gaps between regions large? Have they changed over time?',
          'Wealth: are the gaps between the rich and poor large, have they changed over time? What pattern of inequality (bottom, linear, top)?',
          'Education: are the gaps in coverage by mother’s education large, have they changed over time?',
          'How do these results link to immunization strategies like fixed post, mobile and outreach?'
        )
      ),
      'Custom Checks' = list(
        parameters = list(
          type = reactive({ input$type }),
          indicator = reactive({ input$indicator })
        ),
        always_include = FALSE,
        single_entry = FALSE,
        prompts = list(
          'Residence: are the gaps between the urban and rural areas large, have they changed over time? What pattern of inequality (bottom, linear, top)?',
          'Region:  Are the gaps between regions large? Have they changed over time?',
          'Wealth: are the gaps between the rich and poor large, have they changed over time? What pattern of inequality (bottom, linear, top)?',
          'Education: are the gaps in coverage by mother’s education large, have they changed over time?',
          'How do these results link to immunization strategies like fixed post, mobile and outreach?'
        )
      )
    )
  )
}
