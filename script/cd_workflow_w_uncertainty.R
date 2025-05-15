# Countdown Analysis R Script
# The purpose of this script is to replicate the Countdown Analysis in R.

# load in cd2030 package
devtools::load_all()

# load in bf data
bf_data <- load_excel_data("~/private/Burkina Faso/Burkina_Faso.xlsx")

# adjust the data include_uncertainty = TRUE uses updated functionality here - use k = 0.25 default as we don't know exact k values used
adjusted_data <- adjust_service_data(bf_data, include_uncertainty = TRUE)

# load in un estimates
un_estimates_data <- load_un_estimates("~/private/Burkina Faso/UN_Estimates_Workshop.dta", country_iso = "BFA", start_year = 2019, end_year = 2023)

# load the wuenic estimates
wuenic_data <- load_wuenic_data('~/private/Burkina Faso/Wuenic_estimates.dta', country_iso = 'BFA')

# load the national survey data and filter only survey greater than 2010
survey_data <- load_survey_data('~/private/Burkina Faso/Survdata_Survdata_Burkina_Faso/all_Survdata_Burkina_Faso.dta', country_iso = 'BFA', admin_level = 'national') %>%
  filter(year >= 2010)

# calculate indicator coverage using adjusted data and un estimates at national level
#calculate_indicator_coverage(adjusted_data, admin_level = "national", un_estimates = un_estimates_data)

# plot the indicator coverage for class 'cd_indicator_coverage'
# plot(coverage, plot ='delivery_coverage_penta1')

# calculate indicator coverage using adjusted data and un estimates, wuenic estimates and survey data at national level

### This function has not been edited to deal with adjusted data with uncertainty ####
coverage <- calculate_coverage(adjusted_data,
                               admin_level = "national",
                               un_estimates = un_estimates_data,
                               wuenic_data = wuenic_data,
                               survey_data = survey_data)

# plot the coverage for class 'cd_coverage'
plot(coverage,
     indicator = 'penta1',
     denominator = 'penta1')
