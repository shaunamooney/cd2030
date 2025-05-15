# Countdown Analysis R Script
# The purpose of this script is to replicate the Countdown Analysis in R.

# load in cd2030 package
devtools::load_all()

# load in bf data
bf_data <- load_excel_data("~/Documents/GitHub/cdroutine/private/Burkina Faso/Burkina_Faso.xlsx")

# adjust the data include_uncertainty = TRUE uses updated functionality here
adjusted_data <- adjust_service_data(bf_data, include_uncertainty = TRUE)

# the following naming structure of columns applies to all indicators:
adjusted_data$instlivebirths # this is the mean - named it the indicator to align with the rest of the code
adjusted_data$instlivebirths_lower # 2.5th percentile
adjusted_data$instlivebirths_upper # 97.5th percentile
adjusted_data$instlivebirths_median # also added in the median for each indicator in the output

# load in un estimates
un_estimates_data <- load_un_estimates("~/Documents/GitHub/cdroutine/private/Burkina Faso/UN_Estimates_Workshop.dta", country_iso = "BFA", start_year = 2019, end_year = 2023)

# load the wuenic estimates
wuenic_data <- load_wuenic_data('~/Documents/GitHub/cdroutine/private/Burkina Faso/Wuenic_estimates.dta', country_iso = 'BFA')

# load the national survey data and filter only survey greater than 2010
survey_data <- load_survey_data('~/Documents/GitHub/cdroutine/private/Burkina Faso/Survdata_Burkina_Faso/all_Burkina_Faso.dta', country_iso = 'BFA', admin_level = 'national') %>%
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
