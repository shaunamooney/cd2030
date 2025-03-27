# Countdown Analysis R Script
# The purpose of this script is to replicate the Countdown Analysis in R.

# load in cd2030 package
devtools::load_all()

# load in bf data
bf_data <- load_excel_data("~/private/Burkina Faso/Burkina_Faso.xlsx")

# adjust the data - use k = 0.25 default as we don't know exact k values used
adjusted_data <- adjust_service_data(bf_data)

# load in un estimates
un_estimates_data <- load_un_estimates("~/private/Burkina Faso/UN_Estimates_Workshop.dta", country_iso = "BFA", start_year = 2019, end_year = 2023)

# calculate indicator coverage using adjusted data and un estimates at national level
calculate_indicator_coverage(adjusted_data, admin_level = "national", un_estimates = un_estimates_data)
