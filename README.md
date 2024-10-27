
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cd2030

<!-- badges: start -->
<!-- badges: end -->

## Introduction

This R package, **cd2030**, is designed for loading, cleaning, and
analyzing data from Countdown 2030 formatted data. The package provides
functions to process administrative, population, reporting, and service
data, handling tasks such as data merging, duplicate detection, and
outlier flagging.

## Installation

You can install the development version of cd2030 from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("damurka/cd2030")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(cd2030)
## basic example code
```

## Overview

The **cd2030** package consists of several functions to: - Load data
from specified sheets in Excel files - Perform data cleaning and column
standardization - Check for duplicate records based on key columns -
Merge multiple datasets into a single structured format - Calculate
derived indicators and flag potential outliers

## Key Functions

- **`load_excel_data`**: Loads and cleans data from a specified Excel
  file, including multiple sheets for administrative, population,
  reporting, and service data.
- **`load_data`**: Loads previously processed data stored in an RDS
  file.
- **`clean_data`**: Cleans individual datasets, standardizing column
  names, removing unnecessary rows and columns, and converting data
  types as necessary.
- **`check_for_duplicates`**: Checks datasets for duplicate rows based
  on specified key columns.
- **`merge_data`**: Merges datasets from different sheets into a single,
  consolidated dataset.
- **`data_preparation`**: Performs final data transformations, including
  rounding, renaming, and calculating population growth rates.
- **`create_indicator`**: Generates additional indicators and flags
  outliers within the dataset based on pre-specified conditions.

## Example Usage

Below are some examples of how to use the **cd2030** package functions.

### Loading and Cleaning Data

``` r
# Load and clean Countdown 2030 data from an Excel file
data <- load_excel_data(
  filename = "path/to/countdown2030_data.xlsx"
)
```

## Contributing

Contributions to improve the **cd2030** package are welcome! Please fork
the repository, make your changes, and submit a pull request.
