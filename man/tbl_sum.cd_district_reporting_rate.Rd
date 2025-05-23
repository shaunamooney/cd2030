% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tbl_sum.reporting_summary.R
\name{tbl_sum.cd_district_reporting_rate}
\alias{tbl_sum.cd_district_reporting_rate}
\title{Summary for District-Level Reporting Rates by Year}
\usage{
\method{tbl_sum}{cd_district_reporting_rate}(x, ...)
}
\arguments{
\item{x}{A \code{cd_district_reporting_rate} object containing district-level reporting rates.}

\item{...}{Additional arguments for compatibility with the S3 method.}
}
\value{
A character vector describing the reporting threshold applied and summarizing
district reporting rates by year.
}
\description{
Provides a custom summary for the \code{cd_district_reporting_rate} object, displaying
a message with the threshold used in calculating district-level reporting rates by year.
This summary serves to validate which districts meet the defined threshold for reporting completeness.
}
