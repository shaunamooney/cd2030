% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/equiplot.R
\name{equiplot_education}
\alias{equiplot_education}
\title{A Specialized Dot Plot for Maternal Education Analysis}
\usage{
equiplot_education(
  .data,
  indicator = c("bcg", "anc1", "pcv3", "opv1", "opv2", "opv3", "penta2", "pcv1", "pcv2",
    "penta1", "penta3", "measles1", "rota1", "rota2", "instdeliveries", "measles2",
    "ipv1", "ipv2", "undervax", "dropout_penta13", "zerodose", "dropout_measles12",
    "dropout_penta3mcv1"),
  x_title = NULL,
  dot_size = NULL
)
}
\arguments{
\item{.data}{A data frame containing the data to be plotted.}

\item{indicator}{A string specifying the indicator to be analyzed (e.g., 'sba').}

\item{x_title}{Optional. A title for the x-axis. Defaults to '\if{html}{\out{<indicator>}} Coverage (\%)'.}
}
\value{
A ggplot object representing the dot plot.
}
\description{
\code{equiplot_education} generates a dot plot comparing coverage across maternal education levels
(no education, primary, and secondary or higher) for a specific indicator across years.
}
\examples{
\dontrun{
  equiplot_education(data, indicator = 'sba')
}

}
