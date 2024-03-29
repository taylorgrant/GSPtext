% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/amzn_ratings_over_time.R
\name{amzn_ratings_over_time}
\alias{amzn_ratings_over_time}
\title{Amazon Ratings Over Time}
\usage{
amzn_ratings_over_time(
  data,
  time = c("month", "year"),
  viz_type = c("bar", "line"),
  trend = c("false", "loess", "lm")
)
}
\arguments{
\item{data}{Data frame created by \code{GSPtext::get_reviews()} or in the same format}

\item{time}{Time frame to aggregate by - either Monthly or Yearly}

\item{viz_type}{Type of graph - either bar graph or line chart}

\item{trend}{Include a trend line in the graph - either "lm" (linear fit) or "loess" (local polynomial fit)}
}
\value{
ggplot2 graphs - either bar or line plots
}
\description{
Graphing star ratings over time. Time frame is either Month or Year. Graph types are either Bar or Line. If using a Line plot, there is an option to include a \code{geom_smooth()} that can be set to either "loess" or "lm".
}
\examples{
\dontrun{
amzn_ratings_over_time(data, time = "month", viz_type = "line", trend = "lm")}
}
