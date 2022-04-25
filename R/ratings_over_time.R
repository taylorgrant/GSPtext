#' Ratings Over Time
#'
#' @description Graphing star ratings over time. Timeframe is either Month or Year. Graph types are either Bar or Line. If using a Line plot, there is an option to include a geom_smooth() that can be set to either "loess" or "lm".
#'
#' @param data Data frame created by `GSPtext::get_reviews()` or in the same format
#' @param time Time frame to aggregate by - either Monthly or Yearly
#' @param viz_type Type of graph - either bar graph or line chart
#' @param trend Include a trend line in the graph - either "lm" or "loess"
#'
#' @return ggplo2 graphs - either bar or line plots
#' @export
#'
#' @examples
#' \dontrun{
#' ratings_over_time(data, time = "month", viz_type = "line", trend = "lm")}
ratings_over_time <- function(data, time = c("month", "year"), viz_type = c("bar", "line"), trend = c("loess", "lm", "FALSE")) {

  # get brand/product from link
  bp <- gsub("\\/.*", "", urltools::url_parse(data$link[1])$path)
  # capitalize time frame
  toCap <- function(x) gsub("(?<=^|_)([a-z])", "\\U\\1", x, perl=TRUE)

  p <- data %>%
    dplyr::mutate(time_agg = lubridate::floor_date(review_date, time)) %>%
    dplyr::group_by(time_agg) %>%
    dplyr::summarise(avg_star = mean(review_stars)) %>%
    ggplot2::ggplot(ggplot2::aes(x = time_agg, y = avg_star)) +
    ggplot2::labs(x = "Timeframe", y = "Avg. Star Rating",
         title = glue::glue("Average Star Rating by {toCap(time)}"),
         subtitle = glue::glue("{bp}"),
         caption = "Analysis by GS&P; Source: Amazon")

  if (viz_type == "bar") {
    p +
      ggplot2::geom_col(fill = "navyblue", alpha = .9) +
      ggplot2::theme_minimal() +
      ggplot2::scale_y_continuous(limits = c(0,5))
  } else if (viz_type == "line" && trend == "loess") {
    p +
      ggplot2::geom_line(color = "black", size = 1) +
      ggplot2::theme_minimal() +
      ggplot2::scale_y_continuous(limits = c(0,5)) +
      ggplot2::geom_smooth(se = FALSE)
  } else if (viz_type == "line" && trend == "lm") {
    p +
      ggplot2::geom_line(color = "black", size = 1) +
      ggplot2::theme_minimal() +
      ggplot2::scale_y_continuous(limits = c(0,5)) +
      ggplot2::geom_smooth(se = FALSE, method = "lm")
  } else {
    p +
      ggplot2::geom_line(color = "black", size = 1) +
      ggplot2::theme_minimal() +
      ggplot2::scale_y_continuous(limits = c(0,5))
  }
}
