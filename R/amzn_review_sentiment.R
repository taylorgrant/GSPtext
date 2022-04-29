#' Review Sentiment
#'
#' @description Estimating the sentiment for each review of a product. The function leverages the \href{https://github.com/trinker/sentimentr}{sentimentr} package for estimation at the level of each review text.
#'
#' \link{sentimentr} accounts for "valence shifters" within texts - the terms that can negate or amplify the sentiment of nearby terms, (e.g., "better not" or "not a lie") - by using weights to account for a positive or negative shift.
#'
#' @param data Data frame created by `GSPtext::get_reviews()` or in the same format
#'
#' @return Tibble of original data with added sentiment information -- ("word count", "sd", "avg sentiment")
#' @export
#'
#' @examples
#' \dontrun{
#' sentiment <- amzn_review_sentiment(data)
#'
#' # what's the strength of the relationship b/n sentiment and rating
#' summary(lm(ave_sentiment ~ stars, data = sentiment$data)) %>% broom::tidy()
#'
#' # find review with max or min sentiment
#' sentiment[which.max(sentiment$ave_sentiment),]
#' sentiment[which.min(sentiment$ave_sentiment),]
#'
#' # summarize sentiment by review rating
#' sentiment <- amzn_review_sentiment(data)
#' sentiment %>% dplyr::group_by(stars) %>%
#' dplyr::summarise(avg_sentiment = mean(ave_sentiment),
#' sd = mean(sd, na.rm = TRUE), reviews = dplyr::n())
#'
#' # aggregate to monthly level and plot average sentiment
#' sentiment <- amzn_review_sentiment(data)
#' sentiment %>%
#'   dplyr::mutate(month = lubridate::floor_date(date, "month")) %>%
#'   dplyr::group_by(month) %>%
#'   dplyr::summarise(monthly_sentiment = mean(ave_sentiment)) %>%
#'   ggplot2::ggplot(aes(x = month, y = monthly_sentiment)) +
#'   ggplot2::geom_line()}
amzn_review_sentiment <- function(data){
  bp <- gsub("\\/.*", "", urltools::url_parse(data$link[1])$path)
  cat("Estimating the sentiment for each review. This will just take a minute...\n")
  out <- data %>%
    dplyr::mutate(sentiment = purrr::map(text, sentimentr::sentiment_by)) %>%
    tidyr::unnest(sentiment) %>%
    dplyr::select(-element_id)

  # plotting changes over time

    p <- out %>%
      dplyr::select(date, stars, ave_sentiment) %>%
      tidyr::pivot_longer(names_to = "metric",
                   values_to = "value",
                   -date) %>%
      dplyr::group_by(metric) %>%
      dplyr::mutate(value = scale(value),
             ma13 = slider::slide_dbl(value, mean, .before = 6, .after = 6, .complete = TRUE)) %>%
      ggplot2::ggplot(ggplot2::aes(x = date, y = ma13, group = metric, color = metric)) +
      ggplot2::geom_line(size = .6) +
      ggplot2::geom_smooth(method = "lm", se = FALSE) +
      ggplot2::scale_color_manual(values = c("#003f5c", "#ff7c43"),
                         name = "Change in:",
                         labels = c("Sentiment", "Rating")) +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = NULL, y = NULL,
           title = "Trending changes between sentiment and rating",
           subtitle = glue::glue("{bp}"),
           caption = glue::glue("Analysis by GS&P; Source: Amazon\ \n Based on {dim(data)[1]} reviews"))

    out <- list(data = out, graph = p)

}


