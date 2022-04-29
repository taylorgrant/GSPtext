#' Review Sentiment
#'
#' @description Estimating the sentiment for each review of a product. The function leverages the \href{https://github.com/trinker/sentimentr}{sentimentr} package for estimation at the level of each review text.
#'
#' \link{sentimentr} accounts for "valence shifters" within texts - the terms that can negate or amplify the sentiment of nearby terms, (e.g., "better not" or "not a lie") - by using weights to account for a positive or negative shift.
#'
#' @param data Data frame created by `GSPtext::get_reviews()` or in the same format
#'
#' @return Tibble of original data with added sentiment information -- ("word count", "sd", "avg sentiment"). ggplot scatterplot of the average sentiment of each review over time.
#' @export
#'
#' @examples
#' \dontrun{
#' sentiment <- amzn_review_sentiment(data)
#'
#' # what's the strength of the relationship b/n sentiment and rating
#' summary(lm(ave_sentiment ~ stars + word_count, data = sentiment$data)) %>% broom::tidy()
#'
#' # find review with max or min sentiment
#' sentiment$data[which.max(sentiment$data$ave_sentiment),]
#' sentiment$data[which.min(sentiment$data$ave_sentiment),]
#'
#' # summarize sentiment by review rating
#' sentiment <- amzn_review_sentiment(data)
#' sentiment %>% dplyr::group_by(stars) %>%
#' dplyr::summarise(avg_sentiment = mean(ave_sentiment),
#' sd = mean(sd, na.rm = TRUE), reviews = dplyr::n())
#'
#' # aggregate to monthly level and plot average sentiment
#' sentiment <- amzn_review_sentiment(data)
#' sentiment$data %>%
#'   dplyr::mutate(month = lubridate::floor_date(date, "month")) %>%
#'   dplyr::group_by(month) %>%
#'   dplyr::summarise(monthly_sentiment = mean(ave_sentiment)) %>%
#'   ggplot2::ggplot(aes(x = month, y = monthly_sentiment)) +
#'   ggplot2::geom_line()}
amzn_review_sentiment <- function(data){
  bp <- gsub("\\/.*", "", urltools::url_parse(data$link[1])$path)
  cat(crayon::cyan("Estimating the sentiment for each review. This will just take a minute...\n"))

  out <- data %>%
    dplyr::mutate(sentiment = purrr::map(text, sentimentr::sentiment_by)) %>%
    tidyr::unnest(sentiment) %>%
    dplyr::select(-element_id)

  # plotting changes over time
    p <- out %>%
      ggplot2::ggplot(ggplot2::aes(x = date, y = ave_sentiment, fill = ave_sentiment, size = word_count)) +
      ggplot2::geom_point(pch = 21, stroke = .4, color = "darkgray") +
      viridis::scale_fill_viridis(option = "A") +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = NULL, y = "Sentiment of Review",
           title = "Sentiment of Amazon Reviews Over Time",
           subtitle = glue::glue("{bp}"),
           caption = glue::glue("Bubble size reflects the word count of the review\nAnalysis by GS&P; Source: Amazon\ \n Based on {dim(data)[1]} reviews")) +
    ggplot2::theme(
            legend.position = "none")

    out <- list(data = out, graph = p)

}




