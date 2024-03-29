#' Text to Emotion
#'
#' Applying the \href{https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm}{NRC lexicon} to the review texts. Each term from the reviews is matched to the lexicon. For each star rating, all emotional terms are then counted and percentages calculated for each grouping (rating).
#'
#' The NRC lexicon should be a one time download.
#'
#' @param data Data frame created by `GSPtext::get_reviews()` or in the same format
#' @param grouping Should the data be grouped by Star Rating or Review Year
#'
#' @import textdata
#' @return List with three objects - "data" (star/year, sentiment, n count, percentage), "graph1" (dodged bar graph), "graph3 (stacked bar)
#' @export
#'
#' @examples
#' \dontrun{
#' out <- text_to_emotion(data, "stars")
#'
#' # to call each object within the list
#' out$data
#' out$graph1
#' out$graph2
#'
#' # if only want to visualize a graph
#' text_to_emotion(data, "stars")$graph1
#' text_to_emotion(data, "stars")$graph2
#'
#' # or can also use year as an argument
#' text_to_emotion(data, "year)
#'}
text_to_emotion <- function(data, grouping = c("stars", "year")){

  # check
  if (!tolower(grouping) %in% c("stars", "year")) stop("Grouping can only be 'stars' or 'year'\n")

  bp <- gsub("\\/.*", "", urltools::url_parse(data$link[1])$path)
  toCap <- function(x) gsub("(?<=^|_)([a-z])", "\\U\\1", x, perl=TRUE)

  # get lexicon (should only be a 1 time download)
  tidytext::get_sentiments("nrc")

  quo_var <- rlang::sym(grouping)

  out <- data %>%
    dplyr::mutate(year = lubridate::year(date),
                  text = gsub("[[:punct:]]|[[:digit:]]", "", text)) %>%
    tidytext::unnest_tokens(word, text) %>%
    dplyr::anti_join(tidytext::stop_words) %>%
    dplyr::inner_join(tidytext::get_sentiments("nrc")) %>%
    dplyr::filter(!sentiment %in% c("positive", "negative")) %>%
    dplyr::count(!!quo_var, sentiment) %>%
    dplyr::group_by(!!quo_var) %>%
    dplyr::mutate(frac = n/sum(n),
           txtcol = ifelse(sentiment %in% c("trust", "surprise", "sadness", "joy"),
                           "black", "white"),
           sentiment = toCap(sentiment))

  # doing this the long way
  if (grouping == "stars") {
    p1 <- out %>%
      ggplot2::ggplot(ggplot2::aes(x = sentiment, y = frac, group =  !!quo_var, fill = factor(!!quo_var))) +
      ggplot2::geom_bar(stat = "identity", position = "dodge", width = .7, alpha = .95) +
      ggplot2::scale_fill_manual(values = c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF", "#7AA6DCFF",
                                            "#003C67FF", "#8F7700FF", "#3B3B3BFF", "#A73030FF", "#4A6990FF"),
                                 name = "Star Rating") +
      ggplot2::labs(x = NULL, y = NULL,
                    title = glue::glue("Percentage of Emotional Terms by Star Rating"),
                    subtitle = glue::glue("{bp}"),
                    caption = glue::glue("Percentage based on total emotional terms used in reviews for each star.\nAnalysis by GS&P; Source: Amazon\ \n Based on {dim(data)[1]} reviews")) +
      ggplot2::theme_minimal() +
      ggplot2::scale_y_continuous(labels = scales::percent)

    p2 <- out %>%
      dplyr::mutate(stars = paste0(stars, "-star"),
             stars = factor(stars, levels = c("1-star", "2-star", "3-star",
                                              "4-star", "5-star")),
             sentiment = toCap(sentiment)) %>%
      ggplot2::ggplot(ggplot2::aes(x = stars, y = frac, group = sentiment, fill = sentiment)) +
      ggplot2::geom_col() +
      ggplot2::scale_fill_manual(values = c("#003f5c", "#2f4b7c", "#665191", "#a05195",
                                   "#d45087", "#f95d6a", "#ff7c43", "#ffa600"),
                        name = "Sentiment") +
      ggplot2::coord_flip() +
      ggplot2::geom_text(ggplot2::aes(x = stars, y = frac, group = sentiment,
                                      label = scales::percent(frac, accuracy = 1)),
                position = ggplot2::position_stack(vjust = .5), size = 3.5, color = out$txtcol) +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = NULL, y = NULL,
           title = "Percentage of Emotional Terms by Star Rating",
           subtitle = glue::glue("{bp}"),
           caption = glue::glue("Percentage based on total emotional terms used within each star rating.\nAnalysis by GS&P; Source: Amazon\nBased on {dim(data)[1]} reviews"))
    return (list(data = out[,1:4], graph1 = p1, graph2 = p2))
  } else {
      p1 <- out %>%
        ggplot2::ggplot(ggplot2::aes(x = sentiment, y = frac, group =  year, fill = factor(year))) +
        ggplot2::geom_bar(stat = "identity", position = "dodge", width = .7, alpha = .95) +
        ggplot2::scale_fill_manual(values = c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF", "#7AA6DCFF",
                                              "#003C67FF", "#8F7700FF", "#3B3B3BFF", "#A73030FF", "#4A6990FF"),
                                   name = "Year") +
        ggplot2::labs(x = NULL, y = NULL,
                      title = glue::glue("Percentage of Emotional Terms by Review Year"),
                      subtitle = glue::glue("{bp}"),
                      caption = glue::glue("Percentage based on total emotional terms used in reviews for each {grouping}.\nAnalysis by GS&P; Source: Amazon\ \n Based on {dim(data)[1]} reviews")) +
        ggplot2::theme_minimal() +
        ggplot2::scale_y_continuous(labels = scales::percent)

      p2 <- out %>%
        dplyr::mutate(year = factor(year),
                      sentiment = toCap(sentiment)) %>%
        ggplot2::ggplot(ggplot2::aes(x = year, y = frac, group = sentiment, fill = sentiment)) +
        ggplot2::geom_col() +
        ggplot2::scale_fill_manual(values = c("#003f5c", "#2f4b7c", "#665191", "#a05195",
                                              "#d45087", "#f95d6a", "#ff7c43", "#ffa600"),
                                   name = "Sentiment") +
        ggplot2::coord_flip() +
        ggplot2::geom_text(ggplot2::aes(x = year, y = frac, group = sentiment,
                                        label = scales::percent(frac, accuracy = 1)),
                           position = ggplot2::position_stack(vjust = .5), size = 3.5, color = out$txtcol) +
        ggplot2::scale_y_continuous(labels = scales::percent) +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = NULL, y = NULL,
                      title = "Percentage of Emotional Terms by Review Year",
                      subtitle = glue::glue("{bp}"),
                      caption = glue::glue("Percentage based on total emotional terms used in reviews for each {grouping}.\nAnalysis by GS&P; Source: Amazon\nBased on {dim(data)[1]} reviews"))
      return (list(data = out[,1:4], graph1 = p1, graph2 = p2))
    }
}
