#' Most Frequent Terms by Star Rating
#'
#' @description Function to calculate the top terms for each star rating.
#'
#' @param data Data frame created by `GSPtext::get_reviews()` or in the same format
#' @param count Number of terms to return per star rating
#'
#' @return Returns a list with two items - item 1 ($data) is the data frame with top terms for each star rating; item 2 ($graph) is a ggplot bar graph faceted by star rating
#' @export
#'
#' @examples
#' \dontrun{
#' out <- amzn_frequent_terms(data, 15)
#' # to access data
#' top_terms <- out$data
#'
#' # to call the graph
#' out$graph
#' }
amzn_frequent_terms <- function(data, count) {
  bp <- gsub("\\/.*", "", urltools::url_parse(data$link[1])$path)

  out <- data %>%
    dplyr::mutate(text = gsub("[[:punct:]]|[[:digit:]]", "", text)) %>%
    tidytext::unnest_tokens(word, text) %>%
    dplyr::anti_join(tidytext::stop_words) %>%
    dplyr::count(word, stars, sort = TRUE) %>%
    dplyr::group_by(stars) %>%
    dplyr::slice_max(order_by = n, n = count, with_ties = FALSE)

  p <- out %>%
    dplyr::mutate(stars = paste0(stars, " star"),
           stars = factor(stars, levels = c("1 star", "2 star", "3 star",
                                            "4 star", "5 star")),
           word = tidytext::reorder_within(word, n, stars)) %>%
    ggplot2::ggplot(ggplot2::aes(x = word, y = n, group = stars, fill = stars)) +
    ggplot2::geom_col() +
    ggplot2::facet_wrap(~stars, ncol = 3, scales = "free_y") +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c("#E41A1C", "#FF7F00", "#4DAF4A", "#984EA3", "#377EB8")) +
    tidytext::scale_x_reordered() +
    ggplot2::labs(x = NULL, y = NULL,
         title = glue::glue("Top {count} terms for each star rating"),
         subtitle = glue::glue("{bp}"),
         caption = glue::glue("Analysis by GS&P; Source: Amazon\ \n Based on {dim(data)[1]} reviews")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
          panel.grid.minor = ggplot2::element_blank(),
          strip.background = ggplot2::element_rect(fill = "lightgray", color = NA))
  return (list(data = out, graph = p))
}
