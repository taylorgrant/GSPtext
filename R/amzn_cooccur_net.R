#' Co-Occurrence Networks
#'
#' @description Count and plot the number of times pairs of words co-occur within reviews.
#'
#' The function requires the user to specify which star-rating to isolate and to choose how frequently terms must co-occur.
#'
#' @param data Data frame created by `GSPtext::get_reviews()` or in the same format
#' @param star Star rating of reviews that should be plotted
#' @param nn Minimum number of co-occurrences that should be used in the plot
#'
#' @return ggraph plot
#' @export
#'
#' @examples
#' \dontrun{
#' amzn_cooccur_net(data, star = 5, nn = 15)}
amzn_cooccur_net <- function(data, star, nn) {

  bp <- gsub("\\/.*", "", urltools::url_parse(data$link[1])$path)

  word_pairs <- data %>%
    dplyr::mutate(text = gsub("[[:punct:]]|[[:digit:]]", "", text)) %>%
    dplyr::mutate(doc_id = dplyr::row_number()) %>%
    dplyr::filter(stars == star) %>%
    tidytext::unnest_tokens(word, text) %>%   # tokenize
    dplyr::filter(!word %in% tidytext::get_stopwords()$word) %>%  # remove stopwords
    widyr::pairwise_count(word, doc_id, sort = TRUE, upper = FALSE)

  if (sum(word_pairs$n > nn) < 5) {
    stop("Not enough word pair co-occurrences to graph, please reduce the number of co-occurrences or try a different star-rating...\n")
  }

  cat(crayon::cyan("With a floor of", nn, "co-occurrences, the graph is based on", table(word_pairs$n >= nn)[2], "word pairs...\n"))
  # graph co-occurrences
  word_pairs %>%
    dplyr::filter(n >= nn) %>%  # only word pairs that occur X or more times
    igraph::graph_from_data_frame() %>% #convert to graph
    ggraph::ggraph(layout = "fr") +
    ggraph::geom_edge_link(ggplot2::aes(edge_alpha = n, edge_width = n),
                           edge_colour = "dodgerblue") +
    ggraph::scale_edge_alpha(range = c(0.1, 1)) +
    ggraph::scale_edge_width(range = c(1, 5)) +
    ggraph::geom_node_point(size = 3) +
    ggraph::geom_node_text(ggplot2::aes(label = name), repel = TRUE,
                   size = 3,
                   point.padding = ggplot2::unit(0.2, "lines")) +
    ggplot2::theme_void() +
    ggplot2::labs(title = glue::glue('Term Co-Occurrences across {star}-star reviews'),
         subtitle = glue::glue("{bp}"),
         caption = glue::glue("Analysis by GS&P; Source: Amazon\ \nBased on {sum(data$stars == star)} {star}-star reviews"))

}
