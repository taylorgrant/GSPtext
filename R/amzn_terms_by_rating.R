#' Terms by Amazon Rating
#'
#' @description Graph with the most frequent terms used and each term is positioned by its average star rating. The dashed line represents the average star rating for the product.
#' @param data Data frame created by `GSPtext::get_reviews()` or in the same format
#'
#' @return ggplot2 graph - scatterplot with overlaid text
#' @export
#'
#' @examples
#' \dontrun{
#' amzn_terms_by_rating(data)}
amzn_terms_by_rating <- function(data) {

  # get brand/product from link
  bp <- gsub("\\/.*", "", urltools::url_parse(data$link[1])$path)

  data %>%
    dplyr::mutate(text = gsub("[[:punct:]]|[[:digit:]]", "", text)) %>%
    tidytext::unnest_tokens(word, text) %>%
    dplyr::anti_join(tidytext::stop_words) %>%
    dplyr::group_by(word) %>%
    dplyr::summarise(
      n = dplyr::n(),
      rating = mean(stars)
    ) %>%
    ggplot2::ggplot(ggplot2::aes(n, rating)) +
    ggplot2::geom_hline(
      yintercept = mean(data$stars), lty = 2,
      color = "gray50", size = 1
    ) +
    ggplot2::geom_jitter(color = "midnightblue", alpha = 0.4, size = .5) +
    ggplot2::geom_text(ggplot2::aes(label = word),
              check_overlap = TRUE,
              vjust = "top", hjust = "left", size = 3
    ) +
    ggplot2::scale_x_log10() +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(colour = "darkgray",
                                                     size = .2)) +
    ggplot2::labs(x = "Term Count", y = "Star Rating",
         title = "Most common words used in reviews, by review's star rating",
         subtitle = glue::glue("{bp}"),
         caption = glue::glue("Analysis by GS&P; Source: Amazon\ \n Dashed line represents averaage star rating for this product: {round(mean(data$stars), 2)} average.\nBased on {dim(data)[1]} reviews")
    )
}
