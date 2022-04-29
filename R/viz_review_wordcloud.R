#' Review Wordclouds
#'
#' @description Tokenizing text and plotting in wordclouds. Two comparison wordclouds available ("ratings" and "sentiment")
#'
#' This uses the `wordcloud::wordcloud()` function so we have to specify the format and save type beforehand. See examples below
#'
#' @param data Data frame created by `GSPtext::get_reviews()` or in the same format
#' @param type Type of word cloud, either "overall" or "comparison"
#' @param comp_type If "comparison," then either "ratings" (1&2 star vs 4&5 star) or "sentiment" (based on the Bing lexicon)
#'
#' @importFrom graphics par
#' @importFrom reshape2 acast
#'
#' @return Wordcloud
#' @export
#'
#' @examples
#' \dontrun{
#' viz_review_wordcloud(data, type = "comparison", comp_type = "rating"),
#' viz_review_wordcloud(data, type = "overall")
#'
#' # To save a wordlcoud:
#' png("~/Desktop/wordcloud.png") # or any other directory/filename
#' viz_review_wordcloud(data, type = "comparison", comp_type = "rating")
#' dev.off()}
viz_review_wordcloud <- function(data, type = c("overall", "comparison"), comp_type = c("rating", "sentiment")) {

  # get brand/product from link
  bp <- gsub("\\/.*", "", urltools::url_parse(data$link[1])$path)
  toCap <- function(x) gsub("(?<=^|_)([a-z])", "\\U\\1", x, perl=TRUE)

  if (tolower(type) == "comparison" && tolower(comp_type) == "rating") {
    cat("Splitting terms by Hi (5 & 4 star) and Lo (1 & 2 star) reviews\n\n")

    # graph
    par(mar = rep(0, 4))
    data %>%
      dplyr::mutate(text = gsub("[[:punct:]]|[[:digit:]]", "", text)) %>%
      dplyr::mutate(rating = ifelse(stars > 3, "4 & 5 star", "1 & 2 star")) %>%
      tidytext::unnest_tokens(word, text) %>%
      dplyr::anti_join(tidytext::stop_words) %>%
      dplyr::count(word, rating, sort = TRUE) %>%
      acast(word ~ rating, value.var = "n", fill = 0) %>%
      wordcloud::comparison.cloud(colors = c("red", "dodgerblue"),
                                  scale = c(4, .5),
                                  title.size = 3,
                                  max.words =  300)

  } else if (tolower(type) == "comparison" && tolower(comp_type) == "sentiment") {
    par(mar = rep(0, 4))
    data %>%
      dplyr::mutate(text = gsub("[[:punct:]]|[[:digit:]]", "", text)) %>%
      tidytext::unnest_tokens(word, text) %>%
      dplyr::anti_join(tidytext::stop_words) %>%
      dplyr::inner_join(tidytext::get_sentiments("bing")) %>%
      dplyr::count(word, sentiment, sort = TRUE) %>%
      dplyr::mutate(sentiment = toCap(sentiment)) %>%
      reshape2::acast(word ~ sentiment, value.var = "n", fill = 0) %>%
      wordcloud::comparison.cloud(colors = c("red", "dodgerblue"),
                                  scale = c(4, .5),
                                  title.size = 3,
                                  max.words =  300)

  } else {
    par(mar = rep(0, 4))
    data %>%
      dplyr::mutate(text = gsub("[[:punct:]]|[[:digit:]]", "", text)) %>%
      tidytext::unnest_tokens(word, text) %>%
      dplyr::anti_join(tidytext::stop_words) %>%
      dplyr::count(word) %>%
      with(wordcloud::wordcloud(word, n,
                                colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02"),
                                scale = c(4.5, .5),
                                max.words =  300))
  }
  par(mar=c(5.1, 4.1, 4.1, 2.1))
}
