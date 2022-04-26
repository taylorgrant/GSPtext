#' Get Amazon Reviews
#'
#' @description Convenience wrapper around functions that scrape Amazon reviews. Provide a product link and this will determine the number of pages to crawl over and will return a clean data frame of reviews.
#'
#' @param link A link to an Amazon product
#'
#' @return Data frame with date, stars, headline, review text, and the paginated link.
#' @export
#'
#' @examples
#' \dontrun{
#' url <- "https://www.amazon.com/Fashion-Focus-Sweaters-Chihuahua-Clothing/product-reviews/B07L1LYTPX/ref=cm"
#' data <- amzn_get_reviews(url)}
amzn_get_reviews <- function(link) {

  # get reviews
  tmp <- scraper(link)
  # identify pages with errors (returned as NA)
  errors <- which(is.na(tmp))
  cat("There were css errors on the following pages [", errors, "]. \nAs a result, reviews from these pages have been dropped from the data...")

  # drop NA and reduce list to data frame
  out_df <- tmp[!is.na(tmp)] %>%
    purrr::reduce(dplyr::bind_rows) %>%
    dplyr::mutate(date = as.Date(trimws(gsub(".*\\on", "", date)), format = "%B %d, %Y"),
                  stars = as.numeric(trimws(gsub("\\out.*", "", stars))),
                  text = trimws(gsub("[\r\n]", "", text)))

  review_summary(out_df)
  out_df
}
