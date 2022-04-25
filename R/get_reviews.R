#' Get Reviews
#'
#' @description Convenience wrapper around functions that scrape Amazon reviews. Provide a product link and this will determine the number of pages to crawl over and will return a clean data frame of reviews.
#' The function provides a prompt for the user to paste in the product url.
#'
#' @return Data frame with date, stars, headline, review text, and the paginated link.
#' @export
#'
#' @examples
#' \dontrun{
#' get_reviews()}
get_reviews <- function() {

  # user prompt
  link <- readline(cat(crayon::bgCyan("Please provide the url of the product you wish to get reviews for:")))

  # get reviews
  tmp <- scraper(link)
  # identify pages with errors (returned as NA)
  errors <- which(is.na(tmp))
  cat(crayon::red("There were css errors on the following pages [", errors, "]. \nAs a result, reviews from these pages have been dropped from the data..."))

  # drop NA and reduce list to data frame
  out_df <- tmp[!is.na(tmp)] %>%
    purrr::reduce(dplyr::bind_rows) %>%
    dplyr::mutate(review_date = as.Date(trimws(gsub(".*\\on", "", review_date)), format = "%B %d, %Y"),
                  review_stars = as.numeric(trimws(gsub("\\out.*", "", review_stars))),
                  review_text = trimws(gsub("[\r\n]", "", review_text)))
}
