#' Page Count
#'
#' @description Takes first page of Amazon product reviews and determines how many pages of reviews are available
#'
#' @param url URL built with `build_url()` function
#'
#' @return Numeric value of total pages
#' @export
#'
#' @examples
#' \dontrun{
#' page_count(url)}
page_count <- function(url){
  p <- 1
  url_p1 <- glue::glue(url)
  page <- rvest::read_html(url_p1)
  cat(crayon::blue("Counting pages to crawl over...\n"))
  out <- page %>%
    rvest::html_elements("#filter-info-section") %>%
    rvest::html_text() %>%
    tibble::as_tibble_col(column_name = "value") %>%
    dplyr::mutate(value = trimws(gsub("\n", "", value)),
                  value = gsub(",", "", value),
                  value = as.numeric(regmatches(value, gregexpr("\\d+", value))[[1]][2]),
                  value = ceiling(value/10)) %>%
    dplyr::pull()
}
