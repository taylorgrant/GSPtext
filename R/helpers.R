#' Build URL
#'
#' @description Provide a url to an Amazon product and function creates the proper paginated review url. The url can include any number of parameters.
#'
#' @param link A link to an Amazon product
#'
#' @return Character string - properly formatted url with pagination
#' @export
#'
#' @examples
#' \dontrun{
#' build_url(
#' "https://www.amazon.com/AmazonBasics-Nespresso-Storage-Capsule-Capacity/dp/B010RLCH2U/ref=xxxx")}
build_url <- function(link) {
  if(!grepl("amazon", link)) stop("Link must be for Amazon.com")
  parsed <- urltools::url_parse(link)
  # keep everything up to 3rd / in the path
  parsed$path <- sub("^(([^/]*/){2}[^/]*).*", "\\1", parsed$path)
  parsed$path <- sub("\\/.*\\/", "/product-reviews/", parsed$path)
  parsed$parameter <- "ref=cm_cr_getr_d_paging_btm_next_{p}?ie=UTF8&reviewerType=all_reviews&sortBy=recent&pageNumber={p}"
  out <- urltools::url_compose(parsed)
}


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
  cat(crayon::blue("Counting pages to crawl over..."))
  out <- page %>%
    rvest::html_elements("#filter-info-section") %>%
    rvest::html_text() %>%
    tibble::as_tibble_col(column_name = "value") %>%
    dplyr::mutate(value = trimws(gsub("\n", "", value)),
                  value = gsub(",", "", value),
                  value = as.numeric(regmatches(value, gregexpr("\\d+", value))[[1]][2]),
                  value = ceiling(value/10)) %>%
    dplyr::pull()
  cat(crayon::blue(glue::glue("{out} pages...\n")))
}


#' Scraper of Reviews
#'
#' @description Function to pull Amazon reviews. Given a standard product link, it proprerly formats a paginated url, determines the number of pages to crawl, and then returns the reviews as a list
#'
#' @param link String url for Amazon product
#'
#' @return List of tibbles with date of review, star rating, headline, text, and link to review
#' @export
#'
#' @examples
#' \dontrun{
#' scraper("url")}
scraper <- function(link) {

  # get paginated reviews url
  p_url <- build_url(link)
  # determine total number of pages
  page_total <- page_count(p_url)
  p <- 1:page_total

  cat(crayon::blue("Pulling all reviews now. This may take a few minutes depending on product popularity...\n"))
  reviews <- purrr::possibly(function(link, p) {
    # progress bar
    pb$tick()
    # set up css
    star_rating <- "#cm_cr-review_list .review-rating"
    rev_hed <- ".a-text-bold span"
    rev_date <- "#cm_cr-review_list .review-date"
    rev_text <- ".review-text-content span"

    # function with css
    get_it <- purrr::possibly(function(pg, element){
      pg %>%
        rvest::html_elements(element) %>%
        rvest::html_text()
    }, otherwise = NA_character_)

    # build page url and read html
    paged_url <- glue::glue(build_url(link))
    page <- rvest::read_html(paged_url)

    # scrape each part of the review
    star <- get_it(page, star_rating)
    hl <- get_it(page, rev_hed)
    date <- get_it(page, rev_date)
    text <- get_it(page, rev_text)

    # put together in
    tibble::tibble(date = date,
                   stars = star,
                   headline = hl,
                   text = text,
                   link = paged_url
    )

  }, otherwise = NA_character_)
  pb <- progress::progress_bar$new(total = length(p))
  out <- purrr::map2(p_url, p, reviews)
}
