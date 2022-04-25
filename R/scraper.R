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
    tibble::tibble(review_date = date,
           review_stars = star,
           review_headline = hl,
           review_text = text,
           link = paged_url
    )

  }, otherwise = NA_character_)
  pb <- progress::progress_bar$new(total = length(p))
  out <- purrr::map2(p_url, p, reviews)
}
