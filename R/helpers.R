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
  cat(crayon::cyan("Counting pages to crawl over..."))
  out <- page %>%
    rvest::html_elements("#filter-info-section") %>%
    rvest::html_text() %>%
    tibble::as_tibble_col(column_name = "value") %>%
    dplyr::mutate(value = trimws(gsub("\n", "", value)),
                  value = gsub(",", "", value),
                  value = as.numeric(regmatches(value, gregexpr("\\d+", value))[[1]][2]),
                  value = ceiling(value/10)) %>%
    dplyr::pull()
  cat(crayon::cyan(glue::glue("{out} pages...\n\n")))
  return(out)
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
#' url <- "https://www.amazon.com/Fashion-Focus-Sweaters-Chihuahua-Clothing/product-reviews/B07L1LYTPX"
#' scraper(url)}
scraper <- function(link) {

  # get paginated reviews url
  p_url <- build_url(link)
  # determine total number of pages
  page_total <- page_count(p_url)
  p <- 1:page_total

  cat(crayon::cyan("Pulling all reviews now. This may take a few minutes depending on product popularity...\n\n"))
  reviews <- purrr::possibly(function(link, p) {
    # progress bar
    pb$tick()
    # set up css
    star_rating <- "#cm_cr-review_list .review-rating"
    rev_hed <- ".a-text-bold span"
    rev_date <- "#cm_cr-review_list .review-date"
    rev_text <- ".review-text-content"

    # function with css
    get_it <- purrr::possibly(function(pg, element){
      pg %>%
        rvest::html_elements(element) %>%
        rvest::html_text()
    }, otherwise = NA_character_)

    # find ugc images
    get_img <- purrr::possibly(function(pg) {
      pg %>%
        rvest::html_elements(".review-image-tile") %>%
        rvest::html_attr('src')
    }, otherwise = NA_character_)

    # build page url and read html
    paged_url <- glue::glue(build_url(link))
    page <- rvest::read_html(paged_url)

    # drop empty elements in vector if more than 10 elements
    drop_empty <- function(x) {
      check_empty <- function(x) all(x != "")
      if (length(x) > 10 & check_empty(x) == FALSE) {
        x[x != ""]
      } else {
        x
      }
    }

    # scrape each part of the review
    star <- drop_empty(get_it(page, star_rating))
    hl <- drop_empty(get_it(page, rev_hed))
    date <- drop_empty(get_it(page, rev_date))
    text <- drop_empty(get_it(page, rev_text))
    images <- paste(get_img(page), collapse = ", ")

    # put together in
    tibble::tibble(date = date,
                   stars = star,
                   headline = hl,
                   text = text,
                   link = paged_url
    ) %>%
      dplyr::mutate(imgcol = paste(images, collapse = ", "))

  }, otherwise = NA_character_)
  pb <- progress::progress_bar$new(total = length(p))
  out <- purrr::map2(p_url, p, reviews)
}

#' Summary Review
#'
#' @description Summarizing the reviews scraped.
#'
#' Console prints the number of reviews scraped, average word count,
#'
#' @param data Data frame created by `GSPtext::get_reviews()` or in the same format
#'
#' @return Information is printed in console for summary purposes
#' @export
#'
#' @examples
#' \dontrun{
#' review_summary(data)}
review_summary <- function(data) {

  bp <- gsub("\\/.*", "", urltools::url_parse(data$link[1])$path)

  # review count
  rev_ct <- dim(data)[1]

  # word counts average
  avg_per <- data %>%
    dplyr::summarise(avg = mean(lengths(gregexpr("\\w+", text)))) %>%
    dplyr::pull()

  avg_star <- data %>%
    dplyr::group_by(stars) %>%
    dplyr::summarise(avg_wordcount = mean(lengths(gregexpr("\\w+", text))))

  # reviews over time
  by_time <- data %>%
    dplyr::mutate(year = lubridate::year(date)) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(review_count = dplyr::n(),
                     avg_rating = mean(stars),
                     avg_wordcount = mean(lengths(gregexpr("\\w+", text))))

  cat("\n\nSummary of:", crayon::bold$bgBlue(bp),
      crayon::bold("\n\nReviews scraped:"), rev_ct,
      crayon::bold("\nAverage Star Rating:"), round(mean(data$stars), 2),
      crayon::bold("\nAverage number of words per review:"), round(avg_per),
      crayon::bold("\n\nAverage number of words per star rating:"))
  print(knitr::kable(avg_star, format = "rst", digits = 2))
  cat(crayon::bold("\nBreakdown of reviews by year:"))
  print(knitr::kable(by_time, format = "rst", digits = 2))

  cat(crayon::bold("\nGeneric link to reviews:"), data$link[1])
}


#' Make Collage of UGC Images
#'
#' @description Using the magick package to read UGC images and create a collage
#'
#' @param images Array of urls; each url is a UGC image link
#'
#' @return png collage; found on ~/Desktop with name `ugc_collage.png`
#' @export
#'
#' @examples
#' \dontrun{
#' make_collage(out_images)}
make_collage <- function(images) {

  # create temporary folder to store columns
  dir_create <- function(x) ifelse(!dir.exists(x), dir.create(x), FALSE)
  folder_loc <- file.path("~/Desktop", "tmp")
  dir_create(folder_loc)

  # column function
  make_column <- function(i, images, no_rows){

    ff <- tibble::tibble(f = images[(i*no_rows+1):((i+1)*no_rows)]) %>%
      dplyr::filter(!is.na(f)) %>%
      dplyr::pull()

    # progress
    pb$tick()

    i <- stringr::str_pad(i, 2, pad = "0")
    magick::image_read(ff) %>%
      magick::image_scale("x100") %>%
      magick::image_border('white', '2x2') %>%
      magick::image_append(stack = TRUE) %>%
      magick::image_write(
        file.path("~/Desktop", "tmp", paste0("cols", i,".png")),
        format = "png")
  }

  # call column function and create with proper sizing
  file_count <- length(images)
  no_rows <- 5
  no_cols <- ceiling(file_count/no_rows)
  # establish progress bar
  pb <- progress::progress_bar$new(total = file_count)
  purrr::walk(0:(no_cols-1),
              make_column,
              images = images,
              no_rows = no_rows)

  # append images
  colfiles <- dir(folder_loc, pattern = ".png")

  magick::image_append(magick::image_read(dir(file.path(folder_loc),
                                              pattern = ".png",
                                              full.names = TRUE)),
                       stack = FALSE) %>%
    magick::image_write(file.path("~/Desktop", "ugc_collage.png"),
                        format = "png")
  # delete the temporary folder
  unlink("~/Desktop/tmp", recursive=TRUE)
}
