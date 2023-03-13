#' Get Amazon Reviews
#'
#' @description Convenience wrapper around functions that scrape Amazon reviews. Provide a product link and this will determine the number of pages to crawl over and will return a clean data frame of reviews.
#'
#' @param link A link to an Amazon product
#' @param get_images Whether to download UGC images included with reviews
#'
#' @return Data frame with date, stars, headline, review text, and the paginated link.
#' @export
#'
#' @examples
#' \dontrun{
#' url <- "copy_paste_url"
#' data <- amzn_get_reviews(url)}
amzn_get_reviews <- function(link, get_images = c("false", "true")) {
  # get reviews
  tmp <- scraper(link)
  # identify pages with errors (returned as NA)
  # errors <- which(is.na(tmp))
  # cat("There were css errors on the following pages [", errors, "]. \nAs a result, reviews from these pages have been dropped from the data...")

  # drop NA and reduce list to data frame
  out_df <- tmp[!is.na(tmp)] %>%
    purrr::reduce(dplyr::bind_rows) %>%
    dplyr::mutate(date = as.Date(trimws(gsub(".*\\on", "", date)), format = "%B %d, %Y"),
                  stars = as.numeric(trimws(gsub("\\out.*", "", stars))),
                  text = trimws(gsub("[\r\n]", "", text)))

  if (tolower(get_images) == "false") {
    # print summary of the data
    review_summary(out_df)

    out_df # %>%
      # dplyr::select(-imgcol)
  } else if (tolower(get_images) == "true" && all(is.na(out_df$imgcol))) {
    # print summary of the data
    cat(crayon::cyan("No images were found in the reviews...\n"))
    review_summary(out_df)

    out_df %>%
      dplyr::select(-imgcol)

  } else {
    out_images <- out_df %>%
      dplyr::distinct(imgcol) %>%
      tidyr::separate_rows(imgcol, sep = ", ") %>%
      dplyr::pull()

    out_images <- regmatches(out_images,regexpr(".*\\.jpg|.png",out_images, perl = TRUE))

    cat(crayon::cyan("\n\nSaving a collage of UGC images to your desktop with the filename 'ugc_collage.png'...\n"))
    make_collage(out_images)

    # print summary of the data
    review_summary(out_df)

    out_df %>%
      dplyr::select(-imgcol)
    }

}
