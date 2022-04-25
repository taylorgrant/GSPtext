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
