#' Terms in Context
#'
#' @description Shamelessly borrowing the `quanteda::kwic()` function (\url{https://quanteda.io/index.html}) for keyword search
#'
#' @param data Data frame created by `GSPtext::get_reviews()` or in the same format
#' @param pattern Keyword or phrase to search for (See examples)
#' @param window Number of words to display on either side of the keyword match
#' @param valuetype Type of pattern match. "glob" for glob-style wildcards, "regex" for regular expressions, "fixed" for exact matching
#'
#' @return quanteda object with 7 variables (\code{docname or index, from, to, pre, keyword, post, pattern})
#' @export
#'
#' @examples
#' \dontrun{
#' # single token
#' term_context(data, pattern = "quality", window = 4, valuetype = "glob")
#' term_context(data, pattern = "qual", valuetype = "regex", window = 4)
#'
#' # phrase matching
#' term_context(data, pattern = "cheap quality", window = 4)
#' }
term_context <- function(data, pattern, window =5, valuetype = c("glob", "regex", "fixed")){
  # tokenize reviews
  rev_tkns <- quanteda::tokens(data$text, remove_punct = TRUE)

  # use quanteda::kwic()
  if (lengths(gregexpr("\\w+", pattern)) > 1) {
    quanteda::kwic(rev_tkns, quanteda::phrase(pattern), window, valuetype)
  } else {
    quanteda::kwic(rev_tkns, pattern, window, valuetype)
  }
}
