
#' @keywords internal
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom rlang %||% !! !!!
#'
"_PACKAGE"

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")
  utils::globalVariables(
    c(
      "name",
      "value",
      "factor_name"
    )
  )

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
