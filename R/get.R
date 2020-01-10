#' factor levels
#'
#' Using [cat_gather] requires factor levels to be supplied in the form
#'   of a list. The names of the list should correspond to names of
#'   factors. The values of the list should be the levels of the factor.
#'   This function creates that list for you.
#'
#' @param data a data frame containing factors.
#'
#' @param ... factor variables in `data` to obtain factor levels for.
#'   If unspecified, all factor variables are considered. This argument
#'   is compatible with `tidyselect` commands.
#'
#' @export
get_factor_levels <- function(data, ...){

  data %<>% check_cats()

  if(is.null(data)) return(NULL)

  cats <- get_factors(data)
  cols <- rlang::enquos(...)

  if(purrr::is_empty(cols)){
    .cols <- cats
  } else {
    .cols <- intersect(
      cats,
      tidyselect::vars_select(names(data), !!!cols)
    )
  }

  purrr::map(data[, .cols], levels)

}


get_factors <- function(data){

  data %<>% check_cats()

  if(is.null(data)) return(NULL)

  var_is_factor <- purrr::map_lgl(data, is.factor)

  names(which(var_is_factor))

}


