
#' Herd Cats
#'
#' @description many modeling functions expect matrix input with
#'   factor levels one-hot encoded.
#'
#'   - `cat_spread` will one-hot encode any factor or character variable
#'   in `data` and return a one-hot encoded `tibble`.
#'
#'   - [cat_gather] will apply the inverse operation and convert one-hot
#'   encoded columns back into factors.
#'
#' @param data Data with categorical variables (i.e., factors)
#'   that need to be spread into one-hot format.
#'
#' @param ... Column names to spread into one-hot format. This can also be
#'   a tidyselect specification (e.g., `starts_with()`, `contains()`, etc.).
#'   If unspecified, all character and factor variables in `data` will be
#'   spread into one-hot format.
#'
#' @param sparsify_nas (logical) Should NAs be converted to 0s?.
#'   Default is `FALSE`.
#'
#' @param na_cols (logical) Should columns be generated to indicate
#'   the presence of NAs? This argument only applies to factor columns
#'   with at least one NA. Default is `FALSE`.
#'
#' @param drop_cols (logical) Should the resulting data frame exclude
#'   the original columns that were spread into one-hot format? Default
#'   is `TRUE`.
#'
#' @param drop_unused_levels (logical) Should columns of all 0s be
#'   dropped? This occurs if `data` has unused factor levels. Default
#'   is `FALSE`.
#'
#' @return a [tibble][tibble::tibble-package] with categorical variables
#'   herded as you like.
#'
#' @examples
#'
#' df <- data.frame(x = rep(letters[1:2], 50), y = 1:100)
#'
#' one_hot_df <- cat_spread(df)
#' print(one_hot_df)
#'
#' cat_gather(one_hot_df, factor_levels = list(x=c('a','b')))
#'
#'
#' @export

cat_spread <- function(
  data,
  ...,
  sparsify_nas = FALSE,
  na_cols = FALSE,
  drop_cols = TRUE,
  drop_unused_levels = FALSE
){

  # check the data (it needs to have >=1 categorical variable to proceed)
  if(is.null(check_cats(data))) return(data)
  # convert character variables to factor variables if needed
  data <- check_cats(data)

  # get categorical variables
  cats <- get_factors(data)
  # quote the user supplied variable selections
  cols <- rlang::enquos(...)

  # initialize .cols
  # this is what will be used if there are no user-supplied selections.
  .cols <- cats

  # if user supplies variable names
  if(!purrr::is_empty(cols)){

    # convert to character values

    .cols <- intersect(
      tidyselect::vars_select(names(data), !!!cols),
      cats
    )

    if(purrr::is_empty(.cols))
      stop("No categorical variables were selected", call. = FALSE)

  }

  # Check to see if the user asked to spread an ID column
  nlevels <- purrr::map_int(
    .x = purrr::set_names(.cols),
    .f = ~ length(levels(data[[.x]]))
  )

  fctrs_to_list <- nlevels == nrow(data)

  # If they did, give my opinionated warning to them, but still proceed
  if(any(fctrs_to_list)){

    fctrs_to_list <- names(fctrs_to_list)[which(fctrs_to_list)]

    warning(
      'Selected variables appear to be ID columns: ',
      list_things(fctrs_to_list),
      "\nspreading ID columns is almost never a good idea.",
      call. = FALSE
    )

  }

  # call data.table and mltools for the heavy lifting
  output <- do.call(
    what = mltools::one_hot,
    args = list(
      dt = data.table::as.data.table(data),
      cols = .cols,
      sparsifyNAs = sparsify_nas,
      naCols = na_cols,
      dropCols = drop_cols,
      dropUnusedLevels = drop_unused_levels
    )
  )

  # convert output to tibble
  tibble::as_tibble(output)

}


#' Herd Cats
#'
#' @description many modeling functions expect matrix input with
#'   factor levels one-hot encoded.
#'
#'   - [cat_spread] will one-hot encode any factor or character variable
#'   in `data` and return a one-hot encoded `tibble`.
#'
#'   - `cat_gather` will apply the inverse operation and convert one-hot
#'   encoded columns back into factors.
#'
#' @param data Data with one-hot encoded variables that have names formatted
#'   as <NAME>_<CATEGORY>. For example, `x_a` and `x_b` should be column
#'   names for a one hot encoded variable `x` with levels `'a'` and `'b'`.
#'
#' @param factor_levels a list with names and values corresponding to
#'   factor names and factor levels, respectively. For example,
#'   `list(x = c('a','b'))` shows the expected input to gather a
#'   categorical variable named `x` with levels `'a'` and `'b'`
#'
#' @export
cat_gather <- function(
  data,
  factor_levels
) {

  old_names <- new_names <- names(data)

  factor_names <- factor_levels %>%
    tibble::enframe() %>%
    tidyr::unnest(cols = value) %>%
    dplyr::mutate(factor_name = paste(name, value, sep = "_")) %>%
    dplyr::select(factor_name, name) %>%
    tibble::deframe()

  new_names <- unique(dplyr::recode(old_names, !!!factor_names))

  for(i in seq_along(factor_levels)){

    .factor <- names(factor_levels)[i]
    .levels <- factor_levels[[i]]
    .names <- paste(.factor, .levels, sep = '_')

    new_col <- data[, .names] %>%
      apply(1, which.max) %>%
      as.numeric() %>%
      factor(levels = 1:length(.names), labels = .levels)

    data[[.factor]] <- new_col
    data[, .names] <- NULL

  }

  data[, new_names]

}

#' transfer factor levels
#'
#' @description take the factor levels in training data
#'   and copy them over to testing data. This is an important
#'   pre-processing step for data splits that may have
#'   different factor levels in training and testing sets.
#'
#' @param to the data that factor levels are transferred to
#' @param from the data that factor levels are transferred from
#'
#' @note `to` and `from` must have the same factor columns. For example,
#'   if `to` has a factor named `A` and `from` does not have a factor
#'   of the same name, the function will stop and tell you which
#'   factor variables are missing.
#'
#' @export
#'
cat_transfer <- function(to, from){

  chr_vars <- purrr::map_lgl(from, is.character)

  if(any(chr_vars)){
    warning(
      "the following character variables in ",
      "from should be converted to factors ",
      "before using transfer_factors: ",
      list_things(names(which(chr_vars))),
      call. = FALSE
    )
  }

  # check that the two frames have the same factor variables

  fctrs_to <- get_factors(to)
  fctrs_from <- get_factors(from)

  fctrs_only_in_to <- setdiff(fctrs_to, fctrs_from)
  fctrs_only_in_from <- setdiff(fctrs_from, fctrs_to)

  if(!rlang::is_empty(fctrs_only_in_to)){
    stop(
      paste(
        "to has some factors that are not in from:",
        list_things(fctrs_only_in_to)
      )
    )
  }

  if(!rlang::is_empty(fctrs_only_in_from)){
    stop(
      paste(
        "from has some factors are not in to:",
        list_things(fctrs_only_in_from)
      )
    )
  }

  levels_from <- purrr::map(
    .x = purrr::set_names(fctrs_from, fctrs_from),
    .f = ~ levels(from[[.x]])
  )

  for(f in names(levels_from)){
    to[[f]] %<>% factor(levels = levels_from[[f]])
  }

  return(to)

}



