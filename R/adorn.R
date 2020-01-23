




#' Adorning cats
#'
#' For graphs or tables, categorical variables are often accompanied
#'   by sample sizes and corresponding proportions for each category.
#'   For example, group A had 10 participants, accounting for 50%
#'   of the A versus B study. These functions add descriptive
#'   counts and proportions to existing categorical variables
#'   so that presenting these data is easier.
#'
#' @param x a categorical variable (i.e., a factor)
#' @param sep a string that will be placed between the current levels
#'   of `x` and the adorning sample size values.
#' @param n_string a string showing how you would like to indicate
#'   that the number of observations in each group will be
#'   presented. For example, if a group of size 10 is called `'A'`,
#'   then using `n_string = 'N = '` will turn the category `'A'`
#'   into `'A, N = 10'`
#'
#' @return a factor with new levels
#' @export
#'
#' @examples
#'
#' x <- factor(letters[c(1,1,1,1, 2,2,2,2)])
#' cat_adorn_nobs(x)
#'
cat_adorn_nobs <- function(x, sep = ', ', n_string = 'N = '){

  .counts <- table(x)
  .levels <- levels(x)
  .labels <- glue::glue('{.levels}{sep}{n_string}{.counts}')

  factor(x, levels = .levels, labels = as.character(.labels))

}

#' Adorning cats
#'
#' For graphs or tables, categorical variables are often accompanied
#'   by sample sizes and corresponding proportions for each category.
#'   For example, group A had 10 participants, accounting for 50%
#'   of the A versus B study. These functions add descriptive
#'   counts and proportions to existing categorical variables
#'   so that presenting these data is easier.
#'
#' @param x a categorical variable (i.e., a factor)
#' @param sep a string that will be placed between the current
#'   levels of `x` and the adorning proportions.
#' @param parentheses `TRUE` or `FALSE`. If `TRUE`, parentheses
#'   are placed around the percentage values. If `FALSE`, then
#'   nothing is placed around the percentage values.
#'
#' @return a factor with new levels
#' @export
#'
#' @examples
#'
#' x <- factor(letters[c(1,1,1,1, 2,2,2,2)])
#' cat_adorn_perc(x, parentheses = FALSE)
#' cat_adorn_perc(cat_adorn_nobs(x))
#'
#'
cat_adorn_perc <- function(x, sep = ' ', parentheses = TRUE){

  .counts <- table(x)
  .percts <- adapt_round(100 * .counts / sum(.counts))
  .levels <- levels(x)

  .labels <- if(parentheses) {
    glue::glue('{.levels}{sep}({.percts}%)')
  } else {
    glue::glue('{.levels}{sep}{.percts}%')
  }

  factor(x, levels = .levels, labels = as.character(.labels))

}

adapt_round <- function(x){

  n <- length(x)

  if(n==0) return("NA")

  output <- rep( "NA", n )

  if(all(is.na(x))) return(rep("NA", n))

  if(!is.numeric(x)) stop("x should be numeric", call. = FALSE)

  x_abs <- abs(x)

  loop_index <- which(!is.na(x))

  for(i in loop_index){

    if(x_abs[i] < 10) {
      dig = 2
    } else if(x_abs[i] < 100){
      dig = 1
    } else {
      dig = 0
    }

    output[i] <- format(
      round(x[i], dig),
      nsmall = dig,
      big.mark = ','
    )

  }

  output

}
