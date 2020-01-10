
check_cats <- function(data){

  # check if the data have character variables
  var_is_character <- purrr::map_lgl(data, is.character)

  if(any(var_is_character)){

    warning(
      'some variables in data are characters.',
      ' These will be converted to factors',
      call. = FALSE
    )

    data %<>% dplyr::mutate_if(is.character, as.factor)

  }

  # make sure there is at least one factor
  var_is_factor <- purrr::map_lgl(data, is.factor)

  if(!any(var_is_factor)){
    warning('There are no factor variables in data', call. = FALSE)
    return(NULL)
  }

  data

}
