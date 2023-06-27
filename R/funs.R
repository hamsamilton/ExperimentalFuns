#' dilution_calculator
#'
#' This function takes as input any three of start concentration, start volume,
#' final concentration, and final volume, and returns the missing value. used for
#' calculating dilutions for experiments.
#'
#' @param start_concentration The starting concentration. Default is NA.
#' @param start_volume The starting volume. Default is NA.
#' @param final_volume The final volume. Default is NA.
#' @param final_concentration The final concentration. Default is NA.
#'
#' @return The missing value calculated from the given equation.
#'
#' @examples
#' calculate_missing(start_concentration = 10, start_volume = 20, final_volume = 40)
#' calculate_missing(start_concentration = 10, start_volume = 20, final_concentration = 20)
#'
#' @export
#'
dilution_calculator <- function(start_concentration = NA, start_volume = NA, final_volume = NA, final_concentration = NA){

  if(sum(is.na(c(start_concentration, start_volume, final_volume, final_concentration))) != 1){
    stop("Exactly three out of four parameters must be provided!")
  }

  if(is.na(start_concentration)){
    return((final_concentration * final_volume) / start_volume)
  } else if(is.na(start_volume)){
    return((final_concentration * final_volume) / start_concentration)
  } else if(is.na(final_volume)){
    return((start_concentration * start_volume) / final_concentration)
  } else if(is.na(final_concentration)){
    return((start_concentration * start_volume) / final_volume)
  }

}



