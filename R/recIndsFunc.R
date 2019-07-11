#' calculates number of recovered individuals from marked and recoveries data frame
#'
#' This function calculates the number of recovered individuals marked in one breeding area
#' given the data frame of dead recoveries and optionally a
#' @param recoveries data frame of recoveries with column on marking location
#' @param marked data frame with column for marking location names
#' @return vector of number of recovered individuals per breeding area
#' @export
#' @examples recIndsFunc()

recIndsFunc <- function(breedingAreaNames,recoveries){
    as.numeric(table(recoveries$markArea)[match(breedingAreaNames,names(table(recoveries$markArea)))])
}
