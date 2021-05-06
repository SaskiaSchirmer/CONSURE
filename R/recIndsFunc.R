#' calculates number of recovered individuals from marked and recoveries data frame
#'
#' This function calculates the number of recovered individuals marked in one breeding area
#' given the data frame of dead recoveries and a vector of names of breeding areas
#' @param breedingAreaNames character vector with names of breeding areas
#' @param recoveries data frame of recoveries with column on marking location
#' @return vector of number of recovered individuals per breeding area
#' @export
#' @examples recIndsFunc(c("early","late"),robinsInit$recoveryData)

recIndsFunc <- function(breedingAreaNames,recoveries){
    as.numeric(table(recoveries$markArea)[match(breedingAreaNames,names(table(recoveries$markArea)))])
}
