#' raw real-world data for robins
#'
#' A dataset containing data simulated with constant recovery probability in
#' one-dimensional space and continuous estimates of all parameters.
#'
#' @docType data
#' @usage data(robinsInit)
#' @format A list of objects.
#' \describe{
#'   \item{markData}{data.frame with names of the area where the marking took place (markArea) and number of marked individuals per area}
#'   \item{recoveryData}{data.frame with one row for every dead recovery. Columns are names of the area where the marking took place (markArea), geographical coordinates (recLat and recLon) and age at death (age)}
#'   \item{window}{object of class owin containing the boundaries of the world map in a rectangle around the data points}
#'   \item{observationTime}{numeric, length of observation time. One length for all individuals in the dataset.}
#' }
#' 
#' @references This data set was artificially created for the CONSURE package.
#' @keywords datasets
#' @examples
#'
#' data(robinsInit)
#' str(robinsInit)
#'
"robinsInit"
