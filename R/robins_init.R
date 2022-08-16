#' raw real-world data for robins
#'
#' A dataset containing the raw data of robins marked at the ringing station
#' Greifswalder Oie, Germany, during autumn migration and dead recoveries
#' between November and February in Europe and Africa.
#'
#'
#' @docType data
#' @usage data(robins_init)
#' @format A list of objects.
#' \describe{
#'   \item{mark_data}{data.frame with names of the area where the marking took
#'   place (mark_area) and number of marked individuals per area}
#'   \item{recoveryData}{data.frame with one row for every dead recovery.
#'   Columns are names of the area where the marking took place (markArea),
#'   geographical coordinates (recLat and recLon) and age at death (age)}
#'   \item{window}{object of class owin containing the boundaries of the world
#'   map in a rectangle around the data points}
#'   \item{observationTime}{numeric, length of observation time. One length
#'   for all individuals in the dataset.}
#' }
#'
#' @references The data is provided by the Verein Jordsand e.V. and the
#' Beringungszentrale Hiddensee.
#' @keywords datasets
#' @examples
#'
#' data(robins_init)
#' str(robins_init)
"robins_init"
