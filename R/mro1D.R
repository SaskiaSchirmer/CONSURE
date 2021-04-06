#' simulated data for the continuous approach (1D)
#'
#' A dataset containing data simulated with constant recovery probability in
#' one-dimensional space and continuous estimates of all parameters.
#'
#' @docType data
#' @usage data(mro1D)
#' @format A markRecaptureObject and a numeric:
#' \describe{
#'   \item{mro}{An object of the type markRecaptureObject containing the simulated
#'   data with the true values of the parameteres and the estimated values of the
#'   continuous approach}
#'   \item{res_x}{numeric, resolution for the plots}
#' }
#' 
#' @references This data set was artificially created for the CONSURE package.
#' @keywords datasets
#' @examples
#'
#' data(mro1D)
#' str(mro)
#'
"mro1D"