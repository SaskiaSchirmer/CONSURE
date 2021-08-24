#' simulated data for the combined approach (2D)
#'
#' A dataset containing data simulated with increasing recovery probability in
#' two-dimensional space and continuous and combined estimates of all parameters.
#'
#' @docType data
#' @usage data(mro2DIncreasing)
#' @format A markRecaptureObject.
#' \describe{
#'   \item{mro1DIncreasing}{An object of the type markRecaptureObject containing the simulated
#'   data with the true values of the parameteres and the estimated values of the
#'   continuous approach}
#'   \item{split}{numeric vector indicating for each point in space to which
#'    discrete wintering area it belongs}
#' }
#'
#' @references This data set was artificially created for the CONSURE package.
#' @keywords datasets
#' @examples
#'
#' data(mro2DIncreasing)
#' str(mro2DIncreasing)
#'
"mro2DIncreasing"
