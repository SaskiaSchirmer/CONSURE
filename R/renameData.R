#' rename recovery data with default column names
#'
#' This function renames the recovery data with default column names.
#' @param recoveryData data.frame with dead recoveries, containing information
#' on breeding origin, geographical coordinates of recovery and age at death
#' @param xname character, name of column containing information on longitude,
#' specify this in one-dimensional case
#' @param yname character, name of column containing information on latitude,
#' defaults to NULL, do not specify in one-dimensional case, but use xname
#' instead
#' @param timename character, name of column containing information on age at
#' death
#' @param markAreaName character, name of column containing information on
#' breeding origin
#' @return recoveryData with default column names
#' @export
#' @examples{
#'     renameData(recoveryData = robinsInit$recoveryData,
#'                xname        = "recLon",
#'                yname        = "recLat",
#'                timename     = "age",
#'                markAreaName = "markArea")
#' }
renameData <- function(recoveryData, xname, yname = NULL, timename,
                       markAreaName) {
  if (is.null(yname)) {
    colnamesMatrix <- matrix(
      c(
        markAreaName, xname, timename,
        "markArea", "longitude", "age"
      ),
      ncol = 2, nrow = 3
    )
  } else {
    colnamesMatrix <- matrix(
      c(
        markAreaName, xname, yname, timename,
        "markArea", "longitude", "latitude", "age"
      ),
      ncol = 2, nrow = 4
    )
  }

  colnames(colnamesMatrix) <- c("real", "default")

  index <- match(colnamesMatrix[, "real"], colnames(recoveryData))

  colnames(recoveryData) <- colnamesMatrix[, "default"][index]
  return(recoveryData)
}
