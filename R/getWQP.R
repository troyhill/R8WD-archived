
#' Pull and pre-process Water Quality Portal data
#'
#' @param organization organization name(s), sent to dataRetrieval::readWQPdata
#' @param characteristicName parameter name(s), sent to dataRetrieval::readWQPdata
#' @param startDate start date for data in format 'mm-dd-yyyy', sent to dataRetrieval::readWQPdata
#' @param endDate end date for data in format 'mm-dd-yyyy', sent to dataRetrieval::readWQPdata
#' @param multiplier multiplier used for replacing values below detection limits (see R8WD::preProcessResults)
#'
#' @return list of dataframes
#' @importFrom dataRetrieval readWQPdata
#' @export
#'
getWQP <- function(organization = Tribal_org,
                   characteristicName = params$params,
                   startDate = "01-01-2015",
                   endDate   = "12-31-2022",
                   multiplier = 0.5) {
  dat <- dataRetrieval::readWQPdata(organization = organization,
                             characteristicName = characteristicName,
                             startDate = startDate,
                             endDate   = endDate)

  dat2 <- R8WD::preProcessResults(dat, multiplier = multiplier)
  invisible(dat2)
}