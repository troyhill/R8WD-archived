

#' Generate a Water Quality Report for an organization
#'
#' @param org organization (short name in WQP) whose data should be used to build the report
#' @param extFile name of report-generating script, located in the inst/extdata folder of the R8WD R package
#'
#' @return Quarto report
#' @export
#' @examples
#' create_report(org = 'Tribe 1')
#' create_report(org = 'Tribe 2')
#'
create_report <- function(org = 'CHEYRIVR',
                            extFile = 'script_hello_worldv3.R') {
  assign("args1", org, envir = .GlobalEnv)# <<- org # assign as global variable
  # print(args1)
  source(system.file('extdata', extFile, package = 'R8WD'))
}
