

#' Generate a Water Quality Report for an organization
#'
#' @param org organization (short name in WQP) whose data should be used to build the report. Multiple organization names can be input as a character vector; names will be homogenized and data will be pooled for analysis purposes.
#' @param extFile name of report-generating script, located in the inst/extdata folder of the R8WD R package
#'
#' @return Quarto markdown document
#' @export
#' @examples
#' create_report(org = 'Tribe 1')
#' create_report(org = 'Tribe 2')
#'
create_report <- function(org = 'TURTLEMT',
                            extFile = 'script_generateReport.qmd') {
  targetFile <- system.file('extdata', extFile, package = 'R8WD')
  newFile    <- tempfile('wqp_report', fileext = '.qmd')
  token      <- '\'REPLACE_THIS_TEXT\'' # capture quotations

  ### if org is a vector, create an insertable string. This is benign where length == 1, so no ifelse statement
  org <- paste0("\'", paste0(gsub(x = toupper(org), pattern = "\'|\"", replacement = ''), collapse = '\',\''), "\'")
  ### test for presence
  # grep(token, readLines(targetFile), value = TRUE)
  newText    <- gsub(x = readLines(targetFile), pattern = token, replacement = org)

  fileConn   <- file(newFile)
  writeLines(newText, fileConn)
  close(fileConn)

  utils::browseURL(newFile)

  # assign("args1", org, envir = .GlobalEnv)# <<- org # assign as global variable
  # fil <- tempfile('deleteMe', fileext = '.rds')
  # saveRDS(object = args1, file = fil, compress = FALSE)
  # print(args1)
  # source(system.file('extdata', extFile, package = 'R8WD'))
  # quarto::quarto_render(input = system.file('extdata', extFile, package = 'R8WD'))
  ### inconsistent quarto rendering error.
  # list.files('C://Program Files//RStudio//bin//quarto//bin//')
  # quarto_location <- 'C://Program Files//RStudio//bin//quarto//bin//quarto.cmd'
  # if (file.exists(quarto_location)) {
  #   Sys.setenv(QUARTO_PATH="/home/roger/quarto-cli/package/dist/bin/quarto")
  # }
}
