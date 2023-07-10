

#' Generate a Water Quality Report for an organization
#'
#' @param org organization (short name in WQP) whose data should be used to build the report. Multiple organization names can be input as a character vector; names will be homogenized and data will be pooled for analysis purposes.
#' @param startDate start date for data used in the report, in format `\%m-\%d-\%Y`
#' @param endDate final date for data used in the report, in format `\%m-\%d-\%Y`
#' @param parameters parameters to use in report. Must be entered as an index of acceptable parameters listed in params$params (e.g., `parameters = c(1:4,7)`)
#' @param extFile name of report-generating script, located in the inst/extdata folder of the R8WD R package
#' @param prompt_user if TRUE, user is prompted to use one of the organization codes in the `tribes` object provided with `R8WD`.
#'
#' @return Quarto markdown document
#' @export
#' @examples
#' create_report(org = 'Tribe 1')
#' create_report(org = 'Tribe 2')
#'
create_report <- function(org = 'TURTLEMT',
                          startDate = '01-01-2015',
                          endDate   = '12-31-2022',
                          parameters = c(1:length(params$params)),
                            extFile = 'script_generateReport.qmd',
                          prompt_user = TRUE) {
  targetFile <- system.file('extdata', extFile, package = 'R8WD')
  newFile    <- tempfile('wqp_report', fileext = '.qmd')
  token      <- '\'REPLACE_THIS_TEXT\'' # capture quotations

  if (prompt_user) {
    if (!any(grepl(pattern = paste0('^', toupper(org), '$'), x = tribes))) {
      stop(toupper(org), ' not found in list of Tribal organizations. Recommended organization names:\n', paste0(sort(tribes), collapse = '\n'))
    }
  }

  ### if org is a vector, create an insertable string. This is benign where length == 1, so no ifelse statement
  org <- paste0("\'", paste0(gsub(x = toupper(org), pattern = "\'|\"", replacement = ''), collapse = '\',\''), "\'")
  ### test for presence
  # grep(token, readLines(targetFile), value = TRUE) # remove ^ and $ to make this work
  newText    <- gsub(x = readLines(targetFile), pattern = token, replacement = org)

  ### add dates
  newText    <- gsub(x = newText, pattern = 'REPLACE_START_DATE', replacement = startDate)
  newText    <- gsub(x = newText, pattern = 'REPLACE_END_DATE', replacement = endDate)

  ### add params
  # parameters
  # REPLACE_PARAMS
  newParams  <- paste0(paste0(gsub(x = as.character(parameters), pattern = "\'|\"", replacement = ''), collapse = ','))
  # tst <- gsub(x = readLines(targetFile), pattern = token, replacement = newParams)
  newText    <- gsub(x = newText, pattern = '\'REPLACE_PARAMS\'', replacement = newParams)

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
