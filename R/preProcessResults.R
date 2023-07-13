
#' Process raw WQP data prior to analysis
#'
#' @param data data downloaded from WQP
#' @param multiplier multiplier applied to the detection limit when replacing values below detection limits. Default is 0.5
#' @param value_column name of the column in `data` containing values/concentrations
#' @param convert_ug_to_mg logical; if TRUE, values with units of ug/L are converted to mg/L
#' @param DL_column name of the column containing detection limit information
#' @param unit_column name of column identifying units
#'
#' @return dataframe
#' @export
#'
preProcessResults <- function(data, multiplier = 0.5, value_column = 'ResultMeasureValue', convert_ug_to_mg = TRUE, DL_column = 'DetectionQuantitationLimitMeasure.MeasureValue', unit_column = 'ResultMeasure.MeasureUnitCode') {
  ### function lightly pre-processes WQP data
  ### - removes non-numeric characters
  ### - replaces non-detects with multiplier * MDL
  ### - replaces values estimated as greater-than and less-than X with X
  ### - converts anything expressed in ug/kg to mg/kg

  ### TODO: how to handle values without reported MDLs? get median DL/QL by org-year-analyte?

  ### remove commas
  data[, value_column][grep(x = data[, value_column], pattern = ",")] <-  gsub(x = data[, value_column][grep(x = data[, value_column], pattern = ",")], pattern = ',', replacement = '')


  ### modify greater-than values
  n_grtr <- length(grep(x = data[, value_column], pattern = ">"))
  if ((length(n_grtr) > 0) & (n_grtr > 0)) {
    cat(n_grtr, 'values had greater-than symbols; replaced with highest measureable value\n')
    data[, value_column][grep(x = data[, value_column], pattern = ">")] <-  gsub(x = data[, value_column][grep(x = data[, value_column], pattern = ">")], start = 2, stop = nchar( data[, value_column][grep(x = data[, value_column], pattern = ">")]))
  }
  ### modify 'nm' values with NAs - 'not measured'?. This appears in WYDEQ_WATERSHED/WQX data
  n_nm <- length(grep(x = trimws(data[, value_column]), pattern = "^nm$|^NM$"))
  if ((length(n_nm) > 0) & (n_nm > 0)) {
    cat(n_nm, ' values were input as "nm"; these were replaced with NAs.\n')
    data[, value_column] <-  gsub(x = trimws(data[, value_column]), pattern = "^nm$|^NM$", replace = NA)
  }

  ### modify detection limits input as '<X' - replace with X. This appears in WYDEQ_WATERSHED/WQX data
  n_less_than <- length(grep(x = trimws(data[, DL_column]), pattern = "<"))
  if ((length(n_less_than) > 0) & (n_less_than > 0)) {
    cat(n_less_than, ' detection limit values had less-than symbols; replaced with lowest detectable value (e.g., a detection limit of "<1 mg/L" becomes "1 mg/L")\n')
    data[, DL_column] <-  as.numeric(gsub(x = trimws(data[, DL_column]), pattern = "<", replace = ''))
  }

  ### modify non-detects to be function of detection limit
  if (!is.numeric(data[, value_column])) {
    for (i in 1:nrow(data)) {
      if(grepl(x = data[, value_column][i], pattern = "Non-detect|^0$|<") && !is.na(data[, DL_column][i])) {
        ### TODO: how to handle cases without DL/QL listed? use average MDL for the parameter and add data flag?
        data[, value_column][i] <- multiplier * data[, DL_column][i]
        ### the above may be inaccurate for values that are entered as less than the lower end of a calibration curve.
        cat('Non-detect modified: replaced observation ', i, '(', data$CharacteristicName[i], ') with', multiplier*data[, DL_column][i], '\n')
      }
    }
  }

  ### replace values with ResultDetectionConditionText == 'Not Detected' with MDL
  if(any(grepl(x = names(data), pattern = 'ResultDetectionConditionText'))) {
    for (i in 1:nrow(data)) {
      if(grepl(x = tolower(data$ResultDetectionConditionText[i]), pattern = 'not detected|present below quantification limit')) {
        data[, value_column][i] <- multiplier * data[, DL_column][i]
        cat('replaced observation ', i, '(', data$CharacteristicName[i], ') with', multiplier*data[, DL_column][i], '\n')
      }
    }
  }



  ### replaces non-NA values below the MDL/PQL (set to zero or left as character) with multiplier*MDL/PQL
  if (is.numeric(data[, value_column])) {
    for (i in 1:nrow(data)) {
      if(!is.na(data[, value_column][i]) && (data[, value_column][i] == 0) && !is.na(data[, DL_column][i])) {
        data[, value_column][i] <- multiplier * data[, DL_column][i]
        cat('replaced observation ', i, '(', data$CharacteristicName[i], ') with', multiplier*data[, DL_column][i], '\n')
      }
    }
  }

  if (convert_ug_to_mg) {
    cat(sum(grepl(x = tolower(data[, unit_column]), pattern = 'ug/l')), ' observations changed from ug/L to mg/L\n')
    data[, value_column][grep(x = tolower(data[, unit_column]), pattern = 'ug/l')] <- as.numeric(data[, value_column][grep(x =  tolower(data[, unit_column]), pattern = 'ug/l')]) / 1e3
    data[, unit_column][grep(x = tolower(data[, unit_column]), pattern = 'ug/l')] <- 'mg/L'
  }

  ### convert values from character to numeric
  data[, value_column] <- as.numeric(data[, value_column])

  invisible(data)
}

