#' Summarize quality control data in a WQP dataset
#'
#' @param data data downloaded from WQP and pre-processed using `preProcessResults`
#'
#' @return list of dataframes:  all rep data; lightly processed rep data; rep performance summarized (as RPD) by Tribe, analyte, year; blank data (field and lab); blank performance summarized (as number of values above MDLs) by Tribe, analyte, year, field/lab; data with field reps averaged and blanks removed
#' @export
#'
#' @importFrom plyr ddply
#' @importFrom plyr summarize
#' @importFrom plyr join_all
#' @importFrom plyr rbind.fill
#' @importFrom stats sd



summarizeQC <- function(data) {
  ### function summarizes replicates and blanks in WQX data
  ### function will always summarize rep performance by calendar year, organization, and analyte

  ### function returns a list with a bunch of dataframes:
  ### - all rep data
  ### - lightly processed rep data
  ### - rep performance summarized (as RPD) by Tribe, analyte, year
  ### - blank data (field and lab)
  ### - blank performance summarized (as number of values above MDLs) by Tribe, analyte, year, field/lab
  ### - data with field reps averaged and blanks removed


  ### Replicate performance
  ### pull out dups, exclude blanks
  tmpDat           <- data[!grepl(x = data$ActivityTypeCode, pattern = 'Quality Control Sample-Field Blank|Quality Control Sample-Lab Blank'), ]
  indicator_vector <- paste0(tmpDat$OrganizationFormalName,"__", tmpDat$OrganizationIdentifier, '__', tmpDat$MonitoringLocationIdentifier, "__", tmpDat$CharacteristicName, "__",tmpDat$ActivityStartDate)
  tmpDat$id        <- indicator_vector
  tmpDat           <- tmpDat[tmpDat$id %in% indicator_vector[duplicated(indicator_vector)], ]
  tmpDat           <- tmpDat[order(tmpDat$id), ]
  tmpDat$year      <- as.numeric(substr(tmpDat$ActivityStartDate, 1, 4))

  ### Identify lab reps
  lab.reps                <- tmpDat$id[grepl(x = tmpDat$ActivityTypeCode, pattern = 'Quality Control Sample-Lab Duplicate')]
  tmpDat$ActivityTypeCode <- ifelse(tmpDat$id %in% lab.reps, 'Lab replicate', 'Field replicate') # hesitant to overwrite this column but I think it makes sense to use the existing column.
  ###

  ### ISSUE: FUN not found in local environment
  fieldReps_proc_tmp    <- plyr::ddply(tmpDat, c('OrganizationFormalName', 'OrganizationIdentifier', 'MonitoringLocationIdentifier', 'CharacteristicName', 'ActivityTypeCode', 'id', 'year'),
                                      # .fun = function(ResultMeasureValue) {FUN(as.numeric(ResultMeasureValue))})
                                      plyr::summarize,
                                      RPD  = rpd(as.numeric(ResultMeasureValue))) # ,
  # n = sum(!is.na(as.numeric(ResultMeasureValue)))) # zeroes = reps were identical. Suggestive of raw data not being included
  fieldReps_proc_mean   <- plyr::ddply(tmpDat, c('OrganizationFormalName', 'OrganizationIdentifier', 'CharacteristicName', 'ActivityTypeCode', 'id', 'year') ,
                                       plyr::summarize,
                                       aver  = mean(as.numeric(ResultMeasureValue), na.rm = TRUE),
                                       MDL   = mean(DetectionQuantitationLimitMeasure.MeasureValue, na.rm = TRUE))
  reps_all    <- plyr::join_all(list(fieldReps_proc_tmp, fieldReps_proc_mean), by = c('OrganizationFormalName', 'OrganizationIdentifier', 'year',  'CharacteristicName', 'ActivityTypeCode', 'id'))
  reps_all    <- reps_all[order(reps_all$OrganizationFormalName, -reps_all$year, reps_all$CharacteristicName), ]

  ### summarize rep performance by Tribe, analyte, year
  rep.summary <- plyr::ddply(reps_all, c('OrganizationIdentifier', 'CharacteristicName', 'ActivityTypeCode', 'year'),
                             plyr::summarize,
                             n        = sum(!is.na(aver)),
                             RPD.mean = mean(RPD, na.rm = TRUE),
                             RPD.sd   = sd(RPD, na.rm = TRUE)
  )
  ### tmpDat[tmpDat$id == reps_all$id[2], ] # strange
  ### todo: resolve warnings appropriately
  # Warning messages:
  #   1: In diff(x, na.rm = TRUE) : NAs introduced by coercion

  ### Create dataset with averaged reps instead of both reps (double-counting in any stats)
  returnDat <- data[which(!duplicated(indicator_vector)), ]
  # confirm reps removed
  # indicator_vector2 <- paste0(returnDat$OrganizationIdentifier,"-", returnDat$MonitoringLocationIdentifier, "-", returnDat$CharacteristicName, "-", returnDat$ActivityStartDate)
  # any(duplicated(indicator_vector2))
  ### remove rep values (lose a lot of columns; this could be improved)
  ### rename and join data
  newDat        <- as.data.frame(do.call('rbind', strsplit(as.character(fieldReps_proc_mean$id),'__',fixed=TRUE)))
  names(newDat) <- c('OrganizationFormalName', 'OrganizationIdentifier', 'MonitoringLocationIdentifier', 'CharacteristicName', 'ActivityStartDate')
  newDat$ResultValueMeasure <- fieldReps_proc_mean$aver
  newDat$DetectionQuantitationLimitMeasure.MeasureValue <- fieldReps_proc_mean$MDL
  # newDat$units

  returnDat2 <- plyr::rbind.fill(list(returnDat, newDat))
  # nrow(returnDat2) == nrow(returnDat)+nrow(newDat) # should be TRUE
  ### now remove blanks
  returnDat2 <- returnDat2[!grepl(x = returnDat2$ActivityTypeCode, pattern = 'Quality Control Sample-Field Blank|Quality Control Sample-Lab Blank'), ]


  ### Separate blanks
  blanks <- data[grepl(x = data$ActivityTypeCode, pattern = 'Quality Control Sample-Field Blank|Quality Control Sample-Lab Blank'), ]
  ### ignore pH
  blanks <- blanks[!grepl(x = blanks$CharacteristicName, pattern = 'pH'), ]
  blanks$year <- as.numeric(substr(x = blanks$ActivityStartDate, start = 1, 4))
  ### Summarize blank performance
  blanks$exceedMDL <- ifelse(blanks$ResultMeasureValue > blanks$DetectionQuantitationLimitMeasure.MeasureValue, 1, 0)
  blank.summary <- plyr::ddply(blanks, c('OrganizationFormalName', 'OrganizationIdentifier', 'CharacteristicName', 'ActivityTypeCode', 'year'),
                               plyr::summarize,
                               n          = sum(!is.na(ResultMeasureValue)),
                               blank.mean = mean(ResultMeasureValue, na.rm = TRUE),
                               blank.sd   = sd(ResultMeasureValue, na.rm = TRUE),
                               exceedances= sum(exceedMDL)
  )
  # blank.summary$loc <- paste0(OrganizationIdentifier, '__', MonitoringLocationIdentifier)

  ### return data with averaged reps, rep summary
  invisible(list(rep_raw       = tmpDat,        # all rep data
                 rep_proc      = reps_all,      # lightly processed rep data
                 rep_summary   = rep.summary,   # rep performance summarized by Tribe, analyte, year
                 blank_proc    = blanks,        # blank data (field and lab)
                 blank_summary = blank.summary, # summarized blank data by Tribe, analyte, year, field/lab
                 data          = returnDat2))   # data with field reps averaged and blanks removed
}

