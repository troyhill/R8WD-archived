#' Summarize quality control data in a WQP dataset
#'
#' @param data data downloaded from WQP and pre-processed using `preProcessResults`
#'
#' @return list of dataframes:  all rep data; lightly processed rep data; rep performance summarized (as RPD if there is a maximum of two replicates in a set, or as coefficents of variation for n>2) by Tribe, analyte, year; blank data (field and lab); blank performance summarized (as number of values above MDLs) by Tribe, analyte, year, field/lab; data with field reps averaged and blanks removed
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
  ### - rep performance summarized (as RPD or CV) by Tribe, analyte, year
  ### - blank data (field and lab)
  ### - blank performance summarized (as number of values above MDLs) by Tribe, analyte, year, field/lab
  ### - data with field reps averaged and blanks removed

  ### TODO: make this handle all varieties of blanks, not just lab and field.
  ### TODO: incorporate other QC measures (matrix spikes?)
  ### TODO: find a dataset with lab reps to see if the function mishandles anything (e.g., what is behavior if a field rep is also lab repped? How are lab reps linked to parent sample?)

  ### convert pH from log scale to molar [H+]
  if (length(data$ResultMeasureValue[grep(x = data$CharacteristicName, pattern = 'pH')]) > 0) {
    data$ResultMeasureValue[grep(x = data$CharacteristicName, pattern = 'pH')] <- 10^(-1*data$ResultMeasureValue[grep(x = data$CharacteristicName, pattern = 'pH')])
  }
  ### check that pH has no detection limit entered
  # data$DetectionQuantitationLimitMeasure.MeasureValue[grep(x = data$CharacteristicName, pattern = 'pH')]
  ### Replicate performance
  ### pull out dups, exclude blanks
  ### including ActivityDepthHeightMeasure.MeasureValue in the ID avoids counting depth profiles as replicates (sometimes n = 17)
  ### still consistently seeing more than n=2 reps
  tmpDat           <- data[!grepl(x = data$ActivityTypeCode, pattern = 'Quality Control Sample-Field Blank|Quality Control Sample-Lab Blank'), ]
  indicator_vector <- paste0(tmpDat$OrganizationFormalName,"__", tmpDat$OrganizationIdentifier, '__', tmpDat$MonitoringLocationIdentifier, "__", tmpDat$CharacteristicName, "__", tmpDat$ActivityDepthHeightMeasure.MeasureValue, "__",tmpDat$ActivityStartDate)
  tmpDat$id        <- indicator_vector
  tmpDat           <- tmpDat[tmpDat$id %in% indicator_vector[duplicated(indicator_vector)], ]
  tmpDat$year      <- as.numeric(substr(tmpDat$ActivityStartDate, 1, 4))

  ### Identify lab reps
  lab.reps                <- tmpDat$id[grepl(x = tmpDat$ActivityTypeCode, pattern = 'Quality Control Sample-Lab Duplicate')]
  tmpDat$ActivityTypeCode <- ifelse(tmpDat$id %in% lab.reps, 'Lab replicate', 'Field replicate') # hesitant to overwrite this column but I think it makes sense to use the existing column.
  ###
  tmpDat           <- tmpDat[order(tmpDat$OrganizationFormalName, tmpDat$ActivityStartDate, tmpDat$MonitoringLocationIdentifier, tmpDat$CharacteristicName), ]

  ### Create dataset with averaged reps instead of both reps (double-counting in any stats)
  returnDat <- data[which(!duplicated(indicator_vector)), ]


  ### determine whether to use RPD or CV (are all reps pairs or are there cases with n>2?)
  if (nrow(tmpDat) > 0) {
    largest_replicate_set <- max(table(tmpDat$id))
    ### These may be duplicate entries? Everything is identical, including the values:
    # tst <- data[!grepl(x = data$ActivityTypeCode, pattern = 'Quality Control Sample-Field Blank|Quality Control Sample-Lab Blank'), ]
    # tst$id <- paste0(tst$OrganizationFormalName,"__", tst$OrganizationIdentifier, '__', tst$MonitoringLocationIdentifier, "__", tst$CharacteristicName, "__", tst$ActivityDepthHeightMeasure.MeasureValue, "__",tst$ActivityStartDate)
    # tst[grep(x = tst$id, pattern = 'Turtle Mountain Environmental Office__TURTLEMT__TURTLEMT-WHEATBEACH__E_coli__NA__2018-07-31'), ]
    ### ISSUE: FUN not found in local environment
    if (largest_replicate_set == 2) {
      fieldReps_proc_tmp    <- plyr::ddply(tmpDat, c('OrganizationFormalName', 'OrganizationIdentifier', 'MonitoringLocationIdentifier', 'CharacteristicName', 'ActivityTypeCode', 'id', 'ActivityStartDate', 'year'),
                                           # .fun = function(ResultMeasureValue) {FUN(as.numeric(ResultMeasureValue))})
                                           plyr::summarize,
                                           n     = sum(!is.na(as.numeric(ResultMeasureValue))),
                                           RPD   = rpd(as.numeric(ResultMeasureValue)),
                                           aver  = mean(as.numeric(ResultMeasureValue), na.rm = TRUE),
                                           MDL   = mean(DetectionQuantitationLimitMeasure.MeasureValue, na.rm = TRUE),
                                           MDLs_identical = as.logical(sd(na.omit(DetectionQuantitationLimitMeasure.MeasureValue)) == 0)
      ) # ,
      variation_measure <- 'Relative Percent Difference'
      # n = sum(!is.na(as.numeric(ResultMeasureValue)))) # zeroes = reps were identical. Suggestive of raw data not being included
    } else if (largest_replicate_set > 2) {
      fieldReps_proc_tmp    <- plyr::ddply(tmpDat, c('OrganizationFormalName', 'OrganizationIdentifier', 'MonitoringLocationIdentifier', 'CharacteristicName', 'ActivityTypeCode', 'id',  'ActivityStartDate', 'year'),
                                           # .fun = function(ResultMeasureValue) {FUN(as.numeric(ResultMeasureValue))})
                                           plyr::summarize,
                                           n     = sum(!is.na(as.numeric(ResultMeasureValue))),
                                           RPD   = cv(as.numeric(ResultMeasureValue)),
                                           aver  = mean(as.numeric(ResultMeasureValue), na.rm = TRUE),
                                           MDL   = mean(DetectionQuantitationLimitMeasure.MeasureValue, na.rm = TRUE),
                                           MDLs_identical = as.logical(sd(na.omit(DetectionQuantitationLimitMeasure.MeasureValue)) == 0)
      )
      variation_measure <- 'Coefficient of Variation'
    }
    message('Replicate sets had a maximum n = ', largest_replicate_set,'; ', variation_measure, ' used as variation measure.')

    # fieldReps_proc_mean   <- plyr::ddply(tmpDat, c('OrganizationFormalName', 'OrganizationIdentifier', 'MonitoringLocationIdentifier', 'CharacteristicName', 'ActivityTypeCode', 'id', 'year') ,
    #                                      plyr::summarize,
    #                                      aver  = mean(as.numeric(ResultMeasureValue), na.rm = TRUE),
    #                                      MDL   = mean(DetectionQuantitationLimitMeasure.MeasureValue, na.rm = TRUE), # TODO: note if MDLs are not identical. For now, conservative behavior seems like reporting the highest MDL
    #                                      n     = sum(!is.na(as.numeric(ResultMeasureValue))),
    #                                      MDLs_identical = as.logical(sd(na.omit(DetectionQuantitationLimitMeasure.MeasureValue)) == 0)
    #                                      )
    # reps_all    <- plyr::join_all(list(fieldReps_proc_tmp, fieldReps_proc_mean), by = c('OrganizationFormalName', 'OrganizationIdentifier', 'year',  'CharacteristicName', 'ActivityTypeCode', 'id'))
    reps_all    <- fieldReps_proc_tmp[order(fieldReps_proc_tmp$OrganizationFormalName, fieldReps_proc_tmp$ActivityStartDate, fieldReps_proc_tmp$MonitoringLocationIdentifier, fieldReps_proc_tmp$CharacteristicName), ]

    ### summarize rep performance by Tribe, analyte, year
    rep.summary <- plyr::ddply(reps_all, c('OrganizationIdentifier', 'CharacteristicName', 'ActivityTypeCode', 'year'),
                               plyr::summarize,
                               n          = sum(!is.na(aver)),
                               RPD.median = median(RPD, na.rm = TRUE),
                               RPD.IQR    = IQR(RPD, na.rm = TRUE)
    )
    rep.summary$variation_measure <- variation_measure
    ### tmpDat[tmpDat$id == reps_all$id[2], ] # strange
    ### todo: resolve warnings appropriately
    # Warning messages:
    #   1: In diff(x, na.rm = TRUE) : NAs introduced by coercion
    # confirm reps removed
    # indicator_vector2 <- paste0(returnDat$OrganizationIdentifier,"-", returnDat$MonitoringLocationIdentifier, "-", returnDat$CharacteristicName, "-", returnDat$ActivityStartDate)
    # any(duplicated(indicator_vector2))
    ### remove rep values (lose a lot of columns; this could be improved)
    ### rename and join data
    newDat        <- as.data.frame(do.call('rbind', strsplit(as.character(reps_all$id),'__',fixed=TRUE)))
    names(newDat) <- c('OrganizationFormalName', 'OrganizationIdentifier', 'MonitoringLocationIdentifier', 'CharacteristicName', 'ActivityDepthHeightMeasure.MeasureValue', 'ActivityStartDate')
    newDat$ResultValueMeasure <- reps_all$aver
    newDat$DetectionQuantitationLimitMeasure.MeasureValue <- reps_all$MDL
    newDat$ActivityStartDate <- as.Date(newDat$ActivityStartDate, format = '%Y-%m-%d')
    # newDat$units
    returnDat2 <- plyr::rbind.fill(list(returnDat, newDat))
  } else {
    returnDat2  <- returnDat
    reps_all    <- data.frame(OrganizationFormalName = NA,
                              OrganizationIdentifier = NA,
                              MonitoringLocationIdentifier = NA,
                              CharacteristicName = NA,
                              ActivityTypeCode = NA,
                              id = NA,
                              ActivityStartDate = NA,
                              year = NA,
                              date = NA,
                              n     = NA,
                              RPD = NA,
                              aver  = NA,
                              MDL   = NA,
                              MDLs_identical = NA
                              )
    rep.summary <- data.frame(OrganizationIdentifier = NA,
    CharacteristicName = NA,
    ActivityTypeCode   = NA,
    year       = NA,
    n          = NA,
    RPD.median = NA,
    RPD.IQR    = NA)
  }


  # nrow(returnDat2) == nrow(returnDat)+nrow(newDat) # should be TRUE
  ### remove blanks and all other QC data
  returnDat2 <- returnDat2[!grepl(x = returnDat2$ActivityTypeCode, pattern = 'Quality Control Sample'), ] # 'Quality Control Sample-Field Blank|Quality Control Sample-Lab Blank'), ]
  returnDat2 <- returnDat2[order(returnDat2$OrganizationFormalName, returnDat2$ActivityStartDate, returnDat2$MonitoringLocationIdentifier, returnDat2$CharacteristicName), ]

  ### Separate blanks
  blanks <- data[grepl(x = data$ActivityTypeCode, pattern = 'Quality Control Sample-Field Blank|Quality Control Sample-Lab Blank'), ] # pattern = 'Blank'), ] #
  ### pH entry mis-categorized as blank in umut
  ### TODO:
  # blanks      <- blanks[!grepl(x = blanks$CharacteristicName, pattern = 'pH'), ]
  blanks$year <- as.numeric(substr(x = blanks$ActivityStartDate, start = 1, 4))
  ### Summarize blank performance
  blanks$exceedMDL <- ifelse(blanks$ResultMeasureValue > blanks$DetectionQuantitationLimitMeasure.MeasureValue, 1, 0)
  blank.summary <- plyr::ddply(blanks, c('OrganizationFormalName', 'OrganizationIdentifier', 'CharacteristicName', 'ActivityTypeCode', 'year'),
                               plyr::summarize,
                               n          = sum(!is.na(ResultMeasureValue)),
                               blank.mean = mean(ResultMeasureValue, na.rm = TRUE),
                               blank.sd   = sd(ResultMeasureValue, na.rm = TRUE),
                               above_MDL= sum(exceedMDL)
  )
  # blank.summary$loc <- paste0(OrganizationIdentifier, '__', MonitoringLocationIdentifier)

  ### sort dataframes
  blanks <- blanks[order(blanks$OrganizationFormalName, blanks$ActivityStartDate, blanks$CharacteristicName), ]

  ### convert pH from molar [H+] to log scale
  if (length(blanks$ResultMeasureValue[grep(x = blanks$CharacteristicName, pattern = 'pH')]) > 0)     blanks$ResultMeasureValue[grep(x = blanks$CharacteristicName, pattern = 'pH')]             <- -log10(blanks$ResultMeasureValue[grep(x = blanks$CharacteristicName, pattern = 'pH')])
  if (length(blank.summary$blank.mean[grep(x = blank.summary$CharacteristicName, pattern = 'pH')]) > 0)    blank.summary$blank.mean[grep(x = blank.summary$CharacteristicName, pattern = 'pH')]  <- -log10(blank.summary$blank.mean[grep(x = blank.summary$CharacteristicName, pattern = 'pH')])
  if (length(blank.summary$blank.sd[grep(x = blank.summary$CharacteristicName, pattern = 'pH')]) > 0) blank.summary$blank.sd[grep(x = blank.summary$CharacteristicName, pattern = 'pH')]         <- -log10(blank.summary$blank.sd[grep(x = blank.summary$CharacteristicName, pattern = 'pH')])
  if (length(returnDat2$ResultMeasureValue[grep(x = returnDat2$CharacteristicName, pattern = 'pH')]) > 0) returnDat2$ResultMeasureValue[grep(x = returnDat2$CharacteristicName, pattern = 'pH')] <- -log10(returnDat2$ResultMeasureValue[grep(x = returnDat2$CharacteristicName, pattern = 'pH')])
  if (length(tmpDat$ResultMeasureValue[grep(x = tmpDat$CharacteristicName, pattern = 'pH')]) > 0)     tmpDat$ResultMeasureValue[grep(x = tmpDat$CharacteristicName, pattern = 'pH')]             <- -log10(tmpDat$ResultMeasureValue[grep(x = tmpDat$CharacteristicName, pattern = 'pH')])
  if (!all(is.na(reps_all)) && (length(reps_all$aver[grep(x = reps_all$CharacteristicName, pattern = 'pH')]) > 0)) { # if there are reps and they include pH
    reps_all$aver[grep(x = reps_all$CharacteristicName, pattern = 'pH')] <- -log10(reps_all$aver[grep(x = reps_all$CharacteristicName, pattern = 'pH')])
    }

  ### return data with averaged reps, rep summary
  invisible(list(rep_raw       = tmpDat,        # all rep data
                 rep_proc      = reps_all,      # lightly processed rep data
                 rep_summary   = rep.summary,   # rep performance summarized by Tribe, analyte, year
                 blank_proc    = blanks,        # blank data (field and lab)
                 blank_summary = blank.summary, # summarized blank data by Tribe, analyte, year, field/lab
                 data          = returnDat2))   # data with field reps averaged and blanks removed
}

