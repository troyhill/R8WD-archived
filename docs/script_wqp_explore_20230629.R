### Script shows an approach to pulling tribal data from WQP and
### summarizing field and lab blanks and replicates


### install and load any needed packages
pkgs <- c('dataRetrieval', 'ggplot2', 'plyr', 'remotes', 'tidyverse')
if (!all(pkgs %in% installed.packages())) { install.packages(pkgs[which(!pkgs %in% installed.packages())])}
# remotes::install_github('troyhill/R8WD') # install/update R8WD

library(tidyverse)
library(dataRetrieval)
library(ggplot2)
library(plyr)
library(R8WD)

# pull data ---------------------------------------------------------------

### tribe names and parameters are provided with the package
### consider making a wrapper that fails gracefully when a param or org doesn't have data
tribeData1 <- dataRetrieval::readWQPdata(organization = tribes,
                                         characteristicName = params$params,
                                         startDate = "01-01-2015",
                                         endDate   = "12-31-2022")

### change negative pH readings to NAs
nrow(
  tribeData1[(tribeData1$CharacteristicName %in% 'pH') & (tribeData1$ResultMeasureValue <= 0) & !is.na(tribeData1$ResultMeasureValue), c(1, 7, 32, 34, 36)]
) # 100 unflagged pH observations <=0?

bs <- tribeData1[(tribeData1$CharacteristicName %in% 'pH') & (tribeData1$ResultMeasureValue <= 0) & !is.na(tribeData1$ResultMeasureValue), ]
unique(bs$OrganizationIdentifier)          # SRST, LWRBRULE
unique(substr(bs$ActivityStartDate, 1, 4)) # 2016, 2019, 2020, 2021
# write.csv(bs, file = 'negative_pH_data_20230620.csv', row.names = FALSE)
### change to NA
tribeData1[(tribeData1$CharacteristicName %in% 'pH') & (tribeData1$ResultMeasureValue <= 0) & !is.na(tribeData1$ResultMeasureValue), 'ResultMeasureValue'] <- NA



tribeData2 <- preProcessResults(tribeData1, multiplier = 0.5)
tribeData2$CharacteristicName <- params$new_param[match(tribeData2$CharacteristicName, params$params)]


# a  <- head(tribeData2$CharacteristicName)
# a2 <- params$new_param
# a3 <- params$params
# a2[match(a, a3)]

# Summarize quality control samples for all tribes, params --------------------------------------------------

# unique(tribeData1$MeasureQualifierCode)
# Translating data flags: https://www.waterqualitydata.us/portal_userguide/
# [1] NA  "U" "H" "J" "B" "T"
# B = Detection in blank, Analyte found in sample and associated blank
# U = Not Detected: The analyte was analyzed for, but was not detected at a level greater than or equal to the level of the adjusted Contract Required Quantitation Limit (CRQL) for sample and method.
# H = holding time violation
# J = Estimated: The analyte was positively identified and the associated numerical value is the approximate concentration of the analyte in the sample.
# T = Hardness by Calculation Method - Standard Methods 2340B - 19th Ed
# D = Contract Required Quantitation Limit (CRQL) not met due to sample matrix interference, dilution required.
# L = Lowest available reporting limit for the analytical method used.
# DL = Not Detected: The analyte was not detected at a level >= to the Method Detection Limit for the analysis.
# PNQ = No Quantifiable Result Reported
# T = Hardness by Calculation Method - Standard Methods 2340B - 19th Ed


### summarize blank and rep performance
allSummary <- summarizeQC(data = tribeData2)

ggplot(allSummary$rep_proc[grepl(x = allSummary$rep_proc$ActivityTypeCode, pattern = 'Field replicate'), ], aes(x = OrganizationIdentifier, y = RPD, col = OrganizationIdentifier)) +
  geom_boxplot() + theme_bw() +
  theme(axis.text.x=element_blank()) +
  facet_wrap(. ~ CharacteristicName)

ggplot(allSummary$rep_summary[grepl(x = allSummary$rep_summary$ActivityTypeCode, pattern = 'Field replicate'), ], aes(x = year, y = RPD.mean, col = OrganizationIdentifier)) +
  geom_line(linetype = 2) +
  facet_wrap(. ~ CharacteristicName) + theme_bw() +
  geom_pointrange(aes(ymax = RPD.mean + RPD.sd, ymin = RPD.mean - RPD.sd), position = position_jitter(width = 0.1, height = NULL, seed = 1)) +
  labs(y = 'Mean relative percent difference (\u00b11SD)', title = 'Field replicate performance', x = '', colour = 'Tribe')

ggplot(allSummary$rep_summary[grepl(x = allSummary$rep_summary$ActivityTypeCode, pattern = 'Lab replicate'), ], aes(x = year, y = RPD.mean, col = OrganizationIdentifier)) +
  geom_line(linetype = 2) +
  facet_wrap(. ~ CharacteristicName) + theme_bw() +
  geom_pointrange(aes(ymax = RPD.mean + RPD.sd, ymin = RPD.mean - RPD.sd), position = position_jitter(width = 0.1, height = NULL, seed = 1)) +
  labs(y = 'Mean relative percent difference (\u00b11SD)', title = 'Lab replicate performance', x = '', colour = 'Tribe')


ggplot(allSummary$blank_summary[grepl(x = allSummary$blank_summary$ActivityTypeCode, pattern = 'Quality Control Sample-Field Blank'), ], aes(x = year, y = exceedences/n, col = OrganizationIdentifier)) +
  geom_line(linetype = 2) + geom_point() +
  facet_wrap(. ~ CharacteristicName) + theme_bw() +
  labs(y = 'Fraction of blanks exceeding MDL', title = 'Field blank performance', x = '', colour = 'Tribe')

ggplot(allSummary$blank_summary[grepl(x = allSummary$blank_summary$ActivityTypeCode, pattern = 'Quality Control Sample-Lab Blank'), ], aes(x = year, y = exceedences/n, col = OrganizationIdentifier)) +
  geom_line(linetype = 2) + geom_point() +
  facet_wrap(. ~ CharacteristicName) + theme_bw() +
  labs(y = 'Fraction of blanks exceeding MDL', title = 'Lab blank performance', x = '', colour = 'Tribe')






# Identify all parameters collected by each tribe ----------------------------------


# for (i in 1:length(tribes)) {
#   skip_to_next <- FALSE
#   tribeData_tmp <- dataRetrieval::readWQPdata(organization = tribes[i],
#                                               startDate = "01-01-2015",
#                                               endDate   = "12-31-2022")
#   params_tmp <- count(tribeData_tmp$CharacteristicName)
#   tryCatch(params_tmp$tribe <- tribes[i], error = function(e) { skip_to_next <<- TRUE})
#     if(skip_to_next) { next }
#     params_tmp$tribe <- tribes[i]
#     if (i == 1) {
#       params_fin <- params_tmp
#     } else {
#       params_fin <- rbind(params_fin, params_tmp)
#     }
#     cat(tribes[i], ' complete\n')
# }
# write.csv(params_fin, file = '/docs/data_paramNames_20230628.csv', row.names = FALSE)



# Pull hydrologic data ----------------------------------------------------
### goal: add hydrologic params as columns in chemical dataset
### TODO: check units on these params. what is velocity-discharge?

### pull and lightly process
hydro_params <- c(#'Cross-section Depth',
                  'Depth',
                  # 'Depth of water column',
                  'Depth, Secchi disk depth',
                  'Depth, bottom',
                  # 'Depth, data-logger (non-ported)',
                  # 'Depth, data-logger (ported)',
                  # 'Discharge (stage unknown)',
                  # 'Discharge - Velocity Area Depth',
                  'Discharge, River/Stream',
                  'Flow',
                  # 'Flow rate',
                  # 'Flow rate, instantaneous',
                  'Flow, severity (choice list)',
                  'Stream flow',
                  # 'Stream flow, instantaneous',
                  # 'Stream flow, mean. daily',
                  # 'Stream velocity (choice list)',
                  # 'Thalweg Depth',
                  # 'Thalweg Depth by Pole',
                  # 'Thalweg Depth by Sonar',
                  # 'Velocity - stream',
                  'Velocity-discharge' # not sure what this means. check units if it's used
)

hydDat <- dataRetrieval::readWQPdata(organization = tribes,
                                         characteristicName = hydro_params,
                                         startDate = "01-01-2015",
                                         endDate   = "12-31-2022")

# hydDat2 <- preProcessResults(hydDat, multiplier = 0.5)
# hydDat3 <- summarizeQC(data = hydDat2)
# summary(as.numeric(hydDat$ResultMeasureValue)) # only 362 NAs
# summary(hydDat3$data$ResultMeasureValue) # 167000?? # 861 NAs??

hydDat$ResultMeasureValue[!grepl(x = hydDat$CharacteristicName, pattern = 'Flow, severity (choice list)')] <- as.numeric(gsub(hydDat$ResultMeasureValue[!grepl(x = hydDat$CharacteristicName, pattern = 'Flow, severity (choice list)')], pattern = '>|<| ', replacement = ''))
hydDat$ActivityStartDateTime <- as.POSIXct(hydDat$ActivityStartDateTime, format = '%Y-%m-%d %H:%M:%S')
hydDat2 <- hydDat[, c("OrganizationIdentifier", "MonitoringLocationIdentifier", 'CharacteristicName', 'ActivityStartDateTime', 'ResultMeasureValue')]

hydDat3 <- reshape(hydDat2, idvar = c("OrganizationIdentifier",  "MonitoringLocationIdentifier", "ActivityStartDateTime"), timevar = "CharacteristicName", v.names = "ResultMeasureValue", direction = "wide", sep = "_")
hydDat3[, c(4:9, 11)] <- lapply(X = hydDat3[, c(4:9, 11)], FUN = as.numeric)
names(hydDat3) <- gsub(x = names(hydDat3), pattern = 'ResultMeasureValue_', replacement = '')
names(hydDat3) <- gsub(x = names(hydDat3), pattern = ' |,|-|[()]|/', replacement = '_')


### make chem data wide (~9 params) and join it with hydro data
newDat         <- allSummary$data[, c("OrganizationIdentifier", "MonitoringLocationIdentifier", 'CharacteristicName', 'ActivityStartDateTime', 'ResultMeasureValue')]
newDat$ActivityStartDateTime <- as.POSIXct(newDat$ActivityStartDateTime, format = '%Y-%m-%d %H:%M:%S')
newDat2        <- reshape(newDat, idvar = c("OrganizationIdentifier",  "MonitoringLocationIdentifier", "ActivityStartDateTime"), timevar = "CharacteristicName", v.names = "ResultMeasureValue", direction = "wide", sep = "_")
names(newDat2) <- gsub(x = names(newDat2), pattern = 'ResultMeasureValue_', replacement = '')

### join 'em
newDat3 <- join_all(list(newDat2, hydDat3), by = c("OrganizationIdentifier", "MonitoringLocationIdentifier", 'ActivityStartDateTime'))
head(newDat3[!is.na(newDat3$Flow), ])

plot(newDat3[, c(4:18, 20)])

ggplot(newDat3, aes(x = Depth, y = Temperature, col = OrganizationIdentifier)) + geom_point() + theme_classic() + xlim(c(0, 20)) + ylim(0, 30) + facet_wrap(. ~ OrganizationIdentifier, scales = 'free')

ggplot(newDat3, aes(x = Flow, y = Temperature, col = OrganizationIdentifier)) + geom_point() + theme_classic() + xlim(c(0, 50000)) + ylim(0, 30) + facet_wrap(. ~ OrganizationIdentifier, scales = 'free')

### Looks like DO data had % saturation input in some cases instead of mg/L
ggplot(newDat3, aes(x = Flow, y = DO, col = OrganizationIdentifier)) + geom_point() + theme_classic() + xlim(c(0, 50000)) + facet_wrap(. ~ OrganizationIdentifier, scales = 'free')

# nothing of interest
ggplot(newDat3, aes(x = Flow, y = TP, col = OrganizationIdentifier)) + geom_point() + theme_classic()# + ylim(0, 1) + xlim(c(0, 50000)) + facet_wrap(. ~ OrganizationIdentifier, scales = 'free')
ggplot(newDat3, aes(x = Flow, y = NO3_N, col = OrganizationIdentifier)) + geom_point() + theme_classic() # + xlim(c(0, 50000)) + facet_wrap(. ~ OrganizationIdentifier, scales = 'free')
ggplot(newDat3, aes(x = Flow, y = E_coli, col = OrganizationIdentifier)) + geom_point() + theme_classic() + ylim(0, 500) + xlim(c(0, 50000)) # + facet_wrap(. ~ OrganizationIdentifier, scales = 'free')


ggplot(newDat3, aes(x = Stream_flow, y = TP, col = OrganizationIdentifier)) + geom_point() + theme_classic() + ylim(0, 1) + xlim(c(0, 50000))# + facet_wrap(. ~ OrganizationIdentifier, scales = 'free')
ggplot(newDat3, aes(x = Velocity_discharge, y = TP, col = OrganizationIdentifier)) + geom_point() + theme_classic() + ylim(0, 1) + xlim(c(0, 500))# + facet_wrap(. ~ OrganizationIdentifier, scales = 'free')
ggplot(newDat3, aes(x = Discharge__River_Stream, y = TP, col = OrganizationIdentifier)) + geom_point() + theme_classic() #+ ylim(0, 1) + xlim(c(0, 50000))# + facet_wrap(. ~ OrganizationIdentifier, scales = 'free')
