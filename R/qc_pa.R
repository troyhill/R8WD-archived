
#' Quality control screening of Purple Air data
#'
#' @param dat dataset obtained with `get_pa()`
#' @param sensor_agreement_pct required level of sensor agreement, expressed as percent difference from the lower of the two sensors.
#' @param sensor_agreement_ug required level of sensor agreement, expressed in absolute terms
#' @param column_name name of the column with data of interest.
#' @param sensor_name name of column with sensor IDs
#'
#' @return dataframe with a data-flag column (e.g., 'pm2.5_atm_flag') and a column (e.g., 'pm2.5_atm_qc') that only includes data meeting the sensor agreement threshold.
#' @export
#'
#' @examples
#' qc_pa
#'
qc_pa <- function(dat,
                  sensor_agreement_pct = 0.80, # expressed as decimal fraction
                  sensor_agreement_ug  = 5,    # expressed as ug/m3
                  column_name      = 'pm2.5_atm_', # name of column to check (without a/b)
                  sensor_name      = 'sensor_index'
) {
  ### removes data that don't meet A/B agreement threshold
  ### agreement calculated as (highest value - lowest value)/lowest
  column_names   <- paste0(column_name, c('a', 'b'))
  col_index <- which(tolower(names(dat)) %in% tolower(column_names))
  ### remove NAs
  dat <- na.omit(dat)

  difference_pct <- apply(X = dat[, col_index], MARGIN = 1, FUN = function(x) {diff(range(x, na.rm = TRUE)) / min(x, na.rm = TRUE)})
  difference_abs <- apply(X = dat[, col_index], MARGIN = 1, FUN = function(x) {diff(range(x, na.rm = TRUE))})
  ### observations are accepted if they are within 20% of each other OR the difference is less than 5ug/m3
  ### so, only rejected if both conditions are met
  obs_rejected   <- sum((difference_pct > sensor_agreement_pct) & (difference_abs > sensor_agreement_ug), na.rm = TRUE) # reject if difference is greater than threshold
  which_rejected <- which((difference_pct > sensor_agreement_pct) & (difference_abs > sensor_agreement_ug))
  dat[, paste0(column_name, 'flag')] <- NA
  dat[, paste0(column_name, 'qc')]   <- rowMeans(dat[, col_index], na.rm = TRUE)

  ### show a plot
  tst.dat       <- data.frame(x = 0:500)
  tst.dat$y.max <- tst.dat$x*(1+(1-sensor_agreement_pct))
  # tst.dat$y.max[tst.dat$y.max < sensor_agreement_ug] <- 5
  tst.dat$y.other1 <- tst.dat$x + 5
  tst.dat$y.other2 <- apply(tst.dat[, c('y.other1', 'y.max')], FUN = max, MARGIN = 1)


  for (i in 1:length(unique(dat[, sensor_name]))) {
    sensor_index <- which(dat[, sensor_name] %in% unique(dat[, sensor_name])[i])
    tmpDat <- dat[sensor_index, col_index]
    tmp <- summary(lm(tmpDat))
    rsq <- round(tmp$r.squared, 3)
    int <- round(coef(lm(tmpDat))[1], 2)
    cf  <- round(coef(lm(tmpDat))[2], 2)

    sensor_rejected_ug  <- which((difference_abs[sensor_index] > sensor_agreement_ug))
    sensor_rejected_pct <- which((difference_pct[sensor_index] > sensor_agreement_pct))
    sensor_rejected <- which((difference_pct[sensor_index] > sensor_agreement_pct) & (difference_abs[sensor_index] > sensor_agreement_ug))


    graphics::plot(tmpDat,
                   main = paste0('Sensor agreement check for\nsensor ', unique(dat[, sensor_name])[i], ', ', gsub(x = column_name, pattern = '_', replacement = ' ')), #' (B = A*', cf, '+', int, '; r2 = ', rsq, ')'),
                   pch = 19, las = 1, cex = 0.75,
                   ylab = 'Sensor B', xlab = 'Sensor A')
    # graphics::lines(x = tst.dat$x, y = tst.dat$y.other2, col = 'red3', lty = 2)
    # graphics::lines(x = tst.dat$y.other2, y = tst.dat$x, col = 'red3', lty = 2)
    graphics::abline(a = 0, b = 1, col = 'gray30', lty = 2)
    # if (length(sensor_rejected) > 0) {
    graphics::points(tmpDat[sensor_rejected_ug, ], bg = 'bisque1', pch = 21, cex = 1.5)
    graphics::points(tmpDat[sensor_rejected_pct, ], bg = 'orangered', pch = 21, cex = 1.5)
    graphics::points(tmpDat[sensor_rejected, ], bg = 'red', pch = 21, cex = 1.5)
    legend(x = 'topleft',  inset = .05,
           legend = c(paste0('Data (n = ', sum(!is.na(tmpDat[,1])), ')'),
                      paste0('A-B variation exceeding ', sensor_agreement_pct*100, '% (n = ', length(sensor_rejected_pct), ')'),
                      paste0('A-B variation exceeding ', sensor_agreement_ug, ' ug/m3 (n = ', length(sensor_rejected_ug), ')'),
                      paste0('A-B variation exceeding both QC criteria (n = ', length(sensor_rejected), ')')),
           bty = 'n',
           pch = c(21, 21, 21, 21),
           pt.bg = c('black', 'orangered', 'bisque1', 'red')
           )
    # }
  }

  if (obs_rejected > 0) {
    cat(obs_rejected, 'observations R-flagged due to sensor agreement less than ', sensor_agreement_pct*100, '%:', as.character(dat[which(difference_pct > sensor_agreement_pct), 'date']), '\n')
    dat[which_rejected, paste0(column_name, 'flag')] <- 'R'
    dat[which_rejected, paste0(column_name, 'qc')]   <- NA
  }
  invisible(dat)
}

