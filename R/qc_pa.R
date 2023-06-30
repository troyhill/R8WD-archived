
#' Quality control screening of Purple Air data
#'
#' @param dat dataset obtained with `get_pa()`
#' @param sensor_agreement required level of sensor agreement, expressed as percent difference from the lower of the two sensors.
#' @param column_name name of the column with data of interest.
#'
#' @return dataframe
#' @export
#'
#' @examples
#' qc_pa
#'
qc_pa <- function(dat              = dat,
                  sensor_agreement = 0.70, # expressed as decimal fraction
                  column_name      = 'pm2.5_atm_' # name of column to check (without a/b)
) {
  ### removes data that don't meet A/B agreement threshold
  ### agreement calculated as (highest value - lowest value)/lowest
  column_names   <- paste0(column_name, c('a', 'b'))
  difference_pct <- apply(X = dat[, column_names], MARGIN = 1, FUN = function(x) {diff(range(x, na.rm = TRUE)) / min(x, na.rm = TRUE)})
  obs_rejected   <- sum(difference_pct > sensor_agreement, na.rm = TRUE) # reject if difference is greater than threshold
  if (obs_rejected > 0) {
    cat(obs_rejected, 'observations removed due to sensor agreement less than ', sensor_agreement*100, '%:', as.character(dat[which(difference_pct > sensor_agreement), 'date']), '\n')
    dat <- dat[which(difference_pct <= sensor_agreement), ]
  }
  invisible(dat)
}

