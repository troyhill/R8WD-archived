#' Join WQP Measurement and Site Data
#'
#' @description This function takes data from the same WQP query and combines
#'              site data with measurements to map observations with physical 
#'              sampling locations
#'
#' @param fullData output of query to WQP portal using readWQPdata function in 
#'        the dataRetrieval package
#' @param sites output of query to WQP portal using whatWQPsites function in the
#'        dataRetrieval package
#'
#' @return a combined data frame of WQP measurements with corresponding sampling
#'         site data
#'

JoinWQPProfiles <- function(fullData = "null",
                            sites = "null"){
  
  fullData.df <- fullData
  
  sites.df <- sites
  
  # Join station data to the WQP data
  if (length(sites.df > 1)) {
    if (nrow(sites.df) > 0) {
      join1 <- fullData.df %>%
        # join stations to results
        dplyr::left_join(sites.df, by = "MonitoringLocationIdentifier", multiple = "all") %>%
        # remove ".x" suffix from column names
        dplyr::rename_at(dplyr::vars(dplyr::ends_with(".x")), ~ stringr::str_replace(., "\\..$", "")) %>%
        # remove columns with ".y" suffix
        dplyr::select_at(dplyr::vars(-dplyr::ends_with(".y")))
    } else {join1 = fullData.df}
  } else {join1 = fullData.df}
  return (join1)
}
