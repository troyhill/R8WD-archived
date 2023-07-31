#' Create Summary Map of Sampling Locations
#'
#' @param .data full WQP data frame with site location data
#' @param parameter optional input if user wants to map mean values for a parameter
#' @param parameterColumn name of the column with parameter names (not likely to require adjustment)
#' @param digits digits used for displaying rounded data
#'
#' @return an interactive map of sample locations displayed as a Leaflet plot
#'
#' @importFrom leaflet addLegend
#' @importFrom leaflet colorBin
#' @importFrom leaflet addProviderTiles
#' @importFrom leaflet providerTileOptions
#' @importFrom leaflet clearShapes
#' @importFrom leaflet fitBounds
#' @importFrom leaflet addCircleMarkers
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr filter
#'
#' @export

create_map_alt <- function(.data, parameter = 'Temperature', parameterColumn = 'CharacteristicName', digits = 3){
  suppressWarnings({

    # taken from this stackoverflow: https://stackoverflow.com/questions/58505589/circles-in-legend-for-leaflet-map-with-addcirclemarkers-in-r-without-shiny
    addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5){
      colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, "px; height:", sizes, "px")
      labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes,
                               "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")

      return(leaflet::addLegend(map, colors = colorAdditions, labels = labelAdditions,
                                opacity = opacity, title = "Measurements"))
    }

    site_size = data.frame(Sample_n = c("<9",">10",">50",">100",">200",">500",">1500"),
                           Point_size = c(3,5,8,10,15,20,30))

    removed.sd <- dplyr::filter(.data, is.na(HUCEightDigitCode))
    sumdat <- .data %>%
      dplyr::filter(!(is.na(HUCEightDigitCode))) %>%
      dplyr::group_by(MonitoringLocationIdentifier, MonitoringLocationName, LatitudeMeasure, LongitudeMeasure) %>%
      dplyr::summarise("Sample_Count" = length(unique(ActivityIdentifier)), # ???
                       "Visit_Count" = length(unique(ActivityStartDate)),
                       "Parameter_Count" = length(unique(CharacteristicName)))
    sumdat$radius <- 3
    sumdat$radius <- ifelse(sumdat$Sample_Count>10,5,sumdat$radius)
    sumdat$radius <- ifelse(sumdat$Sample_Count>50,8,sumdat$radius)
    sumdat$radius <- ifelse(sumdat$Sample_Count>100,10,sumdat$radius)
    sumdat$radius <- ifelse(sumdat$Sample_Count>200,15,sumdat$radius)
    sumdat$radius <- ifelse(sumdat$Sample_Count>500,20,sumdat$radius)
    sumdat$radius <- ifelse(sumdat$Sample_Count>1500,30,sumdat$radius)

    site_legend <- subset(site_size, site_size$Point_size %in% unique(sumdat$radius))

    pal <- leaflet::colorBin(
      palette = "Blues",
      domain = sumdat$Parameter_Count)

    map <- leaflet::leaflet()%>%
      leaflet::addProviderTiles("Esri.WorldTopoMap", group = "World topo",
                                options = leaflet::providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE)) %>%
      # get rid of whatever was there before if loading a second dataset
      leaflet::clearShapes() %>%
      # fit boundaries of the map to a relevant window
      leaflet::fitBounds(lng1 = min(sumdat$LongitudeMeasure), lat1 = min(sumdat$LatitudeMeasure),
                         lng2 = max(sumdat$LongitudeMeasure), lat2 = max(sumdat$LatitudeMeasure))


    if (is.na(parameter)) {
      map <- map %>%
        leaflet::addCircleMarkers(data = sumdat, lng =~ as.numeric(LongitudeMeasure), lat =~ as.numeric(LatitudeMeasure),
                                  color="black",
                                  # fillColor =~ pal(Parameter_Count),
                                  fillOpacity = 0.2,
                                  stroke = TRUE, weight = 1.5,
                                  # radius=sumdat$radius,
                                  popup = paste0("Site ID: ", sumdat$MonitoringLocationIdentifier,
                                                 "<br> Site Name: ", sumdat$MonitoringLocationName,
                                                 "<br> Measurement Count: ", sumdat$Sample_Count, # why use AvtivityIdentifier?
                                                 "<br> Visit Count: ", sumdat$Visit_Count,
                                                 "<br> Characteristic Count: ", sumdat$Parameter_Count))

    } else if (!is.na(parameter) && !any(grepl(pattern = parameter, x = unique(.data[, parameterColumn])))) {
      ### if user wants to show a parameter, make sure the parameter exists in the dataset
      ### if parameter is specified but not found, error out
      stop(parameter, ' not found in data. Try one of these: ', paste0(unique(.data[, parameterColumn]), collapse = ', '))
    } else if (!is.na(parameter) && any(grepl(pattern = parameter, x = unique(.data[, parameterColumn])))) {
      ### if parameter is specified and present,
      ### create df with values for plotting
      plotdat <- .data[grepl(x = .data[, parameterColumn], pattern = parameter), ] %>%
        dplyr::filter(!(is.na(HUCEightDigitCode))) %>%
        dplyr::group_by(MonitoringLocationIdentifier, MonitoringLocationName, LatitudeMeasure, LongitudeMeasure) %>%
        dplyr::summarise("median" = median(ResultMeasureValue, na.rm = TRUE),
                         "interquartile range"    = IQR(ResultMeasureValue, na.rm = TRUE),
                         "mean"   = mean(ResultMeasureValue, na.rm = TRUE),
                         "sd"     = sd(ResultMeasureValue, na.rm = TRUE),
                         "n"      = sum(!is.na(ResultMeasureValue))
                         )
      pal <- leaflet::colorBin(
        palette = "Blues",
        domain = plotdat$median)


      map <- map %>%
        leaflet::addCircleMarkers(data = plotdat, lng =~ as.numeric(LongitudeMeasure), lat =~ as.numeric(LatitudeMeasure),
                                  color="black",
                                  fillColor =~ pal(plotdat$median),
                                  fillOpacity = 0.7,
                                  stroke = TRUE, weight = 1.5,
                                  # fill   = plotdat$median,
                                  popup  = paste0("Site ID: ", plotdat$MonitoringLocationIdentifier,
                                                 "<br> Site Name: ", plotdat$MonitoringLocationName,
                                                 "<br> Measurement Count: ", plotdat$n,
                                                 "<br> Median (IQR): ", paste0(round(plotdat$median, digits), "\u00B1", round(plotdat$`interquartile range`, digits)),
                                                 "<br> Mean (SD): ", paste0(round(plotdat$mean, digits), "\u00B1", round(plotdat$sd, digits))))

    }
    return(map)
  })
}
