#' Create Summary Map of Sampling Locations
#'
#' @param .data full WQP data frame with site location data
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

create_map <- function(.data){
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
    sumdat = .data %>%
      dplyr::filter(!(is.na(HUCEightDigitCode))) %>%
      dplyr::group_by(MonitoringLocationIdentifier, MonitoringLocationName, LatitudeMeasure, LongitudeMeasure) %>%
      dplyr::summarise("Sample_Count" = length(unique(ActivityIdentifier)), "Visit_Count" = length(unique(ActivityStartDate)),
                       "Parameter_Count" = length(unique(CharacteristicName)), "Organization_Count" = length(unique(OrganizationIdentifier)))
    sumdat$radius = 3
    sumdat$radius = ifelse(sumdat$Sample_Count>10,5,sumdat$radius)
    sumdat$radius = ifelse(sumdat$Sample_Count>50,8,sumdat$radius)
    sumdat$radius = ifelse(sumdat$Sample_Count>100,10,sumdat$radius)
    sumdat$radius = ifelse(sumdat$Sample_Count>200,15,sumdat$radius)
    sumdat$radius = ifelse(sumdat$Sample_Count>500,20,sumdat$radius)
    sumdat$radius = ifelse(sumdat$Sample_Count>1500,30,sumdat$radius)

    site_legend = subset(site_size, site_size$Point_size %in% unique(sumdat$radius))

    pal <- leaflet::colorBin(
      palette = "Blues",
      domain = sumdat$Parameter_Count)

    map = leaflet::leaflet()%>%
      leaflet::addProviderTiles("Esri.WorldTopoMap", group = "World topo",
                                options = leaflet::providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE)) %>%
      # get rid of whatever was there before if loading a second dataset
      leaflet::clearShapes() %>%
      # fit boundaries of the map to a relevant window
      leaflet::fitBounds(lng1 = min(sumdat$LongitudeMeasure), lat1 = min(sumdat$LatitudeMeasure),
                         lng2 = max(sumdat$LongitudeMeasure), lat2 = max(sumdat$LatitudeMeasure)) %>%
      leaflet::addCircleMarkers(data = sumdat, lng =~ as.numeric(LongitudeMeasure), lat =~ as.numeric(LatitudeMeasure),
                                color="black",fillColor =~ pal(Parameter_Count), fillOpacity = 0.7,
                                stroke = TRUE, weight = 1.5, radius=sumdat$radius,
                                popup = paste0("Site ID: ", sumdat$MonitoringLocationIdentifier,
                                               "<br> Site Name: ", sumdat$MonitoringLocationName,
                                               "<br> Measurement Count: ", sumdat$Sample_Count,
                                               "<br> Visit Count: ", sumdat$Visit_Count,
                                               "<br> Characteristic Count: ", sumdat$Parameter_Count)) %>%
      leaflet::addLegend("bottomright", pal = pal, values =sumdat$Parameter_Count,
                         title = "Characteristics", opacity = 0.5) %>%
      addLegendCustom(colors = "black",
                      labels = site_legend$Sample_n, sizes = site_legend$Point_size*2)
    return(map)
  })
}
