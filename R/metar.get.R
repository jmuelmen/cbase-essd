#' Get METARs at A-Train overpass locations (2008)
#' 
#' @export
#'
#'
get.metar.2008 <- function() {
    data(metar.2008) %>%
        dplyr::filter(valid) %>%
        dplyr::transmute(station.icao = trim(station.icao),
                         station.name = station.name,
                         date = date,
                         datetime = as.POSIXct(DateUTC, tz = "UTC"),
                         episode = episode,
                         metar = metar)
}
