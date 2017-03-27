#' Get METARs at A-Train overpass locations (2008)
#' 
#' @export
#'
#'
get.metar.2008 <- function() {
    data(metar.2008, envir = environment())
    metar.2008 %>%
        dplyr::filter(valid) %>%
        dplyr::transmute(station.icao = trim(station.icao),
                         station.name = factor(station.name),
                         date = date,
                         datetime = as.POSIXct(DateUTC, tz = "UTC"),
                         episode = episode,
                         metar = metar) %>%
        dplyr::left_join(stations.metar() %>%
                         dplyr::select(-station.name) %>%
                         dplyr::mutate(station.icao = as.character(station.icao)),
                         by = "station.icao") %>%
        dplyr::mutate(station.icao = factor(station.icao))
}

#' Get list of METAR stations
#' 
#' @export
#'
#'
stations.metar <- function() {
    f <- system.file("extdata", "stations.txt", package = "cbasetools", mustWork = TRUE)
    stations <- read.csv(f, comment = "!")

    strtolonlat <- function(x) {
        sgn <- 1
        minutes <- x[2]
        if (any(grep("W", minutes)) || any(grep("S", minutes)))
            sgn <- -1
        minutes <- strsplit(minutes, c("[EWNS]"))[[1]]
        sgn * (as.numeric(x[1]) + as.numeric(minutes) / 60)
    }
    
    stations$lon <- sapply(strsplit(as.character(stations$LON), " "), strtolonlat)
    stations$lat <- sapply(strsplit(as.character(stations$LAT), " "), strtolonlat)

    stations %>%
        dplyr::filter(M == "X")  %>% ## METAR site
        dplyr::transmute(station.icao = factor(trim(as.character(ICAO))),
                         station.name = factor(trim(as.character(STATION))),
                         elevation.m = as.numeric(ELEV),
                         lon, lat) %>%
        dplyr::filter(station.icao != "")
}
