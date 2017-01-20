#' Great-circle distances
#'
#' Calculate great-circle distances based on the haversin formula
#'
#' @param lon1 Vector of longitudes (in degrees) 
#' @param lon2 Vector of longitudes (in degrees) 
#' @param lat1 Vector of latitudes (in degrees) 
#' @param lat2 Vector of latitudes (in degrees)
#' @return Vector of distances (in km) between points
#'
#' @export
#'
#' @examples
#' 
dist.gc <- function(lon1, lon2, lat1, lat2) {
    haversin <- function(theta) sin(0.5 * theta)^2
    re <- 6371

    lon1 <- lon1 / 180 * pi
    lon2 <- lon2 / 180 * pi
    lat1 <- lat1 / 180 * pi
    lat2 <- lat2 / 180 * pi

    theta <- sqrt(haversin(lat1 - lat2) +
                  cos(lat1) * cos(lat2) * haversin(lon1 - lon2))
    dist <- 2 * re * asin(pmin(pmax(theta, 0), 1))
    dist
}

#' Trim whitespace from string
#'
#' @param x Character
#' @return Character without leading or trailing whitespace
#'
#' @export
#'
trim <- function( x ) {
    gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}
        
#' Convert standard time to format to Y/m/d date
#'
#' 
#'
#' @param time  Vector of POSIXct values
#' @return Vector of character in Y/m/d format
#'
#' @export
#'
#' @examples
#' 
convert.date <- function(time) {
    date <- format(time, "%Y/%m/%d")
}
