#' METARs at A-Train overpass locations (2007)
#'
#' A dataset containing METARs downloaded from wunderground.com at
#' A-Train overpass locations in 2007
#'
#' @format A data frame with \eqn{n} variables:
#' \describe{
#'   \item{lon}{longitude; -180 < lon < 180}
#'   \item{lat}{latitude}
#'   \item{denom}{denominator for fractions (character):
#'     \describe{
#'        \item{all}{number of satellite overpasses}
#'        \item{cloud}{number of cloudy satellite overpasses}
#'        \item{rain}{number of raining satellite overpasses}
#'     }}
#' }
#' @source See the first example in \code{\link{get.metars.from.archive}}
"metar.2007"

#' METARs at A-Train overpass locations (2008)
#'
#' A dataset containing METARs downloaded from wunderground.com at
#' A-Train overpass locations in 2008
#'
#' @format A data frame with \eqn{n} variables:
#' \describe{
#'   \item{lon}{longitude; -180 < lon < 180}
#'   \item{lat}{latitude}
#'   \item{denom}{denominator for fractions (character):
#'     \describe{
#'        \item{all}{number of satellite overpasses}
#'        \item{cloud}{number of cloudy satellite overpasses}
#'        \item{rain}{number of raining satellite overpasses}
#'     }}
#' }
#' @source See the first example in \code{\link{get.metars.from.archive}}
"metar.2008"

#' CBASE retrievals (2007 and 2008)
#'
#' A dataset containing the CBASE cloud base data
#'
#' @format A data frame with \eqn{n} variables:
#' \describe{
#'   \item{lon}{longitude; -180 < lon < 180}
#'   \item{lat}{latitude}
#'   \item{denom}{denominator for fractions (character):
#'     \describe{
#'        \item{all}{number of satellite overpasses}
#'        \item{cloud}{number of cloudy satellite overpasses}
#'        \item{rain}{number of raining satellite overpasses}
#'     }}
#' }
#' @source \code{\link{get.cbase.bitsies}}
"cbase"
