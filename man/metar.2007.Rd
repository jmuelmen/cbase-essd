% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{metar.2007}
\alias{metar.2007}
\title{METARs at A-Train overpass locations (2007)}
\format{A data frame with \eqn{n} variables:
\describe{
  \item{lon}{longitude; -180 < lon < 180}
  \item{lat}{latitude}
  \item{denom}{denominator for fractions (character):
    \describe{
       \item{all}{number of satellite overpasses}
       \item{cloud}{number of cloudy satellite overpasses}
       \item{rain}{number of raining satellite overpasses}
    }}
}}
\source{
See the first example in \code{\link{get.metars.from.archive}}
}
\usage{
metar.2007
}
\description{
A dataset containing METARs downloaded from wunderground.com at
A-Train overpass locations in 2007
}
\keyword{datasets}
