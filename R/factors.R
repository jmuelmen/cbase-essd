factor.relabel <- function(x, labels, ordered = FALSE) {
    if (is.character(x)) {
        factor(x, levels = labels, labels = labels, ordered = ordered)
    } else {
        factor(x, levels = 0 : (length(labels) - 1), labels = labels,
               ordered = ordered)
    }
}

#' @export
factor.feature.type <- function(x) {
    labels <- c("invalid",
                "clear air",
                "cloud",
                "aerosol",
                "stratospheric feature",
                "surface",
                "subsurface",
                "no signal")
    factor.relabel(x, labels)
}

#' @export
factor.feature.type.qa <- function(x) {
    factor.relabel(x,
                   labels = c("none",
                              "low",
                              "medium",
                              "high"),
                   ordered = TRUE)
}

#' @export
factor.ice.water.phase <- function(x) {
    factor.relabel(x,
                   labels = c("unknown",
                              "randomly oriented ice",
                              "water",
                              "horizontally oriented ice"))
}

#' @export
factor.ice.water.phase.qa <- function(x) {
    factor.relabel(x,
                   labels = c("none",
                              "low",
                              "medium",
                              "high"),
                   ordered = TRUE)
}

#' @export
factor.horizontal.averaging <- function(x) {
    factor.relabel(x,
                   labels = c("NA",
                              "1/3 km",
                              "1 km",
                              "5 km",
                              "20 km",
                              "80 km"),
                   ordered = TRUE)
}

#' @export
factor.vfm <- function(df) {
    mutate(df,
           feature.qa.lowest.cloud 	= factor.feature.type.qa(feature.qa.lowest.cloud),
           horizontal.averaging.lowest.cloud.min = factor.horizontal.averaging(horizontal.averaging.lowest.cloud.min),
           horizontal.averaging.lowest.cloud.max = factor.horizontal.averaging(horizontal.averaging.lowest.cloud.max),
           phase.lowest.cloud 		= factor.ice.water.phase(phase.lowest.cloud),
           phase.qa.lowest.cloud 	= factor.ice.water.phase.qa(phase.qa.lowest.cloud),
           feature.above.surface 	= factor.feature.type	(feature.above.surface))
}
