#' Divide METAR overpasses into episodes
#'
#' Remember to run the data frame through \code{mutate(date =
#' convert.date(time))} first
#'
#' @param df.metar Data frame.
#' @return Data frame with episode numbers (multiple overpasses on the
#'     same date) in ascending order
#'
#' @export
#'
#' @examples
#'
add.metar.episodes <- function(df.metar) {
    dplyr::filter(df.metar, station.icao != "") %>%
        plyr::ddply(~ station.name + station.icao + date,
                    function(x) {
                        dt <- diff(as.numeric(range(x$time)))
                        if (dt < 100) {
                            return(data.frame(time = median(x$time), dt = dt, episode = 1))
                        } else {
                            hc <- stats::hclust(dist(x$time))
                            n <- 2
                            while (TRUE) {
                                groups <- stats::cutree(hc, k = n)
                                ret <- plyr::adply(1:n, 1, function(i) {
                                    dt <- diff(as.numeric(range(x$time[groups == i])))
                                    data.frame(time = median(x$time[groups == i]),
                                               dt = dt,
                                               episode = i)
                                })
                                if (all(ret$dt < 100))
                                    return(ret)
                                n <- n + 1
                            }
                        }
                    }, .parallel = TRUE)
}

#' Retrieve METARS by date and ICAO code
#'
#' Remember to run the data frame through \code{mutate(date =
#' convert.date(time))} first
#'
#' @param stat.dates Data frame.  Output of \link{add.metar.episodes}.
#' @return Vector of distances (in km) between points
#'
#' @export
#'
#' @examples
#' 
get.metars.from.archive <- function(stat.dates) {
    ## lf.metar.cache <- list.files("/home_local/jmuelmen/metars", ".*rds")
    lf.metar.cache <- list.files("metars", ".*rds")

    gc()
    
    doParallel::registerDoParallel(cores = 20) ## any more, and wunderground starts timing out

    plyr::ddply(stat.dates, ~ station.name + station.icao + date + episode, function(x) {
        res <- try(with(x, {
            ## fname.metar <- sprintf("/home_local/jmuelmen/metars/%s-%s.rds",
            fname.metar <- sprintf("metars/%s-%s.rds",
                                   trim(station.icao),
                                   format(as.POSIXct(date, format = "%Y/%m/%d"), "%Y-%m-%d"))
            if (any(grepl(basename(fname.metar), lf.metar.cache))) {
                print(sprintf("reading %s from cache", basename(fname.metar)))
                metar <- readRDS(fname.metar)
            } else {
                print(sprintf("reading %s from http", basename(fname.metar)))
                h <- curl::new_handle()
                ## set a cookie expressing the desire for the METAR to be included in the csv
                curl::handle_cookies(h)
                req <- curl::curl_fetch_memory(
                    "http://www.wunderground.com/cgi-bin/findweather/getForecast?setpref=SHOWMETAR&value=1", handle = h)
                metar.url <-
                    sprintf("http://www.wunderground.com/history/airport/%s/%s/DailyHistory.html?format=1",
                            trim(station.icao), format(seq(as.POSIXct(date, format = "%Y/%m/%d") - 86400,
                                                           as.POSIXct(date, format = "%Y/%m/%d") + 86400, by = "day"),
                                                       "%Y/%m/%d"))
                metar <- ldply(metar.url, function(metar.url)
                    read.csv(curl::curl(metar.url, handle = h), stringsAsFactors = FALSE, comment = "<"))
                ##print(metar)
                ## if (!(any(grepl("FullMetar", names(metar))))) {
                ##     ## print(sprintf("%s: %s", metar.url, names(metar)))
                ## } else {
                metar <- metar[!grepl("AAXX", metar$FullMetar), ]
                saveRDS(metar, fname.metar)
            }
            metar[which.min(abs(as.POSIXct(as.character(metar$DateUTC), tz = "UTC") - time)), ]
        }))
        if (class(res) == "try-error")
            return(data.frame(metar = as.character(NA),
                              error = as.character(res),
                              valid = FALSE,
                              DateUTC = as.character(NA),
                              stringsAsFactors = FALSE))
        if (!any(grepl("FullMetar", names(res))))
            return(data.frame(metar = as.character(NA),
                              error = as.character(NA),
                              valid = FALSE,
                              DateUTC = as.character(NA),
                              stringsAsFactors = FALSE))
        with(res, {
            if (is.character(FullMetar) && length(FullMetar) > 0 && !is.null(FullMetar)) {
                ret <- data.frame(metar = as.character(FullMetar),
                                  error = as.character(NA),
                                  valid = TRUE,
                                  DateUTC = DateUTC,
                                  stringsAsFactors = FALSE)
            } else {
                ret <- data.frame(metar = as.character(NA),
                                  error = as.character(NA),
                                  valid = FALSE,
                                  DateUTC = as.character(NA),
                                  stringsAsFactors = FALSE)
            }
            return(ret)
        })
    }, .parallel = TRUE)
}
