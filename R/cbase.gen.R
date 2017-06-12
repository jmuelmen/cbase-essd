#' CBASE generator functions
#' @param path Character.  Apply algorithm to all HDF files in this path
#' @param out.name Character.  Output file name
#' @return A data frame containing cloud bases
#' @name CBASE_gen
NULL

#' @describeIn CBASE_gen Bases by non-attenuated CALIOP in thin
#'     clouds (using CALIOP VFM)
#' @export
bases.cbase <- function(path = "/home/jmuelmen/CALIOP/VFM.v4.10/2008",
                        pattern = "CAL_LID_L2_VFM-Standard-V4-10.*hdf",
                        out.name = "cloud-bases.rds",
                        combination = TRUE,
                        ncores = 72) {

    ## Combine local bases?  if so, we'll need the SVM models for
    ## local base correction
    if (combination) {
        library(e1071)
        cor.svm <- readRDS("~/r-packages/models.svm.rds")
    }
    
    lf <- ## "/tmp/CER-NEWS_CCCM_Aqua-FM3-MODIS-CAL-CS_RelB1_905906.20071226.hdf"
        list.files(path = path, pattern = pattern, recursive = TRUE, full.names = TRUE)

    sds <- hdf::h4list(lf[1])

    doParallel::registerDoParallel(cores = ncores)
    res <- plyr::adply(lf, 1, function(fname) {
        ## can we take the easy way out (results are already cached)?
        out.fname <- paste("cloud-bases", gsub(".hdf", ".rds", basename(fname)), sep = "/")
        if (length(list.files("cloud-bases", gsub(".hdf", ".rds", basename(fname)))) != 0)
            return(readRDS(out.fname))

        cal.datestring <- strsplit(basename(fname), "\\.")[[1]][2]
        cal.date <- as.POSIXlt(cal.datestring, format = "%Y-%m-%d", tz = "UTC") +
            (hdf::h4read(fname,sds,"Profile_UTC_Time")[1] %% 1) * 86400
        
        Feature_Classification_Flags <- hdf::h4read(fname, sds, "Feature_Classification_Flags")[1166:5515,] ## lower troposphere
        dim(Feature_Classification_Flags) <- dim(Feature_Classification_Flags) * c(1/15, 15) ## rearrange the packed structure

        altitude <- seq(-0.5, 8.17, by = 30e-3) %>% rev()
        lon <- hdf::h4read(fname,sds,"Longitude") %>% rep(each = 15)
        lat <- hdf::h4read(fname,sds,"Latitude") %>% rep(each = 15)
        time <- hdf::h4read(fname,sds,"Profile_Time") %>% rep(each = 15)
        Day_Night_Flag <- hdf::h4read(fname,sds,"Day_Night_Flag") %>%
            factor.day.night() %>%
            rep(each = 15)
        Land_Water_Mask <- hdf::h4read(fname,sds,"Land_Water_Mask") %>%
            factor.land.sea() %>%
            rep(each = 15)
        profile <- rep(1:15, length.out = length(time))
        ipoint.40  <- rep(1 : ceiling(length(lat) / 3 / 80 ), each = 3 * 80 )[1 : length(lat)]
        ipoint.100 <- rep(1 : ceiling(length(lat) / 3 / 200), each = 3 * 200)[1 : length(lat)]
        ## lon.interp <- approx(x = seq_len(length(lon)) - 1,
        ##                      y = lon,
        ##                      xout = (seq_len((length(lon)) * 15) - 8) / 15)$y 
        ## lat.interp <- approx(x = seq_len(length(lat)) - 1,
        ##                      y = lat,
        ##                      xout = (seq_len((length(lat)) * 15) - 8) / 15)$y 
        ## time.interp <- spline(x = seq_len(length(time)) - 1,
        ##                       y = time - time[1],
        ##                       xout = (seq_len((length(time)) * 15) - 8) / 15,
        ##                       method = "hyman")$y + cal.date
        
        Feature_Type <- bitwAnd(Feature_Classification_Flags, 7)  %>%
            factor.feature.type()
            ## factor(levels = 0:7, labels = c("invalid",
            ##                                 "clear air",
            ##                                 "cloud",
            ##                                 "aerosol",
            ##                                 "stratospheric feature",
            ##                                 "surface",
            ##                                 "subsurface",
            ##                                 "no signal"))
        ## dim(Feature_Type) <- dim(Feature_Classification_Flags)
        
        Feature_Type_QA <- bitwAnd(Feature_Classification_Flags, bitwShiftL(3,3)) %>% bitwShiftR(3) %>%
            factor.qa()
            ## factor(levels = 0:3, labels = c("none",
            ##                                 "low",
            ##                                 "medium",
            ##                                 "high"), ordered = TRUE)
        ## dim(Feature_Type_QA) <- dim(Feature_Classification_Flags)

        Ice_Water_Phase <- bitwAnd(Feature_Classification_Flags, bitwShiftL(3,5)) %>% bitwShiftR(5) %>%
            factor.ice.water.phase()
            ## factor(levels = 0:3, labels = c("unknown",
            ##                                 "randomly oriented ice",
            ##                                 "water",
            ##                                 "horizontally oriented ice"))
        ## dim(Ice_Water_Phase) <- dim(Feature_Classification_Flags)

        Ice_Water_Phase_QA <- bitwAnd(Feature_Classification_Flags, bitwShiftL(3,7)) %>% bitwShiftR(7) %>%
            factor.qa()
            ## factor(levels = 0:3, labels = c("none",
            ##                                 "low",
            ##                                 "medium",
            ##                                 "high"), ordered = TRUE)
        ## dim(Ice_Water_Phase_QA) <- dim(Feature_Classification_Flags)

        Horizontal_averaging <- bitwAnd(Feature_Classification_Flags, bitwShiftL(7,13)) %>% bitwShiftR(13) %>%
            factor.horizontal.averaging()
            ## factor(levels = 0:5, labels = c("NA",
            ##                                 "1/3 km",
            ##                                 "1 km",
            ##                                 "5 km",
            ##                                 "20 km",
            ##                                 "80 km"), ordered = TRUE)
        ## mask <- aaply(array(Feature_Type, dim(Feature_Classification_Flags)), 2, function(x) {
        ##     any(x == "surface") && any(x == "cloud")
        ## }, .progress = "text")

        ## which(mask) %>%
        ##     head(30) %>%
        ##     as.vector()
        
        df <- expand.grid(altitude = altitude, time = time - time[1] + cal.date) %>% 
            mutate(profile = rep(profile, each = 290),
                   lon = rep(lon, each = 290),
                   lat = rep(lat, each = 290),
                   ipoint.40  = rep(ipoint.40 , each = 290),
                   ipoint.100 = rep(ipoint.100, each = 290),
                   Feature_Type = (Feature_Type),
                   Feature_Type_QA = (Feature_Type_QA),
                   Ice_Water_Phase = (Ice_Water_Phase),
                   Ice_Water_Phase_QA = (Ice_Water_Phase_QA),
                   Horizontal_averaging = Horizontal_averaging,
                   Day_Night_Flag = rep(Day_Night_Flag, each = 290),
                   Land_Water_Mask = rep(Land_Water_Mask, each = 290)) %>%
            group_by(ipoint.40, altitude) %>%
            mutate(lon.40 = lon[ceiling(n() / 2)],
                   lat.40 = lat[ceiling(n() / 2)],
                   time.40 = time[ceiling(n() / 2)]) %>%
            group_by(ipoint.100, altitude) %>%
            mutate(lon.100 = lon[ceiling(n() / 2)],
                   lat.100 = lat[ceiling(n() / 2)],
                   time.100 = time[ceiling(n() / 2)]) %>%
            ungroup()
                   

        ## helper function to identify feature above surface
        feature.above.surface <- function(vfm) {
            stack <- vertical.features.stack(vfm)
            stack.idx.surface <- which(stack == "surface")
            if (stack.idx.surface > 1)
                stack[stack.idx.surface - 1]
            else NA
        }
        
        df %>%
            group_by(time, profile) %>%
            filter(any(Feature_Type == "surface"), any(Feature_Type == "cloud")) %>%
            mutate(
                ## label layers
                labels = label.vertical.features(Feature_Type)
                ## ## find surface label
                ## label.sfc = labels[Feature_Type == "surface"][1],
                ## ## find label of lowest cloud layer
                ## label.lowest.cloud = max(labels[Feature_Type == "cloud"]),
                ## ## find (minimum) QA flag of lowest layer
                ## feature.qa.lowest.cloud = min(Feature_Type_QA[labels == label.lowest.cloud]),
                ## ## find level number of lowest level of lowest cloud
                ## lev.lowest.cloud = max(which(labels == label.lowest.cloud)),
                ## ## find phase of lowest level of lowest cloud
                ## phase.lowest.cloud = Ice_Water_Phase[lev.lowest.cloud],
                ## phase.qa.lowest.cloud = Ice_Water_Phase_QA[lev.lowest.cloud],
                ## ## find feature type above surface
                ## feature.above.surface = feature.above.surface(Feature_Type)
            ) %>%
            summarize(
                ## find surface label
                label.sfc = labels[Feature_Type == "surface"][1],
                ## find label of lowest cloud layer
                label.lowest.cloud = max(labels[Feature_Type == "cloud"]),
                ## find (minimum) QA flag of lowest layer
                feature.qa.lowest.cloud = min(Feature_Type_QA[labels == label.lowest.cloud]),
                ## find (median) horizontal averaging of lowest layer
                ## horizontal.averaging.lowest.cloud.median = median(Horizontal_averaging[labels == label.lowest.cloud]),
                ## find (min) horizontal averaging of lowest layer
                horizontal.averaging.lowest.cloud.min = min(Horizontal_averaging[labels == label.lowest.cloud]),
                ## find (max) horizontal averaging of lowest layer
                horizontal.averaging.lowest.cloud.max = max(Horizontal_averaging[labels == label.lowest.cloud]),
                ## find level number of bottom of lowest cloud
                lev.lowest.cloud.base = max(which(labels == label.lowest.cloud)),
                ## find level number of top of lowest cloud
                lev.lowest.cloud.top = min(which(labels == label.lowest.cloud)),
                ## find phase of lowest level of lowest cloud
                phase.lowest.cloud = Ice_Water_Phase[lev.lowest.cloud.base],
                phase.qa.lowest.cloud = Ice_Water_Phase_QA[lev.lowest.cloud.base],
                ## find feature type above surface
                feature.above.surface = feature.above.surface(Feature_Type),
                ## find cloud base altitude
                cloud.base.altitude = altitude[lev.lowest.cloud.base],
                ## find cloud top altitude
                cloud.top.altitude = altitude[lev.lowest.cloud.top],
                ## find level number of surface
                lev.surface = min(which(Feature_Type == "surface")),
                surface.elevation = altitude[lev.surface],
                lon = lon[1], lat = lat[1],
                ipoint.40 = ipoint.40[1], lon.40 = lon.40[1],
                lat.40 = lat.40[1], time.40 = time.40[1],
                ipoint.100 = ipoint.100[1], lon.100 = lon.100[1],
                lat.100 = lat.100[1], time.100 = time.100[1],
                day.night.flag = Day_Night_Flag[1],
                land.water.mask = Land_Water_Mask[1],
                len = n()
            ) %>%
            factor.vfm() %>%
            ungroup() %>%
            select(time, profile, ## len,
                   lon, lat,
                   ipoint.40, lon.40, lat.40, time.40,
                   ipoint.100, lon.100, lat.100, time.100,
                   day.night.flag, land.water.mask,
                   feature.qa.lowest.cloud,
                   ## horizontal.averaging.lowest.cloud.median,
                   horizontal.averaging.lowest.cloud.min,
                   horizontal.averaging.lowest.cloud.max,
                   phase.lowest.cloud, phase.qa.lowest.cloud,
                   feature.above.surface,
                   cloud.top.altitude,
                   cloud.base.altitude,
                   surface.elevation) -> res

        saveRDS(res, file = out.fname)
        ## res <- readRDS(file = out.fname)
        
        if (combination) {
            ## some preparatory work: add thickness, AGL heights, filter
            res.prep <- res %>%
                dplyr::mutate(caliop = cloud.base.altitude - surface.elevation) %>%
                dplyr::filter(cloud.base.altitude < 3,
                              caliop > 0) %>%
                dplyr::mutate(thickness = cloud.top.altitude - cloud.base.altitude)

            ## correct and combine bases for 40 km segments
            res.40 <- res.prep %>%
                dplyr::group_by(ipoint.40) %>%
                ## distance from segment midpoint
                dplyr::mutate(dist = dist.gc(lon, lon.40, lat, lat.40)) %>%
                ## multiplicity (extrapolated to D_max = 100 km, which
                ## was used for tuning)
                dplyr::mutate(resolution.out = n() * 2.5) %>%
                dplyr::ungroup() %>%
                correct.cbase.lm(cor.svm) %>%
                dplyr::mutate(segment = ipoint.40) %>%
                cbase.combine.segment()

            gsub(".hdf", ".rds", basename(fname)) %>%
                gsub("CAL_LID_L2_VFM-Standard-V4-10", "CBASE-40", .) %>%
                paste("cloud-bases", ., sep = "/") %>%
                saveRDS(res.40, .)

            ## correct and combine bases for 100 km segments
            res.100 <- res.prep %>%
                dplyr::group_by(ipoint.100) %>%
                ## distance from segment midpoint
                dplyr::mutate(dist = dist.gc(lon, lon.100, lat, lat.100)) %>%
                ## multiplicity 
                dplyr::mutate(resolution.out = n()) %>%
                dplyr::ungroup() %>%
                correct.cbase.lm(cor.svm) %>%
                dplyr::mutate(segment = ipoint.100) %>%
                cbase.combine.segment()
            
            gsub(".hdf", ".rds", basename(fname)) %>%
                gsub("CAL_LID_L2_VFM-Standard-V4-10", "CBASE-100", .) %>%
                paste("cloud-bases", ., sep = "/") %>%
                saveRDS(res.100, .)
        }
        
        return(res)
    }, .parallel = TRUE, .id = "ifile")

    res <- dplyr::mutate(res, ifile = factor(ifile, levels = 1 : length(lf), labels = basename(lf)))
    
    saveRDS(res, out.name)
    return(res)
}

#' @describeIn CBASE_gen Bases by non-attenuated CALIOP in thin clouds (using DARDAR mask)
#' @export
bases.cbase.dardar <- function(path = "/projekt3/climate/DATA/SATELLITE/MULTI_SENSOR/DARDAR/DARDAR_MASK/2007",
                               out.name = "cloud-bases.rds") {
    lf <- ## "/tmp/CER-NEWS_CCCM_Aqua-FM3-MODIS-CAL-CS_RelB1_905906.20071226.hdf"
        list.files(path = path, pattern = "DARDAR-MASK.*hdf", recursive = TRUE, full.names = TRUE)

    sds <- hdf::h4list(lf[1])
    height <- hdf::h4read(lf[1],sds,"CS_TRACK_Height")

    doParallel::registerDoParallel(cores = 20)
    res <- plyr::adply(lf, 1, function(fname) {
        print(fname)
        gc()

        out.fname <- paste("cloud-bases", gsub(".hdf", ".rds", basename(fname)), sep = "/")
        ## if (length(list.files("odran-bases", gsub(".hdf", ".rds", basename(fname)))) != 0)
        ##     return(readRDS(out.fname))
        
        mask.cal <- hdf::h4read(fname,sds,"CALIPSO_Mask")
        darmask.liq <- hdf::h4read(fname,sds,"DARMASK_Liquid")
        calmask.cloud <- hdf::h4read(fname,sds,"CALIPSO_Mask")

        lat <- hdf::h4read(fname,sds,"CLOUDSAT_Latitude")
        lon <- hdf::h4read(fname,sds,"CLOUDSAT_Longitude")
        time <- hdf::h4read(fname,sds,"CLOUDSAT_UTC_Time")

        dardar.datestring <- strsplit(basename(fname), "_")[[1]][3]
        dardar.date <- as.POSIXlt(dardar.datestring, format = "%Y%j", tz = "UTC") ## strips off the %H%M%S --> 00:00:00 UTC

        dardar.time <- dardar.date + time

        ## Odran's way of getting the cloud base: liquid clouds, below
        ## 5 km, thin enough that CALIOP sees the underlying surface
        surf <- apply(mask.cal,2,function(x) {any(x==0)})
        cld <- apply(darmask.liq,2,function(x,y,z) {any(y[which(x==1)]<5)},y=height)
        idx.ok <- which(surf & cld)

        z.base <- array(NA,length(cld))
        z.base[idx.ok] <- sapply(idx.ok, function(x) tail(height[which(darmask.liq[,x]==1)],1))

        ## T.base <- array(NA,length(cld))
        ## T.base[idx.ok] <- sapply(idx.ok, function(x) tail(height[which(darmask.liq[,x]==1)],1))
        cal.cld <- apply(calmask.cloud,2,function(x,y,z) {any(y[which(x %in% 3)]<5)},y=height)
        cal.idx.ok <- which(surf & cal.cld)
        cal.med.cld <- apply(calmask.cloud,2,function(x,y,z) {any(y[which(x %in% 3:4)]<5)},y=height)
        cal.med.idx.ok <- which(surf & cal.med.cld)
        z.base.cal <- array(NA,length(cal.cld))
        z.base.cal[cal.idx.ok] <- sapply(cal.idx.ok, function(x) tail(height[which(calmask.cloud[,x] %in% 3)],1))
        z.base.cal.med <- array(NA,length(cal.med.cld))
        z.base.cal.med[cal.med.idx.ok] <- sapply(cal.med.idx.ok, function(x) tail(height[which(calmask.cloud[,x] %in% 3:4)],1))

        res <- dplyr::data_frame(dardar.time, lon = as.vector(lon), lat = as.vector(lat),
                                 surf, cld, z.base, cal.cld, z.base.cal, cal.med.cld,
                                 z.base.cal.med)
        ## str(res)
        saveRDS(res, file = out.fname)
        return(res)
    }, .parallel = TRUE)
    res <- dplyr::select(res, -X1)
    saveRDS(res, out.name)
    return(res)
}

#' @describeIn CBASE_gen Bases by 2B-GEOPROF-LIDAR CloudSat/CALIOP combination
#' @export
bases.2b.geoprof.lidar <- function(path = "/projekt3/climate/DATA/SATELLITE/CLOUDSAT/2B-GEOPROF-LIDAR/2008") {
    lf <- ## "/tmp/CER-NEWS_CCCM_Aqua-FM3-MODIS-CAL-CS_RelB1_905906.20071226.hdf"
        list.files(path = path, pattern = ".*hdf", recursive = TRUE, full.names = TRUE)

    sds <- hdf::h4list(lf[1], FALSE)

    res <- plyr::adply(lf, 1, function(fname) {
        print(fname)
        gc()

        out.fname <- paste("2b-geoprof-lidar-bases", gsub(".hdf", ".rds", basename(fname)), sep = "/")
        ## if (length(list.files("odran-bases", gsub(".hdf", ".rds", basename(fname)))) != 0)
        ##     return(readRDS(out.fname))

        base <- hdf::h4read(fname,sds,"LayerBase")
        base <- replace(base, base == -99, NA)
        flag.base <- hdf::h4read(fname,sds,"FlagBase")
        flag.base <- replace(flag.base, flag.base == -9, NA)
        lon <-  hdf::h4read(fname,sds,"Longitude")
        lat <-  hdf::h4read(fname,sds,"Latitude")
        time <- hdf::h4read(fname,sds,"Profile_time") + hdf::h4read(fname,sds,"UTC_start")

        dardar.datestring <- strsplit(basename(fname), "_")[[1]][1]
        dardar.date <- as.POSIXlt(dardar.datestring, format = "%Y%j", tz = "UTC") ## strips off the %H%M%S --> 00:00:00 UTC

        dardar.time <- dardar.date + time

        res <- data_frame(dardar.time, lon, lat, base = base[1,], flag.base = flag.base[1,])
        saveRDS(res, file = out.fname)
        return(res)
    }, .parallel = TRUE)

    ## attributes(res$lon) <- NULL
    ## attributes(res$lat) <- NULL

    select(res, -X1) %>% saveRDS("2b-geoprof-lidar-bases-2008.rds")

    ## res2 <- as_data_frame(res) %>% filter(!is.na(z.base)) %>% select(-(surf:cld))
    ## ##saveRDS(res2, "odran-bases-2007-summary.rds")
    ## res2
}
