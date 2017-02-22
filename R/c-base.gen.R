#' C-BASE generator functions
#' @param path Character.  Apply algorithm to all HDF files in this path
#' @param out.name Character.  Output file name
#' @return A data frame containing cloud bases
#' @name C-BASE_gen
NULL

#' @describeIn C-BASE_gen Bases by non-attenuated CALIOP in thin clouds
#' @export
bases.cbase <- function(path = "/projekt3/climate/DATA/SATELLITE/MULTI_SENSOR/DARDAR/DARDAR_MASK/2007",
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
    res <- select(res, -X1)
    saveRDS(res, out.name)
    return(res)
}

#' @describeIn C-BASE_gen Bases by 2B-GEOPROF-LIDAR CloudSat/CALIOP combination
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
