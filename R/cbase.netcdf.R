
cbase.create.nc <- function(df, out.fname,
                            title = "CBASE cloud base height estimate, 40 km Dmax",
                            trajectory_id) {
    dim.time <- ncdf4::ncdim_def("time", "seconds since 1993-01-01 00:00:00", df$time,
                                 unlim = TRUE, create_dimvar = TRUE,
                                 calendar = "gregorian", longname = "TAI time") 
    dim.strlen <- ncdf4::ncdim_def("strlen", "", 1:21, 
                                 unlim = FALSE, create_dimvar = FALSE) 

    var.cbh <- ncdf4::ncvar_def("cbh", "m",
                           list(dim.time), ## unlimited dim last
                           longname = "cloud base altitude")
    var.cbh_err <- ncdf4::ncvar_def("cbh_error", "m",
                           list(dim.time), 
                           longname = "cloud base altitude standard error")
    var.trajectory_id <- ncdf4::ncvar_def("trajectory_id", "",
                                          list(dim.strlen), prec = "char",
                                          longname = "trajectory id")

    var.lon <- ncdf4::ncvar_def("longitude", "degrees_east", list(dim.time))
    var.lat <- ncdf4::ncvar_def("latitude", "degrees_north", list(dim.time))

    nc.out <- ncdf4::nc_create(out.fname,
                        list(var.cbh, var.cbh_err, var.trajectory_id, var.lon, var.lat))
    ncdf4::ncvar_put(nc.out, var.cbh, df$pred.ceilo.msl)
    ncdf4::ncvar_put(nc.out, var.cbh_err, df$pred.rmse)
    ncdf4::ncvar_put(nc.out, var.trajectory_id, trajectory_id)
    ncdf4::ncvar_put(nc.out, var.lon, df$lon)
    ncdf4::ncvar_put(nc.out, var.lat, df$lat)

    ## set attributes
    ncdf4::ncatt_put(nc.out, "time", "standard_name", "time")
    ncdf4::ncatt_put(nc.out, "time", "description",
                     "TAI time corresponding to the CALIOP footprint in the middle of the segment")

    ncdf4::ncatt_put(nc.out, "latitude", "standard_name", "latitude")
    ncdf4::ncatt_put(nc.out, "longitude", "standard_name", "longitude")

    ncdf4::ncatt_put(nc.out, var.cbh, "source", "CBASE algorithm Version 1.0")
    ncdf4::ncatt_put(nc.out, var.cbh, "standard_name", "cloud_base_altitude")
    ncdf4::ncatt_put(nc.out, var.cbh, "units", "m")
    ncdf4::ncatt_put(nc.out, var.cbh, "description",
                     "Height of lowest cloud base above mean sea level")
    ncdf4::ncatt_put(nc.out, var.cbh, "ancillary_variables", "cbh_error")
    ncdf4::ncatt_put(nc.out, var.cbh, "coordinates", "time longitude latitude")

    ncdf4::ncatt_put(nc.out, var.cbh_err, "source", "CBASE algorithm Version 1.0")
    ncdf4::ncatt_put(nc.out, var.cbh_err, "standard_name", "cloud_base_altitude standard_error")
    ncdf4::ncatt_put(nc.out, var.cbh_err, "units", "m")
    ncdf4::ncatt_put(nc.out, var.cbh_err, "description",
                     "Standard error on height of lowest cloud base above mean sea level") 
    ncdf4::ncatt_put(nc.out, var.cbh_err, "coordinates", "time longitude latitude")

    ncdf4::ncatt_put(nc.out, var.trajectory_id, "cf_role", "trajectory_id")
    ncdf4::ncatt_put(nc.out, var.trajectory_id, "description",
                     "Trajectory identified by UTC trajectory start date and (D)ay/(N)ight flag") 
    
    ncdf4::ncatt_put(nc.out, 0, "Conventions", "CF-1.6")
    ncdf4::ncatt_put(nc.out, 0, "title", title)
    ncdf4::ncatt_put(nc.out, 0, "references", "Muelmenstaedt et al., doi:10.5194/essd-xxxx, 2017")
    ncdf4::ncatt_put(nc.out, 0, "source",
                     "satellite, CALIOP Vertical Feature Mask processed by CBASE algorithm")
    ncdf4::ncatt_put(nc.out, 0, "institution", "University of Leipzig")
    ncdf4::ncatt_put(nc.out, 0, "history",
                     sprintf("cbasetools::cbase.create.nc (version %s) run on %s",
                             packageVersion("cbasetools"), format(Sys.time(),
                                                                  "%F %T %Z")))
    ncdf4::ncatt_put(nc.out, 0, "featureType", "trajectory")

    ncdf4::nc_close(nc.out)
}
