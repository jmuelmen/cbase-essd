#! /usr/bin/Rscript --vanilla

library(getopt)
library(cbasetools)
library(plyr)

combine.cbase.metar.dummy <-
    function(eval = get.metar.2008(),
             retrieval = dbtools::db_spec("cloud-bases-2008.sqlite", "cloudbase"),
             resolution = resolution.min_cbh,
             ncores = 72,
             ...) {
        str(eval);
        print(retrieval);
        print(resolution);
        print(ncores);
        return(resolution);
    }


spec <- matrix(c(
##    'verbose', 'v', 2, "integer",
    'help'              , 'h', 0, "logical",    "",
    'ncores'            , 'n', 1, "integer",    "default: 72",
    'out.name'          , 'o', 1, "character",  "",
    'path'              , 'p', 1, "character",  "",
    'pattern'           , 'x', 1, "character",  "default: CAL_LID_L2_VFM-Standard-V40-10.*hdf"
), byrow=TRUE, ncol=5);

opt <- getopt(spec, opt = commandArgs(TRUE));

## if help was asked for, print a friendly message and exit with a
## non-zero error code
if ( !is.null(opt$help) ) {
    cat(getopt(spec, usage=TRUE));
    q(status=1);
}

## get rid of spurious "ARGS" list element
if (names(opt)[1] == "ARGS")
    opt <- opt[-1];

ret <- do.call(bases.cbase, opt);

q();

