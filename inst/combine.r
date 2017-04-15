#! /usr/bin/env Rscript --vanilla

library(getopt)
library(cbasetools)
library(plyr)

combine.cbase.metar.dummy <-
    function(eval = get.metar.2008(),
             retrieval = dbtools::db_spec("cloud-bases-2008.sqlite", "cloudbase"),
             resolution = resolution.min_cbh,
             ncores = 72) {
        str(eval);
        print(retrieval);
        print(resolution);
        print(ncores);
    }


spec = matrix(c(
##    'verbose', 'v', 2, "integer",
    'eval'              , 'e', 1, "character",  "",
    'help'              , 'h', 0, "logical",    "",
    'method'            , 'm', 1, "character",  "combination method; default: combine.cbase",
    'ncores'            , 'n', 1, "integer",    "default: 72",
    'retrieval'         , 'r', 1, "character",  "",
    'resolution'        , 'x', 1, "character",  ""
), byrow=TRUE, ncol=5);

opt = getopt(spec, opt = commandArgs(TRUE));

## if help was asked for print a friendly message
## and exit with a non-zero error code
if ( !is.null(opt$help) ) {
    cat(getopt(spec, usage=TRUE));
    q(status=1);
}

## get rid of spurious "ARGS" list element
if (names(opt)[1] == "ARGS")
    opt <- opt[-1];

## parse arguments
args <- llply(opt, function(text) {
    eval(parse(text = text));
});

if (is.null(args$method))
    args$method <- combine.cbase;

do.call(args$method,  ## comination method
        args[-(names(args) == "method")] ## all the other arguments
        );
q();

