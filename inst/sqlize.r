#! /usr/bin/Rscript --vanilla

library(getopt)
library(plyr)
library(methods)

spec = matrix(c(
##    'verbose', 'v', 2, "integer",
    'help'              , 'h', 0, "logical",    "",
    'input'             , 'i', 1, "character",  "",
    'output'            , 'o', 1, "character",  "",
    'table'             , 't', 1, "character",  "",
    'indexes'           , 'x', 1, "character",  ""
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

process <- function(input, output, table, indexes) {
    df <- readRDS(input);
    cbasetools::df.to.db(df, output, table, indexes);
}

ret <- do.call(process, args);
q();

