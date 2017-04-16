#!/bin/bash

module load r/3.2.0

## RPATH=/pf/b/b380126/echam-ham/echam6.1-ham2.2-moz0.9/run
RPATH=.

echo $*

Rscript --vanilla ${RPATH}/combine.r $*
