#!/bin/bash

## R 3.3 depends on these shared libraries, but they do not exist on the compute nodes
for i in libicuuc.so.42 libicui18n.so.42 libicudata.so.42 ; do
    if ! [ -e $i ] ; then
	cp  /usr/lib64/${i}.1 .
	ln -s ${i}.1 $i
    fi
done

module load r

## RPATH=/pf/b/b380126/echam-ham/echam6.1-ham2.2-moz0.9/run
export RPATH=.
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$PWD

echo "$*"

Rscript --vanilla ${RPATH}/combine.r $*
