export PATH=$PATH:/sw/rhel6-x64/hdf4/hdf-4.2.10-gcc48/bin
module load r/3.2.0
salloc -A bb0839 --partition=compute2 --exclusive -N 1 --mem=250G

library(cbasetools)
df <- combine.cbase.metar(cloudbase = list(filename = "cloud-bases-2008.sqlite", table = "cloudbase"), n.cores = 72)
saveRDS(df, "cbm.rds")
