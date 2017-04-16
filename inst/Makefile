COMBINE = ./combine.sh
COMBINEFLAGS = ## --method=combine.cbase.metar -n 72

SRUNENV = SLURM_JOB_PARTITION=compute2 SLURM_CPU_FREQ_REQ=High
SRUNFLAGS = -A bb1002 -p compute --time=08:00:00 --exclusive -N 1 --mem=250G

%.rds:	cloud-bases-2008.sqlite
	@ echo using R to make $@ from $<
	env $(SRUNENV) srun $(SRUNFLAGS) \
		$(COMBINE) $(COMBINEFLAGS) $(EXTRACOMBINEFLAGS) \
		--method=combine.cbase.metar.dummy \
		--retrieval="list(filename='$<',table='cloudbase')" \
		-o "'$@'"

.PHONY:	all
all:	cbm-min.rds
