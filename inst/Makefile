COMBINE = ./combine.sh
METARCOMBINEFLAGS = -n 2 --method=combine.cbase.metar \
		--retrieval="list(filename='$<',table='cloudbase')" \
		-o "'$@'"

SQLIZE = ./sqlize.sh

SRUNENV = SLURM_JOB_PARTITION=compute2 SLURM_CPU_FREQ_REQ=High
SRUNFLAGS = -A bb0839 -p compute2 --time=08:00:00 --exclusive -N 1 --mem=250G

$(HOME)/r-packages/cloud-bases-2007.sqlite:	$(HOME)/cloud-bases-2007.rds
	$(SQLIZE) -i "'$<'" -o "'$@'" -t "'cloudbase'" -x 'list("lon","lat","time")'

cloud-bases-2007.sqlite:	$(HOME)/r-packages/cloud-bases-2007.sqlite
	ln -s $<

cloud-bases-2008.sqlite:
	ln -s $(HOME)/r-packages/cloud-bases-2008.sqlite

cbm-min_cbh.rds:	cloud-bases-2008.sqlite
	@ echo using R to make $@ from $<
## env $(SRUNENV) srun $(SRUNFLAGS) 
		$(COMBINE) $(METARCOMBINEFLAGS) \
		--resolution=resolution.min_cbh

cbm-min_cbh.qual.rds:	cloud-bases-2008.sqlite
	@ echo using R to make $@ from $<
## env $(SRUNENV) srun $(SRUNFLAGS) 
		$(COMBINE) $(METARCOMBINEFLAGS) \
		--resolution=resolution.min_cbh.qual

cbm-quantile_cbh.qual.rds:	cloud-bases-2008.sqlite
	@ echo using R to make $@ from $<
## 	env $(SRUNENV) srun $(SRUNFLAGS) 
		$(COMBINE) $(METARCOMBINEFLAGS) \
		--resolution="c(q0.01=resolution.quantile_cbh.qual(0.01),resolution.quantile_cbh.qual(0.02),resolution.quantile_cbh.qual(0.05),resolution.quantile_cbh.qual(0.1),resolution.quantile_cbh.qual(0.2))"


.PHONY:	all
all:	cbm-min_cbh.rds cbm-min_cbh.qual.rds cbm-quantile_cbh.qual.rds
