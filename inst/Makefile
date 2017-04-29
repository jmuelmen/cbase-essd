COMBINE = ./combine.sh
METARCOMBINEFLAGS = -n 72 --method=combine.cbase.metar \
		--retrieval="list(filename='$<',table='cloudbase')" \
		-o "'$@'"

SQLIZE = ./sqlize.sh

SRUNENV = SLURM_JOB_PARTITION=compute2 SLURM_CPU_FREQ_REQ=High
SRUNFLAGS = -A bb1002 -p compute2 --time=08:00:00 --exclusive -N 1 --mem=250G

$(HOME)/cloud-bases-2007.sqlite:	$(HOME)/cloud-bases-2007.rds
	$(SQLIZE) -i "'$<'" -o "'$@'" -t "'cloudbase'" -x 'list("lon","lat","time")'

cloud-bases-2007.sqlite:	$(HOME)/cloud-bases-2007.sqlite
	ln -s $<

cloud-bases-2008.sqlite:
	ln -s $(HOME)/cloud-bases-2008.sqlite

cbm-all.rds:	cloud-bases-2008.sqlite
	@ echo using R to make $@ from $<
	env $(SRUNENV) srun $(SRUNFLAGS) \
		$(COMBINE) $(METARCOMBINEFLAGS) \
		--resolution=resolution.all

cbm-all-2007.rds:	cloud-bases-2007.sqlite
	@ echo using R to make $@ from $<
	env $(SRUNENV) srun $(SRUNFLAGS) \
		$(COMBINE) $(METARCOMBINEFLAGS) \
		--resolution=resolution.all \
		--eval="get.metar.2007()"

cbm-min_dist.rds:	cloud-bases-2008.sqlite
	@ echo using R to make $@ from $<
	env $(SRUNENV) srun $(SRUNFLAGS) \
		$(COMBINE) $(METARCOMBINEFLAGS) \
		--resolution=resolution.min_dist

cbm-min_cbh.rds:	cloud-bases-2008.sqlite
	@ echo using R to make $@ from $<
	env $(SRUNENV) srun $(SRUNFLAGS) \
		$(COMBINE) $(METARCOMBINEFLAGS) \
		--resolution=resolution.min_cbh

cbm-min_cbh.qual.rds:	cloud-bases-2008.sqlite
	@ echo using R to make $@ from $<
	env $(SRUNENV) srun $(SRUNFLAGS) \
		$(COMBINE) $(METARCOMBINEFLAGS) \
		--resolution=resolution.min_cbh.qual

cbm-quantile_cbh.qual.rds:	cloud-bases-2008.sqlite
	@ echo using R to make $@ from $<
	env $(SRUNENV) srun $(SRUNFLAGS) \
		$(COMBINE) $(METARCOMBINEFLAGS) \
		--resolution="c(resolution.quantile_cbh.qual(0.01),resolution.quantile_cbh.qual(0.02),resolution.quantile_cbh.qual(0.05),resolution.quantile_cbh.qual(0.1),resolution.quantile_cbh.qual(0.2))"

cbm-min_cbh-2007.rds:	cloud-bases-2007.sqlite
	@ echo using R to make $@ from $<
	env $(SRUNENV) srun $(SRUNFLAGS) \
		$(COMBINE) $(METARCOMBINEFLAGS) \
		--resolution=resolution.min_cbh \
		--eval="get.metar.2007()"

cbm-min_cbh.qual-2007.rds:	cloud-bases-2007.sqlite
	@ echo using R to make $@ from $<
	env $(SRUNENV) srun $(SRUNFLAGS) \
		$(COMBINE) $(METARCOMBINEFLAGS) \
		--resolution=resolution.min_cbh.qual \
		--eval="get.metar.2007()"


.PHONY:	all
all:	cbm-min_cbh.rds cbm-min_cbh.qual.rds cbm-quantile_cbh.qual.rds
