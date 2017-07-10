COMBINE = ./combine.sh
METARCOMBINEFLAGS = -n 72 --method=combine.cbase.metar \
		--retrieval="list(filename='$<',table='cloudbase')" \
		-o "'$@'"

SQLIZE = ./sqlize.sh

SRUNENV = SLURM_JOB_PARTITION=compute2 SLURM_CPU_FREQ_REQ=High
SRUNFLAGS = -A bb1002 -p compute2 --time=08:00:00 --exclusive -N 1 --mem=250G

CBASE = ./cbase.sh
CBASEFLAGS = 
HDFDIR = /work/bb0839/b380126/CALIOP/VFM.v4.10
CBASEDIR = $(subst CALIOP/VFM.v4.10,CBASE,$(HDFDIR))
HDF_2007 := $(shell find $(HDFDIR) -name \*.hdf)
HDF_2007_STRUCTURE := $(shell find $(HDFDIR)/2007 -type d)
CBASE_2007_STRUCTURE := $(subst $(HDFDIR),$(CBASEDIR),$(HDF_2007_STRUCTURE) )

$(HOME)/cloud-bases-2008-local.sqlite: $(HOME)/cloud-bases-2008.rds
	$(SQLIZE) -i "'$<'" -o "'$@'" -t "'cloudbase'" -x 'list("ifile","ipoint.40","ipoint.100")'

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

cbase-2007.rds:	## $(HDF_2007)
##mkdir -p $(CBASE_2007_STRUCTURE)
	mkdir -p cloud-bases
	sbatch $(CBASE) $(CBASEFLAGS) -o $@ -p $(HDFDIR)/2007

cbase-2008.rds:	## $(HDF_2007)
##mkdir -p $(CBASE_2007_STRUCTURE)
	mkdir -p cloud-bases
	sbatch $(CBASE) $(CBASEFLAGS) -o $@ -p $(HDFDIR)/2008

.PHONY:	all
all:	cbase-2007.rds ## cbase-2008.rds ## cbm-min_cbh.rds cbm-min_cbh.qual.rds cbm-quantile_cbh.qual.rds
