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
CBASEDIR_40 = $(subst CALIOP/VFM.v4.10,CBASE-40,$(HDFDIR))
CBASEDIR_100 = $(subst CALIOP/VFM.v4.10,CBASE-100,$(HDFDIR))

HDFDIR_2007 = /work/bb0839/b380126/CALIOP/VFM.v4.10/2007
HDF_2007_STRUCTURE := $(shell find $(HDFDIR_2007) -type d)
CBASE_2007_STRUCTURE := $(subst $(HDFDIR),$(CBASEDIR_40),$(HDF_2007_STRUCTURE) ) $(subst $(HDFDIR),$(CBASEDIR_100),$(HDF_2007_STRUCTURE) )
HDF_2007 := $(shell find $(HDFDIR_2007) -name \*.hdf)
CBASE_2007 := $(subst .hdf,.nc,$(subst CAL_LID_L2_VFM-Standard-V4-10,CBASE-40,$(subst $(HDFDIR),$(CBASEDIR_40),$(HDF_2007)))) $(subst .hdf,.nc,$(subst CAL_LID_L2_VFM-Standard-V4-10,CBASE-100,$(subst $(HDFDIR),$(CBASEDIR_100),$(HDF_2007))))

HDFDIR_2008 = /work/bb0839/b380126/CALIOP/VFM.v4.10/2008
HDF_2008_STRUCTURE := $(shell find $(HDFDIR_2008) -type d)
CBASE_2008_STRUCTURE := $(subst $(HDFDIR),$(CBASEDIR_40),$(HDF_2008_STRUCTURE) ) $(subst $(HDFDIR),$(CBASEDIR_100),$(HDF_2008_STRUCTURE) )
HDF_2008 := $(shell find $(HDFDIR_2008) -name \*.hdf)
CBASE_2008 := $(subst .hdf,.nc,$(subst CAL_LID_L2_VFM-Standard-V4-10,CBASE-40,$(subst $(HDFDIR),$(CBASEDIR_40),$(HDF_2008)))) $(subst .hdf,.nc,$(subst CAL_LID_L2_VFM-Standard-V4-10,CBASE-100,$(subst $(HDFDIR),$(CBASEDIR_100),$(HDF_2008))))

## VPATH = $(subst " ",":",$(CBASE_2007_STRUCTURE))

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

cbase-2007.rds:	$(HDF_2007)
	mkdir -p cloud-bases
	sbatch $(CBASE) $(CBASEFLAGS) -o $@ -p $(HDFDIR)/2007

cbase-2008.rds:	$(HDF_2008)
	mkdir -p cloud-bases
	sbatch $(CBASE) $(CBASEFLAGS) -o $@ -p $(HDFDIR)/2008

CBASE%.nc:
	@ if ! [ -e cloud-bases/`basename $@` ] ; then \
		echo cloud-bases/`basename $@` does not exist ; \
	else cp -a cloud-bases/`basename $@` $@ ; \
	fi

.PHONY: tree
tree:
	@ mkdir -p $(CBASE_2007_STRUCTURE) $(CBASE_2008_STRUCTURE)
##	for i in $(HDF_2007) ; do echo $i ; done

.PHONY:	all
all:	tree $(CBASE_2007) $(CBASE_2008)

