#!/bin/bash

#SBATCH --account=bb1002
#SBATCH --partition=compute2
#SBATCH --job-name=cbase
#SBATCH --nodes=2
#SBATCH --mem-per-cpu=10G
#SBATCH --ntasks-per-node=18
#SBATCH --time=01:00:00
#SBATCH --mail-type=FAIL

# limit stacksize ... adjust to your programs need
ulimit -s 102400

# Environment settings to run a MPI parallel program
# compiled with BullxMPI and Mellanox libraries
# Load environment
module load intel mxm fca bullxmpi_mlx
module load r

# Settings for Open MPI and MXM (MellanoX Messaging)
# library
export OMPI_MCA_pml=cm
export OMPI_MCA_mtl=mxm
export OMPI_MCA_mtl_mxm_np=0
export MXM_RDMA_PORTS=mlx5_0:1
export MXM_LOG_LEVEL=ERROR
# Disable GHC algorithm for collective communication
export OMPI_MCA_coll=^ghc

export PATH=$PATH:/sw/rhel6-x64/hdf4/hdf-4.2.10-gcc48/bin

# Use srun (not mpirun or mpiexec) command to launch
# programs compiled with any MPI library
mpirun -n 1 Rscript --vanilla ./cbase.r $*
