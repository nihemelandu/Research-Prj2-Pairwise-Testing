#!/bin/bash

#Account and Email Information
#SBATCH --mail-user=ngoziihemelandu@u.boisestate.edu   # email address
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END
#SBATCH --mail-type=FAIL

#SBATCH -J R-Sta-Test     # R-01-type_1
#SBATCH -o R-outputs/results.o%j # output and error file name (%j expands to jobID)
#SBATCH -e R-outputs/errors.e%j
#SBATCH -N 1              # Run one process
#SBATCH -p bsudfq            # queue (partition) -- shortq, defq, ipowerq, eduq, gpuq, piret, ipc-research, bsudfq, bigmem.
#SBATCH -t 35-00:00:00      # run time (d-hh:mm:ss)

ulimit -v unlimited
ulimit -s unlimited
ulimit -u 3000   #r2 set to 1000


#Rscript R-scripts/01-margins.R

# Run the code
exec "$@"