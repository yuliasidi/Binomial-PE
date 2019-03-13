#!/bin/bash
#!/bin/bash
#SBATCH -p general
#SBATCH -n 192
#SBATCH --time=360
#SBATCH --output=/home/yus16116/results.log
#SBATCH --error=/home/yus16116/results.err
#SBATCH --mail-type=ALL  # Event(s) that triggers email notification (BEGIN,END,FAIL,ALL)
#SBATCH --mail-user=yulia.sidi@uconn.edu      # Destination email address


source /etc/profile.d/modules.sh
module purge
module load gcc/5.4.0-alt r/3.5.1-openblas-gcc540 mpi/openmpi/1.10.1-gcc

# If MPI tells you that forking is bad uncomment the line below 
export OMPI_MCA_mpi_warn_on_fork=0

Rscript fullH0_29_do10.R 
