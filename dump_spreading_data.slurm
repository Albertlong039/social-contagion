#!/bin/bash
#SBATCH --job-name="dump_spreading_data"
#SBATCH --nodes=1
#SBATCH --cpus-per-task=10
#SBATCH --partition=smp
#SBATCH --cluster=smp
#SBATCH --time=0-10:30:00
#SBATCH --mem-per-cpu=4G

#SBATCH -o /ihome/arahimian/yil305/cai-contagion/output/output.out
#SBATCH -e /ihome/arahimian/yil305/cai-contagion/errors/errors.err
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=YIL305@pitt.edu

module load python/intel-3.6

python3 /ihome/arahimian/yil305/cai-contagion/dump_spreading_data.py

