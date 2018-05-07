#!/bin/bash 

#SBATCH --nodes=1                                 # Number of Nodes 
#SBATCH --mail-type=ALL                           # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=myEmail@gmail.com             # Where to send mail
#SBATCH --ntasks=1                                # Run a single task
#SBATCH --cpus-per-task=24                        # Number of CPU cores per task
#SBATCH --mem-per-cpu=8000			  # allocated memory for the task
#SBATCH -p nodename				  # name of cluster node
#SBATCH --requeue                                 # Allow the job to be requeued
#SBATCH -e myJob.err 				  # File to which STDERR will be written 
#SBATCH -o myJob.out 				  # File to which STDOUT will be written 
#SBATCH -J myJob 				  # Job name 

module load R/MS3.4.1				  # call a preinstalled module(program) on the cluster

Rscript connect_calc_25.R			  # Run the R script in bash 
 
