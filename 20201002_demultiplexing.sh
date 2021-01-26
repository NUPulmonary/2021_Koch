#### Demultiplexing using bcl2fastq

###### Pool1

#!/bin/bash
#SBATCH -A b1042
#SBATCH -p genomics
#SBATCH -t 1:30:00
#SBATCH -N 1
#SBATCH --mem=16G
#SBATCH --ntasks-per-node=12
#SBATCH --mail-user=kishore.anekalla@northwestern.edu
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --job-name="20201003_pool1_bcl2fastq"

module purge
cd /projects/b1042/Ridgelab/200922_NB501488_0394_AHHVKNBGXG/
module load bcl2fastq/2.19.1 
bcl2fastq --sample-sheet /projects/b1042/Ridgelab/SampleSheet_pool1.csv  --loading-threads 8 --writing-threads 8 --processing-threads 8 --output-dir /projects/b1042/Ridgelab/pool1_fastq/

####### Pool2 

#!/bin/bash
#SBATCH -A b1042
#SBATCH -p genomics
#SBATCH -t 12:00:00
#SBATCH -N 1
#SBATCH --mem=48G
#SBATCH --ntasks-per-node=12
#SBATCH --mail-user=kishore.anekalla@northwestern.edu
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --job-name="20201002_pool2_bcl2fastq"

module purge
cd /projects/b1042/Ridgelab/200923_NB501488_0395_AHHVKMBGXG/
module load bcl2fastq/2.19.1 
bcl2fastq --sample-sheet /projects/b1042/Ridgelab/SampleSheet_pool2.csv  --loading-threads 8 --writing-threads 8 --processing-threads 8 --output-dir /projects/b1042/Ridgelab/pool2_fastq/


####### Pool3 


#!/bin/bash
#SBATCH -A b1042
#SBATCH -p genomics
#SBATCH -t 12:00:00
#SBATCH -N 1
#SBATCH --mem=48G
#SBATCH --ntasks-per-node=12
#SBATCH --mail-user=kishore.anekalla@northwestern.edu
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --job-name="20201002_pool3_bcl2fastq"

module purge
cd /projects/b1042/Ridgelab/200924_NB501488_0396_AHN7MCBGXG/
module load bcl2fastq/2.19.1 
bcl2fastq --sample-sheet /projects/b1042/Ridgelab/SampleSheet_pool3.csv  --loading-threads 8 --writing-threads 8 --processing-threads 8 --output-dir /projects/b1042/Ridgelab/pool3_fastq/


####### Pool4

#!/bin/bash
#SBATCH -A b1042
#SBATCH -p genomics
#SBATCH -t 12:00:00
#SBATCH -N 1
#SBATCH --mem=48G
#SBATCH --ntasks-per-node=12
#SBATCH --mail-user=kishore.anekalla@northwestern.edu
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --job-name="20201206_pool4_bcl2fastq"

module purge
cd /projects/b1042/Ridgelab/201130_NB501488_0415_AH3F3MBGXH/
module load bcl2fastq/2.19.1 
bcl2fastq --sample-sheet /projects/b1042/Ridgelab/SampleSheet_pool4.csv  --loading-threads 8 --writing-threads 8 --processing-threads 8 --output-dir /projects/b1042/Ridgelab/pool4_fastq/
