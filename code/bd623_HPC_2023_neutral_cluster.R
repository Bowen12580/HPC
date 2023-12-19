# CMEE 2022 HPC exercises R code pro forma
# For neutral model cluster run

rm(list=ls())  # Clear the workspace, a good practice
source("/rds/general/user/bd623/home/run_files/bd623_HPC_2023_main.R")  # Load all the required functions

# Read the cluster job index (for the cluster)
iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX"))

# Job index used for local testing
#iter <- 2

# Control the random seed to ensure each simulation is unique
set.seed(iter)

# Choose community size based on the job index
if (iter <= 25) {
  size <- 500
} else if (iter <= 50) {
  size <- 1000
} else if (iter <= 75) {
  size <- 2500
} else {
  size <- 5000
}

# Set the speciation rate
speciation_rate <- 0.002604  

# Create a filename to store the results, ensuring each simulation has a unique filename
result_filename <- paste0("output_", iter, ".rda")

# Call the simulation function, set wall_time to 11.5 hours
neutral_cluster_run(speciation_rate, size, wall_time = 690, interval_rich = 1, interval_oct = size / 10, burn_in_generations = 8 * size, output_file_name = result_filename)
