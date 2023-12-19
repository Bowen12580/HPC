# Clear the workspace and close graphical interfaces
rm(list=ls())
graphics.off()

# Load necessary functions
source("/rds/general/user/bd623/home/run_files/bd623_HPC_2023_main.R")  # Load all required functions

# Read the job number from the cluster
iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX"))

# Set the random seed
set.seed(iter)

# Choose initial conditions
if (iter <= 25) {
  initial_state <- state_initialise_adult(4, 100)
} else if (iter <= 50) {
  initial_state <- state_initialise_adult(4, 10)
} else if (iter <= 75) {
  initial_state <- state_initialise_spread(4, 100)
} else {
  initial_state <- state_initialise_spread(4, 10)
}

# Create a result file name
result_filename <- paste0("simulation_result_", iter, ".rda")

# Initialize the results list
results <- vector("list", 150)

clutch_distribution <- c(0.06,0.08,0.13,0.15,0.16,0.18,0.15,0.06,0.03)

growth_matrix <- matrix(c(0.1, 0.0, 0.0, 0.0,
                          0.5, 0.4, 0.0, 0.0,
                          0.0, 0.4, 0.7, 0.0,
                          0.0, 0.0, 0.25, 0.4),
                        nrow=4, ncol=4, byrow=TRUE)

reproduction_matrix <- matrix(c(0.0, 0.0, 0.0, 2.6,
                                0.0, 0.0, 0.0, 0.0,
                                0.0, 0.0, 0.0, 0.0,
                                0.0, 0.0, 0.0, 0.0),
                              nrow=4, ncol=4, byrow=TRUE)

# Perform simulations and save results
for (i in 1:150) {
  results[[i]] <- stochastic_simulation(initial_state, growth_matrix, reproduction_matrix, clutch_distribution, 120)
}

# Save simulation results
save(results, file=result_filename)
