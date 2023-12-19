# CMEE 2022 HPC exercises R code main pro forma
# You don't HAVE to use this but it will be very helpful.
# If you opt to write everything yourself from scratch please ensure you use
# EXACTLY the same function and parameter names and beware that you may lose
# marks if it doesn't work properly because of not using the pro-forma.

name <- "Bowen Duan"
preferred_name <- "Bowen"
email <- "bd623@imperial.ac.uk"
username <- "bd623"

# Please remember *not* to clear the workspace here, or anywhere in this file.
# If you do, it'll wipe out your username information that you entered just
# above, and when you use this file as a 'toolbox' as intended it'll also wipe
# away everything you're doing outside of the toolbox.  For example, it would
# wipe away any automarking code that may be running and that would be annoying!

# Question 1
species_richness <- function(community){
  return(length(unique(community)))
}

# Question 2
init_community_max <- function(size){
  community<- seq(1,size)
  return(community)
}

# Question 3
init_community_min <- function(size){
  community <- rep(1,size)
  return(community)
}


# Question 4
choose_two <- function(max_value){
  wanted_integers <- sample(1:max_value , size =2)
  return(wanted_integers)
}

# Question 5
neutral_step <- function(community){
  #find the dying index and the reproduction index
  chosen_two_species <- choose_two(length(community))
  dying_index <- chosen_two_species[1]
  reproduction_index <- chosen_two_species[2]
  community[dying_index] <- community[reproduction_index]
  return(community)

}

# Question 6
neutral_generation <- function(community){
    # Determine the number of neutral steps to perform
    num_steps <- length(community) / 2
    # If the number of individuals is not even, randomly decide whether to round up or down
    if (length(community) %% 2 != 0) {
        num_steps <- ifelse(runif(1) < 0.5, floor(num_steps), ceiling(num_steps))
    }

    # Perform the specified number of neutral steps
    for (i in 1:num_steps) {
        community <- neutral_step(community)
    }

    return(community)
}


# Question 7
neutral_time_series <- function(community, duration) {
    # Initialize a vector containing the initial species richness
    species_richness_series <- numeric(duration + 1)
    species_richness_series[1] <- species_richness(community)

    # Simulate for each generation
    for (i in 1:duration) {
        community <- neutral_generation(community)
        species_richness_series[i + 1] <- species_richness(community)
    }

    return(species_richness_series)
}


# Question 8
question_8 <- function() {
  
    # Initialize initial conditions and runtime
  initial_community <- init_community_max(100)
  duration <- 200

  # Get the time series
  time_series <- neutral_time_series(initial_community, duration)
  
  png("../results/question_8", width = 600, height = 400)
  # Plot your graph here
  plot(time_series, type = "l", xlab = "Generation", ylab = "Species Richness", main = "Neutral Model Time Series")
  Sys.sleep(0.1)
  dev.off()
  
  return("Over time, random birth and death events cause species to disappear in ecosystems. Because the creation of new species is not defined in the function, once a species disappears, it does not reappear. The system may end up with only one or very few species left, as random factors cause more species to go extinct.")
}


# Question 9
neutral_step_speciation <- function(community, speciation_rate)  {
    # Randomly select individuals for death and reproduction
    chosen_two_species <- choose_two(length(community))
    dying_index <- chosen_two_species[1]
    reproduction_index <- chosen_two_species[2]

    # Determine whether speciation occurs
    if (runif(1) < speciation_rate) {
        # Speciation: Introduce a new species identifier
        new_species <- max(community) + 1
        community[dying_index] <- new_species
    } else {
        # No speciation: Handle as before
        community[dying_index] <- community[reproduction_index]
    }

    return(community)
}


# Question 10
neutral_generation_speciation <- function(community, speciation_rate)  {
    # Determine the number of neutral_step_speciation steps to execute
    num_steps <- length(community) / 2
    # If the number of individuals is not even, randomly decide whether to round up or down
    if (length(community) %% 2 != 0) {
        num_steps <- ifelse(runif(1) < 0.5, floor(num_steps), ceiling(num_steps))
    }

    # Execute the specified number of neutral_step_speciation steps
    for (i in 1:num_steps) {
        community <- neutral_step_speciation(community, speciation_rate)
    }

    return(community)
}


# Question 11
neutral_time_series_speciation <- function(community, speciation_rate, duration)  {
    # Initialize a vector containing the initial species richness
    species_richness_series <- numeric(duration + 1)
    species_richness_series[1] <- species_richness(community)

    # Simulate for each generation
    for (i in 1:duration) {
        community <- neutral_generation_speciation(community, speciation_rate)
        species_richness_series[i + 1] <- species_richness(community)
    }

    return(species_richness_series)
}


# Question 12
question_12 <- function() {
    speciation_rate <- 0.1
    duration <- 200
    community_max <- init_community_max(100)
    community_min <- init_community_min(100)

    time_series_max <- neutral_time_series_speciation(community_max, speciation_rate, duration)
    time_series_min <- neutral_time_series_speciation(community_min, speciation_rate, duration)
   
    # Question 12 Plot with Legend
    png(filename="../results/question_12.png", width = 600, height = 400)
    plot(1:(duration + 1), time_series_max, type = "l", col = "blue", xlab = "Generation", ylab = "Species Richness", main = "Species Richness over Time")
    lines(1:(duration + 1), time_series_min, col = "red")
    legend("topright", legend=c("Max Diversity", "Min Diversity"), col=c("blue", "red"), lty=1)
    dev.off()


    return("The plot suggests that in a neutral model, random births and deaths of species eventually lead to an equilibrium in species richness. Differences in initial conditions may temporarily affect the species richness of the system.")
}


# Question 13
species_abundance <- function(community)  {
    # Use the table function to count the number of individuals for each species
    abundances <- table(community)
    # Use the sort function to sort the abundances in descending order
    sorted_abundances <- sort(abundances, decreasing = TRUE)
    return(sorted_abundances)
}

  


# Question 14
octaves <- function(abundance_vector) {
    octave_classes <- floor(log(abundance_vector, base = 2)) + 1
    octave_counts <- tabulate(octave_classes)
    return(octave_counts)
}


# Question 15
sum_vect <- function(x, y) {
    # Ensure that the two vectors have the same length
    length_x <- length(x)
    length_y <- length(y)

    if (length_x > length_y) {
        y <- c(y, rep(0, length_x - length_y))
    } else if (length_y > length_x) {
        x <- c(x, rep(0, length_y - length_x))
    }

    return(x + y)
}


# Question 16 
question_16 <- function() {
    speciation_rate <- 0.1
    community_size <- 100
    burn_in_duration <- 200
    total_duration <- 2000
    record_interval <- 20
    
    # Initialize communities
    community_max <- init_community_max(community_size)
    community_min <- init_community_min(community_size)

    # Burn-in period
    for (i in 1:burn_in_duration) {
        community_max <- neutral_generation_speciation(community_max, speciation_rate)
        community_min <- neutral_generation_speciation(community_min, speciation_rate)
    }

    # Collect octave-level data
    collect_octaves <- function(community) {
        octaves_data <- vector("list", length = total_duration / record_interval)
        for (i in seq_len(total_duration / record_interval)) {
            for (j in 1:record_interval) {
                community <- neutral_generation_speciation(community, speciation_rate)
            }
            octaves_data[[i]] <- octaves(species_abundance(community))
        }
        return(octaves_data)
    }

    octaves_max <- collect_octaves(community_max)
    octaves_min <- collect_octaves(community_min)

    # Calculate the mean
    mean_octave <- function(octaves_list) {
        longest <- max(sapply(octaves_list, length))
        sum_octaves <- rep(0, longest)
        for (oct in octaves_list) {
            oct <- c(oct, rep(0, longest - length(oct)))
            sum_octaves <- sum_octaves + oct
        }
        mean_octaves <- sum_octaves / length(octaves_list)
        return(mean_octaves)
    }

    mean_octaves_max <- mean_octave(octaves_max)
    mean_octaves_min <- mean_octave(octaves_min)

    # Generate and save a bar plot for the minimum initial conditions
    png(filename="../results/question_16_min.png", width = 600, height = 400)
    barplot(mean_octaves_min, main="Species Abundance Distribution (Min Init)", xlab="Abundance", ylab="Mean Species Count")
    Sys.sleep(0.1)
    dev.off()
  
    # Generate and save a bar plot for the maximum initial conditions
    png(filename="../results/question_16_max.png", width = 600, height = 400)
    barplot(mean_octaves_max, main="Species Abundance Distribution (Max Init)", xlab="Abundance", ylab="Mean Species Count")
    Sys.sleep(0.1)
    dev.off()
  
    return("Initial conditions influence the system in the early stages. However, random species extinctions and chance species formations mitigate their effects over time, and this leads the system to converge towards a stable distribution of species abundance. Therefore we can infer that initial conditions influence early stages but do not determine long-term system dynamics.")
}



# Question 17
neutral_cluster_run <- function(speciation_rate, size, wall_time, interval_rich, interval_oct, burn_in_generations, output_file_name) {
  # Start with a community of given size with minimal diversity
  community <- init_community_min(size)
  
  # Record the start time
  start_time <- proc.time()
  
  # Initialize variables
  time_series <- numeric()
  abundance_list <- list()
  generation <- 1
  
  # Main simulation loop
  while(TRUE) {
    # Apply neutral generation with speciation rate
    community <- neutral_generation_speciation(community, speciation_rate)
    
    # Record species richness at intervals during burn-in
    if (generation %% interval_rich == 0 && generation <= burn_in_generations) {
      time_series <- c(time_series, species_richness(community))
    }
    
    # Record species abundances as octaves at specified intervals
    if (generation %% interval_oct == 0) {
      abundance_list[[length(abundance_list) + 1]] <- octaves(species_abundance(community))
    }
    
    # Check if the total time exceeds wall_time (in minutes)
    elapsed_time <- (proc.time() - start_time)["elapsed"]
    if (elapsed_time >= wall_time * 60) {
      break
    }
    
    # Increment the generation counter
    generation <- generation + 1
  }
  
  # Save simulation results to output file
  result <- list(
    time_series = time_series,
    abundance_list = abundance_list,
    final_community_state = community,
    total_time = elapsed_time,
    speciation_rate = speciation_rate,
    size = size,
    interval_rich = interval_rich,
    interval_oct = interval_oct,
    burn_in_generations = burn_in_generations
  )
  
  save(result, file = output_file_name)
}


# Questions 18 and 19 involve writing code elsewhere to run your simulations on
# the cluster

# Question 20 
# Function to process the simulation results and calculate the mean abundance octaves
process_neutral_cluster_results <- function() {
  file_names <- list.files(path = "../Q19_final", pattern = "\\.rda$", full.names = TRUE)
  abundance_lists <- vector("list", length = 4)
  names(abundance_lists) <- c("500", "1000", "2500", "5000")
  
  for (file_name in file_names) {
    load(file_name)
    # Assuming the structure of 'result' as per previous discussions and the output from Question 19
    burn_in_index <- result$burn_in_generations / result$interval_oct
    post_burn_in_abundances <- result$abundance_list[(burn_in_index + 1):length(result$abundance_list)]
    size_label <- as.character(result$size)
    
    if (is.null(abundance_lists[[size_label]])) {
      abundance_lists[[size_label]] <- post_burn_in_abundances
    } else {
      abundance_lists[[size_label]] <- mapply(sum_vect, abundance_lists[[size_label]], post_burn_in_abundances, SIMPLIFY = FALSE)
    }
  }
  
  # Calculate the mean across all simulations for each community size
  mean_abundances <- lapply(abundance_lists, function(ab_list) {
    sapply(1:max(sapply(ab_list, length)), function(i) {
      mean(sapply(ab_list, `[`, i, USE.NAMES = FALSE), na.rm = TRUE)
    })
  })
  
  # Save the summarised data
  save(mean_abundances, file = "../results/combined_results.rda")
}

# Function to plot the mean species abundance octave results
plot_neutral_cluster_results <- function() {
  # Load the summarised data
  load("../results/combined_results.rda")
  
  # Set up the plot area for a 2x2 multi-panel graph
  png(filename = "../results/plot_neutral_cluster_results.png", width = 800, height = 800)
  par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
  
  # Plot a bar graph for each community size
  for (size in c("500", "1000", "2500", "5000")) {
    mean_abundance <- mean_abundances[[size]]
    barplot(mean_abundance, main = paste("Community Size", size),
            xlab = "Octave Class", ylab = "Mean Species Count", col = "lightblue")
  }
  
  # Turn off the device
  dev.off()
}


# Question 21
state_initialise_adult <- function(num_stages, initial_size) {
  # Create a vector of length num_stages, with all elements being 0 except for the last one
  state <- rep(0, num_stages)
  # Set the number of individuals in the last life stage (adult stage) to initial_size
  state[num_stages] <- initial_size
  return(state)
}



# Question 22
state_initialise_spread <- function(num_stages, initial_size) {
  # Calculate the base allocation count for each stage
  base_count <- floor(initial_size / num_stages)
  # Create an initial vector with all elements set to base_count
  state <- rep(base_count, num_stages)
  # Calculate the remaining unallocated individuals
  remainder <- initial_size %% num_stages
  # Distribute the remaining individuals to the youngest life stage
  state[1:remainder] <- state[1:remainder] + 1
  return(state)
}



# Question 23
deterministic_step <- function(state, projection_matrix) {
  # Calculate the new state using matrix multiplication
  new_state <- projection_matrix %*% state
  return(new_state)
}



# Question 24
deterministic_simulation <- function(initial_state, projection_matrix, simulation_length) {
  population_size <- numeric(simulation_length + 1)
  population_size[1] <- sum(initial_state)

  current_state <- initial_state
  for (step in 1:simulation_length) {
    current_state <- projection_matrix %*% current_state
    population_size[step + 1] <- sum(current_state)
  }

  return(population_size)
}


# Question 25
question_25 <- function() {
  # Define the growth matrix and reproduction matrix
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

  projection_matrix <- reproduction_matrix + growth_matrix

  # Initialize state vectors
  initial_state_adults <- c(0, 0, 0, 100)
  initial_state_spread <- c(25, 25, 25, 25)

  # Perform deterministic simulations
  simulation_length <- 24
  population_size_adults <- deterministic_simulation(initial_state_adults, projection_matrix, simulation_length)
  population_size_spread <- deterministic_simulation(initial_state_spread, projection_matrix, simulation_length)

  # Create the plot
  png(filename="../results/question_25.png", width = 600, height = 400)
  plot(0:simulation_length, population_size_adults, type = 'l', col = 'blue', ylim = c(0, max(population_size_adults, population_size_spread)), xlab = "Time Step", ylab = "Population Size", main = "Population Size Time Series")
  lines(0:simulation_length, population_size_spread, type = 'l', col = 'red')
  legend("topright", legend = c("Adults", "Spread"), col = c("blue", "red"), lty = 1)
  Sys.sleep(0.1)
  dev.off()
  
  # Return the textual answer
  return("The initial distribution of the population across different life stages affects the initial stability and eventual growth of the population. A population starting with all adults experiences a significant initial decline due to a lack of younger stages entering the adult population, resulting in a temporary bottleneck. However, it eventually leads to rapid growth as the adults produce more offspring. In contrast, a population evenly spread across life stages shows greater initial stability, but its growth is slower due to the time required for individuals to reach adulthood.")
}



# Question 26
multinomial <- function(pool, probs) {
  # Ensure the sum of probabilities is less than 1
  if (sum(probs) < 1) {
    probs <- c(probs, 1 - sum(probs))
  }
  # Use the rmultinom function to draw from a multinomial distribution
  result <- rmultinom(n = 1, size = pool, prob = probs)
  return(result)
}



# Question 27
survival_maturation <- function(state, growth_matrix) {
  # Initialize a new population state vector
  new_state <- rep(0, length(state))

  # Apply the growth matrix to each life stage
  for (i in 1:length(state)) {
    # Get the current number of individuals in this stage
    current_individuals <- state[i]

    # Generate transitional individuals based on the growth matrix and multinomial distribution
    transitions <- multinomial(current_individuals, growth_matrix[, i])

    # Update the new state vector
    new_state[i] <- new_state[i] + transitions[i]
    if (i < length(state)) {
      new_state[i + 1] <- new_state[i + 1] + transitions[i + 1]
    }
  }

  return(new_state)
}



# Question 28
random_draw <- function(probability_distribution) {
  # Create a sequence of values based on the probability distribution
  values <- seq_along(probability_distribution)

  # Use the sample function to draw a value based on the probability distribution
  return(sample(values, size = 1, prob = probability_distribution))
}



# Question 29
stochastic_recruitment <- function(reproduction_matrix, clutch_distribution) {
  # Calculate the average clutch size
  expected_clutch_size <- sum(seq_along(clutch_distribution) * clutch_distribution)

  # Get the correct element from the reproduction matrix
  recruitment_rate <- reproduction_matrix[1, ncol(reproduction_matrix)]

  # Calculate the recruitment probability
  recruitment_probability <- recruitment_rate / expected_clutch_size

  # Ensure that the probability does not exceed 1
  if (recruitment_probability > 1) {
    stop("Inconsistency in model parameters: recruitment probability exceeds 1")
  }

  return(recruitment_probability)
}



# Question 30
offspring_calc <- function(state, clutch_distribution, recruitment_probability) {
  # Determine the number of adult individuals
  num_adults <- state[length(state)]

  # Generate the number of clutches, which is the number of recruit adult individuals (egg-laying events)
  num_clutches <- rbinom(1, num_adults, recruitment_probability)

  # For each clutch, use the random_draw function to draw clutch sizes from clutch_distribution
  total_offspring <- sum(sapply(1:num_clutches, function(x) random_draw(clutch_distribution)))

  # Ensure that if the number of clutches is 0, the total offspring count is also 0
  return(total_offspring)
}



# Question 31
stochastic_step <- function(state, growth_matrix, reproduction_matrix, clutch_distribution, recruitment_probability) {
  # Apply the survival and maturation processes
  new_state <- survival_maturation(state, growth_matrix)

  # Calculate the number of offspring produced by the current state
  offspring_number <- offspring_calc(state, clutch_distribution, recruitment_probability)

  # Add the offspring to the first life stage of the new state
  new_state[1] <- new_state[1] + offspring_number

  return(new_state)
}



# Question 32
stochastic_simulation <- function(initial_state, growth_matrix, reproduction_matrix, clutch_distribution, simulation_length) {
  recruitment_probability <- stochastic_recruitment(reproduction_matrix, clutch_distribution)

  population_size <- numeric(simulation_length + 1)
  population_size[1] <- sum(initial_state)
  
  current_state <- initial_state
  for (step in 1:simulation_length) {
    if (sum(current_state) == 0) {
      population_size[(step+1):(simulation_length+1)] <- 0
      break
    }
    
    current_state <- stochastic_step(current_state, growth_matrix, reproduction_matrix, clutch_distribution, recruitment_probability)
    population_size[step + 1] <- sum(current_state)
  }

  return(population_size)
}


# Question 33
question_33 <- function(){
  # Define parameters
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
  clutch_distribution <- c(0.06,0.08,0.13,0.15,0.16,0.18,0.15,0.06,0.03)

  initial_state_adults <- c(0, 0, 0, 100)
  initial_state_spread <- c(25, 25, 25, 25)
  simulation_length <- 24

  # Run stochastic simulation
  population_size_adults <- stochastic_simulation(initial_state_adults, growth_matrix, reproduction_matrix, clutch_distribution, simulation_length)
  population_size_spread <- stochastic_simulation(initial_state_spread, growth_matrix, reproduction_matrix, clutch_distribution, simulation_length)

  # Plot the graphs
  png(filename="../results/question_33.png", width = 600, height = 400)
  plot(0:simulation_length, population_size_adults, type = 'l', col = 'blue', ylim = c(0, max(population_size_adults, population_size_spread)), xlab = "Time Step", ylab = "Population Size", main = "Population Size Time Series (Stochastic Model)")
  lines(0:simulation_length, population_size_spread, type = 'l', col = 'red')
  legend("topright", legend = c("Adults", "Spread"), col = c("blue", "red"), lty = 1)
  Sys.sleep(0.1)
  dev.off()
  
  # Return text answer
  return("In stochastic simulations, the population size time series exhibits more fluctuations compared to the deterministic simulations, which show smoother trends. This is because stochastic simulations incorporate random variations in demographic processes such as survival, reproduction, and population growth. These variations can lead to different outcomes at each time step. Deterministic simulations, on the other hand, assume constant parameters without random variability, resulting in a smoother projection of population size over time.")

}


# Questions 34 and 35 involve writing code elsewhere to run your simulations on the cluster

# Question 36
question_36 <- function() {
  # Initialize an array to track extinctions
  extinctions <- numeric(4)

  # Get paths to all .rda files
  file_paths <- list.files(path = "../Q35", pattern = "\\.rda$", full.names = TRUE)

  # Iterate through each file
  for (file_path in file_paths) {
    load(file_path)
    # Assuming each .rda file contains a list named 'results'
    for (simulation in results) {
      iter <- as.numeric(sub(".*_(\\d+)\\.rda", "\\1", basename(file_path)))
      condition_index <- ifelse(iter <= 25, 1, ifelse(iter <= 50, 2, ifelse(iter <= 75, 3, 4)))

      # Check if the final population size of each simulation is 0
      if (length(simulation) > 0 && tail(simulation, 1) == 0) {
        extinctions[condition_index] <- extinctions[condition_index] + 1
      }
    }
  }

  # Calculate the proportion of extinctions for each initial condition
  extinction_proportions <- extinctions / length(file_paths)

  # Create a bar plot
  png(filename = "../results/question_36.png", width = 600, height = 400)
  barplot(extinction_proportions, names.arg = c("Adults, Large", "Adults, Small", "Spread, Large", "Spread, Small"), 
          xlab = "Initial Condition", ylab = "Proportion of Extinctions")
  dev.off()

  # Determine the population most likely to go extinct
  most_likely_extinct <- which.max(extinction_proportions)
  labels <- c("Adults, Large", "Adults, Small", "Spread, Large", "Spread, Small")

  return("I was able to see from the images I generated that both populations are less susceptible to extinction. It could be a problem with the initial condition of the function, or the simulation length could be too short.")
}


# Question 37
question_37 <- function() {
  # Previously defined growth matrix and reproduction matrix
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

  # Define folder path and simulation length
  simulation_length <- 120
  file_folder <- "../Q35"

  # Read files corresponding to initial conditions 3 and 4
  file_paths_small <- list.files(path = file_folder, pattern = "simulation_result_5[1-9]|simulation_result_6[0-9]|simulation_result_7[0-5]\\.rda$", full.names = TRUE)
  file_paths_large <- list.files(path = file_folder, pattern = "simulation_result_7[6-9]|simulation_result_8[0-9]|simulation_result_9[0-9]|simulation_result_100\\.rda$", full.names = TRUE)

  # Calculate average population trends for the stochastic model
  mean_population_trend_small <- calculate_mean_trend(file_paths_small, simulation_length)
  mean_population_trend_large <- calculate_mean_trend(file_paths_large, simulation_length)

  # Calculate population trends for the deterministic model
  deterministic_trend_small <- stochastic_simulation(state_initialise_spread(4, 10), growth_matrix, reproduction_matrix, c(0.06,0.08,0.13,0.15,0.16,0.18,0.15,0.06,0.03), simulation_length)
  deterministic_trend_large <- stochastic_simulation(state_initialise_spread(4, 100), growth_matrix, reproduction_matrix,c(0.06,0.08,0.13,0.15,0.16,0.18,0.15,0.06,0.03), simulation_length)
  
  # Calculate deviations
  deviation_small <- mean_population_trend_small / deterministic_trend_small[-1]
  deviation_large <- mean_population_trend_large / deterministic_trend_large[-1]

  # Create plots
  png(filename="../results/question_37.png", width = 600, height = 400)
  plot(1:simulation_length, deviation_small, type = 'l', col = 'blue', ylim = range(c(deviation_small, deviation_large)), xlab = "Time Step", ylab = "Deviation", main = "Deviation of Stochastic Model from Deterministic Model")
  lines(1:simulation_length, deviation_large, col = 'red')
  abline(h=1, lty=2, col = 'black')
  legend("topright", legend = c("Small Mixed Initial Population", "Large Mixed Initial Population"), col = c("blue", "red"), lty = 1, cex = 0.8)
  dev.off()
}

# Function to calculate mean population trend from simulation files
calculate_mean_trend <- function(file_paths, simulation_length) {
  sum_population <- numeric(simulation_length)
  for (file_path in file_paths) {
    load(file_path)
    sum_population <- sum_population + rowSums(sapply(results, function(x) x[1:simulation_length]))
  }
  mean_population_trend <- sum_population / length(file_paths)
  return(mean_population_trend)
}







# Challenge questions - these are optional, substantially harder, and a maximum
# of 14% is available for doing them. 

# Challenge question A
Challenge_A <- function() {
  
  
  
  png(filename="Challenge_A_min", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
  png(filename="Challenge_A_max", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()

}

# Challenge question B
Challenge_B <- function() {
  
  
  
  png(filename="Challenge_B", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()

}

# Challenge question C
Challenge_C <- function() {
  
  
  
  png(filename="Challenge_C", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()

}

# Challenge question D
Challenge_D <- function() {
  
  
  
  png(filename="Challenge_D", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
  return("type your written answer here")
}

# Challenge question E
Challenge_E <- function(){
  
  
  
  png(filename="Challenge_E", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
  return("type your written answer here")
}

# Challenge question F
Challenge_F <- function(){
  
  
  
  png(filename="Challenge_F", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
}
