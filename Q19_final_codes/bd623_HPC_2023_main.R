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
  #test <- species_richness(c(1,4,4,5,1,6,1))
  # test
  # >>> 4
  return(length(unique(community)))
}

# Question 2
init_community_max <- function(size){
  #seq生成的本身就是一个向量，所以我这个函数编写后输出的community虽然看起来像整数，但其实它是一个向量
  community<- seq(1,size)
  return(community)
  # test1 <- init_community_max(10)
  # test1
  # >>> 1  2  3  4  5  6  7  8  9 10
}

# Question 3
init_community_min <- function(size){
  #这里使用了rep生成一个所有元素均为1的向量，代表所有的个体都属于物种1。即仅仅有一个物种类别，其数量为size
  community <- rep(1,size)
  return(community)
  # test2 <- init_community_min(10)
  # test2
  # >>> 1 1 1 1 1 1 1 1 1 1
}

# test the code 
# test3 <- species_richness(init_community_max(10))
# tes3
# >>> 10

# test4 <- species_richness(init_community_min(10))
# test4
# >>> 1

# Question 4
choose_two <- function(max_value){
  wanted_integers <- sample(1:max_value , size =2)
  return(wanted_integers)
  # test5 <- choose_two(5)
  # one possible output >>> 3 5
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
########################################第六题有差异
# Question 6
neutral_generation <- function(community){
    # 确定要执行的 neutral_step 步骤数量
  num_steps <- length(community) / 2
  # 如果个体数不是偶数，则随机决定是向上还是向下取整
  if (length(community) %% 2 != 0) {
    num_steps <- ifelse(runif(1) < 0.5, floor(num_steps), ceiling(num_steps))
  }

  # 执行指定数量的 neutral_step 步骤
  for (i in 1:num_steps) {
    community <- neutral_step(community)
  }

  return(community)
  
}

# Question 7
neutral_time_series <- function(community,duration)  {
    # 初始化包含初始物种丰富度的向量
  species_richness_series <- numeric(duration + 1)
  species_richness_series[1] <- species_richness(community)

  # 对每一个世代进行模拟
  for (i in 1:duration) {
    community <- neutral_generation(community)
    species_richness_series[i + 1] <- species_richness(community)
  }

  return(species_richness_series)
  
}

# Question 8
question_8 <- function() {
  
    # 初始化初始条件和运行时间
  initial_community <- init_community_max(100)
  duration <- 200

  # 获取时间序列
  time_series <- neutral_time_series(initial_community, duration)
  
  png("../results/question_8", width = 600, height = 400)
  # plot your graph here
  plot(time_series, type = "l", xlab = "Generation", ylab = "Species Richness", main = "Neutral Model Time Series")
  Sys.sleep(0.1)
  dev.off()
  
  return("type your written answer here")
}
#print(question_8())

# Question 9
neutral_step_speciation <- function(community,speciation_rate)  {
    # 随机选择死亡和繁殖个体
  chosen_two_species <- choose_two(length(community))
  dying_index <- chosen_two_species[1]
  reproduction_index <- chosen_two_species[2]

  # 决定是否发生物种形成
  if (runif(1) < speciation_rate) {
    # 物种形成: 引入一个新的物种编号
    new_species <- max(community) + 1
    community[dying_index] <- new_species
  } else {
    # 无物种形成: 与之前一样处理
    community[dying_index] <- community[reproduction_index]
  }

  return(community)
  
}

# Question 10
neutral_generation_speciation <- function(community,speciation_rate)  {
    # 确定要执行的 neutral_step_speciation 步骤数量
  num_steps <- length(community) / 2
  # 如果个体数不是偶数，则随机决定是向上还是向下取整
  if (length(community) %% 2 != 0) {
    num_steps <- ifelse(runif(1) < 0.5, floor(num_steps), ceiling(num_steps))
  }

  # 执行指定数量的 neutral_step_speciation 步骤
  for (i in 1:num_steps) {
    community <- neutral_step_speciation(community, speciation_rate)
  }

  return(community)
  
}

# Question 11
neutral_time_series_speciation <- function(community,speciation_rate,duration)  {
    # 初始化包含初始物种丰富度的向量
  species_richness_series <- numeric(duration + 1)
  species_richness_series[1] <- species_richness(community)

  # 对每一个世代进行模拟
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

    png(filename="../results/question_12.png", width = 600, height = 400)
    plot(1:(duration + 1), time_series_max, type = "l", col = "blue", xlab = "Generation", ylab = "Species Richness", main = "Species Richness over Time")
    lines(1:(duration + 1), time_series_min, col = "red")
    dev.off()

    return("Explain what you found from this plot about the effect of initial conditions. Why does the neutral model simulation give you those particular results?")
}

#print(question_12())

# Question 13
species_abundance <- function(community)  {
    # 使用 table 函数统计每个物种的个体数
  abundances <- table(community)
  # 使用 sort 函数对个体数进行降序排序
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
    # 确保两个向量长度相同
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
 # 初始化社群
    community_max <- init_community_max(community_size)
    community_min <- init_community_min(community_size)

    # 烧录期
    for (i in 1:burn_in_duration) {
        community_max <- neutral_generation_speciation(community_max, speciation_rate)
        community_min <- neutral_generation_speciation(community_min, speciation_rate)
    }

    # 收集八度级数据
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

    # 计算平均值
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

    # 生成并保存最小初始条件的条形图
    png(filename="../results/question_16_min.png", width = 600, height = 400)
    barplot(mean_octaves_min, main="Species Abundance Distribution (Min Init)", xlab="Abundance", ylab="Mean Species Count")
    Sys.sleep(0.1)
    dev.off()
  
    # 生成并保存最大初始条件的条形图
    png(filename="../results/question_16_max.png", width = 600, height = 400)
    barplot(mean_octaves_max, main="Species Abundance Distribution (Max Init)", xlab="Abundance", ylab="Mean Species Count")
    Sys.sleep(0.1)
    dev.off()
  
    return("Initial condition influences the early stages of the simulation, but over time, due to the stochastic nature of speciation and extinction, the system converges to a similar distribution regardless of the initial state.")
}


#print(question_16())

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


# 测试 neutral_cluster_run 函数
# 参数设置
#speciation_rate_test <- 0.1  # 物种形成率
#size_test <- 100  # 社区大小
#wall_time_test <- 10  # 总运行时间，以分钟为单位
#interval_rich_test <- 1  # 记录物种丰富度的间隔（代）
#interval_oct_test <- 10  # 记录物种丰度八度的间隔（代）
#burn_in_generations_test <- 200  # 燃烧期的代数
#output_file_name_test <- "my_test_file_1.rda"  # 输出文件名

# 运行模拟函数
#neutral_cluster_run(speciation_rate_test, size_test, wall_time_test, interval_rich_test, interval_oct_test, burn_in_generations_test, output_file_name_test)





# Questions 18 and 19 involve writing code elsewhere to run your simulations on
# the cluster

# Question 20 
process_neutral_cluster_results <- function() {
  
  
  combined_results <- list() #create your list output here to return
  # save results to an .rda file
  
}

plot_neutral_cluster_results <- function(){

    # load combined_results from your rda file
  
  
  
    png(filename="plot_neutral_cluster_results", width = 600, height = 400)
    # plot your graph here
    Sys.sleep(0.1)
    dev.off()
    
    return(combined_results)
}


# Question 21
state_initialise_adult <- function(num_stages, initial_size) {
  # 创建一个长度为num_stages的向量，其中除了最后一个元素外，其他都是0
  state <- rep(0, num_stages)
  # 在最后一个生命阶段（成年阶段）设置个体数量为initial_size
  state[num_stages] <- initial_size
  return(state)
}


# Question 22
state_initialise_spread <- function(num_stages, initial_size) {
  # 计算每个阶段的基础分配个数
  base_count <- floor(initial_size / num_stages)
  # 创建一个初始全为base_count的向量
  state <- rep(base_count, num_stages)
  # 计算剩余未分配的个体数
  remainder <- initial_size %% num_stages
  # 将剩余的个体分配给最年轻的生命阶段
  state[1:remainder] <- state[1:remainder] + 1
  return(state)
}


# Question 23
deterministic_step <- function(state, projection_matrix) {
  # 使用矩阵乘法计算新状态
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
  # 定义生长矩阵和繁殖矩阵
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

  # 初始化状态向量
  initial_state_adults <- c(0, 0, 0, 100)
  initial_state_spread <- c(25, 25, 25, 25)

  # 进行确定性模拟
  simulation_length <- 24
  population_size_adults <- deterministic_simulation(initial_state_adults, projection_matrix, simulation_length)
  population_size_spread <- deterministic_simulation(initial_state_spread, projection_matrix, simulation_length)

  # 创建图像
  png(filename="../results/question_25.png", width = 600, height = 400)
  plot(0:simulation_length, population_size_adults, type = 'l', col = 'blue', ylim = c(0, max(population_size_adults, population_size_spread)), xlab = "Time Step", ylab = "Population Size", main = "Population Size Time Series")
  lines(0:simulation_length, population_size_spread, type = 'l', col = 'red')
  legend("topright", legend = c("Adults", "Spread"), col = c("blue", "red"), lty = 1)
  Sys.sleep(0.1)
  dev.off()
  
  # 返回文本答案
  return("type your written answer here")
}
#print(question_25())

# Question 26
multinomial <- function(pool, probs) {
  # 确保概率之和为小于1
  if (sum(probs) < 1) {
    probs <- c(probs, 1 - sum(probs))
  }
  # 使用rmultinom函数从多项式分布中抽取
  result <- rmultinom(n = 1, size = pool, prob = probs)
  return(result)
}



# Question 27
survival_maturation <- function(state, growth_matrix) {
  # 初始化新的人口状态向量
  new_state <- rep(0, length(state))

  # 对每个生命阶段应用生长矩阵
  for (i in 1:length(state)) {
    # 获取当前阶段的个体数量
    current_individuals <- state[i]

    # 根据生长矩阵和多项式分布函数生成过渡个体数
    transitions <- multinomial(current_individuals, growth_matrix[ ,i])

    # 更新新状态向量
    new_state[i] <- new_state[i] + transitions[i]
    if (i < length(state)) {
      new_state[i + 1] <- new_state[i + 1] + transitions[i + 1]
    }
  }

  return(new_state)
}


# Question 28
random_draw <- function(probability_distribution) {
  # 根据概率分布生成值的序列
  values <- seq_along(probability_distribution)

  # 使用sample函数根据概率分布抽取一个值
  return(sample(values, size = 1, prob = probability_distribution))
}


# Question 29
stochastic_recruitment <- function(reproduction_matrix, clutch_distribution) {
  # 计算平均窝卵大小
  expected_clutch_size <- sum(seq_along(clutch_distribution) * clutch_distribution)

  # 获取繁殖矩阵的正确元素
  recruitment_rate <- reproduction_matrix[1, ncol(reproduction_matrix)]

  # 计算招募概率
  recruitment_probability <- recruitment_rate / expected_clutch_size

  # 确保概率不超过1
  if (recruitment_probability > 1) {
    stop("Inconsistency in model parameters: recruitment probability exceeds 1")
  }

  return(recruitment_probability)
}


# Question 30
offspring_calc <- function(state, clutch_distribution, recruitment_probability) {
  # 确定成年个体的数量
  num_adults <- state[length(state)]

  # 使用二项分布生成招募成年个体的数量，即产卵的数量
  num_clutches <- rbinom(1, num_adults, recruitment_probability)

  # 对每个窝卵，使用random_draw函数从clutch_distribution中抽取窝卵大小
  total_offspring <- sum(sapply(1:num_clutches, function(x) random_draw(clutch_distribution)))

  # 确保如果窝卵数为0，则总后代数也为0
  return(total_offspring)
}


# Question 31
stochastic_step <- function(state, growth_matrix, reproduction_matrix, clutch_distribution, recruitment_probability) {
  # 应用生存和成熟过程
  new_state <- survival_maturation(state, growth_matrix)

  # 计算由当前状态产生的后代数量
  offspring_number <- offspring_calc(state, clutch_distribution, recruitment_probability)

  # 将后代添加到新状态的第一生命阶段
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
  # 定义参数
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

  # 运行随机模拟
  population_size_adults <- stochastic_simulation(initial_state_adults, growth_matrix, reproduction_matrix, clutch_distribution, simulation_length)
  population_size_spread <- stochastic_simulation(initial_state_spread, growth_matrix, reproduction_matrix, clutch_distribution, simulation_length)

  # 绘制图像
  png(filename="../results/question_33.png", width = 600, height = 400)
  plot(0:simulation_length, population_size_adults, type = 'l', col = 'blue', ylim = c(0, max(population_size_adults, population_size_spread)), xlab = "Time Step", ylab = "Population Size", main = "Population Size Time Series (Stochastic Model)")
  lines(0:simulation_length, population_size_spread, type = 'l', col = 'red')
  legend("topright", legend = c("Adults", "Spread"), col = c("blue", "red"), lty = 1)
  Sys.sleep(0.1)
  dev.off()
  
  # 返回文本答案
  return("type your written answer here")
}


# Questions 34 and 35 involve writing code elsewhere to run your simulations on the cluster

# Question 36
question_36 <- function(){
  
  png(filename="question_36", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
  return("type your written answer here")
}

# Question 37
question_37 <- function(){
  
  png(filename="question_37_small", width = 600, height = 400)
  # plot your graph for the small initial population size here
  Sys.sleep(0.1)
  dev.off()
  
  png(filename="question_37_large", width = 600, height = 400)
  # plot your graph for the large initial population size here
  Sys.sleep(0.1)
  dev.off()
  
  return("type your written answer here")
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
