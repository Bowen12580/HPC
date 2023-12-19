# 清理工作区和关闭图形界面
rm(list=ls())
graphics.off()

# 加载函数
source("/rds/general/user/bd623/home/run_files/bd623_HPC_2023_main.R")  # 加载所有需要的函数

# 读取作业编号
iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX"))

# 设置随机数种子
set.seed(iter)

# 选择初始条件
if (iter <= 25) {
  initial_state <- state_initialise_adult(4, 100)
} else if (iter <= 50) {
  initial_state <- state_initialise_adult(4, 10)
} else if (iter <= 75) {
  initial_state <- state_initialise_spread(4, 100)
} else {
  initial_state <- state_initialise_spread(4, 10)
}

# 创建结果文件名
result_filename <- paste0("simulation_result_", iter, ".rda")

# 初始化结果列表
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


# 执行模拟并保存结果
for (i in 1:150) {
  results[[i]] <- stochastic_simulation(initial_state, growth_matrix, reproduction_matrix, clutch_distribution, 120)
}

# 保存模拟结果
save(results, file=result_filename)
