# CMEE 2022 HPC exercises R code pro forma
# For neutral model cluster run

rm(list=ls())  # 清空工作区，好的实践
source("/rds/general/user/bd623/home/run_files/bd623_HPC_2023_main.R")  # 加载所有需要的函数

# 读取集群作业号（用于集群）
iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX"))

# 本地测试时使用的作业号
#iter <- 2

# 控制随机数种子，确保每次模拟都是唯一的
set.seed(iter)

# 根据作业号选择社区大小
if (iter <= 25) {
  size <- 500
} else if (iter <= 50) {
  size <- 1000
} else if (iter <= 75) {
  size <- 2500
} else {
  size <- 5000
}

# 设置物种形成率
speciation_rate <- 0.002604  

# 创建存储结果的文件名，确保每个模拟的文件名是唯一的
result_filename <- paste0("output_", iter, ".rda")

# 调用模拟函数，wall_time设置为11.5小时
neutral_cluster_run(speciation_rate, size, wall_time = 690, interval_rich = 1, interval_oct = size / 10, burn_in_generations = 8 * size, output_file_name = result_filename)

