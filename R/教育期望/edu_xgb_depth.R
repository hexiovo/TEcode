# 安装并加载必要的包（如果还未安装）
lapply(c("readxl", "xgboost", "caret", "ggplot2", "Ckmeans.1d.dp", "ggsci", "scales"), library, character.only = TRUE)

# 迭代次数
iteration_times <- 500
# 读取数据
xlsx_path <- "F:/桌面/文件/实验/教育期望/数据处理/可用数据.xlsx"
all_data <- read_excel(xlsx_path, sheet = 1)

# 对第37-39列和第43列进行Z标准化
columns_to_scale <- c(37:39, 43)
all_data[, columns_to_scale] <- scale(all_data[, columns_to_scale])

# 初始化准确率列表和特征得分列表
accuracy_list <- numeric(iteration_times)
average_accuracy_per_depth <- numeric(16)  # 用于存储不同深度的平均准确率

for (depth in 5:20) {
  
  # 重置准确率列表
  accuracy_list <- numeric(iteration_times)
  
  # 循环迭代次数
  for (iteration in 1:iteration_times) {
    print(paste("Iteration:", iteration))
    
    random_seed <- sample(1:90000, 1)
    set.seed(random_seed)
    all_data <- all_data[sample(nrow(all_data)), ]
    
    # 选择特征和目标变量
    feature_columns <- c(11:25, 29, 35:39, 43)
    target_column <- "Group"
    
    # 确保目标变量在数据集中
    if (!(target_column %in% names(all_data))) {
      stop(paste("目标变量", target_column, "不在数据集中"))
    }
    
    # 确保选择的特征变量和目标变量在数据集中存在
    if (any(!feature_columns %in% seq_along(names(all_data)))) {
      stop("选择的特征变量列索引无效")
    }
    
    # 选择目标变量和特征变量
    selected_data <- all_data[, c(feature_columns, which(names(all_data) == target_column))]
    
    # 处理缺失值（如果有的话）
    selected_data <- na.omit(selected_data)
    
    # 将分类变量转换为因子
    if (target_column %in% names(selected_data)) {
      selected_data[[target_column]] <- as.factor(selected_data[[target_column]])
    }
    
    # 初始化正确预测计数
    correct_predictions <- 0
    total_folds <- 5
    
    # 创建折叠索引
    folds <- createFolds(selected_data[[target_column]], k = total_folds, list = TRUE, returnTrain = FALSE)
    
    for (i in 1:total_folds) {
      # 创建训练集和测试集
      testIndex <- folds[[i]]
      trainData <- selected_data[-testIndex, ]
      testData <- selected_data[testIndex, ]
      
      # 转换数据格式为 xgboost 所需格式
      train_matrix <- xgb.DMatrix(data.matrix(trainData[, -which(names(trainData) == target_column)]), label = as.numeric(trainData[[target_column]]) - 1)
      test_matrix <- xgb.DMatrix(data.matrix(testData[, -which(names(testData) == target_column)]))
      
      # 训练模型
      model_xgb <- xgboost(data = train_matrix, 
                           objective = "multi:softmax", 
                           num_class = length(unique(trainData[[target_column]])), 
                           nrounds = 100, 
                           max_depth = depth, 
                           eta = 0.1, 
                           verbose = 0)
      
      # 进行预测
      pred_xgb <- predict(model_xgb, newdata = test_matrix)
      
      # 计算分类准确率
      correct_predictions <- correct_predictions + sum(pred_xgb == as.numeric(testData[[target_column]]) - 1)
    }
    
    # 计算并存储准确率
    accuracy_list[iteration] <- correct_predictions / nrow(selected_data)
  }
  
  # 计算并存储每次深度的平均准确率
  average_accuracy_xgb <- mean(accuracy_list)
  average_accuracy_per_depth[depth - 4] <- average_accuracy_xgb  # 5-20 对应索引 1-16
  
  # 输出平均准确率
  cat("深度", depth, "的平均准确率:", average_accuracy_xgb, "\n")
}

# 最终输出所有深度的平均准确率
average_accuracy_per_depth

# 创建数据框，depth 从 5 到 20
df <- data.frame(depth = 5:(4 + length(average_accuracy_per_depth)), 
                 accuracy = average_accuracy_per_depth)

# 绘制折线图
ggplot(df, aes(x = depth, y = accuracy)) +
  geom_line(color = "blue", size = 1) +  # 折线
  geom_point(color = "red", size = 2) +  # 数据点
  labs(title = "Average Accuracy per Depth", 
       x = "Depth (5-20)", 
       y = "Accuracy") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +  # 标题居中
  scale_x_continuous(breaks = seq(5, 20, by = 1))  # 设置横坐标刻度从 5 到 20