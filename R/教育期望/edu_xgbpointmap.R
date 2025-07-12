# 安装并加载必要的包（如果还未安装）
lapply(c("readxl", "xgboost", "caret", "ggplot2", "Ckmeans.1d.dp","ggsci","scales"), library, character.only = TRUE)

#迭代次数
iteration_times <- 1000 
# 读取数据
xlsx_path <- "F:/桌面/文件/实验/教育期望/数据处理/可用数据-2.xlsx"
all_data <- read_excel(xlsx_path, sheet = 1)



# 初始化准确率列表和特征得分列表
accuracy_list <- numeric(iteration_times)

# 循环100次
for (iteration in 1:iteration_times) {
  print(paste("Iteration:", iteration))
  
  random_seed <- sample(1:90000, 1)
  set.seed(random_seed)
  all_data <- all_data[sample(nrow(all_data)), ]
  
  # 选择特征和目标变量
  feature_columns <- c(10:24, 28)
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
  
  # 初始化准确率
  correct_predictions <- 0
  total_folds <- 10
  
  # 创建折叠索引
  folds <- createFolds(selected_data[[target_column]], k=total_folds, list=TRUE, returnTrain=FALSE)
  
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
                         nrounds = 110, 
                         max_depth = 11, 
                         eta = 0.11, 
                         verbose = 0)
    
    # 进行预测
    pred_xgb <- predict(model_xgb, newdata = test_matrix)
    
    # 计算分类准确率
    correct_predictions <- correct_predictions + sum(pred_xgb == as.numeric(testData[[target_column]]) - 1)
  }
  
  # 计算并存储准确率
  accuracy_list[iteration] <- correct_predictions / nrow(selected_data)
  
}


# 计算准确率的平均值
average_accuracy_xgb <- mean(accuracy_list)

cat("\014")

# 输出一百次十折交叉验证XGBoost分类准确率的平均值
cat("一百次十折交叉验证XGBoost分类准确率的平均值:", average_accuracy_xgb, "\n")


# 自定义颜色
color_custom = pal_d3("category20c")(20)
custom_color_blue <- color_custom[11]
custom_color_red <- color_custom[12]
custom_color_green <- color_custom[13]

# 创建数据框
accuracy_df <- data.frame(Iteration = 1:1000, Accuracy = accuracy_list)

ggplot(accuracy_df, aes(x = Iteration, y = Accuracy)) +
  geom_point(color = custom_color_red, size = 1) +  # 调整点的大小
  geom_hline(yintercept = 0.33, linetype = "dashed", color = "black") +  # 添加阈值直线
  geom_hline(yintercept = average_accuracy_xgb, linetype = "dashed", color ="black") +  # 添加平均准确率直线
  labs(title = "XGBoost ACC", x = "iteration", y = "Accuracy") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 16),  # 设置 x 轴标题的字号
        axis.title.y = element_text(size = 16),  # 设置 y 轴标题的字号
        axis.text.x = element_text(size = 13),  # 设置 x 轴刻度标签的字号
        axis.text.y = element_text(size = 13)  # 设置 y 轴刻度标签的字号
        ) +  # 将标题居中
  annotate("text", x = 0, y = 0.34, label = "Threshold = 0.33", 
           color = "black", vjust = 0, hjust = 0, cex= 5) +  # 使用 annotate 添加阈值标签
  annotate("text", x =0, y = average_accuracy_xgb + 0.01, 
           label = paste("Average =", round(average_accuracy_xgb, 3)), 
           color = "black", vjust = 0, hjust = 0, cex = 5)  # 使用 annotate 添加平均准确率标签

