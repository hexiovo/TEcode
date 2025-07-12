# 安装并加载必要的包（如果还未安装）
lapply(c("readxl", "xgboost", "caret", "ggplot2", "Ckmeans.1d.dp", "reshape2","ggsci","scales","pROC"), library, character.only = TRUE)

iteration_times <- 1000

# 读取数据
xlsx_path <- "F:/桌面/文件/实验/教育期望/数据处理/可用数据-2.xlsx"
all_data <- read_excel(xlsx_path, sheet = 1)


# 初始化准确率列表、特征得分列表和分类比例矩阵
accuracy_list <- numeric(iteration_times)
classification_matrix <- matrix(0, nrow=3, ncol=3)  # 初始化分类矩阵，3x3矩阵，适用于三类问题

# 循环100次
for (iteration in 1:iteration_times) {
  cat("\014")
  print(paste("Iteration:", iteration))
  
  random_seed <- sample(1:10000, 1)
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
                         nrounds = 100, 
                         max_depth = 5, 
                         eta = 0.1, 
                         verbose = 0)
    
    # 进行预测
    pred_xgb <- predict(model_xgb, newdata = test_matrix)
    
    # 计算分类准确率
    correct_predictions <- correct_predictions + sum(pred_xgb == as.numeric(testData[[target_column]]) - 1)
    
    # 更新分类矩阵
    for (j in 1:length(pred_xgb)) {
      true_label <- as.numeric(testData[[target_column]][j]) - 1
      predicted_label <- pred_xgb[j]
      classification_matrix[true_label + 1, predicted_label + 1] <- classification_matrix[true_label + 1, predicted_label + 1] + 1
    }
  }
  
  # 计算并存储准确率
  accuracy_list[iteration] <- correct_predictions / nrow(selected_data)
}

# 计算准确率的平均值
average_accuracy_xgb <- mean(accuracy_list)

# 计算分类比例矩阵
classification_proportion <- classification_matrix / sum(classification_matrix) * 3

#自定义颜色
color_custom = pal_d3("category20c")(20)
custom_color_blue <- color_custom[11]
custom_color_red <- color_custom[12]
custom_color_green <- color_custom[13]

# 定义分类标签映射
label_map <- c("1" = "Constructive Learning", "2" = "Passive Learning", "3" = "Active Learning")

# 修改分类比例矩阵为长格式
classification_df <- melt(classification_proportion)
colnames(classification_df) <- c("True", "Predicted", "Proportion")

# 将 True 和 Predicted 列的 1/2/3 转换为分类标签
classification_df$True <- factor(classification_df$True, levels = c(1, 2, 3), labels = c("Constructive Learning", "Passive Learning", "Active Learning"))
classification_df$Predicted <- factor(classification_df$Predicted, levels = c(1, 2, 3), labels = c("Constructive Learning", "Passive Learning", "Active Learning"))

# 将 True 列的因子水平手动反转
classification_df$True <- factor(classification_df$True, levels = rev(levels(classification_df$True)))

# 绘制热力图
ggplot(classification_df, aes(x = Predicted, y = True, fill = Proportion)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = custom_color_red) +
  #scale_y_reverse() +  # 反转 y 轴
  labs(title = "Classification Proportion Heatmap",
       x = "Predicted Label",
       y = "True Label") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),  # 移除主要网格线
        panel.grid.minor = element_blank(),  # 移除次要网格线
        plot.title = element_text(hjust = 0.5, size = 17),  # 标题居中
        axis.title.x = element_text(size = 16),  # 设置 x 轴标题的字号
        axis.title.y = element_text(size = 16),  # 设置 y 轴标题的字号
        axis.text.x = element_text(size = 14),  # 设置 x 轴刻度标签的字号
        axis.text.y = element_text(size = 14),
        legend.key.size = unit(1.5, "cm"),  # 设置图例条形高度
        legend.position = c(1.1, 0.520),  # 将图例位置设为右侧并调整上下位置
        legend.text = element_text(size = 16), #设置图例字号
        legend.title = element_text(size = 16)  #设置图例标题
        )+
  coord_fixed(ratio = 1)  # 设置纵横比，压缩纵向比例




cat("\014")

# 输出一百次五折交叉验证XGBoost分类准确率的平均值
cat("一百次五折交叉验证XGBoost分类准确率的平均值:", average_accuracy_xgb, "\n")

# 输出分类比例矩阵
cat("分类比例矩阵:\n")
print(classification_proportion)