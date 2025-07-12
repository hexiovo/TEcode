# 安装并加载必要的包（如果还未安装）
lapply(c("readxl", "xgboost", "caret", "ggplot2", "Ckmeans.1d.dp","ggsci","scales"), library, character.only = TRUE)

# 迭代次数
iteration_times <- 1000
# 读取数据
xlsx_path <- "F:/桌面/文件/实验/教育期望/数据处理/可用数据-2.xlsx"
all_data <- read_excel(xlsx_path, sheet = 1)

# 初始化准确率列表和特征得分列表
accuracy_list <- numeric(iteration_times)
feature_scores <- rep(0, length(c(10:24, 28,43:45, 49)))  # 与特征数量相同的向量
# 定义你要修改的列的索引
columns_to_rename <- c(10:24, 28, 43:45, 49)

# 定义新的列名标签
new_labels <- c("ISC", "P_word", "P_picture", 
                "C_AttentionPoint", "T_AP-duration", "MT_AP-duration", 
                "Trans_Total", "Trans_between-Cata", "M_Pupil-Fixation", 
                "Trans_Between-Relate", "PTrans_Total", "PTrans_Between-Cata", 
                "M_Duration", "M_Scan-Range", "Stress", "Interest", 
                "Memory Score", "Understand Score", "Transfer Score", 
                "Total Score")

# 确保列数和新标签数匹配
if (length(columns_to_rename) != length(new_labels)) {
  stop("列数与新标签数不匹配")
}

# 修改列名
colnames(all_data)[columns_to_rename] <- new_labels

# 筛选只包含第1组和第3组的数据
selected_groups <- c(1, 3)  # 目标组
all_data <- all_data[all_data$Group %in% selected_groups, ]

# 循环100次
for (iteration in 1:iteration_times) {
  print(paste("Iteration:", iteration))
  
  random_seed <- sample(1:90000, 1)
  set.seed(random_seed)
  all_data <- all_data[sample(nrow(all_data)), ]
  
  # 选择特征和目标变量
  feature_columns <- c(10:24, 28, 43:45, 49)
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
  
  # 特征重要性
  importance_matrix <- xgb.importance(feature_names = colnames(trainData[, -which(names(trainData) == target_column)]), model = model_xgb)
  
  feature_scores <- feature_scores + importance_matrix$Gain +  importance_matrix$Frequency  + importance_matrix$Cover
}

feature_names <- colnames(trainData[, -which(names(trainData) == target_column)])
names(feature_scores) <- feature_names
feature_ranking <- sort(feature_scores[feature_scores > 0], decreasing = TRUE)

# 仅当 feature_ranking 非空时才创建数据框并绘图
if (length(feature_ranking) > 0) {
  feature_ranking_df <- data.frame(Feature = names(feature_ranking), Score = feature_ranking)
  
  ggplot(feature_ranking_df, aes(x = reorder(Feature, Score), y = Score)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(title = "Feature Importance Ranking", x = "Feature", y = "Score") +
    theme_minimal()
} else {
  cat("没有重要性得分大于零的特征。\n")
}

# 计算准确率的平均值
average_accuracy_xgb <- mean(accuracy_list)

# 规范化特征重要性得分
feature_ranking_df$Score <- rescale(feature_ranking_df$Score, to = c(10, 15))  # 将得分缩放至10-15范围

# 使用颜色渐变调色板
palette_colors <- scales::seq_gradient_pal("#D8BFD8", "purple", "Lab")(seq(0, 1, length.out = nrow(feature_ranking_df)))

ggplot(feature_ranking_df, aes(x = reorder(Feature, -Score), y = Score, fill = Score)) +
  geom_bar(stat = "identity", color = "white", size = 0.6) +  # 为条形图加上白色边框
  geom_text(aes(label = round(Score, 1)),  # 在条形上显示得分，保留1位小数
            position = position_stack(vjust = 0.7),  # 标签在条形中央对齐
            size = 3, color = "white") +  # 调整字体大小和颜色
  coord_polar(theta = "x") +  # 使用极坐标将条形图转换为径向图
  scale_fill_gradientn(colours = palette_colors) +  # 应用颜色渐变
  theme_minimal() +  # 使用简洁的背景
  labs(title = "Feature Importance", x = "基因", y = "重要性得分") +
  theme(
    axis.text.y = element_blank(),  # 隐藏y轴标签
    axis.ticks.y = element_blank(),  # 隐藏y轴刻度
    panel.grid = element_blank(),  # 隐藏网格线
    axis.title.y = element_blank(),  # 隐藏y轴标题
    axis.title.x = element_blank(),  # 隐藏x轴标题
    axis.text.x = element_text(size = 7, hjust = 1, vjust = 0.5),  # 调整x轴标签的大小、角度和位置
    plot.title = element_text(hjust = 0.5)  # 将标题居中
  )
cat("\014")

# 打印特征得分
cat("特征重要性排名（前5特征的得分）：\n")
print(feature_ranking)
print(average_accuracy_xgb)
