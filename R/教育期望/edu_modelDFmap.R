# 安装并加载必要的包（如果还未安装）
lapply(c("readxl", "xgboost", "caret", "randomForest", "e1071", "nnet", "class", 
         "rpart", "gbm", "adabag", "ggplot2", "Ckmeans.1d.dp", "ggsci", "scales"
         ,"ranger","MASS"), library, character.only = TRUE)

# 迭代次数
iteration_times <- 100
# 读取数据
xlsx_path <- "F:/桌面/文件/实验/教育期望/数据处理/可用数据-2.xlsx"
all_data <- read_excel(xlsx_path, sheet = 1)

# 对第37-39列和第43列进行Z标准化
columns_to_scale <- c(43:45, 49)
all_data[, columns_to_scale] <- scale(all_data[, columns_to_scale])

# 初始化列表存储准确率
accuracy_list_xgb <- numeric(iteration_times)
accuracy_list_rf <- numeric(iteration_times)
accuracy_list_svm <- numeric(iteration_times)
accuracy_list_lr <- numeric(iteration_times)


# 初始化其他模型的准确率列表
accuracy_list_knn <- numeric(iteration_times)
accuracy_list_dt <- numeric(iteration_times)
accuracy_list_nn <- numeric(iteration_times)
accuracy_list_gbm <- numeric(iteration_times)
accuracy_list_ada <- numeric(iteration_times)
accuracy_list_et <- numeric(iteration_times)
accuracy_list_lda <- numeric(iteration_times)

# 循环迭代100次
for (iteration in 1:iteration_times) {
  print(paste("Iteration:", iteration))
  
  random_seed <- sample(1:90000, 1)
  set.seed(random_seed)
  all_data <- all_data[sample(nrow(all_data)), ]
  
  # 选择特征和目标变量
  feature_columns <- c(10:24, 28,36:45, 49)
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
  
  # 初始化各模型的正确预测数量
  correct_predictions_xgb <- 0
  correct_predictions_rf <- 0
  correct_predictions_svm <- 0
  correct_predictions_lr <- 0
  correct_predictions_knn <- 0  # KNN
  correct_predictions_dt <- 0   # Decision Tree
  correct_predictions_nn <- 0   # Neural Network
  correct_predictions_gbm <- 0  # GBM
  correct_predictions_et <- 0   # Extra Trees
  correct_predictions_lda <- 0   # LDA
  total_folds <- 5
  
  # 创建折叠索引
  folds <- createFolds(selected_data[[target_column]], k = total_folds, list = TRUE, returnTrain = FALSE)
  
  for (i in 1:total_folds) {
    # 创建训练集和测试集
    testIndex <- folds[[i]]
    trainData <- selected_data[-testIndex, ]
    testData <- selected_data[testIndex, ]
    
    # 1. XGBoost 模型
    train_matrix <- xgb.DMatrix(data.matrix(trainData[, -which(names(trainData) == target_column)]), label = as.numeric(trainData[[target_column]]) - 1)
    test_matrix <- xgb.DMatrix(data.matrix(testData[, -which(names(testData) == target_column)]))
    
    model_xgb <- xgboost(data = train_matrix, 
                         objective = "multi:softmax", 
                         num_class = length(unique(trainData[[target_column]])), 
                         nrounds = 100, 
                         max_depth = 5, 
                         eta = 0.1, 
                         verbose = 0)
    
    pred_xgb <- predict(model_xgb, newdata = test_matrix)
    correct_predictions_xgb <- correct_predictions_xgb + sum(pred_xgb == as.numeric(testData[[target_column]]) - 1)
    
    # 2. Random Forest 模型
    model_rf <- randomForest(as.factor(Group) ~ ., data = trainData, ntree = 100)
    pred_rf <- predict(model_rf, newdata = testData)
    correct_predictions_rf <- correct_predictions_rf + sum(pred_rf == testData[[target_column]])
    
    # 3. SVM 模型
    model_svm <- svm(as.factor(Group) ~ ., data = trainData, probability = TRUE)
    pred_svm <- predict(model_svm, newdata = testData)
    correct_predictions_svm <- correct_predictions_svm + sum(pred_svm == testData[[target_column]])
    
    # 4. Logistic Regression 模型
    model_lr <- glm(as.factor(Group) ~ ., data = trainData, family = "binomial")
    pred_lr <- ifelse(predict(model_lr, newdata = testData, type = "response") > 0.5, 1, 0)
    correct_predictions_lr <- correct_predictions_lr + sum(pred_lr == as.numeric(testData[[target_column]]) - 1)
    
    # 5. K-Nearest Neighbors (KNN) 模型
    pred_knn <- knn(train = trainData[, -which(names(trainData) == target_column)], test = testData[, -which(names(testData) == target_column)], cl = trainData[[target_column]], k = 5)
    correct_predictions_knn <- correct_predictions_knn + sum(pred_knn == testData[[target_column]])
    
    # 6. 决策树 (Decision Tree) 模型
    model_dt <- rpart(as.factor(Group) ~ ., data = trainData, method = "class")
    pred_dt <- predict(model_dt, newdata = testData, type = "class")
    correct_predictions_dt <- correct_predictions_dt + sum(pred_dt == testData[[target_column]])
    
    # 7. 神经网络 (Neural Network) 模型
    model_nn <- nnet(as.factor(Group) ~ ., data = trainData, size = 10, maxit = 400, trace = FALSE)
    pred_nn <- predict(model_nn, newdata = testData, type = "class")
    correct_predictions_nn <- correct_predictions_nn + sum(pred_nn == testData[[target_column]])
    
    # 8. Gradient Boosting Machine (GBM) 模型
    if (requireNamespace("gbm", quietly = TRUE)) {
      model_gbm <- gbm(Group ~ ., data = trainData, distribution = "multinomial", n.trees = 100, interaction.depth = 3, verbose = FALSE)
      pred_gbm <- predict(model_gbm, newdata = testData, n.trees = 100, type = "response")
      correct_predictions_gbm <- correct_predictions_gbm + sum(apply(pred_gbm, 1, which.max) == as.numeric(testData[[target_column]]))
    } else {
      warning("gbm package is not installed.")
    }
    
    # 9. Extra Trees 模型
    model_et <- ranger(as.factor(Group) ~ ., data = trainData, num.trees = 100, mtry = 3, min.node.size = 5)
    pred_et <- predict(model_et, data = testData)$predictions
    correct_predictions_et <- correct_predictions_et + sum(pred_et == testData[[target_column]])
    
    # 10. Linear Discriminant Analysis (LDA) 模型
    model_lda <- lda(as.factor(Group) ~ ., data = trainData)
    pred_lda <- predict(model_lda, newdata = testData)$class
    correct_predictions_lda <- correct_predictions_lda + sum(pred_lda == testData[[target_column]])
    
  }
  
  # 计算并存储每个模型的准确率
  accuracy_list_xgb[iteration] <- correct_predictions_xgb / nrow(selected_data)
  accuracy_list_rf[iteration] <- correct_predictions_rf / nrow(selected_data)
  accuracy_list_svm[iteration] <- correct_predictions_svm / nrow(selected_data)
  accuracy_list_lr[iteration] <- correct_predictions_lr / nrow(selected_data)
  accuracy_list_knn[iteration] <- correct_predictions_knn / nrow(selected_data)
  accuracy_list_dt[iteration] <- correct_predictions_dt / nrow(selected_data)
  accuracy_list_nn[iteration] <- correct_predictions_nn / nrow(selected_data)
  accuracy_list_gbm[iteration] <- correct_predictions_gbm / nrow(selected_data)
  accuracy_list_et[iteration] <- correct_predictions_et / nrow(selected_data)
  accuracy_list_lda[iteration] <- correct_predictions_lda / nrow(selected_data)
}

# 计算各模型准确率的平均值
average_accuracy_xgb <- mean(accuracy_list_xgb)
average_accuracy_rf <- mean(accuracy_list_rf)
average_accuracy_svm <- mean(accuracy_list_svm)
average_accuracy_lr <- mean(accuracy_list_lr)
average_accuracy_knn <- mean(accuracy_list_knn)
average_accuracy_dt <- mean(accuracy_list_dt)
average_accuracy_nn <- mean(accuracy_list_nn)
average_accuracy_gbm <- mean(accuracy_list_gbm)
average_accuracy_et <- mean(accuracy_list_et)
average_accuracy_lda <- mean(accuracy_list_lda)


# 输出所有模型的平均准确率
cat("XGBoost 平均准确率:", average_accuracy_xgb, "\n")
cat("Random Forest 平均准确率:", average_accuracy_rf, "\n")
cat("SVM 平均准确率:", average_accuracy_svm, "\n")
cat("Logistic Regression 平均准确率:", average_accuracy_lr, "\n")
cat("KNN 平均准确率:", average_accuracy_knn, "\n")
cat("Decision Tree 平均准确率:", average_accuracy_dt, "\n")
cat("Neural Network 平均准确率:", average_accuracy_nn, "\n")
cat("GBM 平均准确率:", average_accuracy_gbm, "\n")
cat("Extra Trees 平均准确率:", average_accuracy_et, "\n")
cat("LDA 平均准确率:", average_accuracy_lda, "\n")

#自定义颜色
color_custom = pal_d3("category20c")(20)
custom_color_blue <- color_custom[11]
custom_color_red <- color_custom[12]
custom_color_green <- color_custom[13]


# 创建包含模型名称和平均准确率的数据框
accuracy_data <- data.frame(
  Model = c("XGBoost", "Random Forest", "SVM", "Logistic Regression", "KNN", 
            "Decision Tree", "Neural Network", "GBM", "Extra Trees", "LDA"),
  Accuracy = c(average_accuracy_xgb, average_accuracy_rf, average_accuracy_svm, 
               average_accuracy_lr, average_accuracy_knn, average_accuracy_dt, 
               average_accuracy_nn, average_accuracy_gbm, average_accuracy_et, 
               average_accuracy_lda)
)

# 自定义颜色
custom_color <- c(custom_color_blue, custom_color_red, custom_color_green,
                  custom_color_blue, custom_color_red, custom_color_green,
                  custom_color_blue, custom_color_red, custom_color_green,
                  custom_color_blue)

# 绘制柱状图，使用自定义颜色
ggplot(accuracy_data, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = round(Accuracy, 3)), vjust = -0.5, size = 5) +
  labs(title = "Model Performance Comparison", x = "Model", y = "Average Accuracy") +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +  # 限制Y轴为0到1之间，显示百分比
  scale_fill_manual(values = custom_color) +  # 自定义填充颜色
  theme_minimal() +
  theme(
    text = element_text(size = 15),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),  # 将标题居中
    axis.text.x = element_text(size = 10)  # 缩小X轴标识字号
  ) +
  geom_hline(yintercept = 0.33, linetype = "dashed", color = "black", size = 1) +  # 添加虚线
  annotate("text", x = 0, y = 0.36, label = "Threshold = 0.33", color = "black", hjust = 0)  # 在虚线旁边添加标签


