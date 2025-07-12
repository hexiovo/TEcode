# 安装并加载必要的包（如果还未安装）
lapply(c("readxl", "xgboost", "caret", "ggplot2", "Ckmeans.1d.dp", "reshape2", "ggsci", "scales", "pROC", "RColorBrewer"), library, character.only = TRUE)
kfoldnum<- 10
#自定义颜色
color_custom = pal_d3("category20c")(20)
custom_color_blue <- color_custom[11]
custom_color_red <- color_custom[12]
custom_color_green <- color_custom[13]

#迭代次数
iteration_times = 1000
# 读取数据
xlsx_path <- "F:/桌面/文件/实验/教育期望/数据处理/可用数据-2.xlsx"
all_data <- read_excel(xlsx_path, sheet = 1)



# 初始化 auc_list 和 roc_list
auc_list <- vector("list", iteration_times)  # 假设迭代100次
roc_list <- vector("list", length(unique(all_data$Group)))  # 用于存储每个类别的ROC曲线

# 循环100次
for (iteration in 1:iteration_times) {
  cat("\014")
  cat("Iteration:", iteration, "\n")  # 添加简单日志输出
  
  random_seed <- sample(1:10000, 1)
  set.seed(random_seed)
  all_data <- all_data[sample(nrow(all_data)), ]
  
  # 选择特征和目标变量
  feature_columns <-c(10:24, 28)
  target_column <- "Group"
  
  # 确保目标变量和特征变量存在
  selected_data <- all_data[, c(feature_columns, which(names(all_data) == target_column))]
  selected_data <- na.omit(selected_data)
  
  selected_data[[target_column]] <- as.factor(selected_data[[target_column]])
  
  # 创建5折交叉验证
  folds <- createFolds(selected_data[[target_column]], k=kfoldnum, list=TRUE, returnTrain=FALSE)
  
  # 初始化 auc_list[[iteration]] 为长度等于类别数量的向量
  auc_list[[iteration]] <- numeric(length(unique(selected_data[[target_column]])))
  
  for (i in 1:kfoldnum) {
    # 创建训练集和测试集
    testIndex <- folds[[i]]
    trainData <- selected_data[-testIndex, ]
    testData <- selected_data[testIndex, ]
    
    # 转换数据格式为xgboost所需格式
    train_matrix <- xgb.DMatrix(data.matrix(trainData[, -which(names(trainData) == target_column)]), label = as.numeric(trainData[[target_column]]) - 1)
    test_matrix <- xgb.DMatrix(data.matrix(testData[, -which(names(testData) == target_column)]))
    
    # 使用softprob目标函数训练模型
    model_xgb <- xgboost(data = train_matrix, 
                         objective = "multi:softprob",  
                         num_class = length(unique(trainData[[target_column]])), 
                         nrounds = 110, 
                         max_depth = 11, 
                         eta = 0.11, 
                         verbose = 0)
    
    # 进行预测（获得概率分布）
    pred_probs <- predict(model_xgb, newdata = test_matrix)
    
    # 重新调整预测结果格式（为每个类别的概率）
    pred_probs <- matrix(pred_probs, ncol = length(unique(trainData[[target_column]])), byrow = TRUE)
    
    # 计算每个类别的ROC曲线和AUC值
    for (class_id in 0:(length(unique(trainData[[target_column]])) - 1)) {
      roc_curve <- roc(as.numeric(testData[[target_column]]) == (class_id + 1), 
                       pred_probs[, class_id + 1], 
                       direction = "<")
      auc_list[[iteration]][class_id + 1] <- auc(roc_curve)
      
      # 将每次迭代的ROC曲线保存到roc_list中
      if (is.null(roc_list[[class_id + 1]])) {
        roc_list[[class_id + 1]] <- list(roc_curve)
      } else {
        roc_list[[class_id + 1]] <- c(roc_list[[class_id + 1]], list(roc_curve))
      }
    }
  }
}

# 定义插值点
interp_points <- seq(0, 1, by = 0.01)

# 计算每个类别的平均 ROC 曲线
mean_roc_list <- lapply(roc_list, function(class_roc_list) {
  # 创建一个存储 TPR 的矩阵
  tpr_matrix <- matrix(NA, nrow = length(interp_points), ncol = length(class_roc_list))
  
  for (it in 1:length(class_roc_list)) {
    if (!is.null(class_roc_list[[it]])) {
      specificities <- class_roc_list[[it]]$specificities
      sensitivities <- class_roc_list[[it]]$sensitivities
      
      # 插值
      tpr_matrix[, it] <- spline(x = specificities, y = sensitivities, xout = interp_points)$y
      
    }
  }
  
  # 计算平均 TPR
  mean_tpr <- rowMeans(tpr_matrix, na.rm = TRUE)
  
  return(list(fpr = interp_points, tpr = mean_tpr))
})

for (i in 1:3) {
  for (it in 1 : 101){
    mean_roc_list[[i]][["fpr"]][it]= 1 -  mean_roc_list[[i]][["fpr"]][it]
    
  }
}

# 设置图形参数，强制Y轴和X轴长度相同
par(pty = "s")

# 绘制平均ROC曲线，强制Y轴从0到1
plot(mean_roc_list[[1]]$fpr, mean_roc_list[[1]]$tpr, type = "l", col = custom_color_blue, lwd = 2, 
     xlab = "1 - Specificity", ylab = "Sensitivity", main = "Average ROC Curves", 
     ylim = c(0, 1), xlim = c(0, 1),  # 设置Y轴和X轴范围都从0到1
     cex.lab = 1.5,                   # 坐标轴标签字体大小
     cex.axis = 1.2,                  # 坐标轴刻度字体大小
     cex.main = 1.8)                  # 主标题字体大小

# 绘制其他分类的平均ROC曲线
lines(mean_roc_list[[2]]$fpr, mean_roc_list[[2]]$tpr, col = custom_color_red, lwd = 2)
lines(mean_roc_list[[3]]$fpr, mean_roc_list[[3]]$tpr, col = custom_color_green, lwd = 2)

# 添加随机猜测的参考线
abline(0, 1, col = "grey", lty = 2)  # 灰色虚线，表示随机猜测

# 添加图例，并放大图例文本
legend("bottomright", legend = c("Constructive Learning", "Passive Learning", "Active Learning"), 
       col = c(custom_color_blue, custom_color_red, custom_color_green), lwd = 2,
       cex=0.7,box.col = "black", 
       text.width = strwidth("Constructive Learning") * 0.25)  # 控制图例的宽度

# 输出AUC结果
avg_auc <- sapply(auc_list, mean)
cat("Average AUC for each class:\n")
print(avg_auc)

overall_avg_auc <- mean(avg_auc)
cat("Overall Average AUC:", overall_avg_auc, "\n")

# 提取每个类别的AUC值
category_auc_list <- sapply(roc_list, function(class_roc_list) {
  # 如果该类别的roc_list不为空，则计算其AUC
  if (length(class_roc_list) > 0) {
    auc_values <- sapply(class_roc_list, function(roc_curve) {
      auc(roc_curve)
    })
    # 返回平均AUC值
    return(mean(auc_values, na.rm = TRUE))
  } else {
    return(NA)  # 如果该类别没有ROC曲线，则返回NA
  }
})


# 输出每个类别的AUC值
cat("AUC for each class:\n")
print(category_auc_list)

# 输出整体平均AUC
overall_avg_auc <- mean(category_auc_list, na.rm = TRUE)
cat("Overall Average AUC for all classes:", overall_avg_auc, "\n")
