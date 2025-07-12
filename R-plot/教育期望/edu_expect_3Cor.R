# 安装并加载必要的包（如果还未安装）
lapply(c("readxl", "ggplot2", "reshape2", "corrplot", "dplyr"), library, character.only = TRUE)

# 读取数据
xlsx_path <- "F:/桌面/文件/实验/教育期望/数据处理/可用数据.xlsx"
all_data <- read_excel(xlsx_path, sheet = 1)

# 确保Group列为因子
all_data$Group <- as.factor(all_data$Group)

# 对第37-39列和第43列进行Z标准化
columns_to_scale <- c(37:39, 43)
all_data[, columns_to_scale] <- scale(all_data[, columns_to_scale])

# 选择特征列
feature_columns <- c(11:25, 29, 35:39, 43)
selected_data <- all_data[, feature_columns]
# 确保特征列的有效性
valid_feature_columns <- feature_columns[feature_columns <= ncol(all_data)]
# 根据Group分为三组
group1_data <- all_data[all_data$Group == levels(all_data$Group)[1], feature_columns]
group2_data <- all_data[all_data$Group == levels(all_data$Group)[2], feature_columns]
group3_data <- all_data[all_data$Group == levels(all_data$Group)[3], feature_columns]

# 检查每组数据的结构和数据类型
print(str(group1_data))
print(str(group2_data))
print(str(group3_data))

# 确保没有缺失值，并且所有列为数值型
group1_data <- na.omit(group1_data)
group2_data <- na.omit(group2_data)
group3_data <- na.omit(group3_data)

# 计算每组数据的相关性和p值矩阵
correlation_matrix_group1 <- cor(group1_data, use = "complete.obs")
correlation_matrix_group2 <- cor(group2_data, use = "complete.obs")
correlation_matrix_group3 <- cor(group3_data, use = "complete.obs")

# 计算p值矩阵
p_matrix_group1 <- cor(group1_data)
p_matrix_group2 <- cor(group2_data)
p_matrix_group3 <- cor(group3_data)

# 将不显著的相关系数设为NA（显著性水平为p < 0.05）
threshold <- 0.05
correlation_matrix_group1[p_matrix_group1 > threshold] <- NA
correlation_matrix_group2[p_matrix_group2 > threshold] <- NA
correlation_matrix_group3[p_matrix_group3 > threshold] <- NA

# 将相关性矩阵转换为长格式并更名
melted_corr_matrix_group1 <- melt(correlation_matrix_group1, na.rm = TRUE)  # 去掉NA值
melted_corr_matrix_group1$Group <- "教学期望组"

melted_corr_matrix_group2 <- melt(correlation_matrix_group2, na.rm = TRUE)  # 去掉NA值
melted_corr_matrix_group2$Group <- "控制组"

melted_corr_matrix_group3 <- melt(correlation_matrix_group3, na.rm = TRUE)  # 去掉NA值
melted_corr_matrix_group3$Group <- "测试期望组"

# 合并三组的长格式相关性矩阵
melted_corr_matrix <- rbind(melted_corr_matrix_group1, melted_corr_matrix_group2, melted_corr_matrix_group3)

# 使用ggplot2绘制相关性热力图，标题居中
ggplot(data = melted_corr_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlation") +
  facet_wrap(~ Group) +  # 每个Group单独绘制一个图
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +  # 标题居中
  coord_fixed() +
  labs(title = "Correlation Heatmaps for Each Group",
       x = "Variables", y = "Variables")
