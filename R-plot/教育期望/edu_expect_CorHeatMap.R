# 加载必要的包
library(readxl)
library(ggplot2)
library(reshape2)
library(dplyr)
library(ggtext)
library(showtext)

# 加载宋体
font_add("SimSun", regular = "C:/Windows/Fonts/simsun.ttc")
showtext_auto()

# 输入文件地址、读入
xlsx_path <- "F:/桌面/文件/实验/教育期望/数据处理/可用数据-2.xlsx"
all_data <- read_excel(xlsx_path, sheet = 1)

# 提取指定的列
selected_columns <- all_data[, c(7:24, 28, 43:45, 49)]

# 定义新标签
new_labels <- c("Gender", "Age", "Prior-knowledge", "ISC", "P_word", "P_picture", 
                "C_AttentionPoint", "T_AP-duration", "MT_AP-duration", "Trans_Total", "Trans_between-Cata", 
                "M_Pupil-Fixation", "Trans_Between-Relate", "PTrans_Total", "PTrans_Between-Cata", "M_Duration", 
                "M_Scan-Range", "Stress", "Interest", "Memory Score", 
                "Understand Score", "Transfer Score", "Total Score")

# 将新标签应用于数据框的列名
colnames(selected_columns) <- new_labels

# 计算相关性矩阵
cor_matrix <- cor(selected_columns, use = "complete.obs")

# 计算 p 值矩阵
p_matrix <- matrix(NA, ncol = ncol(selected_columns), nrow = ncol(selected_columns))
for (i in 1:ncol(selected_columns)) {
  for (j in 1:ncol(selected_columns)) {
    if (i != j) {
      test <- cor.test(selected_columns[[i]], selected_columns[[j]])
      p_matrix[i, j] <- test$p.value
    } else {
      p_matrix[i, j] <- NA
    }
  }
}
rownames(p_matrix) <- colnames(cor_matrix)
colnames(p_matrix) <- colnames(cor_matrix)

# 将相关性矩阵转换为长格式
cor_melted <- melt(cor_matrix)
p_melted <- melt(p_matrix)

# 合并相关性和 p 值数据
heatmap_data <- cor_melted %>%
  left_join(p_melted, by = c("Var1" = "Var1", "Var2" = "Var2"), suffix = c("_cor", "_p"))

# 为显著性添加符号
heatmap_data$significance <- cut(heatmap_data$value_p, 
                                 breaks = c(-Inf, 0.001, 0.01, 0.05, Inf), 
                                 labels = c("***", "**", "*", ""), 
                                 right = FALSE)

# 绘制热力图
heatmap_plot <- ggplot(data = heatmap_data, aes(x = Var1, y = Var2, fill = value_cor)) +
  geom_tile(color = "white") + 
  geom_text(aes(label = significance), color = "black", size = 5, vjust = 0.8) +  # 添加显著性标记
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       limit = c(-1, 1), name = "Correlation") +
  scale_x_discrete(limits = rev(levels(as.factor(heatmap_data$Var1)))) +  # 反转X轴
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # 设置X轴字体大小
        axis.text.y = element_text(size = 14),  # 设置Y轴字体大小
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 24),  # 标题居中
        legend.title = element_text(size = 17, hjust = 1),  # 图例标题向左移动
        legend.text = element_text(size = 14),  # 图例标签字体大小
        legend.key.size = unit(1, "cm"),  # 设置图例条形高度
        legend.position = c(1.1, 0.520)) +  # 将图例位置设为右侧并调整上下位置
  labs(title = "Correlation Heatmap",  # 添加标题
       x = "",  # 去掉 x 轴标签
       y = "") +  # 去掉 y 轴标签
  coord_fixed(ratio = 1) +  # 设置格子为正方形
  guides(fill = guide_colorbar(barheight = unit(12.465, "cm"), barwidth = unit(0.7, "cm")))  # 设置图例条形的高度和宽度

# 显示热力图
print(heatmap_plot)







