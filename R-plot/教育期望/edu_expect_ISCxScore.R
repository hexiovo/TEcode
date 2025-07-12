# 加载必要的包
lapply(c("readxl", "ggplot2", "dplyr", "tidyr", "showtext", "grid", "ggsci", "scales", "broom", "purrr"), library, character.only = TRUE)

# 输入文件地址、读入数据
xlsx_path <- "F:/桌面/文件/实验/教育期望/数据处理/可用数据.xlsx"
all_data <- read_excel(xlsx_path, sheet = 1)

# 转换数据为长格式
long_data <- all_data %>%
  pivot_longer(cols = c(Memory_T1, Understand_T1, Trans_T1, Score_T1),
               names_to = "Score_Type",
               values_to = "Score_Value") %>%
  mutate(Score_Type = recode(Score_Type,
                             "Memory_T1" = "Memory Score",
                             "Understand_T1" = "Understanding Score",
                             "Trans_T1" = "Transfer Score",
                             "Score_T1" = "Total Score"))

# 自定义颜色
color_custom = pal_d3("category20c")(20)
custom_colors_lp = c("Memory Score" = color_custom[6],  
                     "Understanding Score" = color_custom[7],
                     "Transfer Score" = color_custom[8],
                     "Total Score" = color_custom[9])
custom_colors <- c("Memory Score" = color_custom[11],  
                   "Understanding Score" = color_custom[12],
                   "Transfer Score" = color_custom[13],
                   "Total Score" = color_custom[14])

# 计算 Pearson 相关系数和 p 值
stats <- long_data %>%
  group_by(Score_Type) %>%
  summarise(
    cor_test = list(cor.test(Score_Value, ISC_xy)),  # 使用 cor.test 计算相关系数和 p 值
    .groups = 'drop'
  ) %>%
  mutate(
    r_value = map_dbl(cor_test, ~ .$estimate),   # 提取相关系数
    p_value = map_dbl(cor_test, ~ .$p.value)     # 提取 p 值
  ) %>%
  select(Score_Type, r_value, p_value)  # 选择所需列

# 绘制图形
p <- ggplot(long_data, aes(x = ISC_xy, y = Score_Value, color = Score_Type, fill = Score_Type)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.2) +
  labs(
    title = "Score vs ISC for Correlation",
    x = "ISC",
    y = "Score"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    strip.background = element_blank(),
    strip.text = element_text(size = 14),
    strip.placement = "outside",
    legend.position = "none"
  ) +
  facet_wrap(~ Score_Type, scales = "free_y", nrow = 1, strip.position = "bottom") +
  scale_color_manual(values = custom_colors_lp) +
  scale_fill_manual(values = custom_colors) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

# 手动添加每个子图的r值和p值
text_positions <- data.frame(
  Score_Type = c("Memory Score", "Understanding Score", "Transfer Score", "Total Score"),
  x = c(0.45, 0.45, 0.45, 0.45),  # 所有子图的x位置相同
  y = c(10, 8, 4, 22.5)  # 根据每个子图的y轴范围调整y位置
)

# 创建一个用于存储文本标签的数据框
labels_data <- data.frame(x = numeric(), y = numeric(), label = character(), Score_Type = character())

for (i in 1:nrow(stats)) {
  score_type <- stats$Score_Type[i]
  r_value <- round(stats$r_value[i], 3)
  p_value <- round(stats$p_value[i], 3)
  
  # 获取对应的文本位置
  position <- text_positions[text_positions$Score_Type == score_type, ]
  
  # 创建文本标签
  label_text <- paste("r =", r_value, "\np =", p_value)
  
  # 将文本信息添加到labels_data
  labels_data <- rbind(labels_data, data.frame(x = position$x, y = position$y, label = label_text, Score_Type = score_type))
}

# 添加文本到图形
p <- p + 
  geom_text(data = labels_data, aes(x = x, y = y, label = label), 
            color = "black", 
            size = 5,
            hjust = 0, 
            vjust = 1,
            check_overlap = TRUE)

# 定义添加箭头的函数
add_arrow <- function(grid_xposition, grid_yposition, color = "black", size = 30) {
  grid.text("∧", 
            x = unit(grid_xposition, "npc"), 
            y = unit(grid_yposition, "npc"), 
            gp = gpar(col = color, fontsize = size))
}


# 打印最终图形
print(p)
# 添加箭头
add_arrow(0.0358, 0.96)
add_arrow(0.2786, 0.96)
add_arrow(0.5215, 0.96)
add_arrow(0.7735, 0.96)
