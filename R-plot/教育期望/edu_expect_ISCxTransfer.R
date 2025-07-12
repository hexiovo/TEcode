# 一次性加载多个包
lapply(c("readxl", "ggplot2", "dplyr", "ggsignif", "showtext", "grid","ggsci"), library, character.only = TRUE)

# 输入文件地址、读入
xlsx_path <- "F:/桌面/文件/实验/教育期望/数据处理/可用数据.xlsx"
all_data <- read_excel(xlsx_path, sheet = 1)

# 假设数据框有列 "ISC_xy"、"Trans_T1" 和 "Group"
# 更改 "Group" 列为因子
all_data$Group <- factor(all_data$Group, levels = c(1, 2, 3), 
                         labels = c("Meaningful learning", "mechanical learning", "control"))

# 创建模型并计算相关系数
results <- all_data %>%
  group_by(Group) %>%
  summarize(
    r = cor(ISC_xy, Trans_T1, use = "complete.obs"),
    .groups = 'drop'
  )

# 自定义颜色
color_custom = pal_d3("category20c")(20)
custom_colors_lp = c("Meaningful learning" = color_custom[6],  
                     "mechanical learning" = color_custom[7],
                     "control" = color_custom[8])
custom_colors <- c("Meaningful learning" = color_custom[11],  
                   "mechanical learning" = color_custom[12],
                   "control" = color_custom[13])

# 绘制图形
p <- ggplot(all_data, aes(x = ISC_xy, y = Trans_T1, color = Group)) +
  geom_point() +
  geom_smooth(method = "lm", aes(fill = Group), alpha = 0.2) +
  labs(
    title = "Transfer Score vs ISC",
    x = "ISC",
    y = "Transfer Score"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # 居中标题并放大
    legend.position = "none",  # 取消图例
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    strip.background = element_blank(),  # 去掉facet标签的背景
    strip.text = element_text(size = 14),  # 自定义面板标签字体
    strip.placement = "outside",  # 将facet标签放在图形之外
    axis.title.y.right = element_blank(),  # 取消右侧Y轴标签
    axis.title.x.top = element_blank()  # 取消顶部X轴标签
  ) +
  # 只保留 r 值的显示
  coord_cartesian(clip = 'off') +  # 允许图形超出坐标区域
  geom_text(data = results, aes(x = 0.7, y = 10,  
                                label = paste("r =", round(r, 2))), 
            position = position_nudge(y = 1), size = 5, color = "black") +
  facet_wrap(~ Group, nrow = 1,strip.position = "bottom") +  # 将标签放在图形之外
  scale_color_manual(values = custom_colors_lp) +  # 自定义点和线的颜色
  scale_fill_manual(values = custom_colors) +  # 自定义颜色
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))  # 强制Y从0开始

# 打印图形
print(p)

# 在图形中添加箭头
grid_xposition <- 0.0495  # 根据需要调整箭头的X位置
grid_yposition <- 0.945  # 根据需要调整箭头的Y位置
grid.text("∧", x = unit(grid_xposition, "npc"), y = unit(grid_yposition ,"npc"), gp = gpar(col = "black", fontsize = 30))
