# 一次性加载多个包
lapply(c("readxl", "ggplot2", "reshape2","magrittr","dplyr","ggtext","ggsci","scales","ggsignif","showtext","grid"), library, character.only = TRUE)

#加载宋体
font_add("SimSun", regular = "C:/Windows/Fonts/simsun.ttc")
showtext_auto()

#输入文件地址、读入
xlsx_path <- "F:/桌面/文件/实验/教育期望/数据处理/可用数据.xlsx"
all_data <- read_excel(xlsx_path, sheet = 1)

#转换列模式为行模式，一对一
long_data <- melt(all_data, id.vars = "Group", measure.vars = c("ISC_xy"),
                  variable.name = "Condition", value.name = "Memory")

# 更换内容名，并调整顺序
long_data$Group <- factor(long_data$Group, levels = c(2, 3, 1), 
                          labels = c("Passive Learning", "Active Learning", "Constructive Learning"))

# 将变量名替换为中文
long_data <- long_data %>%
  mutate(Condition = case_when(
    Condition == "ISC_xy" ~ "ISC level",
  ))


#计算误差
summary_data <- long_data %>%
  group_by(Condition, Group) %>%
  summarise(
    Mean = mean(Memory, na.rm = TRUE),
    SE = sd(Memory, na.rm = TRUE) / sqrt(n())
  )


#自定义颜色
color_custom = pal_d3("category20c")(20)
custom_colors <- c("Constructive Learning" = color_custom[16],  # 自定义颜色示例
                   "Passive Learning" = color_custom[17],
                   "Active Learning" = color_custom[18])

# 绘制
p <- ggplot(summary_data, aes(x = Group, y = Mean, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                position = position_dodge(width = 0.5), 
                width = 0.1) +
  labs(x = "Test Condition", y = "ISC level", fill = "Group Condition") +
  theme_minimal() +
  theme(
    #去掉图例
    legend.position = "none",
    # 背景与坐标轴
    panel.background = element_blank(),  # 去掉面板背景
    panel.grid.major = element_blank(),  # 去掉主要网格线
    panel.grid.minor = element_blank(),  # 去掉次要网格线
    axis.line = element_line(colour = "black"),  # 添加坐标轴线
    axis.ticks = element_line(colour = "black"),  # 添加坐标轴刻度线
    axis.title.x = element_text(size = 24),  # 设置 x 轴标题的字号
    axis.title.y = element_text(size = 24),  # 设置 y 轴标题的字号
    axis.text.x = element_text(size = 20),  # 设置 x 轴刻度标签的字号
    axis.text.y = element_text(size = 20),  # 设置 y 轴刻度标签的字号
    strip.text = element_text(size = 20),  # 设置facet标签的字号
    strip.placement = "outside",  # 将facet标签放在图形之外
    strip.background = element_blank()  # 去掉facet标签的背景
  ) +
  coord_cartesian(clip = 'off') +  # 允许图形超出坐标区域
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +  # 确保 x 轴在 y = 0 处
  scale_fill_manual(values = custom_colors) +  # 设置自定义颜色
  facet_wrap(~Condition, nrow = 1, strip.position = "bottom")+  # 将标签放在底部
  coord_fixed(ratio = 5)  # 设置纵横比，压缩纵向比例

p <- p + 
  geom_signif(data=summary_data, aes(xmin=1, xmax=3, annotations="*", y_position=0.73), 
              textsize = 10, vjust = 0.5, tip_length = c(0.06, 0.06), manual=TRUE) +
  geom_signif(data=summary_data, aes(xmin=1, xmax=2, annotations="*", y_position=0.7), 
              textsize = 10, vjust = 0.5, tip_length = c(0.06, 0.06), manual=TRUE) 
print(p)
# 在第一个分面 (Memory Test) 的 y 轴尽头处添加一个箭头
grid_xposition <- 0.325
grid_yposition <- 0.983

grid.text("∧", x = unit(grid_xposition, "npc"), y = unit(grid_yposition, "npc"), gp = gpar(col = "black", fontsize = 30))
