# 一次性加载多个包
lapply(c("readxl", "ggplot2", "reshape2","magrittr","dplyr","ggtext","ggsci","scales","ggsignif","showtext"), library, character.only = TRUE)

#加载宋体
font_add("SimSun", regular = "C:/Windows/Fonts/simsun.ttc")
showtext_auto()

#输入文件地址、读入
xlsx_path <- "F:/桌面/文件/实验/教育期望/数据处理/可用数据.xlsx"
all_data <- read_excel(xlsx_path, sheet = 1)

#转换列模式为行模式，一对一
long_data <- melt(all_data, id.vars = "Group", measure.vars = c("Memory_T1", "Understand_T1","Trans_T1"),
                  variable.name = "Condition", value.name = "Memory")

#更换内容名
long_data$Group <- factor(long_data$Group, levels = 1:3, 
                          labels = c("教育期望", "无期望", "测试期望"))
# 将变量名替换为中文
long_data <- long_data %>%
  mutate(Condition = case_when(
    Condition == "Memory_T1" ~ "记忆测试",
    Condition == "Understand_T1" ~ "理解测试",
    Condition == "Trans_T1" ~ "迁移测试"
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
custom_colors <- c("教育期望" = color_custom[16],  # 自定义颜色示例
                   "无期望" = color_custom[17],
                   "测试期望" = color_custom[18])

filtered_data <- summary_data %>%
  filter(Condition == "记忆测试", Group %in% c("无期望", "测试期望"))

#绘制
p <- ggplot(summary_data, aes(x = Condition, y = Mean, fill = factor(Group))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                position = position_dodge(width = 0.7), 
                width = 0.1) +
  labs(x = "测验类型", y = "测验成绩", fill = "期望水平") +
  theme_minimal()+
  theme(
    #图例
    legend.position = c(0.97, 0.85),  # 调整图例位置
    legend.justification = c(0.8, 0.6),  # 图例锚点位置
    legend.title = element_text(margin = margin(r =1), hjust = 0.5, vjust = 2),# 调整图例标题右边距并精细调整位置
    #legend.background = element_rect(color = "black", linewidth = 0.5), # 添加黑色边框到图例背景
    legend.key.size = unit(0.5, "cm"), # 调整图例方框大小
    
    #背景与坐标轴
    panel.background = element_blank(),  # 去掉面板背景
    panel.grid.major = element_blank(),  # 去掉主要网格线
    panel.grid.minor = element_blank(),  # 去掉次要网格线
    axis.line = element_line(colour = "black"),  # 添加坐标轴线
    axis.ticks = element_line(colour = "black"),# 添加坐标轴刻度线
    axis.title.x = element_text(size = 12),  # 设置 x 轴标签的字号
    axis.title.y = element_text(size = 12),  # 设置 y 轴标签的字号
    text = element_text(family = "SimSun") #更改字体为宋体
  )+
    coord_cartesian(clip = 'off') +  # 允许图形超出坐标区域
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)))+  # 确保 x 轴在 y = 0 处
    scale_fill_manual(values = custom_colors)  # 设置自定义颜色
  
p <- p + 
  geom_signif(data=summary_data,aes(xmin=0.766, xmax=1.236, annotations="*", y_position=18),textsize = 8, vjust = 0.5, tip_length = c(0.04, 0.04),manual=TRUE)+
  geom_signif(data=summary_data,aes(xmin=1.766, xmax=2.236, annotations="*", y_position=18),textsize = 8, vjust = 0.5, tip_length = c(0.04, 0.04),manual=TRUE)+
  geom_signif(data=summary_data,aes(xmin=1.766, xmax=2.00, annotations="**", y_position=17),textsize = 8, vjust = 0.5, tip_length = c(0.04, 0.04),manual=TRUE)

print(p)
