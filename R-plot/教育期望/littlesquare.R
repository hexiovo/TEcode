library(ggplot2)
library(ggsci)

# 创建数据框，包含8个格子的序号
df <- data.frame(
  x = 1:8,
  y = rep(1, 8)
)

# 使用ggplot2绘制连续的小正方形并使用较浅色的ggsci调色板
p <- ggplot(df, aes(x = factor(x), y = y, fill = factor(x))) +
  geom_tile(color = "black", size = 0.5, width = 0.9, height = 0.9) +  # 确保正方形形状
  scale_fill_rickandmorty() +  # 使用ggsci中的"simpsons"配色方案
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) 

# 打印图形
print(p)
