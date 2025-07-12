# 一次性加载多个包
lapply(c("readxl", "bda", "mediation"), library, character.only = TRUE)

#输入文件地址、读入
xlsx_path <- "F:/桌面/文件/实验/教育期望/数据处理/可用数据.xlsx"
all_data <- read_excel(xlsx_path, sheet = 1)

# 提取X, M, Y列
X <- all_data[[7]]  # 第7列自变量
M <- all_data[[11]]  # 第11列中介变量
Y <- all_data[[43]]  # 第43列因变量

# 标准化M和Y
#M <- scale(M)  # 标准化M
#Y <- scale(Y)  # 标准化Y

#X <- c(X,X)
#Y <- c(Y,Y)
#M <- c(M,M)
# 将X转换为因子，并设置基线组为第一组
X <- factor(X)
X <- relevel(X, ref = levels(X)[1])  # 设置第一组为基线组

# 自变量X（分类变量）预测中介变量M
model1 <- lm(M ~ X)
summary(model1)

# 自变量X和中介变量M预测因变量Y
model2 <- lm(Y ~ X + M)
summary(model2)

# 提取回归系数和标准误差
a2 <- coef(summary(model1))["X2", "Estimate"]  # 第二组相对于基线组的系数
a3 <- coef(summary(model1))["X3", "Estimate"]  # 第三组相对于基线组的系数
sa2 <- coef(summary(model1))["X2", "Std. Error"]  # 第二组系数的标准误差
sa3 <- coef(summary(model1))["X3", "Std. Error"]  # 第三组系数的标准误差

b <- coef(summary(model2))["M", "Estimate"]  # 中介变量到因变量的系数
sb <- coef(summary(model2))["M", "Std. Error"]  # 中介变量到因变量系数的标准误差

# Sobel Z统计量公式
sobel_z <- function(a, b, sa, sb) {
  Z <- (a * b) / sqrt((b^2 * sa^2) + (a^2 * sb^2))
  return(Z)
}

# 计算第二组和第三组的Z值
sobel_z_2 <- sobel_z(a2, b, sa2, sb)  # 第二组相对于基线组
sobel_z_3 <- sobel_z(a3, b, sa3, sb)  # 第三组相对于基线组

sobel_z_2
sobel_z_3

# 计算p值
p_value <- function(Z) {
  p <- 2 * (1 - pnorm(abs(Z)))  # 双尾检验
  return(p)
}

# 计算p值
p_value_2 <- p_value(sobel_z_2)
p_value_3 <- p_value(sobel_z_3)

p_value_2
p_value_3

  M <- as.numeric(M)


# 进行Bootstrap中介效应检验
set.seed(123)  # 设置随机种子，保证结果可重复
med.out <- mediate(model1, model2, treat = "X", mediator = "M", boot = TRUE, sims = 5000)
summary(med.out)




