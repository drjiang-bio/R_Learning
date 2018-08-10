# 构建数据集
options(stringsAsFactors = F)
type <- c('A','A','B','B','C','C')
id <- c('1','2','1','2','1','2')
X1 <- sample(1:20, 6)
X2 <- sample(3:23, 6)
data <- data.frame(type, id, X1, X2)
data

library(reshape2)
# 融合
md <- melt(data, id=c('type', 'id'))

# 重塑
## 执行整合
d1 <- dcast(md, type ~ variable, mean);d1
d2 <- dcast(md, id ~ variable, mean);d2
d3 <- dcast(md, type ~ id, mean);d3

## 不执行整合
d4 <- dcast(md, type + id ~ variable);d4
d5 <- dcast(md, type ~ id + variable);d5
d6 <- dcast(md, variable ~ type + id);d6


