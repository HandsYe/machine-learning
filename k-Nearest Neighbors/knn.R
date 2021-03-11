#鉴别葡萄酒品种
#1、收集和探索数据
library(corrplot)
install.packages("rebmix")
data ("wine",package ="rebmix")
wine_new<-wine[,-(ncol(wine))]
cor_new<-cor(wine_new)
#相关性图
corrplot(cor_new,method = "ellipse", type = "upper", tl.pos = "d")
#箱线图
boxplot(wine_new, par(las=2), col = "yellow")
#2、准备测试数据
data_sample <- wine[,1:13]
#数据中心化和标准化
data_sample <- scale( data_sample )
boxplot(data_sample, col = "yellow")
#训练集和测试集
set.seed (2021)
n= nrow ( data_sample )
train <- sample (1:n, 89, replace = FALSE )
head ( train )
#3、训练模型
library(knnGarden)
fit1 <- knnVCN ( data_sample [train ,],
                 wine$Cultivar[ train ],
                 data_sample [-train ,],
                 K = 2,
                 method = "canberra")
#4、评估模型性能
tab1 <- table ( fit1$TstXIBelong ,
                wine$Cultivar [- train ])
tab1
#   1  2  3
#1 35  3  0
#2  0 29  0
#3  0  3 19
#错误率 （3+3）/89 = 6.74%
#5、优化模型性能，多种尝试
fit2 <- knnVCN ( data_sample [train ,],
                 wine$Cultivar [ train ],
                 data_sample [-train ,],
                 K = 2,
                 method = "euclidean")
tab2 <- table ( fit2$TstXIBelong ,
                wine$Cultivar [- train ])
tab2#效果更差了
#   1  2  3
#1 35  5  0
#2  0 26  0
#3  0  4 19

fit3 <- knnVCN ( data_sample [train ,],
                 wine$Cultivar[ train ],
                 data_sample [-train ,],
                 K = 3,
                 method = "canberra")
table ( fit3$TstXIBelong ,
        wine$Cultivar [- train ])
#局限性
#数据太大较慢，对异常值敏感