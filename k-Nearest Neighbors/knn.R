#鉴别葡萄酒品种
#1、收集和探索数据
library(corrplot)
install.packages("rebmix")
data ("wine",package ="rebmix")
wine_new<-wine[,-(ncol(wine))]
cor_new<-cor(wine_new)
corrplot(cor_new,method = "ellipse", type = "upper", tl.pos = "d")
boxplot(wine_new,par(las=2),col = "yellow")
