#鉴别真假钞票
#1、收集和探索数据
install.packages("mclust")
data ("banknote",package ="mclust")
head ( banknote )
tail ( banknote )
table ( banknote$Status )
#相关性分析
library(corrplot)
banknote_new<-banknote[,-1]
cor_new<-cor(banknote_new)
corrplot(cor_new, method = "square", type = "upper", tl.pos = "d")
corrplot(cor_new, add = TRUE, type = "lower", method = "number", diag = FALSE, tl.pos = "n", cl.pos = "n")
#2、准备训练数据和测试数据
set.seed (2018)
N= nrow ( banknote )
train <- sample (1:N, 150 , FALSE )#不放回抽样
head ( train )
#3、训练模型
install.packages("C50")
library (C50)
fitc <- C5.0( Status ~ ., data = banknote [train ,] )
plot ( fitc )
fitc_rules <- C5.0( Status ~., data = banknote [train ,], rules = TRUE )
summary ( fitc_rules )
#4、评估模型性能
predc_train <- predict (fitc, newdata = banknote [train ,], type = "class")
head ( predc_train )
table ( banknote$Status [ train ], predc_train , dnn =c( "Observed Class", "Predicted Class" ))#准确率98%
#测试集的性能
predc <- predict (fitc ,newdata = banknote [-train ,], type = "class")
table ( banknote$Status [- train ], predc , dnn =c( "Observed Class","Predicted Class" ))
#5、优化模型性能,试用其它算法
install.packages("tree")
library ( tree )
fit <- tree ( Status ~., data = banknote [train ,], split ="deviance")
plot (fit);text ( fit )
summary (fit)
#测试集的性能
pred <- predict (fit ,newdata = banknote [-train ,])
tail (pred ,5)
pred.class <- colnames ( pred )[ max.col (pred ,ties.method = c("random"))]
tail ( pred.class ,5)
table ( banknote$Status [- train ], pred.class , dnn =c( "Observed Class","Predicted Class" ))#准确率98%
#6、多次试验，选择模型
#7、决策树的局限性在于不稳定性，另外容易过拟合
#library(oblique.tree)
#library(evtree)