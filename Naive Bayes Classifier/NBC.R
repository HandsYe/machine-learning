#识别汽车分类
#Type |Short |Not Short |Four Doors| Not Four Doors| Black |Not Black
#Cab 200 100 150 150 225 75
#Bus 50 300 25 325 5 345
#Car 300 50 250 100 100 250
#1、计算先验概率
p(cab) = 300/1000 = 0.3
p(bus) = 350/1000 = 0.35
p(car) = 350/1000 = 0.35
P(Short) = 200+50+300/1000 = 0.55
P(FourDoor) = 150+25+250/1000 = 0.425
P(Black) = 225+5+100/1000 = 0.33
#2、计算条件概率
P(Short|Cab) = 200/300 = 0.667
P(Short|Bus) = 50/350 = 0.143
P(Short|Car) = 300/350 = 0.857
P(FourDoor|Cab) = 150/300 = 0.5
P(FourDoor|Bus) = 25/350 = 0.071
P(FourDoor|Car) = 250/350 = 0.714
P(Black|Cab) = 225/300 = 0.75
P(Black|Bus) = 5/350 = 0.014
P(Black|Car) = 100/350 = 0.286
#3、后验概率
P(Cab|Short, FourDoor, Black) = P(Cab) * P(Short|Cab) * P(FourDoor|Cab) * P(FourDoor|Cab) = 0.3 * 0.667 * 0.5 * 0.75 = 0.075
P(Bus|Short, FourDoor, Black) = 0.01
P(Car|Short, FourDoor, Black) = 0.061

#4、优势
#简单、实时、高效

#example
num_attrib <- 2
N <- 100
set.seed (2021)
x <- matrix ( rnorm (N* num_attrib ),
              ncol = num_attrib )
colnames (x) <- c("x1","x2")
y <- as.numeric ((x [ ,1]^2+ x [ ,2]^2) > 2.3)
#plot(density(x[,1]))
#plot(density(x[,2]))
#barplot(summary(as.factor(y)))
y<-as.factor (y)
data <- cbind (y,x)
data <-as.data.frame ( data )
set.seed (2016)
train <- sample (1:N ,70 , FALSE )
install.packages("e1071")
library ( e1071 )
fit <- naiveBayes (x[train ,],y[ train ])
pred_probs <- predict (fit ,data [train , -1] ,type ="raw")
head ( pred_probs )
pred <- predict (fit ,data [train , -1] ,type="class")
head ( pred )
y_train <-y[ train ]
table ( y_train , pred )
pred_test <- predict (fit ,data [-train , -1] ,type="class")
y_test <-y[- train ]
table ( y_test , pred_test )


#example
#识别雷达信号
install.packages("evclass")
data ("ionosphere",package ="evclass")
str( ionosphere )
summary( ionosphere$x [ , 2 ] )
x <- as.data.frame ( ionosphere$x [ , ] )
x$V2 <- NULL
plot(density(x$V1))
x$V1 <- NULL
y<-as.factor ( ionosphere$y )
set.seed (2021)
N=nrow ( ionosphere$x )
train <- sample ( 1:N, 251 ,FALSE)
#训练模型
fit <- naiveBayes ( y [ train ] ~ . ,data = x [ train , ] )
attributes ( fit )
#先验分布
fit$apriori
pred_probs <- predict (fit , x[-train ,],type="raw")
head ( round( pred_probs ,3) ,4)
pred <- predict (fit ,x[-train ,], type="class")
head (pred ,4)
install.packages("caret")
library ( caret )
result <- confusionMatrix (pred ,y[- train ])
result$table
result$overall [1]
#优化性能
x<-as.data.frame ( ionosphere$x [ ,])
x$V2 <- NULL
corrplot(cor(x),type = "upper",method = "square")
findCorrelation (cor(x),cutoff = 0.6 ,exact = TRUE ,names = TRUE )
x$V15 <- NULL
x$V19 <- NULL
x$V21 <- NULL
x$V17 <- NULL
x$V13 <- NULL
x$V11 <- NULL
x$V25 <- NULL
x$V33 <- NULL
fit2 <- naiveBayes (y[ train ] ~ .,data = x[train ,])
pred2 <- predict (fit2 , x[-train ,],type="class")
result2 <- confusionMatrix (pred2 ,y[- train ])
result2$table
result2$overall [1]
