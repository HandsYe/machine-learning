install.packages("ReorderCluster")
library(ReorderCluster)
data ("leukemia",package ="ReorderCluster")

install.packages("usdm")
install.packages("MASS")
library(usdm)
library(MASS)
x<- data.matrix ( leukemia [ ,2:101])
vif <- vifstep (x,th =8)
x<- exclude (x, vif)
y<- leukemia [ ,102]
data <- data.frame (x,y)
set.seed (2021)
N= nrow ( data )
ncol (x)
train <- sample (1:N ,45 , FALSE )
data_train <- data [train ,]
data_test <- data [-train ,]

fit1 <- lda (y ~., data = data_train )
round( fit1$prior ,3)
round( fit1$means [ ,36:38] ,3)
prop_var <- round( prop.table ( fit1$svd ^2) ,4)
names ( prop_var ) <-c(" LD1 "," LD2 ")
round( fit1$scaling ,2)
scale1 <- data.frame ( fit1$scaling )
ord1 <- order(abs( scale1 [ ,1]) , decreasing = TRUE )
names1 <- rownames ( scale1 ) [ ord1 ]
scale1 [ names1 [1:3] ,1]
ord2 <- order (abs( scale1 [ ,2]) ,decreasing = TRUE )
names2 <- rownames ( scale1 ) [ ord2 ]
names2 [1:3]
scale1 [ names2 [1:3] ,1]
pred1_train <- predict(fit1 ,data_train ) $class
head ( pred1_train ,9)
tab1_train <- table ( data$y [ train ],pred1_train )
tab1_train
pred1 <- predict (fit1 , data_test ) $class
tab1 <- table (y[- train ],pred1 )
tab1#准确率78%

x<- data.matrix ( leukemia [ ,2:101])
vif <- vifstep (x,th =5)
x<- exclude (x, vif)
ncol (x)
data <- data.frame (x,y)
set.seed (2021)
N= nrow ( data )
ncol (x)
train <- sample (1:N ,45 , FALSE )
data_train <- data [train ,]
data_test <- data [-train ,]
fit <- lda (y ~., data = data_train )
plot (fit)
pred <- predict (fit , data_test ) $class
tab <- table ( data$y [- train ],pred )
tab#准确率82%