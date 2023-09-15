
setwd("C:/Users/Dell/Desktop/??")
Gene <- read.csv('Gene1000zhuanzhi.csv')  

library("dplyr")
s1 <- filter(Gene,stage=='1')
s2 <- filter(Gene,stage=='2')
row1 <- nrow(s1)
row2 <- nrow(s2)
#in1 <- sample(row1,200)
m1<- s1[train1,]
tt1 <- s1[-train1,]
train2 <- sample(row2,200)
m2 <- s2[train2,]
tt2 <- s2[-train2,]

train_data <- rbind(m1,m2)  
test_data <- rbind(tt1,tt2)

#????in_data <- train_data[,-1]
test_data <- test_data[,-1]


library(pROC) #???
library('e1071')
#
train_data <- train
test_data <- test
train_data$stage = as.factor(train_data$stage)
test_data$stage = as.factor(test_data$stage)
y <- train_data[1,-1001]
#svm
svm<- svm(stage ~ .,
                 data = train_data,
                 type = 'C',kernel = 'radial' )
pre_svm <- predict(svm,newdata = test_data)
obs_p_svm = data.frame(prob=pre_svm,obs=test_data$stage)
obs_p_svm
###
table(test_data$stage,pre_svm,dnn=c("0","1"))
###???_roc <- roc(test_data$stage,as.numeric(pre_svm))
plot(svm_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue",main='SVMģ?ͣ??ǹ???????ROC????')
