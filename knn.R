
library(kknn)
library(sampling)
library(class)
iris.kknn <- kknn(Species~., iris.learn, iris.valid, distance = 1, kernel = "triangular")


data("iris")
m <- dim(iris)[1]
val <- sample(1:m, size = round(m/3), replace = FALSE, prob = rep(1/m, m))
rn <- iris[-val,]
# ??��??- iris[val,]

train_data$stage = as.factor(train_data$stage)
test_data$stage = as.factor(test_data$stage)


kknn <- kknn(stage~., train_data, test_data, k=7,distance = 2)

pre_knn <- fitted(kknn)
pre_knn <- predict(kknn,newdata = test_data)
table(test_data$stage, pre_knn,dnn=c("??ʵֵ","Ԥ?0","ֵ1???�oc(test_data$stage,as.numeric(pre_knn))
plot(knn_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue",main='knn?㷨(?ǹ?????)ROC????')
#2022.4/29
#
knn')
library('class')
set.seed(123456)
errRatio<-vector()   
for(i in 1:30){
  KnnFit<-knn(train=train_data[,-1001],test=test_data[,-1001],cl=train_data[,1001],k=i,prob=FALSE) 
  CT<-table(test_data[,1001],KnnFit) 
  errRatio<-c(errRatio,(1-sum(diag(CT))/sum(CT))*100)    
}
errRatio
plot(errRatio,type="l",xlab="???ڸ???K",yk-value??(%)",mpercentage�?????Ԥknn parameter?)O     
C')
knn_roc <- roc(EXPtest$category,as.numeric(KnnFit))
plot(knn_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue",main='knn?㷨ROC????')
train_data$stage = as.factor(train_data$stage)
test_data$stage = as.factor(test_data$stage)
library(kknn)
kknn <- kknn(stage~., train_data, test_data, k=7,distance = 2)
pre_knn <- fitted(kknn)
pre_knn <- predict(kknn,newdata = test_data)
table(test_data$stage, pre_knn,dnn=c("??ʵֵ","Ԥ?0","1"??߲?AU n <- predict(kknn,newdata = test_data,type='prob')
knn_roc <- roc(test_data$stage,as.numeric(pre_knn[,2]))
plot(knn_roc, print.auc=TRUE, auc.polygon=TRUE,  max.auc.polygon=TRUE,auc.polygon.col="skyblue",main='knn?㷨(?ǹ???? results