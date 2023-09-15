library("randomForest")
library("openxlsx")
install.packages("dyplr")
library("dplyr")

#Gene
Gene <- t(Gene_1000)
write.xlsx(Gene,"C:/Users/Dell/Desktop/??/Gene1000zhuanzhi.xlsx")
Gene <- Gene1000
Gene <- read.xlsx("Gene1000.xlsx")
#
library("dplyr")
s1 <- filter(Gene,stage=='1')
s2 <- filter(Gene,stage=='2')
row1 <- nrow(s1)
row2 <- nrow(s2)
train1 <- sample(row1,280)
m1<- s1[train1,]
train2 <- sample(row2,280)
m2 <- s2[train2,]
cc <- rbind(m1,m2)
write.xlsx(cc,"C:/Users/Dell/Desktop/??/?̶?2.xlsx")

Gene <- cc
a <- nrow(Gene)
train_sub <- sample(a,0.7*a)
train_data <- Gene[train_sub,]
test_data <- Gene[-train_sub,]
set.seed(12345) 
train_data$stage <- as.factor(train_data[,1000])
test_data $stage<-as.factor(test_data[,1000])
rFM<-randomForest(stage~.,data=train_data,importance=TRUE,proximity=TRUE);rFM


#
pre <- predict(rFM,newdata=test_data)
comb <- data.frame(prob=pre,obs=test_data$stage)
#
table(test_data$stage,pre,dnn = c(ֵ"0","1"))
ran <- roc(test_data$stage,as.numeric(pre))
plot(ran,print.auc=T,auc.polygon=T,grid=c(0.1,0.2),grid.col=c("green","red"),max.auc.polygon=T)




