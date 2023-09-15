#cohen_dֵ
#install.packages("openxlsx")
library("openxlsx")
#install.packages("dplyr")
library("dplyr")
data[1,]
test <- read.xlsx("????.xlsx");test
test[-6,]
test <- as.matrix(test)
test[test=='stage i'] <- 99;test
fix(test)


stage1_data <- filter(test,AUY=='stage1')
stage1_data
stage1_data <- as.matrix(stage1_data)
stage1_data <- stage1_data[,-1];stage1_data
stage2_data <- filter(test,AUY=='stage2')
mean(stage1_data[1,])
mean1 <- rep(c(1),times=3)
for(i in 1:nrow(stage1_data)){

  mean1[i] <- mean(stage1_data[i,])
  
  
 

cn_d <- c()
for(i in 1:nrow(test)){
  stage1_data <- test[i,grep(levels(Group)[1],test[,2])]
  stage2_data <- test[i,-grep(levels(Group)[2],test[,2])]
  mean1 <- mean(stage1_data[,-c(1)])
  mean2 <- mean(stage2_data)
  sd1 <- sd(stage1_data)
  sd2 <- sd(stage2_data)
  sigma <- ((length(stage1_data)-1)*sd1^2 +(length(stage2_data)-1)*sd2^2)/(length(stage1_data)+length(stage2_data))
  cohen_d[i] <- abs(mean1-mean2)/sqrt(sigma)
}
#lev- rev(order(cohen_d))
genesname <- rownames(test)[order[1:2]];genesname

