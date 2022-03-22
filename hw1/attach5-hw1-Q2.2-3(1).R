# ISYE 6501 Homework 1 Question 2.2-3

# Import data
library('kknn')
data <- read.table('C:/Users/alanl/Desktop/Gatech MSiA/Courses/ISyE 6501 Introduction to Analytics Modeling/hw1/data 2/credit_card_data-headers.txt',header=TRUE)

# Function for kknn, which will return the percentage of matching points when we use every single data row as test data each time
# Set distance, kernel, and k as parameters of the function
kknn_model <- function(dis_type,kernel_type,k_value){
  pred <- vector()
  for (i in seq(1:nrow(data))){
    model <- kknn(R1~.,data[-i,],data[i,],distance=dis_type,kernel=kernel_type,k=k_value,scale=TRUE)
    if (fitted(model)>0.5){pred[i] <- 1}
    else {pred[i] <- 0}
  }
  matching_percentage <- sum(pred==data[,11])/nrow(data)*100
  return (matching_percentage)
}

# Calculate the matching percentage within different k values and draw a plot
# Using default settings: distance=2(Minkowski Distance) and kernel='optimal'
result_1 <- matrix(nrow=50,ncol=2)
dis_type <- 2
kernel_type <- "optimal"
for (k in seq(1:50)){
  result_1[k,1] <- k
  result_1[k,2] <- kknn_model(dis_type,kernel_type,k)
}
png('scatter_1.png')
plot(x=result_1[,1],y=result_1[,2],type='o',xlab='k values',ylab='matching percentage',ylim=c(78,86))
dev.off()

# Calculate the matching percentage within different k values and draw a plot
# Using some different combinations of distance and kernel for trails
# distance = 1(Euclidean Distance) or 2(Minkowski Distance)
# kernel =  'rectangular'(standard unweighted), 'triangular', or 'optimal'
result_2 <- matrix(nrow=50,ncol=7)
setting_combination <- matrix(nrow=6,ncol=2)
setting_combination[,1] <- c(1,2,1,2,1,2)
setting_combination[,2] <- c('rectangular','rectangular','triangular','triangular','optimal','optimal')
colnames(result_2) <- c('k','Euc.dis_rec','Min.dis_rec','Euc.dis_tri','Min.dis_tri','Euc.dis_opt','Min.dis_opt')
for (i in seq(1:nrow(setting_combination))){
  dis_type <- setting_combination[i,1]
  kernel_type <- setting_combination[i,2]
  for (k in seq(1:50)){
    result_2[k,1] <- k
    result_2[k,i+1] <- kknn_model(dis_type,kernel_type,k)
  }
}
png('scatter_2.png')
plot(x=result_2[,1],y=result_2[,2],type='l',col='black',xlab='k values',ylab='matching percentage',ylim=c(78,86))
lines(x=result_2[,1],y=result_2[,3],col='red')
lines(x=result_2[,1],y=result_2[,4],col='green')
lines(x=result_2[,1],y=result_2[,5],col='blue')
lines(x=result_2[,1],y=result_2[,6],col='grey')
lines(x=result_2[,1],y=result_2[,7],col='orange')
legend('bottom',colnames(result_2)[2:7],lty=1,col=c('black','red','green','blue','grey','orange'))
dev.off()

# Output the results of different distance and kernel trails
write.csv(result_2,'kknn_trail.csv',row.names=FALSE,col.names=TRUE)