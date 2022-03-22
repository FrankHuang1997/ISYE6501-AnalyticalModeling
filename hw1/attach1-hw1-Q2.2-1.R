# ISYE 6501 Homework 1 Question 2.2-1 ksvm with linear kernel

# Import data
library('kernlab')
raw_data <- read.table('C:/Users/alanl/Desktop/Gatech MSiA/Courses/ISyE 6501 Introduction to Analytics Modeling/hw1/data 2/credit_card_data-headers.txt',header=TRUE)
data = data.matrix(raw_data)

# Function for a ksvm model; using the simple linear kernel Vanilladot
ksvm_linear <- function(c_value) {
  model <- ksvm(data[,1:10],data[,11],type='C-svc',kernel='vanilladot',C=c_value,scaled=TRUE)
  x <- model@xmatrix[[1]]
  coe <- model@coef[[1]]
  a <- colSums(x*coe)
  a0 <- -model@b
  pred <- predict(model,data[,1:10])
  match_percentage <- (sum(pred==data[,11]))/nrow(data)*100
  result <- matrix(c(c_value,a,a0,match_percentage),nrow=1,ncol=13)
  return (result)
}

# Try different C values and output the results
c_value_set <- c(1,100,500,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000)
result_matrix <- matrix(nrow=13,ncol=13)
for (c_value in c_value_set) {
  result_matrix[which (c_value_set==c_value),]=ksvm_linear(c_value)
}
colnames(result_matrix) <- c('C_value','a1','a2','a3','a4','a5','a6','a7','a8','a9','a10','a0','points_matching_percentage(%)')

write.csv(result_matrix,'svm_linear.csv',row.names=FALSE,col.names=FALSE)
