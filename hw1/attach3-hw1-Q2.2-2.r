#ISyE 6501 Homework 1 Question 2.2-2 ksvm model with different types of kernel
#import data
credit_card_data_headers <- read.table('/Users/luckyfisher/Desktop/Courses/ISyE\ 6501/hw1/data\ 2.2/credit_card_data-headers.txt', header = TRUE, sep = "\t")
library(kernlab)
#define ksvm model
ksvm_testKernel <- function(kernel_type) {
  model <- ksvm(as.matrix(credit_card_data_headers[,1:10]),as.factor(credit_card_data_headers[,11]), type='C-svc', kernel=kernel_type, C=100, scaled=TRUE)
  a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
  a0 <- -model@b
  pred <- predict(model, credit_card_data_headers[,1:10])
  fitting <- sum(pred == credit_card_data_headers[,11]) / nrow(credit_card_data_headers)
  kernel_effect <- matrix(c(kernel_type, a, a0, fitting), nrow = 1, ncol = 13)
  return(kernel_effect)
}
#input different kernel functions
kernel_set <-
  c('rbfdot', 'polydot', 'vanilladot', 'tanhdot', 'laplacedot', 'besseldot', 'anovadot', 'splinedot')
kernel_table <- matrix(nrow=8,ncol=13)
for (k_value in kernel_set) {
  kernel_table[which(kernel_set==k_value),]=ksvm_testKernel(k_value)
}
colnames(kernel_table)<- 
  c('Kernel_Type', 'a1', 'a2', 'a3', 'a4', 'a5', 'a6', 'a7', 'a8', 'a9', 'a10', 'a0', 'Fitting',)
write.csv(kernel_table, 'svm_testKernel.csv')
show_result <- read.csv('svm_testKernel.csv')
print(show_result)

