data <- read.table('C:\\Users\\huangchengqi\\Desktop\\MS SCE\\19Fall\\ISYE6501\\hw2\\iris.txt',header=TRUE)
data2 <- as.matrix(data[,-5])
k_value_Set <- c(1:10)
kmeans_clustering <- function(k_value){
  model <- kmeans(data2,k_value)
  pred <- model@cluster
}
for(k in k_value_Set){
  
  
}

