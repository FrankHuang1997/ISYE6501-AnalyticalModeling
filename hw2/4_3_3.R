#isye 6501 hw2 4.2
library(cluster)
library(NbClust)
library(fpc)
library(vegan)
#input data
data <- read.table('C:\\Users\\huangchengqi\\Desktop\\MS SCE\\19Fall\\ISYE6501\\hw2\\iris.txt',header=TRUE)
raw_data <- as.matrix(data[,-5])
data2 <- scale(raw_data, center=TRUE, scale=TRUE)

#elbow method, using sum of squared error
wssplot <- function(data,nc=10,seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data,centers=i)$withinss)
  }
  plot(1:nc,wss,type='b',xlab='number of clusters',
       ylab='Within groups sum of squares' )
}
wssplot(data2)

wssplot_2 <- function(data,color_type){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:10){
    set.seed(1234)
    wss[i] <- sum(kmeans(data,centers=i)$withinss)
  }
  lines(1:10,wss,type='b',col=color_type,xlab='number of clusters',
       ylab='Within groups sum of squares' )
}
wssplot_2(data2[,c(1,2)],2)
wssplot_2(data2[,c(1,3)],3)
wssplot_2(data2[,c(1,4)],4)
wssplot_2(data2[,c(2,3)],5)
wssplot_2(data2[,c(2,4)],6)
wssplot_2(data2[,c(3,4)],7)
wssplot_2(data2[,c(1,2,3)],8)
wssplot_2(data2[,c(2,3,4)],9)
wssplot_2(data2[,c(1,2,4)],10)
wssplot_2(data2[,c(1,3,4)],11)


