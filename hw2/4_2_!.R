#isye 6501 hw2 4.2
library(cluster)
library(NbClust)
library(fpc)
library(vegan)
#input data
data <- read.table('C:\\Users\\huangchengqi\\Desktop\\MS SCE\\19Fall\\ISYE6501\\hw2\\iris.txt',header=TRUE)
data2 <- as.matrix(data[,-5])

#pamk.best=pamk(data2)
#cat('number of clusters:',pamk.best$nc,'\n')
#plot(pam(data2,3))
#nb_clust <- NbClust(data2, distance='minkowski',min.nc=2,max.nc=10,method = 'kmeans',index='alllong', alphaBeale=0.1)

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
#k=2
model <- kmeans(data2,2)
pred <- model$cluster
result_matrix <- matrix(c(data[,5],pred),nrow=150,ncol=2)
#k=3
model2 <- kmeans(data2,3)
pred2 <- model2$cluster
result_matrix2 <- matrix(c(data[,5],pred2),nrow=150,ncol=2)
#pam,k=2
km_stats1 <- cluster.stats(dist(data2), model$cluster)
ave_sil1=km_stats1$avg.silwidth
ave_with1=km_stats1$average.within
ave_bet1=km_stats1$average.between
#pam,k=3
km_stats2 <- cluster.stats(dist(data2), model2$cluster)
ave_sil2=km_stats2$avg.silwidth
ave_with2=km_stats2$average.within
ave_bet2=km_stats2$average.between


result_table <- matrix(nrow=2,ncol=3)
colnames(result_table) <- c('Silwidth','AverageWithin','averageBetween')
result_table[1,1]=ave_sil1
result_table[1,2]=ave_with1
result_table[1,3]=ave_bet1
result_table[2,1]=ave_sil2
result_table[2,2]=ave_with2
result_table[2,3]=ave_bet2