#isye 6501 hw2 4.2
library(cluster)
library(NbClust)
library(fpc)
library(vegan)
#input data
data <- read.table('C:\\Users\\huangchengqi\\Desktop\\MS SCE\\19Fall\\ISYE6501\\hw2\\iris.txt',header=TRUE)
raw_data <- as.matrix(data[,-5])
data2 <- scale(raw_data, center=TRUE, scale=TRUE)

#pamk funcion
pamk.best1=pamk(data2[,c(1,2)])$nc
pamk.best2=pamk(data2[,c(1,3)])$nc
pamk.best3=pamk(data2[,c(1,4)])$nc
pamk.best4=pamk(data2[,c(2,3)])$nc
pamk.best5=pamk(data2[,c(2,4)])$nc
pamk.best6=pamk(data2[,c(3,4)])$nc
pamk.best7=pamk(data2[,c(1,2,3)])$nc
pamk.best8=pamk(data2[,c(1,2,4)])$nc
pamk.best9=pamk(data2[,c(1,3,4)])$nc
pamk.best10=pamk(data2[,c(2,3,4)])$nc
pamk.best11=pamk(data2[,c(1,2,3,4)])$nc
k_result <- matrix(c(pamk.best1,pamk.best2,pamk.best3,pamk.best4,pamk.best5,pamk.best6,pamk.best7,pamk.best8,pamk.best9,pamk.best10,pamk.best11),nrow=1,ncol=11)
colnames(k_result) <- c('sl+sw','sl+pl','sl+pw','sw+pl','sw+pw',
                        'pl+pw','sl+sw+pl','sl+sw+pw','sl+pl+pw',
                        'sw+pl+pw','sl+sw+pl+pw')