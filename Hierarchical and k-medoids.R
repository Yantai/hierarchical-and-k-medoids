library("cluster")
library("multtest")
load("primate.rdata")
distance<-as.matrix(dist(primate))

for(n in c(10,10)){
  for(i in c(66:105)){
    primate[i,"gamma"]<-apply(data.frame(primate[c(as.integer(names(sort(distance[i,])[2:n]))),"gamma"]),2,median,na.rm=T)
  }
}
primate$gamma

data<-primate[,c(-1,-10,-11)]
d<-dist(data)

## hierarchical

# single
library(phyclust)
hc.single<-hclust(d,method="single")

store.error.single<-c()
for(i in 2:10){
  hc.single.cluster<-cutree(hc.single,k=i)
  error.single<-1-RRand(as.numeric(primate$class), as.numeric(hc.single.cluster))$Rand 
  store.error.single<-c(store.error.single,error.single)
}
store.error.single # 0.7509158 0.2573260 0.1639194 0.1686813 0.1730769 0.1752747 0.1802198 0.1827839 0.1782051

hc.single.cluster<-cutree(hc.single,k=4)
table.single.4<-table(primate$class,hc.single.cluster)
table.single.4

hc.single.cluster<-cutree(hc.single,k=5)
table.single.5<-table(primate$class,hc.single.cluster)
table.single.5

quartz()
plot(hc.single, hang=-1, labels=primate$class)

si.single.4<-silhouette(cutree(hc.single,k=4),dist=d)
summary(si.single.4)  
quartz()
plot(si.single.4)

si.single.5<-silhouette(cutree(hc.single,k=5),dist=d)
summary(si.single.5) 
quartz()
plot(si.single.5)

store.avg.width.single<-c()
for (i in 2:10){
  si<-silhouette(cutree(hc.single,k=i),dist=d)
  avg.width.single<-summary(si)$avg.width
  store.avg.width.single<-c(store.avg.width.single,avg.width.single)
}
store.avg.width.single # 0.5588978 0.5553507 0.5694269 0.4954185 0.4729187 0.3306103 0.2732398 0.2817304 0.2694172

# average

hc.average<-hclust(d,method="average")

store.error.average<-c()
for(i in 2:10){
  hc.average.cluster<-cutree(hc.average,k=i)
  error.average<-1-RRand(as.numeric(primate$class), as.numeric(hc.average.cluster))$Rand 
  store.error.average<-c(store.error.average,error.average)
}
store.error.average # 0.75091575 0.25732601 0.18113553 0.18772894 0.17527473 0.17967033 0.09157509 0.09304029 0.09450549

hc.average.cluster<-cutree(hc.average,k=4)
table.average.4<-table(primate$class,hc.average.cluster)
table.average.4

hc.average.cluster<-cutree(hc.average,k=5)
table.average.5<-table(primate$class,hc.average.cluster)
table.average.5

hc.average.cluster<-cutree(hc.average,k=8)
table.average.8<-table(primate$class,hc.average.cluster)
table.average.8

quartz()
plot(hc.average, hang=-1, labels=primate$class)

si.average.4<-silhouette(cutree(hc.average,k=4),dist=d)
summary(si.average.4)  
quartz()
plot(si.average.4)

si.average.5<-silhouette(cutree(hc.average,k=5),dist=d)
summary(si.average.5) 
quartz()
plot(si.average.5)

si.average.8<-silhouette(cutree(hc.average,k=8),dist=d)
summary(si.average.8) 
quartz()
plot(si.average.8)

store.avg.width.average<-c()
for (i in 2:10){
  si<-silhouette(cutree(hc.average,k=i),dist=d)
  avg.width.average<-summary(si)$avg.width
  store.avg.width.average<-c(store.avg.width.average,avg.width.average)
}
store.avg.width.average # 0.5588978 0.5553507 0.5867011 0.5706454 0.4960227 0.3537143 0.3440094 0.3249936 0.3022507

# complete

hc.complete<-hclust(d,method="complete")

store.error.complete<-c()
for(i in 2:10){
  hc.complete.cluster<-cutree(hc.complete,k=i)
  error.complete<-1-RRand(as.numeric(primate$class), as.numeric(hc.complete.cluster))$Rand 
  store.error.complete<-c(store.error.complete,error.complete)
}
store.error.complete # 0.6930403 0.2397436 0.1869963 0.1752747 0.1760073 0.1419414 0.1382784 0.1282051 0.1265568

hc.complete.cluster<-cutree(hc.complete,k=4)
table.complete.4<-table(primate$class,hc.complete.cluster)
table.complete.4

hc.complete.cluster<-cutree(hc.complete,k=5)
table.complete.5<-table(primate$class,hc.complete.cluster)
table.complete.5

hc.complete.cluster<-cutree(hc.complete,k=10)
table.complete.10<-table(primate$class,hc.complete.cluster)
table.complete.10

quartz()
plot(hc.complete, hang=-1, labels=primate$class)

si.complete.4<-silhouette(cutree(hc.complete,k=4),dist=d)
summary(si.complete.4)  
quartz()
plot(si.complete.4)

si.complete.5<-silhouette(cutree(hc.complete,k=5),dist=d)
summary(si.complete.5) 
quartz()
plot(si.complete.5)

si.complete.10<-silhouette(cutree(hc.complete,k=10),dist=d)
summary(si.complete.10) 
quartz()
plot(si.complete.10)

store.avg.width.complete<-c()
for (i in 2:10){
  si<-silhouette(cutree(hc.complete,k=i),dist=d)
  avg.width.complete<-summary(si)$avg.width
  store.avg.width.complete<-c(store.avg.width.complete,avg.width.complete)
}
store.avg.width.complete # 0.4944933 0.5660440 0.5762693 0.4550849 0.4500557 0.3002458 0.2984489 0.3015802 0.2832376

## K-medoids
library(fpc)

kmed<-pamk(data)
kmed$nc # 3

table(kmed$pamobject$clustering, primate$class)

quartz()
layout(matrix(c(1,2),1,2))
plot( kmed$pamobject )

error.kmed<-1-RRand(as.numeric(primate$class),as.numeric(kmed$pamobject$clustering))$Rand 
error.kmed # 0.1787546

kmed.5<-pamk(data,5)
error.kmed.5<-1-RRand(as.numeric(primate$class),as.numeric(kmed.5$pamobject$clustering))$Rand 
error.kmed.5 # 0.06923077

table(kmed.5$pamobject$clustering, primate$class)
quartz()
layout(matrix(c(1,2),1,2))
plot( kmed.5$pamobject )

gap.kmed<-clusGap(data,pam,K.max=10,B=100)
gap.kmed

quartz()
plot(gap.kmed, main = "Gap Statistic: k-medoids")
