# Chapter 10 Lab 1: Principal Components Analysis

states=row.names(USArrests)
states
names(USArrests)
apply(USArrests, 2, mean)
apply(USArrests, 2, var)

pr.out = prcomp(USArrests, scale=TRUE)
names(pr.out)
pr.out$center #mean
pr.out$scale  #st dev
pr.out$rotation

dim(pr.out$x)
biplot(pr.out, scale=0)

pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)

pr.out$sdev
pr.var=pr.out$sdev^2
pr.var
pve=pr.var/sum(pr.var)
pve #variance explained/variance total

plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')
a=c(1,2,8,-3)
cumsum(a)


# Chapter 10 Lab 2: Clustering

# K-Means Clustering

set.seed(2)
x=matrix(rnorm(50*2), ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4
km.out=kmeans(x,2,nstart=20)
km.out$cluster
plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=2", xlab="", ylab="", pch=20, cex=2)

set.seed(4)
km.out=kmeans(x,3,nstart=20)
km.out
plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=3", xlab="", ylab="", pch=20, cex=2)

set.seed(3)
km.out=kmeans(x,3,nstart=1)
km.out$tot.withinss
km.out=kmeans(x,3,nstart=20)
km.out$tot.withinss

# Hierarchical Clustering

hc.complete=hclust(dist(x), method="complete")
hc.average=hclust(dist(x), method="average")
hc.single=hclust(dist(x), method="single")
par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc.average, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single, main="Single Linkage", xlab="", sub="", cex=.9)

cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)
cutree(hc.single, 4)
xsc=scale(x)
plot(hclust(dist(xsc), method="complete"), main="Hierarchical Clustering with Scaled Features")
x=matrix(rnorm(30*3), ncol=3)
dd=as.dist(1-cor(t(x)))
plot(hclust(dd, method="complete"), main="Complete Linkage with Correlation-Based Distance", xlab="", sub="")


# Chapter 10 Lab 3: NCI60 Data Example

# The NCI60 data

library(ISLR)
nci.labs=NCI60$labs
nci.data=NCI60$data
dim(nci.data)
nci.labs[1:4]
table(nci.labs)

# PCA on the NCI60 Data

pr.out=prcomp(nci.data, scale=TRUE)
Cols=function(vec){
  cols=rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}
par(mfrow=c(1,2))
plot(pr.out$x[,1:2], col=Cols(nci.labs), pch=19,xlab="Z1",ylab="Z2")
plot(pr.out$x[,c(1,3)], col=Cols(nci.labs), pch=19,xlab="Z1",ylab="Z3")
summary(pr.out)
plot(pr.out)
pve=100*pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow=c(1,2))
plot(pve,  type="o", ylab="PVE", xlab="Principal Component", col="blue")
plot(cumsum(pve), type="o", ylab="Cumulative PVE", xlab="Principal Component", col="brown3")

# Clustering the Observations of the NCI60 Data

sd.data=scale(nci.data)
par(mfrow=c(1,3))
data.dist=dist(sd.data)
plot(hclust(data.dist), labels=nci.labs, main="Complete Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="average"), labels=nci.labs, main="Average Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="single"), labels=nci.labs,  main="Single Linkage", xlab="", sub="",ylab="")

hc.out=hclust(dist(sd.data))
hc.clusters=cutree(hc.out,4)
table(hc.clusters,nci.labs)
par(mfrow=c(1,1))
plot(hc.out, labels=nci.labs)
abline(h=139, col="red")

hc.out
set.seed(2)
km.out=kmeans(sd.data, 4, nstart=20)
km.clusters=km.out$cluster
table(km.clusters,hc.clusters)
hc.out=hclust(dist(pr.out$x[,1:5]))
plot(hc.out, labels=nci.labs, main="Hier. Clust. on First Five Score Vectors")
table(cutree(hc.out,4), nci.labs)


#ex.2

d = as.dist(matrix(c(0, 0.3, 0.4, 0.7, 
                     0.3, 0, 0.5, 0.8,
                     0.4, 0.5, 0.0, 0.45,
                     0.7, 0.8, 0.45, 0.0), nrow=4))
plot(hclust(d, method="complete"))

plot(hclust(d, method="single"))

plot(hclust(d, method="complete"), labels=c(2,1,4,3))

#ex.3
set.seed(1)
x = cbind(c(1, 1, 0, 5, 6, 4), c(4, 3, 4, 1, 2, 0))
x
#a

plot(x[,1], x[,2])
#b

labels = sample(2, nrow(x), replace=T)
labels

centroid1 = c(mean(x[labels==1, 1]), mean(x[labels==1, 2]))
centroid2 = c(mean(x[labels==2, 1]), mean(x[labels==2, 2]))
centroid1
centroid2
plot(x[,1], x[,2], col=(labels+1), pch=20, cex=2)
points(centroid1[1], centroid1[2], col=2, pch=4)
points(centroid2[1], centroid2[2], col=3, pch=4)
#d

euclid = function(a, b) {
  return(sqrt((a[1] - b[1])^2 + (a[2]-b[2])^2))
}
assign_labels = function(x, centroid1, centroid2) {
  labels = rep(NA, nrow(x))
  for (i in 1:nrow(x)) {
    if (euclid(x[i,], centroid1) < euclid(x[i,], centroid2)) {
      labels[i] = 1
    } else {
      labels[i] = 2
    }
  }
  return(labels)
}
labels = assign_labels(x, centroid1, centroid2)
labels
#e

last_labels = rep(-1, 6)
while (!all(last_labels == labels)) {
  last_labels = labels
  centroid1 = c(mean(x[labels==1, 1]), mean(x[labels==1, 2]))
  centroid2 = c(mean(x[labels==2, 1]), mean(x[labels==2, 2]))
  print(centroid1)
  print(centroid2)
  labels = assign_labels(x, centroid1, centroid2)
}
labels
#f

plot(x[,1], x[,2], col=(labels+1), pch=20, cex=2)
points(centroid1[1], centroid1[2], col=2, pch=4)
points(centroid2[1], centroid2[2], col=3, pch=4)


#ex.7

library(ISLR)
set.seed(1)
dsc = scale(USArrests)
a = dist(dsc)^2
b = as.dist(1 - cor(t(dsc)))
summary(b/a)

#ex. 8 

library(ISLR)
set.seed(1)
#a

pr.out = prcomp(USArrests, center=T, scale=T)
pr.var = pr.out$sdev^2
pve = pr.var / sum(pr.var)
pve
#b equation 10.8 diractly to calulate variance explained

loadings = pr.out$rotation
pve2 = rep(NA, 4)
dmean = apply(USArrests, 2, mean)
dsdev = sqrt(apply(USArrests, 2, var))
dsc = sweep(USArrests, MARGIN=2, dmean, "-")
dsc = sweep(dsc, MARGIN=2, dsdev, "/")
for (i in 1:4) {
  proto_x = sweep(dsc, MARGIN=2, loadings[,i], "*")
  pc_x = apply(proto_x, 1, sum)
  pve2[i] = sum(pc_x^2)
}
pve2 = pve2/sum(dsc^2)
pve2

#Ex. 9 hierarchical clustering
library(ISLR)
set.seed(2)
#a

hc.complete = hclust(dist(USArrests), method="complete")
plot(hc.complete)
#b

cutree(hc.complete, 3)
table(cutree(hc.complete, 3))
#c

#with scaling
dsc = scale(USArrests)
hc.s.complete = hclust(dist(dsc), method="complete")
plot(hc.s.complete)
#d

cutree(hc.s.complete, 3)
table(cutree(hc.s.complete, 3))
table(cutree(hc.s.complete, 3), cutree(hc.complete, 3))
#Scaling the variables effects the max height of the dendogram obtained from hierarchical clustering. From a cursory glance,
#it doesn't effect the bushiness of the tree obtained. However, it does affect the clusters obtained from cutting the 
#dendogram into 3 clusters. In my opinion, for this data set the data should be standardized because the data measured has 
#different units ($UrbanPop$ compared to other three columns).

#give to much weight to urban pop w/o scaling, I think. the rusults differ w/o scaling from those obtained using PCA

#ex. 10
#a

set.seed(2)
x = matrix(rnorm(20*3*50, mean=0, sd=1), ncol=50)
x[1:20, ] = x[1:20, ]-10
x[21:40, ] = x[21:40, ] +10
#x[41:60, ]
x[1:20, 11:20] = 1
x[21:40, 1:10] = 2
x[21:40, 21:31] = 2
x[41:60, 11:20] = 1
#The concept here is to separate the three classes amongst two dimensions.

#b

pca.out = prcomp(x)
summary(pca.out)
pca.out$x[,1:2]
plot(pca.out$x[,1:2], col=2:4, xlab="Z1", ylab="Z2", pch=19) 
#c

km.out = kmeans(x, 3, nstart=20)
table(km.out$cluster, c(rep(1,20), rep(2,20), rep(3,20)))
#Perfect match.

#d

km.out = kmeans(x, 2, nstart=20)
km.out$cluster
#All of one previous class absorbed into a single class.

#e

km.out = kmeans(x, 4, nstart=20)
km.out$cluster
#All of one previous cluster split into two clusters.

#f

km.out = kmeans(pca.out$x[,1:2], 3, nstart=20)
table(km.out$cluster, c(rep(1,20), rep(2,20), rep(3,20)))
#Perfect match, once again.

#g

km.out = kmeans(scale(x), 3, nstart=20)
km.out$cluster
#Poorer results than (b): the scaling of the observations effects the distance between them.

#ex. 11

#a

data = read.csv("E:/Documents/Scripts/R/An introduction to statistical learning/Ch10Ex11.csv", header=F)
dim(data)
#b

dd = as.dist(1 - cor(data))
plot(hclust(dd, method="complete"))
plot(hclust(dd, method="single"))
plot(hclust(dd, method="average"))
#Two or three groups depending on the linkage method.

#c

#To look at which genes differ the most across the healthy patients and diseased patients,
#we could look at the loading vectors outputted from PCA to see which genes are used to describe the variance the most.

pr.out = prcomp(t(data))
summary(pr.out)
total_load = apply(pr.out$rotation, 1, sum)
indices = order(abs(total_load), decreasing=T)
indices[1:10]
total_load[indices[1:10]]
#This shows one representation of the top 1% of differing genes.

#(*) I'm not sure this is the correct way to aggregate the loading vector.
