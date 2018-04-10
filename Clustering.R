#### Clustering ----
sf = read.table(file.choose(), header=F)
colnames(sf) = c('area','perimeter','compactness',
                 'length','width','asymmetry_coef','groove_length','class')
sfz=data.frame(scale(sf[,-8]))
#sfz = sf[,-8]
set.seed(333)
nc=NbClust(sfz, min.nc=3, max.nc=6, method="kmeans")

set.seed(333) # 333 
res3 = kmeans(sfz, 3, nstart=25)  ## kmeans() returns an R object
res4 = kmeans(sfz, 4, nstart=25)
res5 = kmeans(sfz, 5, nstart=25)
res6 = kmeans(sfz, 6, nstart=25)
## i.	final cluster centers
res3$centers

res4$centers

res5$centers

res6$centers
## ii. the distributaion of each k
res3$size
res4$size
res5$size
res6$size
## iii. crosstab of class and distribution above
table(sf$class, res3$cluster, dnn=c('Class', 'cluster'))
table(sf$class, res4$cluster, dnn=c('Class', 'cluster'))
table(sf$class, res5$cluster, dnn=c('Class', 'cluster'))
table(sf$class, res6$cluster, dnn=c('Class', 'cluster'))
## iv. Export to a variable the distance of each point from its cluster.
sfz[,8]= sf[8]
sfz[,9]= res3$cluster
sfz[,10]= intra_distance(sfz,9,res3$centers,p=2)

sfz[,11]= res4$cluster
sfz[,12]= intra_distance(sfz,11,res4$centers,p=2)

sfz[,13]= res5$cluster
sfz[,14]= intra_distance(sfz,13,res5$centers,p=2)

sfz[,15]= res6$cluster
sfz[,16]= intra_distance(sfz,15,res6$centers,p=2)

colnames(sfz)[9:16] = c('K=3', 'km3_dist_from_cent',
                        'K=4', 'km4_dist_from_cent',
                        'K=5', 'km5_dist_from_cent',
                        'K=6', 'km6_dist_from_cent')
## choose the best number of clusters 


a = sum(sfz[,10])
b = sum(sfz[,12])
c = sum(sfz[,14])
d = sum(sfz[,16])
par(mfrow= c(1,1))
x = c(3,4,5,6)
y = c(a,b,c,d)
plot(x,y, xlim = range(2:7), xlab='Number of Cluster', ylab='Total Error', las=1, axes=F)
axis(side=1, at=c(3,4,5,6))
axis(side=2)
box()
lines(x,y)


#### sw/sb
SB =function(prop, cmean){
  res = 0
  Total_mean = colMeans(res3$centers)
  for (i in 1:length(prop)){
    res = res +  sum(abs(cmean[i,] - Total_mean)^2) * prop[i]
  }
  return(res)
}

SB3 = SB(res3$size,res3$centers)
SB4 = SB(res4$size,res4$centers)
SB5 = SB(res5$size,res5$centers)
SB6 = SB(res6$size,res6$centers)
SW3 = sum(sfz[,10]^2)
SW4 = sum(sfz[,12]^2)
SW5 = sum(sfz[,14]^2)
SW6 = sum(sfz[,16]^2)
a = SW3/SB3
b = SW4/SB4
c = SW5/SB5
d = SW6/SB6
par(mfrow= c(1,1))
x = c(3,4,5,6)
y = c(a,b,c,d)
plot(x,y, xlim = range(2:7), xlab='Number of Cluster', ylab='sw/sb', las=1, axes=F)
axis(side=1, at=c(3,4,5,6))
axis(side=2)
box()
lines(x,y)



## visualization data by first two CPA 
cp = princomp(sfz[,1:7], scores = T, cor=T)
plot(cp)
cp1 = cp$scores[,1]
cp2 = cp$scores[,2]
plot(cp1, cp2, pch=16, col=sfz[,9])
plot(cp1, cp2, pch=16, col=sfz[,11])
plot(cp1, cp2, pch=16, col=sfz[,13])
plot(cp1, cp2, pch=16, col=sfz[,15])




## HCA
?hclust
sf_hca = sf[1:7]
## single linkage 
hc = hclust(dist(sf_hca), method="single")
plot(hc, hang= -1)
rect.hclust(hc, k=3)
groups = cutree(hc, k=3)
table(sf$class, groups, dnn=c('Class', 'cluster'))

## complete linkage 
hc = hclust(dist(sf_hca), method="complete")
plot(hc, hang= -1)
rect.hclust(hc, k=3)
groups = cutree(hc, k=3)
table(sf$class, groups, dnn=c('Class', 'cluster'))

## compare
# proformance
group_3 = cutree(hc, k=3)
table(sf[,8], group_3)
group_4 = cutree(hc, k=4)
table(sf[,8], group_4)

