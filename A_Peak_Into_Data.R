## initailize data and visualize relationships in both sepal & petal
irs = read.table(file.choose(), header = F, sep=',')
View(irs)
names(irs) =c('sepal_length','sepal_width','petal_length','petal_width','class')

attach(irs)
plot(sepal_length,sepal_width,col=class, xlab='length', ylab='width', main='Sepal')
legend('topright', legend = levels(class), col=unique(class), pch=1, cex=0.39,lty=1:1,lwd=2)

plot(petal_length,petal_width,col=class, xlab='length', ylab='width', main='Petal')
legend('topleft', legend = levels(class), col=unique(class), pch=1, cex=0.39,lty=1:1,lwd=2)


## histogram and density for each variable
par(mfrow=c(2,2))
hist(sepal_length, main='', prob=T)
lines(density(sepal_length, adjust=1), col='red')
hist(sepal_width, main='', prob=T)
lines(density(sepal_width, adjust=1), col='red')
hist(petal_length, main='', prob=T)
lines(density(petal_length, adjust=1), col='red')
hist(petal_width,main='', prob=T)
lines(density(petal_width, adjust=1), col='red')

## using boxplot to detect outliers
par(mfrow=c(1,1))
xlabel = c('overall', 'setosa', 'versicolor', 'virginica')
a = sepal_length
b = sepal_length[class=='Iris-setosa']
c = sepal_length[class=='Iris-versicolor']
d = sepal_length[class=='Iris-virginica']
boxplot(a,b,c,d, names=xlabel, main='sepal_length', las=1)
?boxplot
e = petal_length
f = petal_length[class=='Iris-setosa']
g = petal_length[class=='Iris-versicolor']
h = petal_length[class=='Iris-virginica']
boxplot(e,f,g,h, names=xlabel, main='petal_length', las=1)
