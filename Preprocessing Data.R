## initialize hospital data ----
age = c(23,23,27,27,39,41,47,49,50,52,54,54,56,57,58,58,60,61)
fat = c(9.5,26.5,7.8,17.8,31.4,25.9,27.4,27.2,31.2,34.6,42.5,28.8,33.4,30.2,34.1,32.9,41.2,35.7)
my_data = data.frame(age, fat)
par(mfrow=c(2,1), las=1)
## density plot 
y = seq(0, 0.06, by=0.03)
plot(density(fat), type='l', lwd=2,  main='Density Plot', xlab='', ylab='', col='red', xlim = c(0,70), axes=F)
axis(2, y)
lines(density(age), col='blue', lwd=2)
legend('topleft',inset=.01, cex=.8, box.lty=0, legend=c('%fat', 'age'),fill=c('red', 'blue'))
#boxplot
boxplot(age, fat, horizontal=T, names =c('age', '%fat'), main='Box Plot', ylim = c(0,70))
## z-score
my_data$z_age = round(scale(age, center=T, scale=T), 2)
my_data$z_fat = round(scale(fat, center=T, scale =T),2)
## Min-max
my_data$mm_age = round((age-min(age))/(max(age)-min(age)),2)
my_data$mm_fat = round((fat-min(fat))/(max(fat)-min(fat)),2)

## decimal scaling
myDS = function(x){return(x/10^nchar(as.character(round(max(abs(x)),0))))} 
my_data$ds_age = myDS(age)
my_data$ds_fat = myDS(fat)

View(my_data)
layout(1)
plot(age, fat, main='AGE vs %FAT', ylab='%fat')
abline(lm(fat~age), col='blue')

round(cor(data.frame(age,fat)),2)
round(cov(data.frame(age,fat)),2)

#### binning
############################################################
bintable = data.frame(sales = c(5, 10, 11, 13, 15, 35, 50, 55, 72, 92, 204, 215))
# create ordinals for bin
toOrdinal= function(num){
  ord = c('st', 'nd', 'rd', 'th')
  res = c()
  for (i in 1:num){
    lastchar = substr(i, start=nchar(i), stop=nchar(i))
    if (i%%100>3 & i%%100<21){
      res = append(res,paste(i, ord[4],sep=''))
    }
    else if (lastchar == '1'){
      res = append(res,paste(i, ord[1],sep=''))
    } 
    else if (lastchar == '2'){
      res = append(res,paste(i, ord[2],sep=''))
    }
    else if (lastchar == '3'){
      res = append(res,paste(i, ord[3],sep=''))
    }
    else {
      res = append(res,paste(i, ord[4],sep=''))
    }
  }
  return(res)
}
# smooth bin to its mean
mean_smooth = function(vec, values){
  temp = data.frame(vec, values)
  for (x in levels(temp$vec)){
    temp$res[temp$vec==x] = mean(temp$values[temp$vec==x])
  }
  return(temp$res)
}
# eqaul depth bin but sorted data required
EDbin = function(vec,bincount){
  temp = data.frame(vec)
  L = 1
  U = move = length(vec)/bincount
  for (i in 1:bincount){
    temp$res[L:U] = as.character(i)
    L = L+move
    U = U+move
  }
  return(factor(temp$res, labels = toOrdinal(bincount)))# factor() to level them 
}
# contrete the table 
bintable$EDbin = EDbin(bintable$sales, 3)
bintable$EDsmooth = mean_smooth(bintable$EDbin, bintable$sales)
bintable$EWbin = cut(bintable$sales, 3, labels = toOrdinal(3))
bintable$EWsmooth = round(mean_smooth(bintable$EWbin, bintable$sales),2)
View(bintable)
# compare distributions
par(mfrow= c(3,1))
hist(bintable$sales, main='Histogram of Sales', ylab='', xlab='Original', xlim=c(0,250), ylim=c(0,10), breaks=4)
hist(bintable$EDsmooth, main='', ylab='', xlab='Equal-Depth', xlim=c(0,250), ylim=c(0,10), breaks=8)
hist(bintable$EWsmooth, main='', ylab='', xlab='Equal-Width', xlim=c(0,250), ylim=c(0,10), breaks=4)

