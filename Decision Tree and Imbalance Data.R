#### initialize libs and some functions ----
library(rpart)
library(rpart.plot)
## accuracy, sensitivity, specificity
ASF = function(tbl){
  res = matrix(0,0,2)
  for (n in 1:sqrt(length(tbl))){
    TP = tbl[n,n]   ## [row,col]
    TN = sum(tbl[-n,-n])
    FP = sum(tbl[-n,n])
    FN = sum(tbl[n,-n])
    sen = TP/(TP+FN)
    spc = TN/(TN+FP)
    res = rbind(res, c(sen, spc))
  }
  colnames(res) = c('Sensitivity', 'Specificity')
  rownames(res) = labels(tbl)$Actual
  print(paste('Accuracy:', sum(diag(tbl))/sum(tbl)))
  return(res)
}
## line lowest point, mark node#, and return parent and children size
aux_line = function(df, target){
  min_val = min(target)
  node_count = df[target==min_val,][1,1]
  p_size = df[target==min_val,][1,2]
  c_size = df[target==min_val,][1,3]
  abline(h=min_val, col = 'black', lwd = 0.5)
  abline(v=node_count, col='black', lwd = 1.5)
  text(node_count, 0, as.character(node_count), col = "red", cex=1.5, ps = 0.1, font = 4)
  return(c(p_size, c_size))
}
## random oversampling
my_os_func = function(df,target,seed){
  res = df
  NAME = names(table(target))
  max_amt = max(table(target))
  if (!missing(seed)){set.seed(seed)}
  for(n in NAME){
    temp = df[target==n,]
    cur_amt = dim(temp)[1]
    # iter = time(max_amt,cur_amt)
    iter = max_amt-cur_amt
    if(iter){for(i in 1:iter){res = rbind(res, temp[sample(cur_amt,1),])}} # randomly
  }
  return(res)
} 


#######################################################################################################
## Decision Tree ----
wine = read.csv(file.choose(), header=T, sep=';')
## distribution of wine data
table(wine$quality)
barplot(table(wine$quality), ylim=c(0,700), main='Distribution of Red Wine Data',
        xlab='Quality Class', ylab='Frequency', las=1)
box()
##
set.seed(6)
s = sample(1599,1000)
train = wine[s,]
test = wine[-s,]

res2 = data.frame()
## loop throgh all possible tree
for (p_lim in 500:1){
  c_lim = floor(p_lim/3)
  fit = rpart(quality~., train, method='class', parms = list(split = 'gini'),
                      control = rpart.control(minsplit = p_lim, minbucket = c_lim, cp=0, maxdepth=8))
  
  train_res = predict(fit, train, type="class")
  train_misclf = mean(train_res != train[,12])*100
  test_res = predict(fit, test, type="class")
  test_misclf = mean(test_res != test[,12])*100
  t = table(fit$frame$var)
  cplx = t[names(t)=='<leaf>']
  res2 = rbind(res2, c(cplx, p_lim, c_lim, train_misclf, test_misclf))
}
rm(c_lim, cplx, fit, p_lim, t, test_misclf, test_res, train_misclf, train_res)
colnames(res2) = c('leaf_nodes_count', 'parent_size', 'children_size','train_misclf', 'test_misclf')

x = res2$leaf_nodes_count
y1 = res2$train_misclf
y2 = res2$test_misclf


## overfitting plot
plot(x, y1, main = 'Original Red Wine Data\nDecision Tree Interprtation',
     xlab='leaf_nodes_count', xlim=range(x), ylab ='Error(%)', ylim=c(0,55), type='n', las = 1)
lines(x[order(x)], y1[order(x)], col='blue', lwd = 2)
lines(x[order(x)], y2[order(x)], col='red', lwd = 2)
## auxiliary lines and text
pc_size = aux_line(res2, res2$test_misclf) ## return [minsplit, minbucket] at that point
legend('topright', legend=c('Train','Test', 'overfit_point'), fill=c('blue', 'red', 'black'), cex=0.7)



## recreate the best tree
## node#:11 ----> minsplit:132 , minbucket:44
dtree = rpart(quality~., train, method='class', parms = list(split = 'gini'),
                  control = rpart.control(minsplit =pc_size[1],minbucket =pc_size[2], cp=0, maxdepth=8))
## print tree
prp(dtree, type=1, extra=101,fallen.leaves=F, main="Original Red Wine Data\nDecision Tree", faclen=0)


## create a confusion matrix
test_res = predict(dtree, test, type="class")
dtree.perf = table(test$quality, test_res, dnn=c("Actual", "Predicted"))
## show performance
dtree.perf
ASF(dtree.perf) 

## importance of raviables
colors = c(rep("cyan4", 3), rep('grey', length(dtree$variable.importance)-3))
barplot(dtree$variable.importance, col=colors, space = 1,
        main='Original Red Wine Data\nImportance of Variables', ylim=c(0,80), las=2)
box()

#######################################################################################################
#######################################################################################################
## DT with binning data ----
## bin quality 3,4,5 -> low, and quality 6,7,8 -> high ##
wine_bin = wine
wine_bin[,12] = ifelse(wine_bin$quality %in% c(3,4,5), 'low', 'high')
train_bin = wine_bin[s,]  ## set.seed(6), s = sample(1599,1000)
test_bin = wine_bin[-s,]

## distribution plot of binned data
barplot(rev(table(wine_bin$quality)), ylim=c(0, 1000), main='Distribution of Binned Red Wine Data',
        xlab='Quality Class', ylab='Frequency', las=1)
box()

res3 = data.frame()
## loop throgh all possible trees
for (p_lim in 500:1){
  c_lim = floor(p_lim/3)
  fit = rpart(quality~., train_bin, method='class', parms = list(split = 'gini'),
                      control = rpart.control(minsplit = p_lim, minbucket = c_lim, cp=0, maxdepth=8))
  
  
  train_res = predict(fit, train_bin, type="class")  ## not acc but an array
  train_misclf = mean(train_res != train_bin[,12])*100
  test_res = predict(fit, test_bin, type="class")
  test_misclf = mean(test_res != test_bin[,12])*100
  t = table(fit$frame$var)
  cplx = t[names(t)=='<leaf>']
  res3 = rbind(res3, c(cplx, p_lim, c_lim, train_misclf, test_misclf))
}
colnames(res3) = c('leaf_nodes_count', 'parent_size', 'children_size','train_misclf', 'test_misclf')


x = res3$leaf_nodes_count
y1 = res3$train_misclf
y2 = res3$test_misclf


## overfitting plot
plot(x, y1, main = 'Binned Red Wine Data\nDecision Tree Interprtation',
     xlab='leaf_nodes_count', xlim=range(x), ylab ='Error(%)', ylim=c(0,60), type='n', las = 1)
lines(x[order(x)], y1[order(x)], col='blue', lwd = 2)
lines(x[order(x)], y2[order(x)], col='red', lwd = 2)
## auxiliary lines and text
pc_size = aux_line(res3, res3$test_misclf)
legend('topright', legend=c('Train','Test', 'overfit_point'), fill=c('blue', 'red', 'black'), cex=0.7)



## recreate the best tree
## node#:8 ----> minsplit:111 , minbucket:37
dtree_bin = rpart(quality~., train_bin, method='class', parms = list(split = 'gini'),
                  control = rpart.control(minsplit =pc_size[1],minbucket =pc_size[2], cp=0, maxdepth = 8))

## print tree
prp(dtree_bin, type=1, extra=101,fallen.leaves=F, main="Binned Red Wine Data\nDecision Tree", faclen=0)

## create a confusion matrix
test_res = predict(dtree_bin, test, type="class")
dtree_bin.perf = table(test_bin$quality, test_res, dnn=c("Actual", "Predicted"))
## performance and confusion matrix
dtree_bin.perf
ASF(dtree_bin.perf) 

## importance of raviables
colors = c(rep("cyan4", 3), rep('grey', length(dtree_bin$variable.importance)-3))
barplot(dtree_bin$variable.importance, col=colors, space = 1,
        main='Binned Red Wine Data\nImportance of variables', ylim=c(0,100), las=2)
box()

#######################################################################################################
#######################################################################################################
## DT with oversampling ----
## create a oversampled training set
train_over = my_os_func(train, train$quality, seed = 891294815)
## checking oversampling quantity
table(train$quality)
table(train_over$quality)

res4 = data.frame()
## loop through the tree 
for (p_lim in 500:1){
  c_lim = floor(p_lim/3)
  fit = rpart(quality~., train_over, method='class', parms = list(split = 'gini'),
              control = rpart.control(minsplit = p_lim, minbucket = c_lim, cp=0, maxdepth = 8))
  
  train_res = predict(fit, train, type="class")  ## not acc but an array
  train_misclf = mean(train_res != train[,12])*100
  test_res = predict(fit, test, type="class")
  test_misclf = mean(test_res != test[,12])*100
  t = table(fit$frame$var)
  cplx = t[names(t)=='<leaf>']
  res4 = rbind(res4, c(cplx, p_lim, c_lim, train_misclf, test_misclf))
}
colnames(res4) = c('leaf_nodes_count', 'parent_size', 'children_size','train_misclf', 'test_misclf')

x = res4$leaf_nodes_count
y1 = res4$train_misclf
y2 = res4$test_misclf

## overfitting plot
plot(x, y1, main = 'Oversampling Red Wine Data\nDecision Tree Interprtation',
     xlab='leaf_nodes_count', xlim=range(x), ylab ='Error(%)', ylim=c(0,100), type='n', las = 1)
lines(x[order(x)], y1[order(x)], col='blue', lwd = 2)
lines(x[order(x)], y2[order(x)], col='red', lwd = 2)
## auxiliary lines and text and get parent/children size
pc_size = aux_line(res4, res4$test_misclf)
legend('topright', legend=c('Train','Test', 'overfit_point'), fill=c('blue', 'red', 'black'), cex=0.7)


## recreate the best tree
## node#:10 ----> minsplit:497 , minbucket:165
dtree_over = rpart(quality~., train_over, method='class', parms = list(split = 'gini'),
               control = rpart.control(minsplit =pc_size[1],minbucket =pc_size[2], cp=0, maxdepth = 8))
## print tree
prp(dtree_over, type=1, extra=101,fallen.leaves=F, main="Oversampling Red Wine Data\nDecision Tree", faclen=0)

## create a confusion matrix
test_res = predict(dtree_over, test, type="class")
dtree_over.perf = table(test$quality, test_res, dnn=c("Actual", "Predicted"))

## compare to original tree
dtree.perf
ASF(dtree.perf)

dtree_over.perf
ASF(dtree_over.perf)

## importance of raviables
colors = c(rep("cyan4", 3), rep('grey', length(dtree_over$variable.importance)-3))
barplot(dtree_over$variable.importance, col=colors, space = 1, main='Importance of variables', las=2)
box()

