#### initailize ----
library(rpart)
library(rpart.plot)
library(DMwR2)
library(NbClust)
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
## distance
intra_distance = function(df,k,standard,p){
  kmRES= df[,k]
  res = c()
  for (i in 1: length(kmRES)){
    res[i] = sum(abs(standard[kmRES[i],]-df[i,1:7])^p) ^(1/p)
  }
  return(res)
}

###########################################################################
#### Decision Tree ----

f = read.csv(file.choose(), header=F)
set.seed(0)
s = sample(19945, 12945)
train = f[s,]
test = f[-s,]


res = data.frame()
prev_train= 0
prev_test = 0

for (d_lim in seq(5, 30, by=5)){
for (p_lim in seq(20, 200, by=20)){
  c_lim = floor(p_lim/3)
  fit = rpart(V1~., data = train, method='class', parms = list(split = 'gini'),
              control = rpart.control(minsplit = p_lim, minbucket = c_lim, cp=0, maxdepth = d_lim, xval=10))
  
  train_res = predict(fit, train, type="class")
  train_misclf = mean(train_res != train$V1)*100
  test_res = predict(fit, test, type="class")
  test_misclf = mean(test_res != test$V1)*100
  cplx = length(fit$frame$var)
  row_num = dim(printcp(fit))[1]
  xerror = printcp(fit)[row_num,4]*100
  ## record it if the res is different 
  if (train_misclf!=prev_train & test_misclf!=prev_test){   
    res = rbind(res, c(cplx , d_lim, p_lim, c_lim, train_misclf, test_misclf, xerror))
    prev_train = train_misclf
    prev_test = test_misclf
    }
}}
colnames(res) =  c('leaf_nodes_count', 'max_depth','parent_size', 'children_size','train_misclf', 'test_misclf', 'xerror')


## rebuild the best tree
fit = rpart(V1~., data = train, method='class', parms = list(split = 'gini'),
            control = rpart.control(minsplit = 20, minbucket = 6, cp=0, maxdepth = 20, xval=10))
#xpred.rpart(fit, xval = 10, return.all = FALSE)
plotcp(fit)


## misclassification matrix
test_res = predict(fit, test, type="class")
test_fit.perf = table(test$V1, test_res, dnn=c("Actual", "Predicted"))
test_fit.perf
train_res = predict(fit, train, type="class")
train_fit.perf = table(train$V1, train_res, dnn=c("Actual", "Predicted"))
train_fit.perf

## interpretation of misclassification matrix
ASF(test_fit.perf)
ASF(train_fit.perf)
## most important variables
colors = c(rep("dodgerblue", 3), rep('grey', 20))
barplot(fit$variable.importance, col=colors, space = 1, main='Letter Recognition Data\nImportance of Variables', ylim= c(0,3000))
box()



###########################################################################
#### KNN ----

nn1 = kNN(V1~., train, test, stand=FALSE, k=1)
nn3 = kNN(V1~., train, test, stand=FALSE, k=3)
nn5 = kNN(V1~., train, test, stand=FALSE, k=5)
nn7 = kNN(V1~., train, test, stand=FALSE, k=7)
nn1.perf = table(test$V1, nn1 ,dnn=c("Actual", "Predicted"))
nn3.perf = table(test$V1, nn3 ,dnn=c("Actual", "Predicted"))
nn5.perf = table(test$V1, nn5 ,dnn=c("Actual", "Predicted"))
nn7.perf = table(test$V1, nn7 ,dnn=c("Actual", "Predicted"))
nn1.perf
nn3.perf
nn5.perf
nn7.perf
ASF(nn1.perf)
ASF(nn3.perf)
ASF(nn5.perf)
ASF(nn7.perf)

# Predictor Space???????
library(scatterplot3d)
attach(train)
scatterplot3d(V10,V11,V14, main="Predictor Space" ,pch = '.', angle = 30,type="h")
###########################################################################
