install.packages("xgboost")
library("xgboost")
install.packages("fastDummies")
library("fastDummies")

set.seed(1)
data = read.table('C:/Users/js367/random/course stuff/isye 8803/online/hw11/germancredit.txt')

head(data)

#prep the data by one-hot encoding
categorical_cols=c("V1", "V3", "V4","V6","V7","V9", "V10", "V12", "V14", "V15", "V17", "V19", "V20")
data=dummy_cols(data, select_columns = categorical_cols)
data=data[ , !(names(data) %in% categorical_cols)]
#ensure all data is numeric
data=as.data.frame(sapply(data, as.numeric))
head(data)

# xgboost requires 0 and 1 as the classification values, so
# convert 1s and 2s to 0s and 1s for the response variable

data$V21[data$V21==1]<-0
data$V21[data$V21==2]<-1



#separate the data into train and test
mask_train = sample(nrow(data), size = floor(nrow(data) * 0.8))
cred_train = data[mask_train,] # training data set
cred_train_labels=cred_train[,"V21"]
cred_train_data=cred_train[ , !(names(cred_train) %in% c("V21"))]



cred_test = data[-mask_train, ]  # test data set
cred_test_labels=cred_test[,"V21"]
cred_test_data=cred_test[ , !(names(cred_test) %in% c("V21"))]


#run xboost on training data
xgb <- xgboost(data = as.matrix(cred_train_data), label = as.matrix(cred_train_labels), max.depth = 20, eta = 0.2, nthread = 2, nrounds = 100, verbose=2, objective = "binary:logistic")

#prediction on test data
pred <- predict(xgb, as.matrix(cred_test_data))


# The following code is adapted from solution 10.3 and is used to calculate
# the ROC curve and area under the curve for this solution

# get unique threshold values from the predicted values
thr=sort(unique(c(pred,0,1)), decreasing=TRUE)

# set up variables for ROC and AUC
n = length(pred)
y = cred_test_labels
pos = length(y[y==1])  # number of positive values
neg = length(y[y==0])  # number of negative values
auc=0
last_tpr = 0
last_tnr = 1
# data frame to store results
res.df = data.frame(Thr=double(), TNR=double(), TPR=double(), Acc=double()) #, AUC=double(), ltnr=double(), ltpr=double())

# capture TNR, TPR, Accuracy, AUC contribution at each threshold from predicted values
for (i in thr){
  pred_round <- as.integer(pred > i) 
  acc = sum(y==pred_round)/n
  tp = sum(y[y==1]==pred_round[y==1])
  tn = sum(y[y==0]==pred_round[y==0])
  tpr = tp / pos
  tnr = tn / neg
  # calc AUC contribution
  if (i<1){
    auc = auc + (last_tpr*(last_tnr - tnr))
  }
  df = data.frame(Thr=i, TNR=tnr, TPR=tpr, Acc=acc) #, AUC=auc, ltnr=last_tnr, ltpr=last_tpr)
  res.df = rbind(res.df, df)
  last_tpr = tpr
  last_tnr = tnr
}
auc
# plot ROC
plot(res.df$TNR, res.df$TPR, type='l', xlim=c(1.002,0), ylim=c(0,1.002), 
     yaxs="i", xaxs="i", col='blue', ylab='Sensitivity (TPR)', xlab='Specificity (TNR)', main='ROC curve')
abline(1,-1, col='gray')
legend('center', legend=paste('AUC = ',round(auc,4)), bty='n')

res.df

#A threshold of between 0.1 and 0.2 seems to give the best trade-off between sensitivity and specificity

#comparison to 10.3 (logistic regression)
#AUC = 0.8119
#res.df:
##        Thr    TNR   TPR   Acc
## 2   0.9253 1.0000 0.000 0.693
## 22  0.6967 0.9904 0.196 0.747
## 42  0.6145 0.9615 0.348 0.773
## 62  0.5516 0.9135 0.457 0.773
## 82  0.4686 0.8510 0.533 0.753
## 102 0.3774 0.7981 0.630 0.747
## 122 0.3199 0.7404 0.717 0.733
## 142 0.2699 0.6779 0.793 0.713
## 162 0.2221 0.6058 0.848 0.680
## 182 0.1675 0.5288 0.891 0.640
## 202 0.1375 0.4423 0.913 0.587
## 222 0.1165 0.3654 0.957 0.547
## 242 0.0869 0.2740 0.967 0.487
## 262 0.0626 0.1875 0.989 0.433
## 282 0.0420 0.0962 1.000 0.373
## 302 0.0000 0.0000 1.000 0.307

#The xgboost model achieves similar results as the logistic regression model. The AUC for the model above
#is slightly higher than for the logistic regression model. The accuracy and TNR/TPR ratios seem to be
#better for the logistic regression model for most of the thresholds, however. Either model could be
#considered - xgboost will not always beat logistic regression, and both are typically worth 
#assessing for a classification problem.





























