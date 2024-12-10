#-------------------Question 9.1---------------
library(knitr)
library(DAAG)
library(GGally)

data <- read.table("/week_4_Homework-Summer21/week 4 data-summer/uscrime.txt", header = TRUE)
head(data)
summary(data)
ggpairs(data, columns = c("Po1","Po2", "U1", "U2", "Ed", "Ineq"))
pca <- prcomp(data[, -16], scale. = TRUE)
summary(pca)
plot(seq(1, 15, by = 1), pca$sdev, type = "b", xlab = "principal component", ylab = "standard deviation")

pca_data = data.frame(Crime = data$Crime, pca$x[, 1:8])
model <- lm(Crime ~ ., data = pca_data)
summary(model)

SStot <- sum((data$Crime - mean(data$Crime))^2)
R_squared <- rep(0,15)
adj_R_squared <- rep(0,15)
c_r2 <- rep(0,15)
adj_c_r2 <- rep(0,15)
for(i in 1:15){
  data2 <- data.frame(Crime = data$Crime, pca$x[, 1:i])
  model2 <- lm(Crime ~ ., data = data2)
  c <- cv.lm(data2, model2, m = 5)
  SSres_model2 <- sum(model2$residuals^2)
  R_squared[i] <- 1 - SSres_model2/SStot
  adj_R_squared[i] <- 1 - (1 - R_squared[i])*(nrow(data2) -1)/(nrow(data2)-ncol(data2))
  c_r2[i] <- 1 - attr(c,"ms")*nrow(data2) / SStot
  adj_c_r2[i] <- 1 - (1 - c_r2[i])*(nrow(data2) -1)/(nrow(data2)-ncol(data2))
}
plot(adj_c_r2, type="b", xlab="Number of PCs", ylab="Metrics", xlim=c(0, 16), ylim=c(0, 1))
lines(c_r2, type="b", col="green")
lines(adj_R_squared, type="b", col="red") 
lines(R_squared, type="b", col="blue")
legend("topleft", legend=c("CV Adj R^2","CV R^2","Adj R^2","R^2"),
       col=c("black","green","red","blue"), lty=1, cex=0.5)

means <- colMeans(data[, -16])
sd = apply(data[, -16], 2, sd)
ori_coef <- pca$rotation[, 1:8] %*% coef(model)[-1]
ori_coef
ori_coef_unscaled <- ori_coef / sd
ori_coef_unscaled 
kable(cbind(ori_coef,ori_coef_unscaled ))
a0 <- coef(model)[1] - sum(ori_coef * means / sd)
a0

data_pred <- data.frame(
  M = 14.0,
  So = 0,
  Ed = 10.0,
  Po1 = 12.0,
  Po2 = 15.5,
  LF = 0.640,
  M.F = 94.0,
  Pop = 150,
  NW = 1.1,
  U1 = 0.120,
  U2 = 3.6,
  Wealth = 3200,
  Ineq = 20.1,
  Prob = 0.04,
  Time = 39.0
)

pred <- sum(ori_coef_unscaled * data_pred) + a0
pred

#-------------------Question 10.1---------------
library(tree)
library(randomForest)

tree_model <- tree(Crime ~ ., data)
tree_model
summary(tree_model)
plot(tree_model)
text(tree_model)
tree_model$frame
tree_model$where
pred <- predict(tree_model)
plot(c(1:47), data$Crime, xlab = "data points", ylab = "Crime", type = "b", col = "red")
lines(pred, type = "b", col = "blue" )
legend("topright", legend=c("actual","predict"),
       col=c("red","blue"), lty=1, cex=0.5)
r2 <- 1 - 1896000 / sum((data$Crime - mean(data$Crime))^2)
r2

set.seed(4)
cv_tree <- cv.tree(tree_model)
cv_tree
plot(cv_tree$size, cv_tree$dev, type = "b", xlab = "tree size", ylab = "tree deviance")
plot(cv_tree$size, cv_tree$k, type = "b", xlab = "tree size", ylab = "k value")
tree_model_prune <- prune.tree(tree_model, best = 4)
plot(tree_model_prune)
text(tree_model_prune)
summary(tree_model_prune)
r2_pru <- 1 - 2633000 / sum((data$Crime - mean(data$Crime))^2)
r2_pru

reg_data <- data[data$Po1 < 7.65,]
summary(reg_data)
reg <- lm(Crime ~., reg_data)
summary(reg)

set.seed(4)
rf_model <- randomForest(Crime ~ ., data, importance = TRUE)
rf_model
fr2 <- 1- tail(rf_model$mse, n = 1) * nrow(data) / sum((data$Crime - mean(data$Crime))^2)
fr2
importance(rf_model)
summary(rf_model)
plot(rf_model)

rf_model2 <- randomForest(Crime ~ ., data, importance = TRUE, ntree = 40)
rf_model2
fr22 <- 1- tail(rf_model2$mse, n = 1) * nrow(data) / sum((data$Crime - mean(data$Crime))^2)
fr22
plot(rf_model2)
#--------------------Question 10.3.1--------------
library(ROCR)
library(dplyr)
germancredit <- read.table("/week_4_Homework-Summer21/week 4 data-summer/germancredit.txt", header = FALSE, stringsAsFactors = TRUE)
head(germancredit)
summary(germancredit)
germancredit$V21 = ifelse(germancredit$V21 == 2, 0 , 1) #response, 1=good, 0=bad
summary(germancredit)

set.seed(4)
train_index <- sample(seq_len(nrow(germancredit)), size = 0.8 * nrow(germancredit))
train_data <- germancredit[train_index, ]
test_data <- germancredit[-train_index, ]

cred_model <- glm(V21 ~ ., train_data, family = "binomial")
summary(cred_model)
probs <- predict(cred_model, test_data, type = "response")
probs_c <- ifelse(probs >= 0.5, 1, 0)
qua <- sum(probs_c == test_data$V21) / nrow(test_data)
qua

gc_data <- germancredit %>%
  mutate(V1 = ifelse(V1 == "A13", 1, 0)) %>%  #existing checking account >= 200 DM / salary assignments for at least 1 year
  mutate(V3 = ifelse(V3 == "A30", 1, 0)) %>% #no credits taken/ all credits paid back duly
  mutate(V6 = ifelse(V6 == "A61" | V6 == "A65", 0, 1)) %>% #Savings account/bonds >= 100DM
  mutate(V12 = ifelse(V12 == "A121", 1, 0)) #Property real estate
gc_data <- gc_data[,c("V1", "V3", "V5", "V6", "V8", "V12", "V21")]
colnames(gc_data) <- c("checking", "credit history", "credit amount", "savings", "installment rate", "Property", "target")
summary(gc_data)
head(gc_data)

set.seed(4)
train_index2 <- sample(seq_len(nrow(gc_data)), size = 0.8 * nrow(gc_data))
train_data2 <- gc_data[train_index2, ]
test_data2 <- gc_data[-train_index2, ]

cred_model2 <- glm(target ~ ., train_data2, family = "binomial")
summary(cred_model2)
probs2 <- predict(cred_model2, test_data2, type = "response")
probs2_c <- ifelse(probs2 >= 0.5, 1, 0)
qua2 <- sum(probs2_c == test_data2$target) / nrow(test_data2)
qua2

#-------------Question 10.3.2--------------

cost <- function(t) {
  pred <- as.factor(ifelse(probs > t, 1, 0))
  return(sum(pred == 0 & test_data$V21 == 1) * 1 + sum(pred == 1 & test_data$V21 == 0) * 5)
}
threshold <- seq(0, 1, by = 0.01)
costs <- sapply(threshold, cost)
costs
plot(threshold, costs, type = "b", col = "red")
best_threshold <- threshold[which.min(costs)]
best_threshold
min(costs)

prediction <- as.factor(ifelse(probs > best_threshold, 1, 0))
accuracy <- sum(prediction == test_data$V21) / nrow(test_data)
accuracy
xtabs(~V21 + prediction, data = test_data)

pred <- prediction(probs, test_data$V21)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)

auc <- performance(pred, "auc")
auc@y.values
