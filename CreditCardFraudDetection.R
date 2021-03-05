## ---- warning=FALSE, message=FALSE----
require(ggplot2)
require(dplyr)
require(data.table)
require(gridExtra)
require(corrplot)
################
#import data
#############
rawData <- read.csv("creditcard.csv") 
#################
#describe data
######################
str(rawData)


## ---- results="hide"--------------
head(rawData)


## ---------------------------------
summary(rawData)
table(is.na(rawData))


## ---------------------------------
table(rawData$Class)


## ---------------------------------
imbalance <- with(rawData,table(Class))
ggplot(data = rawData,aes(x = as.factor(Class),
                          fill = as.factor(Class))) +
  geom_bar() +
  ggtitle("Class Distributions \n (0: No Fraud || 1: Fraud)")
hist(rawData$Time)
par(mfrow = c(2,2))
hist(rawData$Amount)
hist(rawData$V1)
hist(rawData$V2)
hist(rawData$V3)


## ---------------------------------
# make dataset balanced
fraud <- filter(rawData, rawData$Class == 1)
nfraud <- filter(rawData, rawData$Class == 0)
set.seed(1234)
subfraud1 <- nfraud[sample(nrow(nfraud),size = nrow(fraud)),]
new_data <- rbind(fraud,subfraud1)
#Randomization
random_index <- sample(1:nrow(new_data), nrow(new_data))
new_data <- new_data[random_index, ]


## ---------------------------------
new_data %>% dim()


## ---------------------------------
# class distrubution in new data

table(new_data$Class)
par(mfrow = c(1,1))
ggplot(data = new_data,aes(x = as.factor(Class),fill = as.factor(Class))) +
  geom_bar() +
  ggtitle("Class Distributions in new data \n (0: No Fraud || 1: Fraud)")


## ---------------------------------
ggplot(new_data, aes(x = Amount, fill = as.factor(Class))) + geom_histogram(bins = 30) + 
  ggtitle("Histogram of transaction amount") + xlab("Amount")
ggplot(new_data, aes(x = Time, fill = as.factor(Class))) + geom_histogram(bins = 100) + 
  ggtitle("Histogram of transaction against time") + xlab("Time")


## ---- message = FALSE-------------
new_data$Class <- as.numeric(new_data$Class)
coroldfeatures <- cor(rawData)
corfeatures <- cor(new_data)
par(mfrow = c(1,1))

corrplot(coroldfeatures, method = "color", type = "full") + 
  title("heatmap of imbalance correlation matrix", line = 1.5)
corrplot(corfeatures, method = "color", type = "full") + 
  title("heatmap of balanced correlation matrix", line = 1.5)
par(mfrow = c(1,1))


## ---------------------------------
g0 <- ggplot(data = new_data, aes(x = as.factor(Class), y = V1, fill = as.factor(Class)))+ 
  geom_boxplot() + ggtitle("boxplot of V1 grouped by Class") + xlab("Class")
g1 <- ggplot(data = new_data, aes(x = as.factor(Class), y = V2, fill = as.factor(Class))) + 
  geom_boxplot() + ggtitle("boxplot of V2 grouped by Class") + xlab("Class")
g2 <- ggplot(data = new_data, aes(x = as.factor(Class), y = V3, fill = as.factor(Class))) + 
  geom_boxplot() + ggtitle("boxplot of V3 grouped by Class") + xlab("Class")
g3 <- ggplot(data = new_data, aes(x = as.factor(Class), y = V4, fill = as.factor(Class))) + 
  geom_boxplot() + ggtitle("boxplot of V4 grouped by Class") + xlab("Class")
g4 <- ggplot(data = new_data, aes(x = as.factor(Class), y = V5, fill = as.factor(Class))) + 
  geom_boxplot() + ggtitle("boxplot of V5 grouped by Class") + xlab("Class")
g5 <- ggplot(data = new_data, aes(x = as.factor(Class), y = V6, fill = as.factor(Class))) + 
  geom_boxplot() + ggtitle("boxplot of V6 grouped by Class") + xlab("Class")
g6 <- ggplot(data = new_data, aes(x = as.factor(Class), y = V7, fill = as.factor(Class))) + 
  geom_boxplot() + ggtitle("boxplot of V7 grouped by Class") + xlab("Class")
g7 <- ggplot(data = new_data, aes(x = as.factor(Class), y = V9, fill = as.factor(Class))) + 
  geom_boxplot() + ggtitle("boxplot of V9 grouped by Class") + xlab("Class")
g8 <- ggplot(data = new_data, aes(x = as.factor(Class), y = V10, fill = as.factor(Class))) + 
  geom_boxplot() + ggtitle("boxplot of V10 grouped by Class") + xlab("Class")
g9 <- ggplot(data = new_data, aes(x = as.factor(Class), y = V11, fill = as.factor(Class))) + 
  geom_boxplot() + ggtitle("boxplot of V11 grouped by Class") + xlab("Class")
g10 <- ggplot(data = new_data, aes(x = as.factor(Class), y = V12, fill = as.factor(Class))) + 
  geom_boxplot() + ggtitle("boxplot of V12 grouped by Class") + xlab("Class")
g11 <- ggplot(data = new_data, aes(x = as.factor(Class), y = V14, fill = as.factor(Class))) + 
  geom_boxplot() + ggtitle("boxplot of V14 grouped by Class") + xlab("Class")
g12 <- ggplot(data = new_data, aes(x = as.factor(Class), y = V16, fill = as.factor(Class))) + 
  geom_boxplot() + ggtitle("boxplot of V16 grouped by Class") + xlab("Class")
g13 <- ggplot(data = new_data, aes(x = as.factor(Class), y = V17, fill = as.factor(Class))) + 
  geom_boxplot() + ggtitle("boxplot of V17 grouped by Class") + xlab("Class")
g14 <- ggplot(data = new_data, aes(x = as.factor(Class), y = V18, fill = as.factor(Class))) + 
  geom_boxplot() + ggtitle("boxplot of V18 grouped by Class") + xlab("Class")
grid.arrange(g0, g1, g2, g3, ncol = 2)
grid.arrange(g4, g5, g6, g7, ncol = 2)
grid.arrange(g8, g9, g10,g11,ncol = 2)
grid.arrange(g12,g13,g14,ncol = 2)


## ----message=FALSE,warning=FALSE----
require(car)
model1 <- lm(Class ~ ., data = new_data)
summary(model1)


## ---------------------------------
par(mfrow = c(2,2))
plot(model1)


## ---------------------------------
vif1 <- vif(model1)
vif1[vif1 == max(vif1)]

model2 <- lm(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 +
               V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + 
               V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 +
               V24 + V25 + V26 + V27 + V28 + Amount, data = new_data)
vif2 <- vif(model2)
vif2[vif2 == max(vif2)]

model3 <- lm(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 +
               V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + 
               V16 + V18 + V19 + V20 + V21 + V22 + V23 +
               V24 + V25 + V26 + V27 + V28 + Amount, data = new_data)
vif3 <- vif(model3)
vif3[vif3 == max(vif3)]

model4 <- lm(Class ~ Time + V1 + V2 + V4 + V5 + V6 +
               V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + 
               V16 + V18 + V19 + V20 + V21 + V22 + V23 +
               V24 + V25 + V26 + V27 + V28 + Amount, data = new_data)
vif4 <- vif(model4)
vif4[vif4 == max(vif4)]

model5 <- lm(Class ~ Time + V1 + V2 + V4 + V5 + V6 +
               V8 + V9 + V10 + V11 + V13 + V14 + V15 + 
               V16 + V18 + V19 + V20 + V21 + V22 + V23 +
               V24 + V25 + V26 + V27 + V28 + Amount, data = new_data)
vif5 <- vif(model5)
vif5[vif5 == max(vif5)]

model6 <- lm(Class ~ Time + V1 + V2 + V4 + V5 + V6 +
               V8 + V9 + V10 + V11 + V13 + V14 + V15 + 
               V18 + V19 + V20 + V21 + V22 + V23 +
               V24 + V25 + V26 + V27 + V28 + Amount, data = new_data)
vif6 <- vif(model6)
vif6[vif6 == max(vif6)]

model7 <- lm(Class ~ Time + V1 + V2 + V4 + V5 + V6 +
               V8 + V9 + V11 + V13 + V14 + V15 + 
               V18 + V19 + V20 + V21 + V22 + V23 +
               V24 + V25 + V26 + V27 + V28 + Amount, data = new_data)
vif7 <- vif(model7)
vif7[vif7 == max(vif7)]

model8 <- lm(Class ~ Time + V1 + V4 + V5 + V6 +
               V8 + V9 + V11 + V13 + V14 + V15 + 
               V18 + V19 + V20 + V21 + V22 + V23 +
               V24 + V25 + V26 + V27 + V28 + Amount, data = new_data)
vif8 <- vif(model8)
vif8[vif8 == max(vif8)]

model9 <- lm(Class ~ Time + V1 + V4 + V5 + V6 +
               V8 + V9 + V11 + V13 + V15 + 
               V18 + V19 + V20 + V21 + V22 + V23 +
               V24 + V25 + V26 + V27 + V28 + Amount, data = new_data)
vif9 <- vif(model9)
vif9[vif9 == max(vif9)]

model10 <- lm(Class ~ Time + V1 + V4 + V6 +
               V8 + V9 + V11 + V13 + V15 + 
               V18 + V19 + V20 + V21 + V22 + V23 +
               V24 + V25 + V26 + V27 + V28 + Amount, data = new_data)
vif10 <- vif(model10)
vif10[vif10 == max(vif10)]#VIF value less than 5


## ----results="hide",warning=FALSE,message=FALSE----
step(model10,direction = "backward")


## ---------------------------------
model11 <- lm(formula = Class ~ V4 + V6 + V8 + V11 + V13 + V18 + V19 + V20 + 
                V23 + V25 + V26 + V28 + Amount, data = new_data)
par(mfrow = c(2,2))
plot(model11)
summary(model11)


## ----message=FALSE,warning=FALSE----
require(glmnet)
glmmodel2 <- glm(Class ~., data = new_data, family = "binomial", maxit = 100)


## ----warning=FALSE,message=FALSE----
vif11 <- vif(glmmodel2)
vif11[vif11 == max(vif11)]

glmmodel3 <- glm(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 +
               V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + 
               V16 + V18 + V19 + V20 + V21 + V22 + V23 +
               V24 + V25 + V26 + V27 + V28 + Amount, data = new_data,family = "binomial")
vif12 <- vif(glmmodel3)
vif12[vif12 == max(vif12)]

glmmodel4 <- glm(Class ~ Time + V1 + V3 + V4 + V5 + V6 +
                   V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + 
                   V16 + V18 + V19 + V20 + V21 + V22 + V23 +
                   V24 + V25 + V26 + V27 + V28 + Amount, data = new_data,family = "binomial")
vif13 <- vif(glmmodel4)
vif13[vif13 == max(vif13)]#VIF value is less than 5
glmmodel4$coefficients[glmmodel4$coefficients == max(glmmodel4$coefficients)]


## ----results = "hide", warning=FALSE, message=FALSE----
step(glmmodel4,direction = "backward")


## ----warning=FALSE,message=FALSE----
glmmodel5 <- glm(formula = Class ~ Time + V3 + V4 + V8 + V12 + V13 + V14 + 
                   V16 + V20 + V22 + V23 + V24 + V26 + Amount, 
                 data = new_data,family = "binomial")
summary(glmmodel5)


## ---------------------------------
par(mfrow = c(1,1))
plot(new_data$V4,glmmodel5$fitted.values)


## ---- warning=FALSE, message=FALSE----
#require(pROC)
require(ModelMetrics)
set.seed(43215)
subfraud2 <- nfraud[sample(nrow(nfraud),size = nrow(fraud)),]
new_data.test <- rbind(fraud,subfraud2)

probs <- predict(glmmodel5,newdata = rawData,type = "response")
lrProbs <- predict(model11,newdata = rawData,type = "response")

pred <- ifelse(probs > 0.5, "1", "0")
res.lm <- ifelse(lrProbs > 0.5, "1", "0")
table(rawData$Class,pred)
table(rawData$Class,res.lm)


## ---------------------------------
cm.glm <- as.data.frame(table(rawData$Class,pred))
cm.lm <- as.data.frame(table(rawData$Class,res.lm))

#Accuracy
print(paste("The Accuracy of logistic regression model is ",
            round(100*mean(pred == rawData$Class),digits = 2),
            "%",sep = ""))
print(paste("The Accuracy of ordinary linear regression model is ",
            round(100*mean(res.lm == rawData$Class),digits = 2),
            "%",sep = ""))
#Percision
Percision.glm <- cm.glm$Freq[cm.glm$Var1 ==1 & (cm.glm$Var1 == cm.glm$pred)]/sum(cm.glm$Freq[cm.glm$pred == 1])
print(paste("The Precision of logistic regression model is ",
            round(100*Percision.glm,digits = 2),
            "%",sep = ""))

Percision.lm <- cm.lm$Freq[cm.lm$Var1 ==1 & (cm.lm$Var1 == cm.lm$res.lm)]/sum(cm.lm$Freq[cm.lm$res.lm == 1])
print(paste("The Precision of ordinary linear regression model is ",
            round(100*Percision.lm,digits = 2),
            "%",sep = ""))
#Recall
Recall.glm <- cm.glm$Freq[cm.glm$Var1 ==1 & (cm.glm$Var1 == cm.glm$pred)]/sum(cm.glm$Freq[cm.glm$Var1 == 1])
print(paste("The Recall of logistic regression model is ",
            round(100*Recall.glm,digits = 2),
            "%",sep = ""))

Recall.lm <- cm.lm$Freq[cm.lm$Var1 ==1 & (cm.lm$Var1 == cm.lm$res.lm)]/sum(cm.lm$Freq[cm.lm$Var1 == 1])
print(paste("The Recall of ordinary linear regression model is ",
            round(100*Recall.lm,digits = 2),
            "%",sep = ""))
#F1 score
F1score.glm <- 2*(Recall.glm*Percision.glm)/(Recall.glm + Percision.glm)
F1score.lm <- 2*(Recall.lm*Percision.lm)/(Recall.lm + Percision.lm)
print(paste("The F1 score of logistic regression model is",
            round(F1score.glm,digits = 2),
            sep = " "))
print(paste("The F1 score of ordinary linear regression model is",
            round(F1score.lm,digits = 2),
            sep = " "))
Accuracy.mat <- c()
F1score.mat <- c()
Accuracy.mat <- c(mean(pred == rawData$Class),mean(res.lm == rawData$Class))
F1score.mat <- c(F1score.glm,F1score.lm)


## ---------------------------------
Estmators <- select(new_data,Time, V1, V3 , V4 , V5 , V6 ,
                      V8 , V9 , V10 , V11 , V12 , V13 , V14 , V15 , 
                      V16 , V18 , V19 , V20 , V21 , V22 , V23 ,
                      V24 , V25 , V26 , V27 , V28 , Amount)
Estmators <- as.matrix(Estmators)
OptmzLogReg <- glmnet(x=Estmators,y=new_data$Class,family = "binomial",alpha = 1)
summary(OptmzLogReg)
plot(OptmzLogReg$beta)


## ---------------------------------
Cvmodel <- cv.glmnet(Estmators,new_data$Class,alpha = 1)
plot(Cvmodel)
Lambda.1se <- Cvmodel$lambda.1se


## ---------------------------------
OptmzLogfit <- glmnet(Estmators,new_data$Class, family = "binomial",
                 alpa = 1,
                 lambda = Lambda.1se)
EstBeta <- OptmzLogfit$beta
EstBeta


## ----message=FALSE,warning=FALSE,results="hide"----
require(party)
require(randomForest)
require(ranger)
str(new_data$Class)
set.seed(2020)
random_index <- sample(1:nrow(new_data), nrow(new_data))
random_new_data <- new_data[random_index, ]
ind <- sample(2,nrow(random_new_data),replace = T,prob = c(0.7,0.3))
tree.train <- random_new_data[ind == 1,]
tree.test <- random_new_data[ind ==2,]


## ---------------------------------
tree.train$Class <- as.factor(tree.train$Class)
fraud.ctree <- ctree(Class~.,data=tree.train)
plot(fraud.ctree)


## ---------------------------------
tree.train.pred <- predict(fraud.ctree)
table(tree.train$Class,tree.train.pred)
#Accuracy
print(paste("The Accuracy of training set is",
            round(100*mean(tree.train.pred == tree.train$Class),digits = 2),
            "%",sep = " "))


## ---------------------------------
tree.valid <- predict(fraud.ctree, newdata = tree.test)
table(tree.test$Class,tree.valid)
print(paste("The Accuracy of valid set is",
            round(100*mean(tree.valid == tree.test$Class),
                  digits = 2), "%",sep = " "))


## ---------------------------------
tree.pred <- predict(fraud.ctree, newdata = rawData)
table(rawData$Class,tree.pred)
cm.tree <- as.data.frame(table(tree.pred,rawData$Class))
print(paste("The Accuracy of Decision tree model is",
            round(100*mean(tree.pred == rawData$Class),digits = 2),
            "%",sep = " "))
Accuracy.mat <- c(Accuracy.mat,mean(tree.pred == rawData$Class))


## ---------------------------------
print(paste("The F1 score of Decision tree model is",
            round(f1Score(rawData$Class,tree.pred),digits = 5),
            sep = " "))
F1score.mat <- c(F1score.mat,f1Score(rawData$Class,tree.pred))


## ---------------------------------
new_data$Class <- as.factor(new_data$Class)
fraud.cforest <- randomForest(Class ~., 
                              data = new_data, 
                              ntree = 1000,
                              importance = TRUE,
                              proximity = TRUE)
print(fraud.cforest)
plot(fraud.cforest)
legend("topright", inset=.05, title="Error type",
       c("1","OOB","0"), fill=c("green","black","red"), horiz=TRUE,
       cex = 0.7)


## ---------------------------------
which.min(fraud.cforest$err.rate[,1])
fraud.cforest$err.rate[which.min(fraud.cforest$err.rate[,1])]


## ---------------------------------
##RF Tuning
tree.train$Class <- as.factor(tree.train$Class)
tree.test$Class <- as.factor(tree.test$Class)
features <- setdiff(names(tree.train), "Class")

set.seed(2020)

RF.tune <- tuneRF(
  x          = tree.train[features],
  y          = tree.train$Class,
  ntreeTry   = 1000,
  mtryStart  = 5,
  stepFactor = 1.5,
  improve    = 0.01,
  trace      = FALSE      # to not show real-time progress 
)


## ---------------------------------
hyper_grid.RF <- expand.grid(
  mtry       = seq(20, 30, by = 2),
  node_size  = seq(1, 4, by = 1),
  sampe_size = c(.55, .632, .70, .80),
  OOB_error   = 0
)
nrow(hyper_grid.RF)


for(i in 1:nrow(hyper_grid.RF)) {
  
  # train model
  FR.tune <- ranger(
    formula         = Class ~ ., 
    data            = tree.train, 
    num.trees       = 1000,
    mtry            = hyper_grid.RF$mtry[i],
    min.node.size   = hyper_grid.RF$node_size[i],
    sample.fraction = hyper_grid.RF$sampe_size[i],
    seed            = 2020
  )
  
  # add OOB error to grid
  hyper_grid.RF$OOB_error[i] <- FR.tune$prediction.error
}
head(hyper_grid.RF[with(hyper_grid.RF, order(hyper_grid.RF$OOB_error)), ],10)


## ---------------------------------
FR.final <- ranger(
  formula         = Class ~ ., 
  data            = tree.train, 
  num.trees       = 1000,
  mtry            = 22,
  min.node.size   = 2,
  sample.fraction = 0.7,
  seed            = 2020
)




## ---------------------------------
forest.pred <- predict(FR.final, data = rawData)
table(rawData$Class,forest.pred$predictions)

print(paste("The Accuracy of Random Forests Model is ",
            round(100*mean(forest.pred$predictions == rawData$Class),digits = 2), "%",sep = ""))

print(paste("The F1 score of Random Forest model is",
            round(f1Score(rawData$Class,forest.pred$predictions),digits = 5),
            sep = " "))
Accuracy.mat <- c(Accuracy.mat,mean(forest.pred$predictions == rawData$Class))
F1score.mat <- c(F1score.mat,f1Score(rawData$Class,forest.pred$predictions))



## ----warning=FALSE,message=FALSE----
require(rsample)
require(gbm)
require(caret)
require(pdp)


## ---------------------------------
new_data <- rbind(fraud,subfraud1)
fraud.gbm <- gbm(
  formula = Class ~.,
  distribution = "bernoulli",
  data = new_data,
  n.trees = 10000,
  interaction.depth = 1,
  shrinkage = 0.001,
  cv.folds = 5,
  n.cores = NULL,
  verbose = FALSE
)
print(fraud.gbm)

min(fraud.gbm$cv.error)

gbm.perf(fraud.gbm, method = "cv")


## ---------------------------------
############################################
#tuning
###################################################

fraud.gbm2 <- gbm(
  formula = Class ~.,
  distribution = "bernoulli",
  data = new_data,
  n.trees = 5000,
  interaction.depth = 3,
  shrinkage = 0.1,
  cv.folds = 5,
  n.cores = NULL,
  verbose = FALSE
)
print(fraud.gbm2)

min(fraud.gbm2$cv.error)

gbm.perf(fraud.gbm2, method = "cv")


## ---------------------------------
#########
#grid search
########
hyper_grid <- expand.grid(
  shrinkage = c(0.01,0.1,.3),
  interaction.depth = c(1,3,5),
  n.minobsinnode = c(5,10,15),
  bag.fraction = c(.65, .8, 1),
  optimal_trees = 0,
  min_valid.error = 0
)

nrow(hyper_grid)
set.seed(2020)

random_index <- sample(1:nrow(new_data), nrow(new_data))
random_new_data <- new_data[random_index, ]

for(i in 1:nrow(hyper_grid)) {
  
  set.seed(123)
  
  # train model
  gbm.tune <- gbm(
    formula = Class ~ .,
    distribution = "bernoulli",
    data = random_new_data,
    n.trees = 5000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .8,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )

  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_valid.error[i] <- min(gbm.tune$valid.error)
}

hyper_grid %>% 
  dplyr::arrange(min_valid.error) %>%
  head(10)


## ---------------------------------
hyper_grid <- expand.grid(
  shrinkage = c(0.1,.2,.3),
  interaction.depth = c(3,4,5),
  n.minobsinnode = c(5,8,10),
  bag.fraction = c(.3, .65,1),
  optimal_trees = 0,
  min_valid.error = 0
)

nrow(hyper_grid)


for(i in 1:nrow(hyper_grid)) {
  
  set.seed(123)
  
  # train model
  gbm.tune <- gbm(
    formula = Class ~ .,
    distribution = "bernoulli",
    data = random_new_data,
    n.trees = 5000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .8,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_valid.error[i] <- min(gbm.tune$valid.error)
}

hyper_grid %>% 
  dplyr::arrange(min_valid.error) %>%
  head(10)


## ---------------------------------
fraud.gbm.final <- gbm(
  formula = Class ~ .,
  distribution = "bernoulli",
  data = random_new_data,
  n.trees = 31,
  interaction.depth = 5,
  shrinkage = 0.2,
  n.minobsinnode = 5,
  bag.fraction = 0.3,
  train.fraction = 1,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)
print(fraud.gbm.final)


## ---------------------------------
gbm.pred <- predict(fraud.gbm.final, n.trees = fraud.gbm.final$n.trees,newdata = rawData,type = "response")


gbm.result <- ifelse(gbm.pred >0.7,"1","0")

table(rawData$Class,gbm.result)
cm.gbm <- as.data.frame(table(rawData$Class,gbm.result))
mean(gbm.result == rawData$Class)
print(paste("The Accuracy of GBM is ",
            round(100*mean(gbm.result == rawData$Class),digits = 2),
            "%",sep = ""))
hist(gbm.pred)
#compare to the non-optimal gbm
gbm.pred2 <- predict(fraud.gbm2, n.trees = fraud.gbm2$n.trees,newdata = rawData)
gbm.result2 <- ifelse(gbm.pred2 >0.5,"1","0")
mean(gbm.result2 == rawData$Class)
table(rawData$Class,gbm.result2)
print(paste("The F1 score of Gradient Boosting Machine model is",
            round(f1Score(rawData$Class,as.numeric(gbm.result)),digits = 5),
            sep = " "))
Accuracy.mat <- c(Accuracy.mat,mean(gbm.result == rawData$Class))
F1score.mat <- c(F1score.mat,f1Score(rawData$Class,as.numeric(gbm.result)))


## ----warning=FALSE,message=FALSE----
require(vip)
vip(fraud.gbm.final) + ggtitle("The importance of features in fruad detection model(gbm)")


## ----warning=FALSE,message=FALSE----
require(pROC)
par(pty = "s")
lmroc <- roc(rawData$Class,probs,plot = TRUE,legacy.axes = TRUE,
    percent = TRUE, xlab= "False Positive Percentage",
    ylab = "True Positive Percentage",print.auc = TRUE,print.auc.x = 30)

plot.roc(rawData$Class,lrProbs,percent = TRUE, col = "#4daf4a",
         print.auc = TRUE,add = TRUE,print.auc.x = 30,print.auc.y = 45)

plot.roc(rawData$Class,as.numeric(tree.pred),percent = TRUE, col = "#7222e3",
         print.auc = TRUE,add = TRUE,print.auc.x = 30,print.auc.y = 55)

plot.roc(rawData$Class,as.numeric(forest.pred$predictions),percent = TRUE, col = "#e3e322",
         print.auc = TRUE,add = TRUE,print.auc.x = 30,print.auc.y = 60)

plot.roc(rawData$Class,as.numeric(gbm.pred),percent = TRUE, col = "#e60e11",
         print.auc = TRUE,add = TRUE,print.auc.x = 30,print.auc.y = 65)
legend(60,10, inset=.05, title="ROC Curve",
       legend = c("LR","GLM","DT","RF","GBM"),fill = c("green","black","blue","yellow","red"),
       cex = 0.45,
       horiz=TRUE)
Selection <- data.frame(Accuracy.mat,F1score.mat,row.names = c("glm","lm","DT","RF","GBM"))
print(Selection)

