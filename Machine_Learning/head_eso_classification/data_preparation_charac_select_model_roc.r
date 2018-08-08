rm(list = ls())
getwd()
library(caret)
load('./train_test.Rdata')

# 一：数据预处理
# ----------------------------------------------------
# 载入数据后需要加查数据形式及类型
str(test);str(train)
str(test[393]);str(train[415])
trainClass <- train[,415]
trainDescr <- train[,-415]
# 1. 删除常数自变量，或是方差极小的自变量
zerovar <- nearZeroVar(trainDescr)
newdata1 <- trainDescr
# 2. 删除强相关性的变量
descrCorr <- cor(newdata1)
highCorr <- findCorrelation(descrCorr, 0.9)
newdata2 <- newdata1[, -highCorr]
# 3. 多重共线性问题
comboInfo <- findLinearCombos(newdata2)
comboInfo$remove
newdata2 <- newdata2[, -comboInfo$remove]
# 4. 检查数据
sum(is.na(newdata2))
sum(complete.cases(newdata2))

# 二：特征选择
# -------------------------------------------------------
# 1. 定义程序测试数目
subsets <- c(2,4,8,16,32)
# 2. 然后定义控制参数，functions是确定用什么样的模型进行自变量排序，
# 本例选择的模型是随机森林即rfFuncs,可以选择的还有lmFuncs(线性回归).
# nbFuncs(朴素贝叶斯),treebagFuncs(装袋决策树),caretFuncs(自定义的训练模型)。
# method是确定用什么样的抽样方法，
# 本例使用cv即交叉检验, 还有提升boot以及留一交叉检验LOOCV
ctrl <- rfeControl(functions = rfFuncs, method = "repeatedcv", repeats = 3,
                   verbose = FALSE, returnResamp = "final")
# 3. 最后使用rfe命令进行特征选择，计算量很大，这得花点时间
Profile <- rfe(newdata2, trainClass, sizes = subsets, rfeControl = ctrl)
# 4. 观察结果
print(Profile)
plot(Profile)
Profile$optVariables


# ----------------------------------------
library(pROC)
gong <- intersect(Profile$optVariables,names(test))
newdata4 = newdata2[, gong]
df.train <- cbind(newdata4,trainClass)
names(df.train)[16] <- 'class'
str(df.train[,16])


control <- trainControl(method = 'repeatedcv', number = 10,
                        classProbs = T, summaryFunction = twoClassSummary)
glm.model <- train(class~., data=df.train, method='glm', 
                   metric='ROC', trControl = control)
svm.model <- train(class~., data=df.train, method='svmRadial', 
                   metric='ROC', trControl = control)
rpart.model <- train(class~., data=df.train, method='rpart', 
                     metric='ROC', trControl = control)
rf.model <- train(class~., data=df.train, method='rf', 
                  metric='ROC', trControl = control)


glm.probs <- predict(glm.model, test[,gong], type='prob')
svm.probs <- predict(svm.model, test[,gong], type='prob')
rpart.probs <- predict(rpart.model, test[,gong], type='prob')
rf.probs <- predict(rf.model, test[,gong], type='prob')
?predict()

glm.ROC <- roc(response = test[,'class'], predictor = glm.probs$eso,
               levels = levels(test[,'class']))
plot(glm.ROC, type='S', col='red')

svm.ROC <- roc(response = test[,'class'], predictor = svm.probs$eso,
               levels = levels(test[,'class']))
plot(svm.ROC, add=T, col='green')

rpart.ROC <- roc(response = test[,'class'], predictor = rpart.probs$eso,
                 levels = levels(test[,'class']))
plot(rpart.ROC, add=T, col='blue')

rf.ROC <- roc(response = test[,'class'], predictor = rf.probs$eso,
              levels = levels(test[,'class']))
plot(rf.ROC, add=T, col='yellow')


cv.values <- resamples(list(glm=glm.model, svm=svm.model, 
                            rpart=rpart.model, rf=rf.model))
summary(cv.values)

dotplot(cv.values, metric = 'ROC')
bwplot(cv.values, layout=c(3, 1))

confusionMatrix(svm.probs, test[,'class'])
# 三、建模与调参
# -----------------------------------------------------------
gong <- intersect(Profile$optVariables,names(test))

# 1. 数据进行划分，分成70%的训练样本和30%检验样本
newdata4 = newdata2[, gong]
inTrain <- createDataPartition(trainClass, p=7/10, list=FALSE)
trainx <- newdata4[inTrain,]
testx <- newdata4[-inTrain,]
trainy <- trainClass[inTrain]
testy <- trainClass[-inTrain]

# 2. 定义模型训练参数，method确定多次交叉检验的抽样方法，
# number确定了划分的重数， repeats确定了反复次数。
fitControl = trainControl(method = "repeatedcv", number = 10, 
                          repeats = 3,returnResamp = "all")
# 确定参数选择范围，本例建模准备使用gbm算法，相应的参数有如下三项
svmGrid = expand.grid(sigma=10^(-6:1),
                      C=10^(-10:10))
# 利用train函数进行训练，使用的建模方法为提升决策树方法，
svmFit1 = train(trainx,trainy,method='svmRadial', trControl=fitControl,
                tuneGrid = svmGrid, verbose = FALSE, prob = T)
# 观察结果
svmFit1
plot(svmFit1)


# 四：模型预测与检验
# ----------------------------------------------------
# 内部验证
svm.pred <- predict(svmFit1, testx, probability=T)
table(svm.pred, testy)
confusionMatrix(svm.pred, testy)

# 外部验证
pred <- predict(svmFit1, test[,gong], probability=T)
confusionMatrix(pred, test$class)
library(ROCR)
pred.prob <- attr(pred, 'probabilities')
pred.to.roc <- pred.prob[,2]
pred.rocr <- prediction(pred.to.roc, test$class)


df.train <- cbind(trainx, trainy)
names(df.train)[17] <- 'class'
library(e1071)
svm.model <- svm(class~., data=df.train, prob=T)
pred <- predict(svm.model, testx, probability=T)