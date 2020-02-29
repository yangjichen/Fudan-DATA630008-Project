#三分类xgboost
library(car)
DealerRisk= read.csv('step1_data.csv',row.names = NULL)
# # method1暂时只用inside指标
#DealerRisk = DealerRisk[,1:25] 
# method2 用全部数据

# 这里筛选出低风险与高风险
data = subset(DealerRisk,风险评级=='低风险'|风险评级=='高风险'|风险评级=='中风险')
shape = dim(data)
# 这里为了让筛选后的dataframe行名连续，方便后续删除
row.names(data) = 1:shape[1]
#删除异常值点
#del = c(106,178,235,252,253)
#data = data[-del,]

#划分数据集=======================================================================
set.seed(1024)
idx = sample(nrow(data),size = round(nrow(data)*0.6), replace = F)
train = data[idx,]
test = data[-idx,]

#==================================================================================
#将自变量和因变量划分
label_cols = '风险评级'

train_label = train$风险评级
#将label转化为0-1变量
levels(train_label)[1] = 2
levels(train_label)[2] = 1
levels(train_label)[3] = 3
train_label  = as.numeric(train_label)-1

train = train[!(names(train) %in% label_cols)]

#0，1，2分别代表低中高
test_label = test$风险评级
levels(test_label)[1] = 2
levels(test_label)[2] = 1
levels(test_label)[3] = 3
test_label  = as.numeric(test_label)-1

test = test[!(names(test) %in% label_cols)]

#===============================================================================================
#将train中chr全部转化为factor类型变量
train = lapply(train, function(col) {
  if (!inherits(col, "character"))
    return (col)
  
  unclass(factor(col))
})

train = as.data.frame(train)
train = data.matrix(train)


#将test中chr全部转化为factor类型变量
test = lapply(test, function(col) {
  if (!inherits(col, "character"))
    return (col)
  
  unclass(factor(col))
})

test = as.data.frame(test)
test = data.matrix(test)
#为xgboost输入调整数据格式======================================================================
#下面使用xgboost
library(xgboost)
model = xgboost(data = as.matrix(train), label = as.numeric(train_label),
                max.depth = 10, eta = 0.09, nthread = 8, nround = 100, 
                subsample = 0.8,
                lambda = 0.5,
                num_class = 3,
                objective = "multi:softmax")
pred = predict(model, as.matrix(test))
#定义acc函数
acc = function(x_pred,x_label){
  ac = sum(as.numeric(x_pred==x_label))/length(x_pred)
  return(ac)
}

#这里查看混淆矩阵和准确率======================================================================
table(pred,test_label)
acc_test = acc(pred,test_label)
acc_test

#这里查看模型
model2 <- xgb.dump(model, with.stats = T)
model2[1:10] #This statement prints top 10 nodes of the model
#获取特征真实名称
names <- dimnames(data.matrix(train))[[2]]
# 计算特征重要性矩阵
importance_matrix <- xgb.importance(names, model = model)
# 制图
xgb.plot.importance(importance_matrix[1:15])
#==============================================================================================
#选择重要的变量后重新做模型
idx = importance_matrix$Feature[1:20]
train = train[,idx]
test = test[,idx]
model = xgboost(data = as.matrix(train), label = as.numeric(train_label),
                max.depth = 8, eta = 0.05, nthread = 8, nround = 100, 
                subsample = 1,
                lambda = 0.1,
                num_class = 3,
                objective = "multi:softmax")
pred = predict(model, as.matrix(test))
#定义acc函数
acc = function(x_pred,x_label){
  ac = sum(as.numeric(x_pred==x_label))/length(x_pred)
  return(ac)
}

#这里查看混淆矩阵和准确率
table(pred,test_label)
acc_test = acc(pred,test_label)
acc_test


