library(xgboost)
#还是使用step2筛选过的数据
library(car)
data= read.csv('step2_data.csv',row.names = NULL)
#使用相同seed划分数据集
set.seed(1024)
idx = sample(nrow(data),size = round(nrow(data)*0.6), replace = F)
train = data[idx,]
test = data[-idx,]

#===============================================================================================
#将自变量和因变量划分
label_cols = '风险评级'

train_label = train$风险评级
train = train[!(names(train) %in% label_cols)]

test_label = test$风险评级
test = test[!(names(test) %in% label_cols)]

#===============================================================================================
#将train中chr全部转化为factor类型变量
train = lapply(train, function(col) {
  if (!inherits(col, "character"))
    return (col)
  
  unclass(factor(col))
})
#将test中chr全部转化为factor类型变量
test = lapply(test, function(col) {
  if (!inherits(col, "character"))
    return (col)
  
  unclass(factor(col))
})
#为xgboost输入调整数据格式======================================================================
train = as.data.frame(train)
train = data.matrix(train)
#将label转化为0-1变量
levels(train_label)[1] = 0
levels(train_label)[2] = 1
train_label  = as.numeric(train_label)-1


test = as.data.frame(test)
test = data.matrix(test)
#将label转化为0-1变量
levels(test_label)[1] = 0
levels(test_label)[2] = 1
test_label  = as.numeric(test_label)-1



#===============================================================================================
#下面使用xgboost
model = xgboost(data = as.matrix(train), label = as.numeric(train_label),
                max.depth = 10, eta = 0.05, nthread = 8, nround = 300, 
                subsample = 0.9,
                lambda = 3,
                objective = "binary:logistic")
prob = predict(model, as.matrix(test))

preds01 = ifelse(prob <= 0.5, 0, 1)
table(test_label, preds01)

