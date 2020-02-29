#三分类softmax
library(car)
DealerRisk= read.csv('step1_data.csv',row.names = NULL)
# 暂时只用inside指标
# DealerRisk = DealerRisk[,1:25] 
# 这里筛选出低风险与高风险
data = subset(DealerRisk,风险评级=='低风险'|风险评级=='高风险'|风险评级=='中风险')
shape = dim(data)
# 这里为了让筛选后的dataframe行名连续，方便后续删除
row.names(data) = 1:shape[1]

#划分数据集=======================================================================
set.seed(1024)
idx = sample(nrow(data),size = round(nrow(data)*0.6), replace = F)
train = data[idx,]
test = data[-idx,]

label_cols = '风险评级'

train_label = train$风险评级
#将label转化为0-1变量
levels(train_label)[1] = 2
levels(train_label)[2] = 1
levels(train_label)[3] = 3
train_label  = as.numeric(train_label)

train = train[!(names(train) %in% label_cols)]


test_label = test$风险评级
levels(test_label)[1] = 2
levels(test_label)[2] = 1
levels(test_label)[3] = 3
test_label  = as.numeric(test_label)

test = test[!(names(test) %in% label_cols)]


acc = function(x_pred,x_label){
  ac = sum(as.numeric(x_pred==x_label))/length(x_pred)
  return(ac)
}

#这里分别尝试用全部inside和logistic里面选出的inside去做回归
library(softmaxreg)
# model  = softmaxReg(train[,3:24], train_label, hidden = c(), funName =
#                       'sigmoid', maxit =500 ,rang = 0.1, type = "class", algorithm = "adagrad",
#                     rate = 0.1, batch = 128)


# softmax基本在75%左右的准确率
idx = c('新车零售完成率' , '净利润率' , 
  '资产负债率' ,'权益比率' ,'营业增长率.当月.去年平均.' ,
  '营业增长率.当月.上月.' , '融资成本率')

model  = softmaxReg(train[,idx], train_label, hidden = c(), funName =
                      'sigmoid', maxit =1000 ,rang = 0.1, type = "class", algorithm = "adagrad",
                    rate = 0.1, batch = 128)

pred = predict(model,test[,idx])
table(pred,test_label)
acc_test = acc(pred,test_label)
acc_test


