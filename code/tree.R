library(openxlsx)
DealerRisk = read.xlsx('DealerRisk.xlsx', 1)
# missing data探索性分析
# missing value of inside accounting 
library(VIM)
par(family='STSong')
aggr(DealerRisk[1:28],prop=F,numbers=F,cex.axis = 0.4,combined = FALSE)
# missing value of outside commercial database
aggr(DealerRisk[29:48],prop=F,numbers=F,cex.axis = 0.4,combined = FALSE)

# 填补missing值，从上面可以看出两部分missing的比例非常不同
# 我认为可以考虑将inside accounting和outside commercial database分开填补
# 问题在于选哪种填补方法
# library(mice)

# =================================================================================================
# 首先将chr类型的数值形变量转换为num, 并处理这些列的NA值
# TODO:可以考虑如何补全后面列的缺失值，或者用更好方法补全前28列
cha2num = c('新车存货周转天数','新车零售完成率','新车批售完成率','新车毛利1（新车销售毛利）',
            '营业增长率（月均）','营业增长率（当月/去年平均）','资产增长率','资本积累率')

for (i in cha2num){
  DealerRisk[i] = as.numeric(DealerRisk[,i])
  idx = is.na(DealerRisk[i])
  DealerRisk[idx,i] = mean(unlist(DealerRisk[,i], use.names=FALSE),na.rm=T)
  print(sum(is.na(DealerRisk[i])))
}
# 现金流量比率这一列会出现31个NA，暂时考虑均值填补
idx = is.na(DealerRisk['现金流量比率'])
DealerRisk[idx,'现金流量比率'] = mean(unlist(DealerRisk[,'现金流量比率'], use.names=FALSE),na.rm=T)

# 到这里所有前28列的NA已经处理完毕==================================================================


# 这里处理由range表格定义的outliers=================================================================
# 看了数据觉得并不是记录错了，可能不需要删除数据，截断就可以
# TODO:可以进一步细化outlier的筛选
outlier = c('新车毛利1（新车销售毛利）','股东权益报酬率','营业增长率（当月/去年平均）','关键人员变动次数')

idx = (DealerRisk['新车毛利1（新车销售毛利）'] > 0.3)
DealerRisk[idx,'新车毛利1（新车销售毛利）'] = 0.3

idx = (DealerRisk['股东权益报酬率'] > 1)
DealerRisk[idx,'股东权益报酬率'] = 1
idx = (DealerRisk['股东权益报酬率'] < -5)
DealerRisk[idx,'股东权益报酬率'] = -5

idx = (DealerRisk['营业增长率（当月/去年平均）'] > 10)
DealerRisk[idx,'营业增长率（当月/去年平均）'] = 10
idx = (DealerRisk['营业增长率（当月/去年平均）'] < -5)
DealerRisk[idx,'营业增长率（当月/去年平均）'] = -5

# 这里删除一定不会用的列=============================================================================
DealerRisk$序号=NULL
DealerRisk$经销商代码=NULL
DealerRisk$开业时间=NULL
DealerRisk$是否列入异常名录原因=NULL

# TODO：风险评级中未进行评级的，对应的建店规模也是未获取，这类可以直接删除。另外还有少部分建店规模未获取可能需要补全一下
idx = (DealerRisk$风险评级 == '未进行风险评估')
DealerRisk = DealerRisk[!idx, ]

#一共只有5个未获取，暂时考虑用频率最高的填补
idx = (DealerRisk$建店规模 == '未获取')
DealerRisk$建店规模[idx] = "4SA"




write.csv(DealerRisk, file = 'step1_data.csv')



#尝试监测离群点=============================================================================




# (b)利用rpart包进行classification and regression tree 方法的探究
install.packages('rpart')
install.packages('partykit')
library(rpart)

# 数据导入
DealerRisk= read.csv('step1_data.csv',row.names = NULL)
#################################################################################
# 暂时只用inside指标 ===========================================================#
#################################################################################
DealerRisk = DealerRisk[,1:26] 
# 这里筛选出低风险与高风险
data = subset(DealerRisk,风险评级=='低风险'|风险评级=='高风险'|风险评级=='中风险')
shape = dim(data)
# 这里为了让筛选后的dataframe行名连续，方便后续删除
row.names(data) = 1:shape[1]
# 只剩下了两类，所以重新做一次factor
data$风险评级 = as.factor(as.character(data$风险评级))

#按照yjc的方法选取的异常值点有  c(106,178,235,252,253)，删除这些异常值点
del = c(106,178,235,252,253)
data = data[-del,]

# 分测试集与训练集
set.seed(1024)
train.idx = sample(nrow(data),size = round(nrow(data)*0.6), replace = F)

# 先单纯的考虑inside的因素
# 拆分数据集
inside.idx = seq(2, 26)
response.idx = 3

train.inside.data = data[train.idx, inside.idx]
test.inside.data = data[-train.idx, inside.idx]

# 进行CART模型拟合
fit.inside.rpart = rpart(风险评级 ~ ., data=train.inside.data, method = 'class')


# rpart 计算在测试集上的准确率
accuracy.rpart.fun <- function(fit.rpart, test.data, response.idx){
  y.test = test.data[response.idx]
  pred.rpart = predict(fit.rpart, test.data[-response.idx])
  pred.test = apply(pred.rpart, MARGIN = 1, which.max)
  acc = sum(pred.test == as.numeric((y.test$风险评级))) / length(pred.test)
  list(pred.test, y.test, acc)
}

results.rpart = accuracy.rpart.fun(fit.inside.rpart, test.inside.data, response.idx)
pred.test = results.rpart[[1]]; y.test = results.rpart[[2]]; acc.inside.rpart = results.rpart[[3]]
acc.inside.rpart
# 使用ctree in partykit
library(partykit)

# ctree 计算在测试集上的准确率
accuracy.ctree.fun <- function(fit.ctree, test.data, response.idx){
  y.test = test.data[response.idx]
  pred.test = predict(fit.ctree, test.data[-response.idx])
  acc = sum(as.numeric(pred.test) == as.numeric((y.test$风险评级))) / length(pred.test)
  list(pred.test, y.test, acc)
}

fit.inside.ctree = ctree(风险评级 ~ ., data=train.inside.data)

results.ctree = accuracy.ctree.fun(fit.inside.ctree, test.inside.data, response.idx)
pred.test = results.ctree[[1]]; y.test = results.ctree[[2]]; acc.ctree = results.ctree[[3]]




##################################################################################
#  考虑inside+outside ===========================================================#
##################################################################################

## Inside imputation + Output missing data label

DealerRisk= read.csv('step1_data.csv',row.names = NULL)

# 这里筛选出低风险与高风险
data = subset(DealerRisk, 风险评级=='低风险'|风险评级=='中风险') #风险评级=='低风险'|风险评级=='高风险'|风险评级=='中风险'

# 对于Outside缺失数据进行处理
inside.names = names(data[1:26])
data[27:ncol(data)]
out.factor.names = c('质权人是否为商业银行','投资人是否变动')
out.time.names = c('关键人员变动次数', '负面新闻数量', '司法拍卖次数', '被执行次数', '失信被执行次数', '法律诉讼数量', '行政处罚次数',
                   '关联风险.严重违法次数' ,'关联风险.失信被执行人次数', '关联风险.被执行人次数')
out.value.names = c('注册资本.万.', '动产抵押非车辆金额.万元.', '股权出质数额.万.', '股权出质数额.实收资本', '非车企投资总额.万元.',
                    '车企投资总额.万元.', '被执行标的金额.元.')

# 这里为了让筛选后的dataframe行名连续，方便后续删除

shape = dim(data)
row.names(data) = 1:shape[1]
# 填充outside里面的空缺
data[out.factor.names] = lapply(data[out.factor.names], as.character)

data[out.factor.names][is.na(data[out.factor.names])] = '缺失'

data[out.factor.names] = lapply(data[out.factor.names], as.factor)

data[out.time.names][is.na(data[out.time.names])] = -1

data[out.value.names][is.na(data[out.value.names])] = -1


# Missing with regression



# 只剩下了两类，所以重新做一次factor
data$风险评级 = as.factor(as.character(data$风险评级))

#按照yjc的方法选取的异常值点有  c(106,178,235,252,253)，删除这些异常值点
del = c(106,178,235,252,253)
data = data[-del,]
data = data[c(inside.names, out.factor.names, out.time.names, out.value.names)]

# 分测试集与训练集
set.seed(1024)
train.idx = sample(nrow(data),size = round(nrow(data)*0.6), replace = F)

# 包含所有的变量
# 拆分数据集
both.idx = seq(2, ncol(data))
response.idx = 3

train.both.data = data[train.idx, both.idx]
test.both.data = data[-train.idx, both.idx]

# 拟合CART
fit.both.rpart = rpart(风险评级 ~ ., data=train.both.data, method = 'class')

results.rpart = accuracy.rpart.fun(fit.both.rpart, test.both.data, response.idx)
pred.test = results.rpart[[1]]; y.test = results.rpart[[2]]; acc.rpart = results.rpart[[3]]

# 拟合CTREE
fit.both.ctree = ctree(风险评级 ~ ., data=train.both.data)

results.ctree = accuracy.ctree.fun(fit.both.ctree, test.both.data, response.idx)
pred.test = results.ctree[[1]]; y.test = results.ctree[[2]]; acc.ctree = results.ctree[[3]]





