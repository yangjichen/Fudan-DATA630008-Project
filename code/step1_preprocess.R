library(openxlsx)
DealerRisk = read.xlsx('DealerRisk.xlsx', 1)
# missing data探索性分析
# missing value of inside accounting 
library(VIM)
par(family='STSong')
aggr(DealerRisk[1:28],prop=F,numbers=F,cex.axis = 0.6,combined = F)
# missing value of outside commercial database
aggr(DealerRisk[29:48],prop=F,numbers=F,cex.axis = 0.4,combined = F)

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
DealerRisk$关键人员变动次数=NULL
DealerRisk$`动产抵押非车辆金额（万元）`=NULL
DealerRisk$质权人是否为商业银行=NULL
DealerRisk$司法拍卖次数=NULL
DealerRisk$`非车企投资总额（万元）`=NULL
DealerRisk$`关联风险-严重违法次数`=NULL
DealerRisk$`被执行标的金额（元）`=NULL
#DealerRisk$'新车毛利1（新车销售毛利）'=NULL

# TODO：风险评级中未进行评级的，对应的建店规模也是未获取，这类可以直接删除。另外还有少部分建店规模未获取可能需要补全一下
idx = (DealerRisk$风险评级 == '未进行风险评估')
DealerRisk = DealerRisk[!idx, ]


#数据填补第一类，NA <- 0 ===============================================================================
NA20list = c('负面新闻数量','股权出质数额（万）','股权出质数额/实收资本','被执行次数','失信被执行次数','法律诉讼数量',
                   '行政处罚次数','投资人是否变动','关联风险-失信被执行人次数','关联风险-被执行人次数')
for (i in NA20list){
  idx = is.na(DealerRisk[i])
  DealerRisk[i][idx] = 0
}

#数据填补第二类，数值型random missing，最好补全 ========================================================
library(mice)
impute = c('注册资本（万）','车企投资总额（万元）','现金流量比率')
idx = (DealerRisk$建店规模 == '未获取')
DealerRisk$建店规模[idx] = '4SA'

names = colnames(DealerRisk)
methods = c()
for (i in 1:length(names)) {
  if (names[i] %in% impute) method = 'cart'
  else method = ''
  methods = c(methods, method)
}

imp = mice(data.frame(DealerRisk), m=5, 
                  maxit =50,meth = methods,print=FALSE,diag=FALSE)

imp = data.frame(complete(imp))
#检查是否还有缺失值,通过检查
sapply(imp , function(x) sum(is.na(x)))

write.csv(imp, file = 'step1_data.csv',row.names = FALSE)









