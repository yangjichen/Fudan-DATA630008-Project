# 初步筛选数据==============================================================================================
library(car)
DealerRisk= read.csv('step1_data.csv',row.names = NULL)
# 暂时只用inside指标
#DealerRisk = DealerRisk[,1:25] 
# 这里筛选出低风险与高风险
data = subset(DealerRisk,风险评级=='低风险'|风险评级=='高风险')
shape = dim(data)
# 这里为了让筛选后的dataframe行名连续，方便后续删除
row.names(data) = 1:shape[1]
# 只剩下了两类，所以重新做一次factor
data$风险评级 = as.factor(as.character(data$风险评级))


# 看能不能用类似linear regression方法寻找异常值点=========================================================
# The default, documented in ?glm.control is 25. 
# You pass control parameters as a list in the glm call: control = list(maxit = 50)

fit = glm(风险评级~., data = data ,family=binomial,
              control = list(maxit = 50))

outlierTest(fit)
hat.plot = function(fit){
  p = length(coefficients(fit))
  n = length(fitted(fit))
  plot(hatvalues(fit),main = 'Index Plot of Hat Values')
  abline(h = c(2,3)*p/n ,col = 'red',lty = 2)
  #identify(1:n, hatvalues(fit),names(hatvalues(fit)))
}
hat.plot(fit)
influencePlot(fit)


#按照这一方法选取的异常值点有  c(106,178,235,252,253)，删除这些异常值点==================================
del = c(106,178,235,252,253)
data = data[-del,]

write.csv(data,file = 'step2_data.csv',row.names = FALSE)

#下面尝试划分数据集进行logistic regression===============================================================
set.seed(1024)
idx = sample(nrow(data),size = round(nrow(data)*0.6), replace = F)
train = data[idx,]
test = data[-idx,]


# fit = glm(风险评级~., data = train ,family=binomial,
#               control = list(maxit = 50))
library(arm)
fit = bayesglm(风险评级~., data = train ,family=binomial,
                                  control = list(maxit = 50))
prob = predict(fit, newdata = test,type ='response')

pred = ifelse(prob>=0.5,"high","low")
pred = factor(pred,order=TRUE,level = c("high","low"))
f<-table(test$风险评级,pred)
f

#上面得到的结果一般，尝试做一次变量选择
model2 = step(object = fit)
summary(model2)
prob = predict(model2, newdata = test,type ='response')

pred = ifelse(prob>=0.5,"high","low")
pred = factor(pred,order=TRUE,level = c("high","low"))
f<-table(test$风险评级,pred)
f

#ROC曲线
library(pROC)
roc_curve <- roc(test$风险评级,prob)
names(roc_curve)
x <- 1-roc_curve$specificities
y <- roc_curve$sensitivities

library(ggplot2)
p <- ggplot(data = NULL, mapping = aes(x= x, y = y))
p + geom_line(colour = 'red') +geom_abline(intercept = 0, slope = 1)+ annotate('text', x = 0.4, y = 0.5, label =paste('AUC=',round(roc_curve$auc,2)))+ labs(x = '1-specificities',y = 'sensitivities', title = 'ROC Curve')



