adhd = read.csv("train_data.csv")
adhd = t(adhd)
colnames(adhd) = as.character(adhd[2,])
adhd = adhd[-(1:2),]
X = apply(adhd[,-1], 2, as.numeric)
adhd = data.frame(adhd[,1], X)
adhd = as.data.frame(adhd)

rownames(adhd) = 1:nrow(adhd)

adhd[adhd == "Controls"]=0
adhd[adhd == "ADHD-C" | adhd == "ADHD-H" | adhd == "ADHD-I"]=1

predictors = as.data.frame(adhd[,-1])
y = as.factor(adhd[,1])

set.seed(2705) 
ind = sample(1:dim(predictors)[1],dim(predictors)[1]*0.2)  
y_test = y[ind]
y_train = y[-ind]
x_test = predictors[ind,]
x_train = predictors[-ind,]

### loading test data
test_data = read.csv("test_data.csv")
str(test_data)
test_data = t(test_data)
colnames(test_data) = as.character(test_data[2,])
test_data = test_data[-(1:2),]
X = apply(test_data[,-1], 2, as.numeric)
test_data = data.frame(test_data[,1], X)
test_data = as.data.frame(test_data)
str(test_data)


rownames(test_data) = 1:nrow(test_data)

test_data[test_data == "Controls"]=0
test_data[test_data == "ADHD-C" | test_data == "ADHD-H" | test_data == "ADHD-I"]=1


data.train = data.frame(y_train,x_train)

library(glmnet)

fit_glm = glmnet(x_train, y_train, family = "binomial", alpha = 1, lambda = NULL)


pred = predict(fit_glm, newx = as.matrix(test_data[,-1]), type = "link", s = 0.05)
pred = (exp(pred))/(exp(pred)+1)
glm_pred = rep(0, length(pred))
glm_pred[pred>.5]=1
table(glm_pred,test_data[,1])
error = 1-mean(glm_pred==test_data[,1])









