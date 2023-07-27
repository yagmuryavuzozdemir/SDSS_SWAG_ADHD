library(caret)
library(swag)

DATA <-  read.csv("train_data.csv")
adhd0 <- t(DATA)
adhd1 <- adhd0[-1, ]

#naming the colnames
colnames(adhd1) <- as.character(adhd1[1, ])
#View(adhd1)
X <- apply(adhd1[,-1],2, as.numeric)
X <- data.frame(adhd1[ ,1],X)
adhd2 <- X[-1, ]
#View(adhd2)
typeof(adhd2)
adhd3 <- as.matrix(adhd2)
#dim(adhd3)
colnames(adhd3)[1] <- "GROUP"
#View(adhd3)

#defining control group=0  
cat_v <- c()
for (i in 1:dim(adhd3)[1]){
  if(adhd3[ ,"GROUP"][i] == "Controls")
  {
    cat_v[i]<-"0"
}else{
    cat_v[i]<-"1"
  }
}
#as.factor(cat_v)
adhd3 <- data.frame(cat_v, adhd3)
Z <- adhd3[ ,-2]
Z <- apply(Z,2,as.numeric)
r <- Z[ ,1]
r <- as.matrix(r)
adhd <- Z[ ,-1]

rm(list=c("adhd0", "adhd1", "adhd2", "adhd3", "Z"))

#training and testing data
## % 80 of the sample size
ind <- floor(0.8 * nrow(adhd))
set.seed(2705)
train_ind <- sample(seq_len(nrow(adhd)), size = ind)

x_train <- as.data.frame(adhd[train_ind, ])
y_train <- as.factor(r[train_ind, ])
x_test <- as.data.frame(adhd[-train_ind, ])
y_test <- as.factor(r[-train_ind, ])


swagcon <-  swagControl(pmax = 20, alpha = 0.05, m = 1711, seed = 2709, verbose = T)

train_swag_svml = swag(
  # arguments for swag
  x = x_train, y = y_train, control = swagcon, auto_control = FALSE,
  # arguments for caret
  trControl = trainControl(method = "repeatedcv", number = 10, repeats=10, allowParallel=F),
  metric = "Accuracy",
  method = "glm", 
  family = "binomial"
  # Use method = "svmRadial" to train this specific learner
  #preProcess = c("center", "scale")
)

save(train_swag_svml, file = "ADHD_package.Rda")
