#----------------------------------------------------------------------------------------------------------------------------
# Jan 2021, Hyoungjoon Lim
# Since all the traces of employees' work remain in the smart work solution, a correlation between log data in 2020 and the promotion results can be derived, 
# and furthermore, it will be possible to predict the promotion results. SVM showed the best performance with 91% accuracy and 83% F1 score.
#----------------------------------------------------------------------------------------------------------------------------
library(sqldf)
library(InformationValue)
library(reshape2)
library(car)
library(corrplot)
library(rpart)

all_data2 <- subset(all_data, substr(all_data$date,1,4) == "2020")
all_data2 <- subset(all_data, 
                      all_data$rank == "manager" ) # optional

summary(as.factor(all_data2$label2))


cat_list <- unique(as.factor(all_data2$label2))


setwd("D:/Desktop/rawdata/210125")
proo <- read.csv("promotion results.csv")
colnames(proo) <- c("b_lev","c_lev","d_lev","e_lev","ID","rank")
pro <- proo
pro <- pro[,c(5:6)]
pro_data <- merge(x=all_data2, y=pro, by='ID')
pro_data2 <- merge(x=all_data2, y=pro, by='ID', all=TRUE)

RES <- data.frame()
for(i in 1:length(cat_list)){
  temp <- sqldf(paste0("select ID,count(*) from pro_data where label2 = '", cat_list[i] ,"' group by ID"))
  colnames(temp)[2] <- as.character(cat_list[i])
  if(i==1){
    RES <- temp
  } else{
    RES <- merge(x=RES, y=temp, by='ID', all=TRUE)
  }
  print(i)
}

for(j in 2:ncol(RES)){
  RES[is.na(RES[,j]),j] <- 0 # NA to zero
}


all_data2 <- subset(pro_data2, is.na(pro_data2$rank.y))

RES_all <- data.frame()
for(i in 1:length(cat_list)){
  temp <- sqldf(paste0("select ID,count(*) from all_data2 where label = '", cat_list[i] ,"' group by ID"))
  colnames(temp)[2] <- as.character(cat_list[i])
  if(i==1){
    RES_all <- temp
  } else{
    RES_all <- merge(x=RES_all, y=temp, by='ID', all=TRUE)
  }
  print(i)
}

for(j in 2:ncol(RES_all)){
  RES_all[is.na(RES_all[,j]),j] <- 0 # NA to zero
}

RES$promote <- 1
RES_all$promote <- 0

lr_data <- rbind(RES, RES_all)
lr_data <- lr_data[,-1]


lr_data2 <- melt(lr_data, id.var="promote")

######### Logistic Regression ###########

cp <- cor(lr_data[,1:(ncol(lr_data)-1)])
corrplot.mixed(cp)
set.seed(123)

ind <- sample(2, nrow(lr_data), replace=TRUE, prob=c(0.6, 0.4))
train <- lr_data[ind==1,]
test <- lr_data[ind==2,]

fit <- glm(promote~., family = "binomial", data=train)
summary(fit)
#train.probs <- predict(fit,train, type="response")
#trainY <- lr_data[ind==1,ncol(lr_data)]
testY <- lr_data[ind==2,ncol(lr_data)]
#confusionMatrix(trainY, train.probs)
#misClassError(trainY, train.probs)

test.probs <- predict(fit,test, type="response")
confusionMatrix(testY, test.probs)
misClassError(testY, test.probs)

options(scipen = 999)
exp(coef(fit))
vif(fit)


########## Decision Tree & Random Forest ###########
library(rpart)
library(partykit)

set.seed(123)
ind <- sample(2, nrow(lr_data), replace=TRUE, prob=c(0.5, 0.5))
train <- lr_data[ind==1,]
test <- lr_data[ind==2,]

tree.train <- rpart(promote~.,data=train)
tree.train$cptable
cp <- min(tree.train$cptable[7,])
prune.tree.train <- prune(tree.train, cp=cp)
plot(as.party(prune.tree.train))

threshold <- 0.04
rparty.test <- predict(prune.tree.train, newdata = test, type="vector") >= threshold
table(rparty.test, test$promote)
(table(rparty.test, test$promote)[1,1]+table(rparty.test, test$promote)[2,2])/sum(table(rparty.test, test$promote))

install.packages("randomForest")
library(randomForest)
lr_data$promote <- factor(lr_data$promote)
m <- randomForest(promote~., data=lr_data)
m
which.min(m$err.rate[,1])
plot(m)


########### SVM ########### 
install.packages("e1071")
library(e1071)

set.seed(123)
ind <- sample(2, nrow(lr_data), replace=TRUE, prob=c(0.55, 0.45))
train <- lr_data[ind==1,]
test <- lr_data[ind==2,]

svm <- svm(promote~., data = train, 
           kernel='radial', cost=500, gamma=5,type="C-classification")
summary(svm)
pred <- predict(svm, test, decision.values=T)
table(pred, test$promote)






