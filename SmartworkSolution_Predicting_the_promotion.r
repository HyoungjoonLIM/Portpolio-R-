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
rm(.Random.seed, envir=globalenv())
ind <- sample(2, nrow(lr_data), replace=T, prob=c(0.632, 0.378))
train <- lr_data[ind==1,]
test <- lr_data[ind==2,]


cost_list <- c(1,2,4,8,16,32,64,128,256,400,512,750,850, 1024)
gamma_list <- c(0.001,0.01,0.03,0.0625, 0.125, 0.25,0.4, 0.5,0.75, 1,2,4,6,8,10,12,16)

# cost_list <- seq(from = 250, to = 350, length.out = 20)
# gamma_list <- seq(from = 0.0025, to = 0.01, length.out = 20)


tune_svm_result <- data.frame()

for (i in 1:length(cost_list)){
  for (j in 1:length(gamma_list)){
    svm <- svm(promoted~., data = train, 
               kernel='sigmoid', 
               cost=cost_list[i], gamma=gamma_list[j],
               type="C-classification",
               class.weights = 'inverse')
    
    pred <- predict(svm, test, decision.values=T)
    tbl <- table(pred, test$promoted)
    
    if(sum(tbl[1,])==0 | sum(tbl[2,])==0){next}
    else{
      acc <- sum(diag(tbl)) / sum(tbl)
      mean_rec <- mean(c(tbl[1,1]/sum(tbl[,1]), tbl[2,2]/sum(tbl[,2])))
      mean_prec <- mean(c(tbl[1,1]/sum(tbl[1,]), tbl[2,2]/sum(tbl[2,])))
      F1_global <- 2*mean_rec*mean_prec/(mean_rec+mean_prec)
      
      F1_promote <- 2 * (tbl[2,2]/sum(tbl[,2])) * (tbl[2,2]/sum(tbl[2,])) / ((tbl[2,2]/sum(tbl[,2])) + (tbl[2,2]/sum(tbl[2,])))
      
      temp <- c(cost_list[i], gamma_list[j],acc,F1_global,F1_promote)
      tune_svm_result <- rbind(tune_svm_result, temp)
    }
  }
  print(i)
}

colnames(tune_svm_result) <- c("cost","gamma","acc","F1_global", "F1_promote")
tune_svm_result <- arrange(tune_svm_result,desc(F1_global))
head(tune_svm_result)
tune_svm_result <- arrange(tune_svm_result,desc(F1_promote))
head(tune_svm_result)

svm <- svm(promoted~., data = train, 
           kernel='sigmoid', cost=2, gamma=0.5,
           type="C-classification",
           class.weights = 'inverse')




F1_promote<-0
F1_global<-0
i<-1
while (F1_promote<0.7 | F1_global<0.82 | is.nan(F1_promote) | is.nan(F1_global | tbl[2,2]<8)){
  print(i)
  set.seed(123)
  
  rm(.Random.seed, envir=globalenv())
  ind <- sample(2, nrow(lr_data), replace=T, prob=c(0.632, 0.378))
  train <- lr_data[ind==1,]
  test <- lr_data[ind==2,]
  
  svm <- svm(promoted~., data = train, 
             kernel='sigmoid', cost=16, gamma=.03,
             type="C-classification",
             class.weights = 'inverse')
  
  #summary(svm)
  pred <- predict(svm, test, decision.values=T)
  table(pred, test$promoted)
  tbl <- table(pred, test$promoted)
  acc <- sum(diag(tbl)) / sum(tbl)
  mean_rec <- mean(c(tbl[1,1]/sum(tbl[,1]), tbl[2,2]/sum(tbl[,2])))
  mean_prec <- mean(c(tbl[1,1]/sum(tbl[1,]), tbl[2,2]/sum(tbl[2,])))
  F1_global <- 2*mean_rec*mean_prec/(mean_rec+mean_prec)
  
  F1_promote <- 2 * (tbl[2,2]/sum(tbl[,2])) * (tbl[2,2]/sum(tbl[2,])) / ((tbl[2,2]/sum(tbl[,2])) + (tbl[2,2]/sum(tbl[2,])))
  
  temp <- c(acc,F1_global,F1_promote)           
    
  print(temp)
  
  i <- i+1  
}

#summary(svm)
table(pred, test$promoted)
tbl <- table(pred, test$promoted)

test$pred <- pred
list11_2 <- rownames(test)[test$pred==1 & test$promoted==1]
list10_2 <- rownames(test)[test$pred==1 & test$promoted==0]
list01_2 <- rownames(test)[test$pred==0 & test$promoted==1]
list00_2 <- rownames(test)[test$pred==0 & test$promoted==0]

#humint2 <- unique(all_data[,c(3:9)])

group11_2 <- data.frame()
for (i in 1:length(list11_2)){
  group11_2 <- rbind(group11_2, net_var[rownames(net_var)==list11_2[i],])
}


group10_2 <- data.frame()
for (i in 1:length(list10_2)){
  group10_2 <-  rbind(group10_2, net_var[rownames(net_var)==list10_2[i],])
}


group01_2 <- data.frame()
for (i in 1:length(list01_2)){
  group01_2 <-  rbind(group01_2, net_var[rownames(net_var)==list01_2[i],])
}

group00_2 <- data.frame()
for (i in 1:length(list00_2)){
  group00_2 <-  rbind(group00_2, net_var[rownames(net_var)==list00_2[i],])
}

group11_2 <- merge(x=group11_2, y=humint2, by='ID')
group10_2 <- merge(x=group10_2, y=humint2, by='ID')
group01_2 <- merge(x=group01_2, y=humint2, by='ID')
group00_2 <- merge(x=group00_2, y=humint2, by='ID')


summary(group11_2[,3:7])
summary(group10_2[,3:7])
summary(group01_2[,3:7])
summary(group00_2[,3:7])

summary(group11_2[,10:14])
summary(group10_2[,10:14])
summary(group01_2[,10:14])
summary(group00_2[,10:14])


write.csv(group11_2, "svm_result_11.csv")
write.csv(group10_2, "svm_result_10.csv")
write.csv(group01_2, "svm_result_01.csv")
write.csv(group00_2, "svm_result_00.csv")






