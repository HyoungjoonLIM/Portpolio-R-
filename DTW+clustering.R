install.packages("data.table")
library(data.table)
install.packages("sqldf")
library(sqldf)
install.packages("stringr")
library(stringr)
install.packages("gdata")
library(gdata)

setwd("E:/학생데이터 분석/2017/DTW_SEM(180418)/result_dtw")
traj <- read.csv("all_data(2017_1,2).csv")
traj$X <- NULL
traj1 <- subset(traj, substr(traj$Date,6,7) == "03" | substr(traj$Date,6,7) == "04" |
                  substr(traj$Date,6,7) == "05" | substr(traj$Date,6,7) == "06") # 1학기
gpa1 <- read.csv("17-1gpa.csv")
survey1 <- read.csv("survey1.csv")
survey2 <- read.csv("survey22.csv")
survey_gpa1 <- sqldf("SELECT gpa1.GPA, survey1.* FROM gpa1, survey1 WHERE gpa1.id = survey1.변환학번")
colnames(survey_gpa1)[2] <- "id"
survey_gpa1 <- sqldf("SELECT survey2.*, survey_gpa1.* FROM survey2, survey_gpa1 WHERE survey2.변환학번 = survey_gpa1.id")
survey_gpa1[,1] <- NULL

gpa2 <- read.csv("17-2gpa.csv")
colnames(gpa2) <- c("id", "GPA")
survey_gpa2 <- sqldf("SELECT gpa2.GPA, survey1.* FROM gpa2, survey1 WHERE gpa2.id = survey1.변환학번")
colnames(survey_gpa2)[2] <- "id"
survey_gpa2 <- sqldf("SELECT survey2.*, survey_gpa2.* FROM survey2, survey_gpa2 WHERE survey2.변환학번 = survey_gpa2.id")
survey_gpa2[,1] <- NULL

log1 <- read.csv("log1_converted.csv")
log1 <- sqldf("SELECT log1.* FROM log1, survey_gpa1 WHERE log1.virID = survey_gpa1.id")
traj1 <- sqldf("SELECT traj1.* FROM traj1, survey_gpa1 WHERE traj1.Stu_id = survey_gpa1.id")

traj2 <- subset(traj, substr(traj$Date,6,7) == "09" | substr(traj$Date,6,7) == "10" |
                  substr(traj$Date,6,7) == "11" | substr(traj$Date,6,7) == "12") # 2학기
traj2 <- sqldf("SELECT traj2.* FROM traj2, survey_gpa2 WHERE traj2.Stu_id = survey_gpa2.id")
log2 <- read.csv("log2_converted.csv")
log2 <- sqldf("SELECT log2.* FROM log2, survey_gpa2 WHERE log2.virID = survey_gpa2.id")
rm(traj,survey2)

summary(survey_gpa1$GPA)
summary(survey_gpa2$GPA)
quantile(survey_gpa1$GPA, probs = c(1/3, 2/3))
quantile(survey_gpa2$GPA, probs = c(1/3, 2/3))

survey_gpa2$cluster2[survey_gpa2$GPA >= 3.32] <- 1
survey_gpa2$cluster2[survey_gpa2$GPA < 3.32] <- 2

survey_gpa2$cluster3[survey_gpa2$GPA >= 3.70] <- 1
survey_gpa2$cluster3[survey_gpa2$GPA >= 3.14 & survey_gpa2$GPA < 3.70] <- 2
survey_gpa2$cluster3[survey_gpa2$GPA < 3.14] <- 3

survey_gpa2$cluster4[survey_gpa2$GPA >= 3.84] <- 1
survey_gpa2$cluster4[survey_gpa2$GPA >= 3.35 & survey_gpa2$GPA < 3.84] <- 2
survey_gpa2$cluster4[survey_gpa2$GPA >= 2.96 & survey_gpa2$GPA < 3.35] <- 3
survey_gpa2$cluster4[survey_gpa2$GPA < 2.96] <- 4

survey_gpa2$cluster2 <- as.factor(survey_gpa2$cluster2)
survey_gpa2$cluster3 <- as.factor(survey_gpa2$cluster3)
survey_gpa2$cluster4 <- as.factor(survey_gpa2$cluster4)

summary(FIN_survey_gpa2$cluster2)
summary(FIN_survey_gpa2$cluster3)
summary(FIN_survey_gpa2$cluster4)

stu_list1 <- unique(survey_gpa1$id)
stu_list2 <- unique(survey_gpa2$id)

TIME_cum2 <- NULL
TIME_avg2 <- NULL

for(i in 1:length(stu_list2)){
  print(i)
  id <- stu_list1[i]
  sample_seq <- subset(log1, virID==id)
  sample_seq <- sample_seq[order(sample_seq$timecreated),]
  login <- NULL
  time <- NULL
  time_cum <- NULL
  time_avg <- NULL
  sample_seq$action <- as.character(sample_seq$action)
  
  if(nrow(sample_seq) != 0){
    for(j in 1:nrow(sample_seq)){
      if(sample_seq$target[j] == "course"){
        login <- rbind(login,j) 
      } else {next}		
    } 
  } else {next}
  
  if(is.null(login)==FALSE){
    for(k in 1:nrow(login)){
      if(k < nrow(login) && login[k+1]-login[k] > 1){
        start <- login[k]
        end <- login[k+1]-1
        time <- rbind(time, c(id, sample_seq$timecreated[end]-sample_seq$timecreated[start]))
        time_cum <- c(id, sum(time[,2]))
        time_avg <- c(id, mean(time[,2]))
      } else {next}
    }
  } else {next}
  
  TIME_cum2 <- rbind(TIME_cum2, time_cum)
  TIME_avg2 <- rbind(TIME_avg2, time_avg)
  
}
colnames(TIME_avg2) <- c("id", "time_avg")
colnames(TIME_cum2) <- c("id", "time_cum")

TIME_avg2 <- as.data.table(TIME_avg2)
TIME_cum2 <- as.data.table(TIME_cum2)

survey_gpa2 <- sqldf("SELECT survey_gpa2.*, TIME_avg2.time_avg FROM survey_gpa2, TIME_avg2 
                     WHERE survey_gpa2.id = TIME_avg2.id")
survey_gpa2 <- sqldf("SELECT survey_gpa2.*, TIME_cum2.time_cum FROM survey_gpa2, TIME_cum2 
                     WHERE survey_gpa2.id = TIME_cum2.id")
survey_gpa2$time_avg <- survey_gpa2$time_avg/3600
survey_gpa2$time_cum <- survey_gpa2$time_cum/3600

setwd("E:/학생데이터 분석")
all1 <- read.csv("all_data(2016_1).csv")
all2 <- read.csv("all_data(2016_2).csv")
all17 <- read.csv("all_data(2017_1,2).csv")
all171 <- subset(all17, substr(all17$Date,6,7) == "03" | substr(all17$Date,6,7) == "04" |
                       substr(all17$Date,6,7) == "05" | substr(all17$Date,6,7) == "06") # 1학기
all172 <- subset(all17, substr(all17$Date,6,7) == "09" | substr(all17$Date,6,7) == "10" |
                   substr(all17$Date,6,7) == "11" | substr(all17$Date,6,7) == "12") # 1학기

# Categorizing 1 to 4
all1_o <- all1
all2_o <- all2
all171_o <- all171
all172_o <- all172

all1 <- all1_o
all2 <- all2_o
all171 <- all171_o
all172 <- all172_o

all1$Category_id[all1$Category_id==2] <- 1
all2$Category_id[all2$Category_id==2] <- 1
all171$Category_id[all171$Category_id==2] <- 1
all172$Category_id[all172$Category_id==2] <- 1

all1$Category_id[all1$Category_id==3] <- 2
all2$Category_id[all2$Category_id==3] <- 2
all171$Category_id[all171$Category_id==3] <- 2
all172$Category_id[all172$Category_id==3] <- 2

all1$Category_id[all1$Category_id==4 | all1$Category_id==5 | all1$Category_id==9 | all1$Category_id==16 |
                   all1$Category_id==10 | all1$Category_id==11 | all1$Category_id==12 | all1$Category_id==8] <- 3
all2$Category_id[all2$Category_id==4 | all2$Category_id==5 | all2$Category_id==9 | all2$Category_id==16 |
                   all2$Category_id==10 | all2$Category_id==11 | all2$Category_id==12 | all2$Category_id==8] <- 3
all171$Category_id[all171$Category_id==4 | all171$Category_id==5 | all171$Category_id==9 | all171$Category_id==16 |
                     all171$Category_id==10 | all171$Category_id==11 | all171$Category_id==12 | all171$Category_id==8] <- 3
all172$Category_id[all172$Category_id==4 | all172$Category_id==5 | all172$Category_id==9 | all172$Category_id==16 |
                     all172$Category_id==10 | all172$Category_id==11 | all172$Category_id==12 | all172$Category_id==8] <- 3

all1$Category_id[all1$Category_id==6 | all1$Category_id==13] <- 4
all2$Category_id[all2$Category_id==6 | all2$Category_id==13] <- 4
all171$Category_id[all171$Category_id==6 | all171$Category_id==13] <- 4
all172$Category_id[all172$Category_id==6 | all172$Category_id==13] <- 4

all1 <- subset(all1, all1$Category_id==1 | all1$Category_id==2 | all1$Category_id==3 | all1$Category_id==4)
all2 <- subset(all2, all2$Category_id==1 | all2$Category_id==2 | all2$Category_id==3 | all2$Category_id==4)
all171 <- subset(all171, all171$Category_id==1 | all171$Category_id==2 | all171$Category_id==3 | all171$Category_id==4)
all172 <- subset(all172, all172$Category_id==1 | all172$Category_id==2 | all172$Category_id==3 | all172$Category_id==4)

sort(unique(all1$Category_id))
sort(unique(all2$Category_id))
sort(unique(all171$Category_id))
sort(unique(all172$Category_id))

setwd("E:/학생데이터 분석/2017/DTW_SEM(180418)/result_dtw/201905_all")
res161 <- read.csv("clustering_result161_all(c=3_5).csv", header=F)
res162 <- read.csv("clustering_result162_all(c=3_5).csv", header=F)
res171 <- read.csv("clustering_result171_all(c=3_8).csv", header=F)
res172 <- read.csv("clustering_result172_all(c=3_8).csv", header=F)
colnames(res161) <- c("id","cluster3")
colnames(res172) <- c("id","cluster3","cluster4","cluster5","cluster6","cluster7","cluster8")
colnames(res171) <- c("id","cluster3","cluster4")
colnames(res172) <- c("id","cluster3","cluster4")

# colnames(res161) <- c("id","cluster4")
# colnames(res162) <- c("id","cluster4")
# colnames(res171) <- c("id","cluster4")
colnames(res172) <- c("id","cluster4")

all1 <- sqldf("SELECT all1.* FROM all1, res161 WHERE all1.Stu_id = res161.id")
all2 <- sqldf("SELECT all2.* FROM all2, res162 WHERE all2.Stu_id = res162.id")
all171 <- sqldf("SELECT all171.* FROM all171, res171 WHERE all171.Stu_id = res171.id")
all172 <- sqldf("SELECT all172.* FROM all172, res172 WHERE all172.Stu_id = res172.id")

input <- all172

inputforANN <- sqldf("select Stu_id,count(*) from input group by Stu_id") # full count

inputforANN1 <- sqldf("select Stu_id,count(*) from input where Category_id = 1 group by Stu_id")	# assignment & quiz

inputforANN2 <- sqldf("select Stu_id,count(*) from input where Category_id = 2 group by Stu_id") # forum & chat

inputforANN3 <- sqldf("select Stu_id,count(*) from input where Category_id = 3 group by Stu_id") # lecture material

inputforANN4 <- sqldf("select Stu_id,count(*) from input where Category_id = 4 group by Stu_id") # notice & schedule

# inputforANN5 <- sqldf("select Stu_id,count(*) from input where Category_id = 5 group by Stu_id") # notice & schedule
# inputforANN6 <- sqldf("select Stu_id,count(*) from input where Category_id = 6 group by Stu_id") # notice & schedule
# inputforANN7 <- sqldf("select Stu_id,count(*) from input where Category_id = 7 group by Stu_id") # notice & schedule
# inputforANN8 <- sqldf("select Stu_id,count(*) from input where Category_id = 8 group by Stu_id") # notice & schedule
# inputforANN9 <- sqldf("select Stu_id,count(*) from input where Category_id = 9 group by Stu_id") # notice & schedule
# inputforANN10 <- sqldf("select Stu_id,count(*) from input where Category_id = 10 group by Stu_id") # notice & schedule
# inputforANN11 <- sqldf("select Stu_id,count(*) from input where Category_id = 11 group by Stu_id") # notice & schedule
# inputforANN12 <- sqldf("select Stu_id,count(*) from input where Category_id = 12 group by Stu_id") # notice & schedule
# inputforANN14 <- sqldf("select Stu_id,count(*) from input where Category_id = 14 group by Stu_id") # notice & schedule
# inputforANN15 <- sqldf("select Stu_id,count(*) from input where Category_id = 15 group by Stu_id") # notice & schedule
# inputforANN16 <- sqldf("select Stu_id,count(*) from input where Category_id = 16 group by Stu_id") # notice & schedule
# 
colnames(inputforANN) <- c("virID","count")

colnames(inputforANN1) <- c("virID","count1")

colnames(inputforANN2) <- c("virID","count2")

colnames(inputforANN3) <- c("virID","count3")

colnames(inputforANN4) <- c("virID","count4")
# colnames(inputforANN5) <- c("virID","count5")
# colnames(inputforANN6) <- c("virID","count6")
# colnames(inputforANN7) <- c("virID","count7")
# colnames(inputforANN8) <- c("virID","count8")
# colnames(inputforANN9) <- c("virID","count9")
# colnames(inputforANN10) <- c("virID","count10")
# colnames(inputforANN11) <- c("virID","count11")
# colnames(inputforANN12) <- c("virID","count12")
# colnames(inputforANN14) <- c("virID","count14")
# colnames(inputforANN15) <- c("virID","count15")
# colnames(inputforANN16) <- c("virID","count16")
# 
# 
inputforANN_fin <- inputforANN
# inputforANN_fin <- sqldf("select virID,inputforANN.count,inputforANN1.count1
# from inputforANN, inputforANN1
# where inputforANN.virID = inputforANN1.virID1")
inputforANN_fin <- merge(x=inputforANN_fin, y=inputforANN1, by='virID', all=TRUE)

inputforANN_fin <- merge(x=inputforANN_fin, y=inputforANN2, by='virID', all=TRUE)

inputforANN_fin <- merge(x=inputforANN_fin, y=inputforANN3, by='virID', all=TRUE)

inputforANN_fin <- merge(x=inputforANN_fin, y=inputforANN4, by='virID', all=TRUE)
# inputforANN_fin <- merge(x=inputforANN_fin, y=inputforANN5, by='virID', all=TRUE)
# inputforANN_fin <- merge(x=inputforANN_fin, y=inputforANN6, by='virID', all=TRUE)
# inputforANN_fin <- merge(x=inputforANN_fin, y=inputforANN7, by='virID', all=TRUE)
# inputforANN_fin <- merge(x=inputforANN_fin, y=inputforANN8, by='virID', all=TRUE)
# inputforANN_fin <- merge(x=inputforANN_fin, y=inputforANN9, by='virID', all=TRUE)
# inputforANN_fin <- merge(x=inputforANN_fin, y=inputforANN10, by='virID', all=TRUE)
# inputforANN_fin <- merge(x=inputforANN_fin, y=inputforANN11, by='virID', all=TRUE)
# inputforANN_fin <- merge(x=inputforANN_fin, y=inputforANN12, by='virID', all=TRUE)
# inputforANN_fin <- merge(x=inputforANN_fin, y=inputforANN14, by='virID', all=TRUE)
# inputforANN_fin <- merge(x=inputforANN_fin, y=inputforANN15, by='virID', all=TRUE)
# inputforANN_fin <- merge(x=inputforANN_fin, y=inputforANN16, by='virID', all=TRUE)

FIN <- inputforANN_fin

FIN_gpa172 <- sqldf("SELECT FIN.*, gpa172.gpa172 FROM FIN, gpa172 WHERE FIN.virID = gpa172.id")

for(j in 2:7){
  FIN_gpa172[is.na(FIN_gpa172[,j]),j] <- 0 # NA to zero
}

FIN_gpa172 <- sqldf("SELECT FIN_gpa172.*, res172.* FROM FIN_gpa172, res172 WHERE FIN_gpa172.virID = res172.id")

write.csv(FIN_gpa161, "FIN_gpa161_all.csv")
write.csv(FIN_gpa162, "FIN_gpa162_all.csv")
write.csv(FIN_gpa171, "FIN_gpa171_all.csv")
write.csv(FIN_gpa172, "FIN_gpa172_all.csv")

clu4_4 <- subset(FIN_gpa162, FIN_gpa162$cluster3==1)
#clu4_4 <- clu4_4[,c(-1,-20)]
#summary(clu4_4)
nrow(clu4_4)
mean(clu4_4$gpa161)

mean(clu4_4$count1) # dorm count
mean(clu4_4$count2) #library
mean(clu4_4$count3) #student union
mean(clu4_4$count4) # lecture room count
#sum(mean(clu4_4$count7),mean(clu4_4$count14)) #physical activity

########## visualization ##########
install.packages("fmsb")
library(fmsb)
library(plyr)

FIN_gpa161_libfem <- read.csv("FIN_gpa161_libfem.csv")
FIN_gpa162_libfem <- read.csv("FIN_gpa162_libfem.csv")
FIN_gpa171_libfem <- read.csv("FIN_gpa171_libfem.csv")
FIN_gpa172_libfem <- read.csv("FIN_gpa172_libfem.csv")

FIN_gpa161_natfem <- read.csv("FIN_gpa161_natfem.csv")
FIN_gpa162_natfem <- read.csv("FIN_gpa162_natfem.csv")
FIN_gpa171_natfem <- read.csv("FIN_gpa171_natfem.csv")
FIN_gpa172_natfem <- read.csv("FIN_gpa172_natfem.csv")

FIN_gpa161_natmale <- read.csv("FIN_gpa161_natmale.csv")
FIN_gpa162_natmale <- read.csv("FIN_gpa162_natmale.csv")
FIN_gpa171_natmale <- read.csv("FIN_gpa171_natmale.csv")
FIN_gpa172_natmale <- read.csv("FIN_gpa172_natmale.csv")

data1 <- data.frame()
for(i in 1:3){
  clu4_4 <- subset(FIN_gpa172, FIN_gpa172$cluster3==i)
  
  cgpa <- mean(clu4_4$gpa172)
  
  all <- mean(clu4_4$count)
  dorm <- mean(clu4_4$count1) # dorm count
  lib <- mean(clu4_4$count2) #library
  lec <- mean(clu4_4$count3) #student union
  uni <- mean(clu4_4$count4) # lecture room count
  
  temp <- c(nrow(clu4_4), cgpa, dorm, lib, uni, lec, all)
  data1 <- rbind(data1, temp) 
}
colnames(data1) <- c("students", "GPA", "Dormitory", "Library", "Student union", "Lecture room", "sum")
data1 <- arrange(data1, desc(GPA))
data1
data1 <- data1[-c(1,5:7),]
data1

dfmax <- apply(data1,2,max) + apply(data1,2,sd)/2
dfmin <- apply(data1,2,min) - apply(data1,2,sd)/2
df_radarchart <- as.data.frame(rbind(dfmax, dfmin, data1))

radarchart(df = df_radarchart[,3:6], seg = 3,
           plty=1, pcol =c(4,1,2), plwd = 3)


#####################################################
require(reshape2)
require(plyr)
require(ggplot2)
require(scales)

coord_radar <- function (theta = "x", start = 0, direction = 1) 
{
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") 
    "y"
  else "x"
  ggproto("CoordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

rescale_df=function(data,groupvar=NULL){
  if(is.null(groupvar)) df=data
  else df=data[,-which(names(data) %in% groupvar)]
  
  select=sapply(df,is.numeric)
  df[select]=lapply(df[select], scales::rescale)
  if(!is.null(groupvar)) {
    df=cbind(df,data[[groupvar]])
    colnames(df)[length(df)]=groupvar
  }        
  df
}

s_data2 <- scale(data2[,1:5])
s_data2 <- as.data.frame(s_data2)

ggRadar=function(data=iris,
                 xvars=NULL,
                 yvar=NULL,autorescale=FALSE,
                 groupvar=NULL,legend.position="bottom",
                 radar=TRUE,polar=FALSE,
                 mean=TRUE,nrow=FALSE,
                 colour="red"){
  if(is.null(xvars)) {
    select=sapply(data,is.numeric)
    xvars=colnames(data)[select]
  }
  if(is.null(yvar)){
    # if(!is.null(groupvar)) {
    #         for(i in 1:length(groupvar)) data[[groupvar[i]]]=factor(data[[groupvar[i]]])
    # }
    # data
    if(autorescale) data=rescale_df(data,groupvar)
    longdf=melt(data,id.vars=groupvar,measure.vars=xvars)
    longdf
    if(mean)
      df=ddply(longdf,c(groupvar,"variable"),summarize,mean(value,na.rm=TRUE))
    if(nrow) 
      df=ddply(longdf,c(groupvar,"variable"),"nrow") 
    
    colnames(df)[length(df)]="value"
    #print(df)
  } else{
    longdf=data
  }
  
  if(is.null(groupvar)){
    p<-ggplot(data=df,aes_string(x="variable",y="value",group=1))+
      geom_point(size=3,colour=colour)+
      geom_polygon(colour=colour,fill=colour,alpha=0)
    
  } else {
    df=df[!(df$variable %in% groupvar),]
    for(i in 1:length(groupvar)) df[[groupvar[i]]]=factor(df[[groupvar[i]]])
    p<-ggplot(data=df,aes_string(x="variable",y="value",
                                 colour=groupvar,fill=groupvar,group=groupvar))+
      geom_point(size=3)+
      geom_polygon(alpha=0)
  }        
  p<- p+ xlab("")+ylab("")+theme(legend.position=legend.position)
  
  if(radar==TRUE) p<-p+coord_radar()
  if(polar==TRUE) p<-p+coord_polar()    
  p    
}

ggRadar(data = s_data2, groupvar="students")






####################### ANOVA #####################

setwd("E:/학생데이터 분석/2017/DTW_SEM(180418)/result_dtw/201812_engmale")
FIN_gpa161_engmale <- read.csv("FIN_gpa161_engmale1234.csv")
FIN_gpa162_engmale <- read.csv("FIN_gpa162_engmale1234.csv")
FIN_gpa171_engmale <- read.csv("FIN_gpa171_engmale1234.csv")
FIN_gpa172_engmale <- read.csv("FIN_gpa172_engmale1234.csv")

ainput <- data.frame(cbind(FIN_gpa172_engmale$gpa172, FIN_gpa172_engmale$cluster3))
ainput <- ainput[ainput$X2!=4,]
ainput2 <- aov(X1~X2, data=ainput)
res <- anova(ainput2)
res
res172_engmale <- res$`Pr(>F)`

setwd("E:/학생데이터 분석/2017/DTW_SEM(180418)/result_dtw/201812_engfem")
FIN_gpa161_engfem <- read.csv("FIN_gpa161_engfem1234.csv")
FIN_gpa162_engfem <- read.csv("FIN_gpa162_engfem1234.csv")
FIN_gpa171_engfem <- read.csv("FIN_gpa171_engfem1234.csv")
FIN_gpa172_engfem <- read.csv("FIN_gpa172_engfem1234.csv")

ainput <- data.frame(cbind(FIN_gpa172_engfem$gpa172, FIN_gpa172_engfem$cluster3))
ainput <- ainput[ainput$X2!=4,]
ainput2 <- aov(X1~X2, data=ainput)
res <- anova(ainput2)
res
res172_engfem <- res$`Pr(>F)`

setwd("E:/학생데이터 분석/2017/DTW_SEM(180418)/result_dtw/201812_libmale")
FIN_gpa161_libmale <- read.csv("FIN_gpa161_libmale1234.csv")
FIN_gpa162_libmale <- read.csv("FIN_gpa162_libmale1234.csv")
FIN_gpa171_libmale <- read.csv("FIN_gpa171_libmale1234.csv")
FIN_gpa172_libmale <- read.csv("FIN_gpa172_libmale1234.csv")

ainput <- data.frame(cbind(FIN_gpa172_libmale$gpa172, FIN_gpa172_libmale$cluster5))
ainput <- ainput[ainput$X2!=5,]
ainput2 <- aov(X1~X2, data=ainput)
res <- anova(ainput2)
res
res172_libmale <- res$`Pr(>F)`

setwd("E:/학생데이터 분석/2017/DTW_SEM(180418)/result_dtw/201812_libfem")
FIN_gpa161_libfem <- read.csv("FIN_gpa161_libfem1234.csv")
FIN_gpa162_libfem <- read.csv("FIN_gpa162_libfem1234.csv")
FIN_gpa171_libfem <- read.csv("FIN_gpa171_libfem1234.csv")
FIN_gpa172_libfem <- read.csv("FIN_gpa172_libfem1234.csv")

ainput <- data.frame(cbind(FIN_gpa172_libfem$gpa172, FIN_gpa172_libfem$cluster4))
ainput <- ainput[ainput$X2!=4,]
ainput2 <- aov(X1~X2, data=ainput)
res <- anova(ainput2)
res
res172_libfem <- res$`Pr(>F)`

setwd("E:/학생데이터 분석/2017/DTW_SEM(180418)/result_dtw/201812_natmale")
FIN_gpa161_natmale <- read.csv("FIN_gpa161_natmale1234.csv")
FIN_gpa162_natmale <- read.csv("FIN_gpa162_natmale1234.csv")
FIN_gpa171_natmale <- read.csv("FIN_gpa171_natmale1234.csv")
FIN_gpa172_natmale <- read.csv("FIN_gpa172_natmale1234.csv")

ainput <- data.frame(cbind(FIN_gpa172_natmale$gpa172, FIN_gpa172_natmale$cluster4))
ainput <- ainput[ainput$X2!=4,]
ainput2 <- aov(X1~X2, data=ainput)
res <- anova(ainput2)
res
res172_natmale <- res$`Pr(>F)`

setwd("E:/학생데이터 분석/2017/DTW_SEM(180418)/result_dtw/201812_natfem")
FIN_gpa161_natfem <- read.csv("FIN_gpa161_natfem1234.csv")
FIN_gpa162_natfem <- read.csv("FIN_gpa162_natfem1234.csv")
FIN_gpa171_natfem <- read.csv("FIN_gpa171_natfem1234.csv")
FIN_gpa172_natfem <- read.csv("FIN_gpa172_natfem1234.csv")

ainput <- data.frame(cbind(FIN_gpa172_natfem$gpa172, FIN_gpa172_natfem$cluster4))
ainput <- ainput[ainput$X2!=4,]
ainput2 <- aov(X1~X2, data=ainput)
res <- anova(ainput2)
res
res172_natfem <- res$`Pr(>F)`

setwd("E:/학생데이터 분석/2017/DTW_SEM(180418)/result_dtw/201905_all")
FIN_gpa161_all <- read.csv("FIN_gpa161_all.csv")
FIN_gpa162_all <- read.csv("FIN_gpa162_all.csv")
FIN_gpa171_all <- read.csv("FIN_gpa171_all.csv")
FIN_gpa172_all <- read.csv("FIN_gpa172_all.csv")

ainput <- data.frame(cbind(FIN_gpa172$gpa172, FIN_gpa172$cluster3))
ainput <- ainput[ainput$X2!=5,]
ainput <- ainput[ainput$X2!=6,]
ainput2 <- aov(X1~X2, data=ainput)
res <- anova(ainput2)
res
res172_all <- res$`Pr(>F)`

res_anova_engmale <- c(res161_engmale[1], res162_engmale[1], res171_engmale[1], res172_engmale[1])
res_anova_engfem <- c(res161_engfem[1], res162_engfem[1], res171_engfem[1], res172_engfem[1])
res_anova_libmale <- c(res161_libmale[1], res162_libmale[1], res171_libmale[1], res172_libmale[1])
res_anova_libfem <- c(res161_libfem[1], res162_libfem[1], res171_libfem[1], res172_libfem[1])
res_anova_natmale <- c(res161_natmale[1], res162_natmale[1], res171_natmale[1], res172_natmale[1])
res_anova_natfem <- c(res161_natfem[1], res162_natfem[1], res171_natfem[1], res172_natfem[1])
res_anova_all <- c(res161_all[1], res162_all[1], res171_all[1], res172_all[1])

res_anova <- data.frame(rbind(res_anova_engmale, res_anova_engfem, res_anova_libmale, res_anova_libfem, 
                              res_anova_natmale, res_anova_natfem, res_anova_all))
colnames(res_anova) <- c("16-1", "16-2", "17-1", "17-2")
setwd("E:/학생데이터 분석/2017/DTW_SEM(180418)/result_dtw/")
write.csv(res_anova, "result_anova.csv", row.names = F)










####### ks-test #######
test1 <- subset(FIN_gpa161, FIN_gpa161$cluster3==2)
test2 <- subset(FIN_gpa161, FIN_gpa161$cluster3==3)

ks.test(test1$count1, test2$count1)$p.value
ks.test(test1$count2, test2$count2)$p.value
ks.test(test1$count3, test2$count3)$p.value
ks.test(test1$count4, test2$count4)$p.value
ks.test(test1$gpa161, test2$gpa161)$p.value




sum(mean(clu4_4$A1_1),mean(clu4_4$A1_2)) # study time per week
#mean(clu4_4$A1_3) # internet
sum(mean(clu4_4$A1_5),mean(clu4_4$A1_9)) #social
mean(clu4_4$A1_10) # hobby

mean(clu4_4$time_cum)
mean(clu4_4$time_avg)

#mean(mean(clu4_4$A3_1),mean(clu4_4$A3_2)) #사회성
(summary(clu4_4$E1_1)[2]*0.5 + summary(clu4_4$E1_1)[3]*3 
  + summary(clu4_4$E1_1)[4]*10 + summary(clu4_4$E1_1)[5]*12)/nrow(clu4_4) #음주횟수/개월 

(summary(clu4_4$E1_2)[1]*1.5 + summary(clu4_4$E1_2)[2]*3.5 + summary(clu4_4$E1_2)[3]*5.5 
  + summary(clu4_4$E1_2)[4]*8 + summary(clu4_4$E1_2)[5]*10)/nrow(clu4_4) #몇 잔? 

(summary(clu4_4$E1_3)[2]*0.5 + summary(clu4_4$E1_3)[3]*1 
  + summary(clu4_4$E1_3)[4]*4 + summary(clu4_4$E1_3)[5]*8)/nrow(clu4_4) #과음횟수/개월 

(summary(clu4_4$E1_4)[1]*5 + summary(clu4_4$E1_4)[2]*15
  + summary(clu4_4$E1_4)[3]*25 + summary(clu4_4$E1_4)[4]*35)/nrow(clu4_4)#흡연 : 개비/일 

(summary(clu4_4$E1_5)[1]*0.5 + summary(clu4_4$E1_5)[2]*1.5 + summary(clu4_4$E1_5)[3]*2.5 + summary(clu4_4$E1_5)[4]*3.5
  + summary(clu4_4$E1_5)[5]*4.5 + summary(clu4_4$E1_5)[6]*5.5 + summary(clu4_4$E1_5)[7]*8 + summary(clu4_4$E1_5)[8]*10)/nrow(clu4_4)

mean(clu4_4$E1_6) # 인터넷 사용 때문에 방해받은 정도
mean(mean(clu4_4$E1_7),mean(clu4_4$E1_8),mean(clu4_4$E1_9)) # sexual knowledge

mean(clu4_4$E3_3-clu4_4$E3_4) # 외향성 
mean(clu4_4$E3_5+clu4_4$E4_1-clu4_4$E4_2) # 행복감  
summary(clu4_4$Q2_1_4)[2]*100/nrow(clu4_4) # 기숙사 경험 여부


rm(clu4_4)

# p-value
FIN_survey_gpa2$E1_1c[FIN_survey_gpa2$E1_1==1] <- 0
FIN_survey_gpa2$E1_1c[FIN_survey_gpa2$E1_1==2] <- 0.5
FIN_survey_gpa2$E1_1c[FIN_survey_gpa2$E1_1==3] <- 3
FIN_survey_gpa2$E1_1c[FIN_survey_gpa2$E1_1==4] <- 10
FIN_survey_gpa2$E1_1c[FIN_survey_gpa2$E1_1==5] <- 12

FIN_survey_gpa2$E1_2c[FIN_survey_gpa2$E1_2==1] <- 1.5
FIN_survey_gpa2$E1_2c[FIN_survey_gpa2$E1_2==2] <- 3.5
FIN_survey_gpa2$E1_2c[FIN_survey_gpa2$E1_2==3] <- 5.5
FIN_survey_gpa2$E1_2c[FIN_survey_gpa2$E1_2==4] <- 8
FIN_survey_gpa2$E1_2c[FIN_survey_gpa2$E1_2==5] <- 10
FIN_survey_gpa2$E1_2c[FIN_survey_gpa2$E1_2=="x"] <- 0

FIN_survey_gpa2$E1_3c[FIN_survey_gpa2$E1_3==1] <- 0
FIN_survey_gpa2$E1_3c[FIN_survey_gpa2$E1_3==2] <- 0.5
FIN_survey_gpa2$E1_3c[FIN_survey_gpa2$E1_3==3] <- 1
FIN_survey_gpa2$E1_3c[FIN_survey_gpa2$E1_3==4] <- 4
FIN_survey_gpa2$E1_3c[FIN_survey_gpa2$E1_3==5] <- 8
FIN_survey_gpa2$E1_3c[FIN_survey_gpa2$E1_3=="x"] <- 0

FIN_survey_gpa2$E1_4c[FIN_survey_gpa2$E1_4==1] <- 5
FIN_survey_gpa2$E1_4c[FIN_survey_gpa2$E1_4==2] <- 15
FIN_survey_gpa2$E1_4c[FIN_survey_gpa2$E1_4==3] <- 25
FIN_survey_gpa2$E1_4c[FIN_survey_gpa2$E1_4==4] <- 35

FIN_survey_gpa2$E1_5c[FIN_survey_gpa2$E1_5==1] <- 0.5
FIN_survey_gpa2$E1_5c[FIN_survey_gpa2$E1_5==2] <- 1.5
FIN_survey_gpa2$E1_5c[FIN_survey_gpa2$E1_5==3] <- 2.5
FIN_survey_gpa2$E1_5c[FIN_survey_gpa2$E1_5==4] <- 3.5
FIN_survey_gpa2$E1_5c[FIN_survey_gpa2$E1_5==5] <- 4.5
FIN_survey_gpa2$E1_5c[FIN_survey_gpa2$E1_5==6] <- 5.5
FIN_survey_gpa2$E1_5c[FIN_survey_gpa2$E1_5==7] <- 8
FIN_survey_gpa2$E1_5c[FIN_survey_gpa2$E1_5==8] <- 10

clu4_1 <- subset(FIN_survey_gpa2, FIN_survey_gpa2$cluster4==1)
clu4_2 <- subset(FIN_survey_gpa2, FIN_survey_gpa2$cluster4==2)
clu4_3 <- subset(FIN_survey_gpa2, FIN_survey_gpa2$cluster4==3)
clu4_4 <- subset(FIN_survey_gpa2, FIN_survey_gpa2$cluster4==4)

t.test(colSums(rbind(clu4_3$count1, clu4_3$count2)), colSums(rbind(clu4_4$count1, clu4_4$count2)))$p.value #pval_dorm
t.test(clu4_3$count3, clu4_4$count3)$p.value #pval_lib
t.test(clu4_3$count6, clu4_4$count6)$p.value #pval_main
t.test(colSums(rbind(clu4_3$count4, clu4_3$count5, clu4_3$count9, clu4_3$count10, clu4_3$count11, clu4_3$count12)), colSums(rbind(clu4_4$count4, clu4_4$count5, clu4_4$count9, clu4_4$count10, clu4_4$count11, clu4_4$count12)))$p.value #pval_lec
#t.test(colSums(rbind(clu4_3$count7, clu4_3$count14)), colSums(rbind(clu4_4$count7, clu4_4$count14)))$p.value #pval_phys
t.test(clu4_3$GPA, clu4_4$GPA)$p.value #pval_gpa

t.test(colSums(rbind(clu4_3$A1_1, clu4_3$A1_2)), colSums(rbind(clu4_4$A1_1, clu4_4$A1_2)))$p.value #pval_study
t.test(colSums(rbind(clu4_3$A1_5, clu4_3$A1_9)), colSums(rbind(clu4_4$A1_5, clu4_4$A1_9)))$p.value #pval_soc
t.test(clu4_3$A1_10, clu4_4$A1_10)$p.value #pval_hob

t.test(clu4_3$time_cum, clu4_4$time_cum)$p.value #pval_timecum
t.test(clu4_3$time_avg, clu4_4$time_avg)$p.value #pval_timeavg

t.test(clu4_3$E1_1c, clu4_4$E1_1c)$p.value #pval_음주횟수
t.test(clu4_3$E1_2c, clu4_4$E1_2c)$p.value #pval_몇잔
t.test(clu4_3$E1_3c, clu4_4$E1_3c)$p.value #pval_과음횟수
t.test(clu4_3$E1_4c, clu4_4$E1_4c)$p.value #pval_개비
t.test(clu4_3$E1_5c, clu4_4$E1_5c)$p.value #pval_일별 인터넷 사용시간 

t.test(clu4_3$E1_6, clu4_4$E1_6)$p.value #pval_internet interruption
t.test(colMeans(rbind(clu4_3$E1_7,clu4_3$E1_8,clu4_3$E1_9)), colMeans(rbind(clu4_4$E1_7,clu4_4$E1_8,clu4_4$E1_9)))$p.value #pval_sexual

t.test(clu4_3$E3_3-clu4_3$E3_4, clu4_4$E3_3-clu4_4$E3_4)$p.value #pval_외향성 
t.test(clu4_3$E3_5+clu4_3$E4_1-clu4_3$E4_2, clu4_4$E3_5+clu4_4$E4_1-clu4_4$E4_2)$p.value #pval_행복감 


rm(clu4_4, clu4_4, clu5_1)


FIN_survey_gpa1$E1_1c[FIN_survey_gpa1$E1_1==1] <- 0
FIN_survey_gpa1$E1_1c[FIN_survey_gpa1$E1_1==2] <- 0.5
FIN_survey_gpa1$E1_1c[FIN_survey_gpa1$E1_1==3] <- 3
FIN_survey_gpa1$E1_1c[FIN_survey_gpa1$E1_1==4] <- 10
FIN_survey_gpa1$E1_1c[FIN_survey_gpa1$E1_1==5] <- 12

FIN_survey_gpa1$E1_2c[FIN_survey_gpa1$E1_2==1] <- 1.5
FIN_survey_gpa1$E1_2c[FIN_survey_gpa1$E1_2==2] <- 3.5
FIN_survey_gpa1$E1_2c[FIN_survey_gpa1$E1_2==3] <- 5.5
FIN_survey_gpa1$E1_2c[FIN_survey_gpa1$E1_2==4] <- 8
FIN_survey_gpa1$E1_2c[FIN_survey_gpa1$E1_2==5] <- 10
FIN_survey_gpa1$E1_2c[FIN_survey_gpa1$E1_2=="x"] <- 0

FIN_survey_gpa1$E1_3c[FIN_survey_gpa1$E1_3==1] <- 0
FIN_survey_gpa1$E1_3c[FIN_survey_gpa1$E1_3==2] <- 0.5
FIN_survey_gpa1$E1_3c[FIN_survey_gpa1$E1_3==3] <- 1
FIN_survey_gpa1$E1_3c[FIN_survey_gpa1$E1_3==4] <- 4
FIN_survey_gpa1$E1_3c[FIN_survey_gpa1$E1_3==5] <- 8
FIN_survey_gpa1$E1_3c[FIN_survey_gpa1$E1_3=="x"] <- 0

FIN_survey_gpa1$E1_4c[FIN_survey_gpa1$E1_4==1] <- 5
FIN_survey_gpa1$E1_4c[FIN_survey_gpa1$E1_4==2] <- 15
FIN_survey_gpa1$E1_4c[FIN_survey_gpa1$E1_4==3] <- 25
FIN_survey_gpa1$E1_4c[FIN_survey_gpa1$E1_4==4] <- 35

FIN_survey_gpa1$E1_5c[FIN_survey_gpa1$E1_5==1] <- 0.5
FIN_survey_gpa1$E1_5c[FIN_survey_gpa1$E1_5==2] <- 1.5
FIN_survey_gpa1$E1_5c[FIN_survey_gpa1$E1_5==3] <- 2.5
FIN_survey_gpa1$E1_5c[FIN_survey_gpa1$E1_5==4] <- 3.5
FIN_survey_gpa1$E1_5c[FIN_survey_gpa1$E1_5==5] <- 4.5
FIN_survey_gpa1$E1_5c[FIN_survey_gpa1$E1_5==6] <- 5.5
FIN_survey_gpa1$E1_5c[FIN_survey_gpa1$E1_5==7] <- 8
FIN_survey_gpa1$E1_5c[FIN_survey_gpa1$E1_5==8] <- 10



t.test(colSums(rbind(clu4_3$count1, clu4_3$count2)), colSums(rbind(clu4_4$count1, clu4_4$count2)))$p.value #pval_dorm
t.test(clu4_3$count3, clu4_4$count3)$p.value #pval_lib
t.test(clu4_3$count6, clu4_4$count6)$p.value #pval_main
t.test(colSums(rbind(clu4_3$count4, clu4_3$count5, clu4_3$count9, clu4_3$count10, clu4_3$count11, clu4_3$count12)), colSums(rbind(clu4_4$count4, clu4_4$count5, clu4_4$count9, clu4_4$count10, clu4_4$count11, clu4_4$count12)))$p.value #pval_lec
#t.test(colSums(rbind(clu4_3$count7, clu4_3$count14)), colSums(rbind(clu4_4$count7, clu4_4$count14)))$p.value #pval_phys
t.test(clu4_3$GPA, clu4_4$GPA)$p.value #pval_gpa

t.test(colSums(rbind(clu4_3$A1_1, clu4_3$A1_2)), colSums(rbind(clu4_4$A1_1, clu4_4$A1_2)))$p.value #pval_study
t.test(colSums(rbind(clu4_3$A1_5, clu4_3$A1_9)), colSums(rbind(clu4_4$A1_5, clu4_4$A1_9)))$p.value #pval_soc
t.test(clu4_3$A1_10, clu4_4$A1_10)$p.value #pval_hob

t.test(clu4_3$time_cum, clu4_4$time_cum)$p.value #pval_timecum
t.test(clu4_3$time_avg, clu4_4$time_avg)$p.value #pval_timeavg

t.test(clu4_3$E1_1c, clu4_4$E1_1c)$p.value #pval_음주횟수
t.test(clu4_3$E1_2c, clu4_4$E1_2c)$p.value #pval_몇잔
t.test(clu4_3$E1_3c, clu4_4$E1_3c)$p.value #pval_과음횟수
t.test(clu4_3$E1_4c, clu4_4$E1_4c)$p.value #pval_개비
t.test(clu4_3$E1_5c, clu4_4$E1_5c)$p.value #pval_일별 인터넷 사용시간 

t.test(clu4_3$E1_6, clu4_4$E1_6)$p.value #pval_internet interruption
t.test(colMeans(rbind(clu4_3$E1_7,clu4_3$E1_8,clu4_3$E1_9)), colMeans(rbind(clu4_4$E1_7,clu4_4$E1_8,clu4_4$E1_9)))$p.value #pval_sexual

t.test(clu4_3$E3_3-clu4_3$E3_4, clu4_4$E3_3-clu4_4$E3_4)$p.value #pval_외향성 
t.test(clu4_3$E3_5+clu4_3$E4_1-clu4_3$E4_2, clu4_4$E3_5+clu4_4$E4_1-clu4_4$E4_2)$p.value #pval_행복감 




























































































































