######### Preprocessing 2017 trajectory #########
install.packages("data.table")
library(data.table)
install.packages("sqldf")
library(sqldf)
install.packages("stringr")
library(stringr)
install.packages("gdata")
library(gdata)


####### Uploading raw data #######
work_dir1 <- "E:/학생데이터 분석/2017/rawdata/processing_rawdata"
setwd(work_dir1)

LUT_stu <- read.csv("Stu_id_lut.csv")
LUT_loc <- read.csv("Location_id_lut.csv")
LUT_loc <- LUT_loc[,1:3]
LUT_class1 <- read.csv("LUT_class_20171.csv")
LUT_class2 <- read.csv("LUT_class_20172.csv")

IO_lib <- read.csv("IO_LIB.csv")
Semi_lib <- read.csv("SEMI_LIB.csv")
Seat_lib <- read.csv("SEAT_LIB.csv")

lent_space <- read.csv("LENT_SPACE.csv")

attend_11 <- read.csv("attend11.csv")
attend_12 <- read.csv("attend12.csv")
attend_21 <- read.csv("attend21.csv")
attend_22 <- read.csv("attend22.csv")

attend1 <- rbind(attend_11, attend_12) 
rm(attend_11, attend_12)

attend2 <- rbind(attend_21, attend_22) 
rm(attend_21, attend_22)

####### preprocessing #######

# lent_space
lent_space <- sqldf("SELECT lent_space.*, LUT_stu.변환학번 FROM lent_space, LUT_stu 
                    WHERE lent_space.아이디 = LUT_stu.학번")
lent_space <- lent_space[, -c(1,2,5,6,9:14)]
lent_space[,6] <- paste(lent_space[,1], lent_space[,2]) 
lent_space <- lent_space[,-c(1,2)]
lent_space8 <- lent_space[,c(1,3,4)]
lent_space9 <- lent_space[,c(2,3,4)]
colnames(lent_space8) <- c("starttime", "Stu_id", "place")
colnames(lent_space9) <- c("endtime", "Stu_id", "place")

lent_space8 <- sqldf("SELECT lent_space8.*, LUT_loc.Category_id FROM lent_space8, LUT_loc
                     WHERE lent_space8.place = LUT_loc.장소명")
lent_space8 <- sqldf("SELECT lent_space8.*, LUT_loc.Location_id FROM lent_space8, LUT_loc
                     WHERE lent_space8.place = LUT_loc.장소명")
lent_space8$Date <- substr(lent_space8$starttime,1,10)
lent_space8$Time <- substr(lent_space8$starttime,12,19)
lent_space8$Flag <- 8
lent_space8 <- lent_space8[,-c(1,3)] 
lent_space8 <- lent_space8[,c(1,4,5,2,3,6)] 

lent_space9 <- sqldf("SELECT lent_space9.*, LUT_loc.Category_id FROM lent_space9, LUT_loc
                     WHERE lent_space9.place = LUT_loc.장소명")
lent_space9 <- sqldf("SELECT lent_space9.*, LUT_loc.Location_id FROM lent_space9, LUT_loc
                     WHERE lent_space9.place = LUT_loc.장소명")
lent_space9$Date <- substr(lent_space9$endtime,1,10)
lent_space9$Time <- substr(lent_space9$endtime,12,19)
lent_space9$Flag <- 9

lent_space9 <- lent_space9[,-c(1,3)] 
lent_space9 <- lent_space9[,c(1,4,5,2,3,6)] 

lent_space <- rbind(lent_space8, lent_space9)
write.csv(lent_space, "lent_space_2017.csv")
rm(lent_space8, lent_space9)

# IO_lib
colnames(IO_lib) <- c("Stu_id", "place", "TIME") 
IO_lib$Date <- substr(IO_lib$TIME,1,10)
IO_lib_78_3 <- subset(IO_lib, IO_lib$place=='국제1층입구1'
                      | IO_lib$place=='국제1층입구2'
                      | IO_lib$place=='국제1층입구3')
IO_lib_78_2 <- subset(IO_lib, IO_lib$place=='국제1층출구4'
                      | IO_lib$place=='국제1층출구5'
                      | IO_lib$place=='국제1층출구6')
IO_lib_79_3 <- subset(IO_lib, IO_lib$place=='국제지하1층 입구1'
                      | IO_lib$place=='국제지하1층 입구2')
IO_lib_79_2 <- subset(IO_lib, IO_lib$place=='국제지하1층 출구3'
                      | IO_lib$place=='국제지하1층 출구4')

IO_lib_78_3$Category_id <- 3
IO_lib_78_3$Location_id <- 78
IO_lib_78_3$Flag <- 3
IO_lib_78_3 <- IO_lib_78_3[,-2]

IO_lib_78_2$Category_id <- 3
IO_lib_78_2$Location_id <- 78
IO_lib_78_2$Flag <- 2
IO_lib_78_2 <- IO_lib_78_2[,-2]

IO_lib_79_3$Category_id <- 3
IO_lib_79_3$Location_id <- 79
IO_lib_79_3$Flag <- 3
IO_lib_79_3 <- IO_lib_79_3[,-2]

IO_lib_79_2$Category_id <- 3
IO_lib_79_2$Location_id <- 79
IO_lib_79_2$Flag <- 2
IO_lib_79_2 <- IO_lib_79_2[,-2]

IO_lib <- rbind(IO_lib_78_2, IO_lib_78_3, IO_lib_79_2, IO_lib_79_3)
rm(IO_lib_78_2, IO_lib_78_3, IO_lib_79_2, IO_lib_79_3)

IO_lib$Time <- str_sub(IO_lib$TIME,-5,-1)
IO_lib <- IO_lib[,-2]
IO_lib$Time <- paste0(IO_lib$Time,":00")
IO_lib <- IO_lib[,c(1,2,6,3,4,5)]
write.csv(IO_lib, "IO_lib_2017.csv")

# Seat_lib
Seat_lib$Category_id <- 3
Seat_lib4 <- Seat_lib[,c(1,2,3,5)]
Seat_lib5 <- Seat_lib[,c(1,2,4,5)]

Seat_lib4 <- sqldf("SELECT Seat_lib4.*, LUT_loc.Location_id FROM Seat_lib4, LUT_loc
                     WHERE Seat_lib4.place = LUT_loc.장소명")
Seat_lib4$Date <- substr(Seat_lib4$starttime,1,10)
Seat_lib4$Time <- str_sub(Seat_lib4$starttime,-5,-1)
Seat_lib4$Time <- paste0(Seat_lib4$Time,":00")
Seat_lib4$Flag <- 4
Seat_lib4 <- Seat_lib4[,-c(2,3)]
Seat_lib4 <- Seat_lib4[,c(1,4,5,2,3,6)]

Seat_lib5 <- sqldf("SELECT Seat_lib5.*, LUT_loc.Location_id FROM Seat_lib5, LUT_loc
                     WHERE Seat_lib5.place = LUT_loc.장소명")
Seat_lib5$Date <- substr(Seat_lib5$endtime,1,10)
Seat_lib5$Time <- str_sub(Seat_lib5$endtime,-5,-1)
Seat_lib5$Time <- paste0(Seat_lib5$Time,":00")
Seat_lib5$Flag <- 5
Seat_lib5 <- Seat_lib5[,-c(2,3)]
Seat_lib5 <- Seat_lib5[,c(1,4,5,2,3,6)]

Seat_lib <- rbind(Seat_lib4,Seat_lib5)
write.csv(Seat_lib, "Seat_lib_2017.csv")
rm(Seat_lib4, Seat_lib5)

# Semi_lib
Semi_lib$Category_id <- 3
colnames(Semi_lib)[1:4] <- c("Stu_id", "place", "starttime", "endtime")
Semi_lib6 <- Semi_lib[,c(1,2,3,5)]
Semi_lib7 <- Semi_lib[,c(1,2,4,5)]

Semi_lib6 <- sqldf("SELECT Semi_lib6.*, LUT_loc.Location_id FROM Semi_lib6, LUT_loc
                     WHERE Semi_lib6.place = LUT_loc.장소명")
Semi_lib6$Date <- substr(Semi_lib6$starttime,1,10)
Semi_lib6$Time <- str_sub(Semi_lib6$starttime,-5,-1)
Semi_lib6$Time <- paste0(Semi_lib6$Time,":00")
Semi_lib6$Flag <- 6
Semi_lib6 <- Semi_lib6[,-c(2,3)]
Semi_lib6 <- Semi_lib6[,c(1,4,5,2,3,6)]

Semi_lib7 <- sqldf("SELECT Semi_lib7.*, LUT_loc.Location_id FROM Semi_lib7, LUT_loc
                     WHERE Semi_lib7.place = LUT_loc.장소명")
Semi_lib7$Date <- substr(Semi_lib7$endtime,1,10)
Semi_lib7$Time <- str_sub(Semi_lib7$endtime,-5,-1)
Semi_lib7$Time <- paste0(Semi_lib7$Time,":00")
Semi_lib7$Flag <- 7
Semi_lib7 <- Semi_lib7[,-c(2,3)]
Semi_lib7 <- Semi_lib7[,c(1,4,5,2,3,6)]

Semi_lib <- rbind(Semi_lib6, Semi_lib7)
write.csv(Semi_lib, "Semi_lib_2017.csv")
rm(Semi_lib6, Semi_lib7)

# attendance
# tag
attend2 <- attend2[,-c(1,8)]
attend2 <- sqldf("SELECT attend2.*, LUT_stu.변환학번 FROM attend2, LUT_stu 
                  WHERE attend2.REG_ID = LUT_stu.학번")
attend2 <- attend2[,-6]
colnames(attend2)[6] <- "Stu_id"
attend2 <- sqldf("SELECT attend2.*, LUT_class2.ROOM, LUT_class2.CLASS_DT, LUT_class2.STARTTIME, LUT_class2.ENDTIME
                 FROM attend2, LUT_class2 WHERE attend2.CLASS_SEQ = LUT_class2.CLASS_SEQ")
attend2$label <- substr(attend2$ROOM,1,1)
attend2 <- subset(attend2, attend2$label == "I")
attend2$label <- NULL
attend2_rec <- subset(attend2, attend2$FLAG == "I" | attend2$FLAG == "R" )
attend2_tag <- subset(attend2, attend2$FLAG == "N" | attend2$FLAG == "B" )


attend2_tag_13 <- subset(attend2_tag, attend2_tag$ATTEND_STATUS == "A")
attend2_tag_14 <- subset(attend2_tag, attend2_tag$ATTEND_STATUS == "S")
attend2_tag_15 <- attend2_tag

attend2_tag_13$Date <- paste0(substr(attend2_tag_13$ATTEND_DT,1,4),"-",substr(attend2_tag_13$ATTEND_DT,5,6)
                           ,"-",substr(attend2_tag_13$ATTEND_DT,7,8))
attend2_tag_13$Time <- paste0(substr(attend2_tag_13$ATTEND_DT,9,10),":",substr(attend2_tag_13$ATTEND_DT,11,12)
                           ,":",substr(attend2_tag_13$ATTEND_DT,13,14))
attend2_tag_14$Date <- paste0(substr(attend2_tag_14$ATTEND_DT,1,4),"-",substr(attend2_tag_14$ATTEND_DT,5,6)
                              ,"-",substr(attend2_tag_14$ATTEND_DT,7,8))
attend2_tag_14$Time <- paste0(substr(attend2_tag_14$ATTEND_DT,9,10),":",substr(attend2_tag_14$ATTEND_DT,11,12)
                              ,":",substr(attend2_tag_14$ATTEND_DT,13,14))
attend2_tag_15$Date <- paste0(substr(attend2_tag_15$CLASS_DT,1,4),"-",substr(attend2_tag_15$CLASS_DT,5,6)
                              ,"-",substr(attend2_tag_15$CLASS_DT,7,8))
for (r in 1:nrow(attend2_tag_15)){
  print(r)
  if (nchar(attend2_tag_15$ENDTIME[r])==3){
    attend2_tag_15$ENDTIME[r] <- paste0("0", attend2_tag_15$ENDTIME[r])
  }
}
attend2_tag_15$Time <- paste0(substr(attend2_tag_15$ENDTIME,1,2),":",substr(attend2_tag_15$ENDTIME,3,4)
                              ,":00")
attend2_tag_13$flag <- 13
attend2_tag_14$flag <- 14
attend2_tag_15$flag <- 15

attend2_tag <- rbind(attend2_tag_13, attend2_tag_14, attend2_tag_15)
rm(attend2_tag_13, attend2_tag_14, attend2_tag_15)
attend2_tag <- attend2_tag[,-c(1:5,8:10)]

attend2_tag$bd <- substr(attend2_tag$ROOM,1,3)
unique(attend2_tag$bd)
attend2_tag$Category_id[attend2_tag$bd=="I진A"] <- 9
attend2_tag$Category_id[attend2_tag$bd=="I자A"] <- 4
attend2_tag$Category_id[attend2_tag$bd=="I진D"] <- 12
attend2_tag$Category_id[attend2_tag$bd=="I자B"] <- 5
attend2_tag$Category_id[attend2_tag$bd=="I종3"] <- 6
attend2_tag$Category_id[attend2_tag$bd=="I진B"] <- 10
attend2_tag$Category_id[attend2_tag$bd=="I종2"] <- 6
attend2_tag$Category_id[attend2_tag$bd=="I진C"] <- 11

attend2_tag$bd[attend2_tag$bd=="I진A"] <- "진리관A"
attend2_tag$bd[attend2_tag$bd=="I자A"] <- "자유관A"
attend2_tag$bd[attend2_tag$bd=="I진D"] <- "진리관D"
attend2_tag$bd[attend2_tag$bd=="I자B"] <- "자유관B"
attend2_tag$bd[attend2_tag$bd=="I종3"] <- "종합관"
attend2_tag$bd[attend2_tag$bd=="I진B"] <- "진리관B"
attend2_tag$bd[attend2_tag$bd=="I종2"] <- "종합관"
attend2_tag$bd[attend2_tag$bd=="I진C"] <- "진리관C"

attend2_tag_j <- subset(attend2_tag, attend2_tag$Category_id==6)
attend2_tag_rest <- subset(attend2_tag, attend2_tag$Category_id!=6)

attend2_tag_j$Location_id <- paste(attend2_tag_j$bd, substr(attend2_tag_j$ROOM,3,5))
attend2_tag_rest$Location_id <- paste(attend2_tag_rest$bd, substr(attend2_tag_rest$ROOM,4,6))
attend2_tag <- rbind(attend2_tag_j,attend2_tag_rest)
rm(attend2_tag_j,attend2_tag_rest)
colnames(attend2_tag)[8] <- "Location_id2" 
attend2_tag <- attend2_tag[,-c(2,6)]
attend2_tag <- attend2_tag_o
attend2_tag <- sqldf("SELECT attend2_tag.*, LUT_loc.Location_id FROM attend2_tag, LUT_loc
                     WHERE attend2_tag.Location_id2 = LUT_loc.장소명")
attend2_tag <- attend2_tag[,-6]
attend2_tag <- attend2_tag[,c(1,2,3,5,6,4)]

write.csv(attend2_tag,"attendance_tag_20172.csv")

#rec
attend2_rec$STARTTIME <- as.character(attend2_rec$STARTTIME)
attend2_rec$ENDTIME <- as.character(attend2_rec$ENDTIME)

for (r in 1:nrow(attend2_rec)){
  print(r)
  if (nchar(attend2_rec$STARTTIME[r])==3){
    attend2_rec$STARTTIME[r] <- paste0("0", attend2_rec$STARTTIME[r])
  }
}
for (r in 1:nrow(attend2_rec)){
  print(r)
  if (nchar(attend2_rec$ENDTIME[r])==3){
    attend2_rec$ENDTIME[r] <- paste0("0", attend2_rec$ENDTIME[r])
  }
}
attend2_rec_16 <- subset(attend2_rec, attend2_rec$ATTEND_STATUS == "A")
attend2_rec_17 <- subset(attend2_rec, attend2_rec$ATTEND_STATUS == "S")
attend2_rec_18 <- attend2_rec
attend2_rec_16$Date <- paste0(substr(attend2_rec_16$CLASS_DT,1,4),"-",substr(attend2_rec_16$CLASS_DT,5,6)
                              ,"-",substr(attend2_rec_16$CLASS_DT,7,8))
attend2_rec_17$Date <- paste0(substr(attend2_rec_17$CLASS_DT,1,4),"-",substr(attend2_rec_17$CLASS_DT,5,6)
                              ,"-",substr(attend2_rec_17$CLASS_DT,7,8))
attend2_rec_18$Date <- paste0(substr(attend2_rec_18$CLASS_DT,1,4),"-",substr(attend2_rec_18$CLASS_DT,5,6)
                              ,"-",substr(attend2_rec_18$CLASS_DT,7,8))
attend2_rec_16$Time <- paste0(substr(attend2_rec_16$STARTTIME,1,2),":",substr(attend2_rec_16$STARTTIME,3,4)
                              ,":00")
attend2_rec_17$Time <- paste0(substr(attend2_rec_17$STARTTIME,1,2),":20",":00")
attend2_rec_18$Time <- paste0(substr(attend2_rec_18$ENDTIME,1,2),":",substr(attend2_rec_18$ENDTIME,3,4)
                              ,":00")
attend2_rec_16$flag <- 16
attend2_rec_17$flag <- 17
attend2_rec_18$flag <- 18

attend2_rec <- rbind(attend2_rec_16, attend2_rec_17, attend2_rec_18)
rm(attend2_rec_16, attend2_rec_17, attend2_rec_18)

attend2_rec_o <- attend2_rec
attend2_rec <- attend2_rec[,-c(1:5,8:10)]

attend2_rec$bd <- substr(attend2_rec$ROOM,1,3)
unique(attend2_rec$bd)
attend2_rec$Category_id[attend2_rec$bd=="I진A"] <- 9
attend2_rec$Category_id[attend2_rec$bd=="I자A"] <- 4
attend2_rec$Category_id[attend2_rec$bd=="I종3"] <- 6
attend2_rec$Category_id[attend2_rec$bd=="I진B"] <- 10
attend2_rec$Category_id[attend2_rec$bd=="I진D"] <- 12
attend2_rec$Category_id[attend2_rec$bd=="I자B"] <- 5
attend2_rec$Category_id[attend2_rec$bd=="I진C"] <- 11
attend2_rec$Category_id[attend2_rec$bd=="I크대"] <- 16

attend2_rec$bd[attend2_rec$bd=="I진A"] <- "진리관A"
attend2_rec$bd[attend2_rec$bd=="I자A"] <- "자유관A"
attend2_rec$bd[attend2_rec$bd=="I종3"] <- "종합관"
attend2_rec$bd[attend2_rec$bd=="I진B"] <- "진리관B"
attend2_rec$bd[attend2_rec$bd=="I진D"] <- "진리관D"
attend2_rec$bd[attend2_rec$bd=="I자B"] <- "자유관B"
attend2_rec$bd[attend2_rec$bd=="I진C"] <- "진리관C"
attend2_rec$bd[attend2_rec$bd=="I크대"] <- "크101"

attend2_rec_j <- subset(attend2_rec, attend2_rec$Category_id==6)
attend2_rec_spo <- subset(attend2_rec, attend2_rec$Category_id==16)
attend2_rec_rest <- subset(attend2_rec, attend2_rec$Category_id!=6 & attend2_rec$Category_id!=16)

attend2_rec_spo$Location_id <- 733
attend2_rec_j$Location_id <- paste(attend2_rec_j$bd, substr(attend2_rec_j$ROOM,3,5))
attend2_rec_rest$Location_id <- paste(attend2_rec_rest$bd, substr(attend2_rec_rest$ROOM,4,6))

attend2_rec <- rbind(attend2_rec_j, attend2_rec_rest)
colnames(attend2_rec)[8] <- "Location_id2" 
attend2_tag_jrest <- attend2_rec
attend2_rec <- sqldf("SELECT attend2_rec.*, LUT_loc.Location_id FROM attend2_rec, LUT_loc
                     WHERE attend2_rec.Location_id2 = LUT_loc.장소명")
attend2_rec$Location_id2 <- NULL

attend2_rec <- rbind(attend2_rec, attend2_rec_spo)
rm(attend2_rec_j, attend2_rec_rest, attend2_tag_jrest, attend2_rec_spo)

attend2_rec <- attend2_rec[,-c(2,6)]
attend2_rec <- attend2_rec[,c(1,2,3,5,6,4)]

write.csv(attend2_rec,"attendance_rec_20172.csv")

# dorm
stu_id_lut <- read.csv("Stu_id_lut.csv")
colnames(stu_id_lut) <- c("real", "Stu_id")
dorm_list_1 <- read.csv("2017-1_dorm_list.csv", header=F)
dorm_list_1 <- dorm_list_1[,3:4]
colnames(dorm_list_1) <- c("name", "real")
dorm_list_2 <- read.csv("2017-2_dorm_list.csv", header=F)
dorm_list_2 <- dorm_list_2[,1:2]
colnames(dorm_list_2) <- c("name", "real")
dorm_list_1 <- sqldf("SELECT dorm_list_1.*, stu_id_lut.Stu_id FROM dorm_list_1, stu_id_lut
                     WHERE dorm_list_1.real = stu_id_lut.real")
dorm_list_2 <- sqldf("SELECT dorm_list_2.*, stu_id_lut.Stu_id FROM dorm_list_2, stu_id_lut
                     WHERE dorm_list_2.real = stu_id_lut.real")
dorm_list_1 <- dorm_list_1[,-2]
dorm_list_2 <- dorm_list_2[,-2]

dorm_a <- read.csv("dorm_A.csv")
dorm_b <- read.csv("dorm_B.csv")
dorm_c <- read.csv("dorm_C.csv")
dorm_d1 <- read.csv("dorm_D1.csv")
dorm_d2 <- read.csv("dorm_D2.csv")
dorm_e <- read.csv("dorm_E.csv")
dorm_f <- read.csv("dorm_F.csv")
dorm_g <- read.csv("dorm_G.csv")

dorm <- rbind(dorm_a, dorm_b, dorm_c, dorm_d1, dorm_d2, dorm_e, dorm_f, dorm_g)

rm(dorm_a, dorm_b, dorm_c, dorm_d1, dorm_d2, dorm_e, dorm_f, dorm_g)

colnames(dorm) <- c("Date","Time","bd","gate","name","cardNo")
dorm$bd <- NULL
dorm$flag <- 1

dorm$Category_id[substr(dorm$gate,4,4)=="A"] <- 1
dorm$Category_id[substr(dorm$gate,4,4)=="B"] <- 1
dorm$Category_id[substr(dorm$gate,4,4)=="C"] <- 1
dorm$Category_id[substr(dorm$gate,4,4)=="D"] <- 2
dorm$Category_id[substr(dorm$gate,4,4)=="E"] <- 2
dorm$Category_id[substr(dorm$gate,4,4)=="F"] <- 2
dorm$Category_id[substr(dorm$gate,4,4)=="G"] <- 2

dorm$Location_id[substr(dorm$gate,4,4)=="A"] <- 3
dorm$Location_id[substr(dorm$gate,4,4)=="B"] <- 4
dorm$Location_id[substr(dorm$gate,4,4)=="C"] <- 5
dorm$Location_id[substr(dorm$gate,4,4)=="D"] <- 6
dorm$Location_id[substr(dorm$gate,4,4)=="E"] <- 7
dorm$Location_id[substr(dorm$gate,4,4)=="F"] <- 8
dorm$Location_id[substr(dorm$gate,4,4)=="G"] <- 9

dorm$gate <- NULL
dorm$cardNo <- NULL

dorm <- dorm[,c(3,1,2,6,5,4)]

dorm1 <- subset(dorm, substr(dorm$Date,6,7) == "03" | substr(dorm$Date,6,7) == "04" |
                  substr(dorm$Date,6,7) == "05" | substr(dorm$Date,6,7) == "06") # 1학기
dorm2 <- subset(dorm, substr(dorm$Date,6,7) == "09" | substr(dorm$Date,6,7) == "10" |
                  substr(dorm$Date,6,7) == "11" | substr(dorm$Date,6,7) == "12") 
dorm_name1 <- data.frame(unique(dorm1$name),unique(dorm1$name))
dorm_name2 <- data.frame(unique(dorm2$name),unique(dorm2$name))
colnames(dorm_name1) <- c("origin", "converted")
colnames(dorm_name2) <- c("origin", "converted")

dorm_name1 <- subset(dorm_name1, substr(dorm_name1$origin,1,2) == "송도")
dorm_name2 <- subset(dorm_name2, substr(dorm_name2$origin,1,2) == "송도")

dorm_name1$converted <- gsub("송도 1학사", "", dorm_name1$converted)
dorm_name1$converted <- gsub("송도 2학사", "", dorm_name1$converted)
dorm_name1$converted <- gsub("송도1학사", "", dorm_name1$converted)
dorm_name1$converted <- gsub("송도2학사", "", dorm_name1$converted)
dorm_name1$converted <- gsub("송도학사", "", dorm_name1$converted)
dorm_name1$converted <- gsub("송도학서", "", dorm_name1$converted)
dorm_name1$converted <- gsub("동", "", dorm_name1$converted)
dorm_name1$converted <- gsub("호", "", dorm_name1$converted)
dorm_name1$converted <- gsub(" ", "", dorm_name1$converted)

dorm_name2$converted <- gsub("송도 1학사", "", dorm_name2$converted)
dorm_name2$converted <- gsub("송도 2학사", "", dorm_name2$converted)
dorm_name2$converted <- gsub("송도1학사", "", dorm_name2$converted)
dorm_name2$converted <- gsub("송도2학사", "", dorm_name2$converted)
dorm_name2$converted <- gsub("송도학사", "", dorm_name2$converted)
dorm_name2$converted <- gsub("송도학서", "", dorm_name2$converted)
dorm_name2$converted <- gsub("동", "", dorm_name2$converted)
dorm_name2$converted <- gsub("호", "", dorm_name2$converted)
dorm_name2$converted <- gsub(" ", "", dorm_name2$converted)

dorm_name1 <- sqldf("SELECT dorm_name1.*, dorm_list_1.Stu_id FROM dorm_name1, dorm_list_1
                    WHERE dorm_name1.converted = dorm_list_1.name")
dorm_name2 <- sqldf("SELECT dorm_name2.*, dorm_list_2.Stu_id FROM dorm_name2, dorm_list_2
                    WHERE dorm_name2.converted = dorm_list_2.name")
dorm1 <- sqldf("SELECT dorm1.*, dorm_name1.Stu_id FROM dorm1, dorm_name1 WHERE dorm1.name = dorm_name1.origin")
dorm2 <- sqldf("SELECT dorm2.*, dorm_name2.Stu_id FROM dorm2, dorm_name2 WHERE dorm2.name = dorm_name2.origin")
dorm <- rbind(dorm1, dorm2)
dorm <- dorm[,-1]
dorm <- dorm[,c(6,1,2,3,4,5)]

write.csv(dorm, "dorm_2017.csv")

# fitness
setwd("E:/학생데이터 분석/2017/rawdata/6. 휘트니스_출입기록(180524 수령)/2017년 입장내역/2017년 입장내역/csv")

fit1 <- read.csv("2017_12월.csv")
fit1$회원명 <- NULL
fit1$휴대폰 <- NULL
fit1$강습명 <- NULL
fit1$출석횟수 <- NULL
fit1$나이 <- NULL
fit1$Stu_id <- as.character(fit1$카드번호)
fit1$카드번호 <- NULL

for (r in 1:nrow(fit1)){
  if (substr(fit1$Stu_id[r],1,1)=="0"){
    fit1$Stu_id[r] <- substr(fit1$Stu_id[r],2,11)
  }
}

fit1 <- sqldf("SELECT fit1.*, LUT_stu.변환학번 FROM fit1, LUT_stu WHERE fit1.Stu_id = LUT_stu.학번")
fit1$Stu_id <- fit1[,ncol(fit1)]
fit1[,ncol(fit1)] <- NULL

fitness <- data.frame()

for (r in 1:nrow(fit1)){
  print(r)
  for (c in 1:(ncol(fit1)-1)){
    temp <- data.frame(fit1$Stu_id[r], paste0("2017-12-",substr(colnames(fit1)[c],2,3)), 
                       fit1[r,c], 604, 7, 12)
    fitness <- rbind(fitness, temp)  
  }
}
colnames(fitness) <- c("Stu_id", "Date", "Time", "Category_id", "LOcation_id", "Flag")
fitness$Time <- as.character(fitness$Time)

del <- NULL
for (r in 1:nrow(fitness)){
  print(r)
  if (is.na(fitness$Time[r])){
    del <- rbind(del,r)
  }
  else if (nchar(fitness$Time[r])==0){
    del <- rbind(del,r)
  }
  else if (nchar(fitness$Time[r])==7){
    fitness$Time[r] <- paste0("0",fitness$Time[r])
  }
}
fitness <- fitness[-del,]

FITNESS <- rbind(FITNESS,fitness)

rm(fit1, fitness)

write.csv(FITNESS, "fitness_2017.csv")

all_data_2017 <- rbind(attend1_rec, attend1_tag, attend2_rec, attend2_tag,
                        dorm,fitness,IO_lib,lent_space,Seat_lib,Semi_lib)

setwd("E:/학생데이터 분석")
write.csv(all_data_2017, "all_data(2017_1,2).csv")

# SSO
sso1 <- read.csv("sso1.csv")
sso1_1 <- read.csv("sso1-1.csv")
sso2 <- read.csv("sso2.csv")
sso2_1 <- read.csv("sso2-1.csv")
sso <- rbind(sso1, sso1_1, sso2, sso2_1)
rm(sso1, sso1_1, sso2, sso2_1)
sso <- sso[,c(1:4,7)]
colnames(sso) <- c("Stu_id","io","Date","Time","ip")

sso <- subset(sso, substr(sso$ip,1,2) == "1." | substr(sso$ip,1,4) == "172.")

sso <- subset(sso, substr(sso$ip,1,6) == "1.233." | substr(sso$ip,1,6) == "172.20" | substr(sso$ip,1,6) == "172.21")

sso$bd[substr(sso$ip,7,9) == "206"| substr(sso$ip,7,9) == "207"|substr(sso$ip,7,9) == "208"] <- "파워플랜트" 
sso$bd[substr(sso$ip,7,9) == "209"] <- "종합관" 
sso$bd[substr(sso$ip,7,9) == "210" | substr(sso$ip,7,9) == "211"] <- "자유관A" 
sso$bd[substr(sso$ip,7,9) == "212" | substr(sso$ip,7,9) == "213"] <- "자유관B" 

sso$bd[substr(sso$ip,7,9) == "214" | substr(sso$ip,7,9) == "215"] <- "진리관A" 
sso$bd[substr(sso$ip,7,9) == "216" | substr(sso$ip,7,9) == "217"] <- "진리관B" 
sso$bd[substr(sso$ip,7,9) == "218" | substr(sso$ip,7,9) == "219"] <- "진리관C" 
sso$bd[substr(sso$ip,7,9) == "220" | substr(sso$ip,7,9) == "221"] <- "진리관D" 

sso$bd[substr(sso$ip,7,9) == "222" | substr(sso$ip,7,9) == "223" |
       substr(sso$ip,7,9) == "224" | substr(sso$ip,7,9) == "225"] <- "언더우드기념도서관" 

sso$bd[substr(sso$ip,7,9) == "228" | substr(sso$ip,7,9) == "229" |
       substr(sso$ip,7,9) == "230"] <- "D" 
sso$bd[substr(sso$ip,7,9) == "231" | substr(sso$ip,7,9) == "232" |
       substr(sso$ip,7,9) == "233" | substr(sso$ip,7,9) == "234"] <- "E" 
sso$bd[substr(sso$ip,7,9) == "235" | substr(sso$ip,7,9) == "236" |
       substr(sso$ip,7,9) == "237"] <- "F" 
sso$bd[substr(sso$ip,7,9) == "238" | substr(sso$ip,7,9) == "239" | substr(sso$ip,7,9) == "240" |
       substr(sso$ip,7,9) == "241" | substr(sso$ip,7,9) == "242" | substr(sso$ip,7,9) == "243"] <- "A" 
sso$bd[substr(sso$ip,7,9) == "244" | substr(sso$ip,7,9) == "245" |
       substr(sso$ip,7,9) == "246"] <- "B"
sso$bd[substr(sso$ip,7,9) == "247" | substr(sso$ip,7,9) == "248" |
       substr(sso$ip,7,9) == "249"] <- "C"
sso$bd[substr(sso$ip,7,9) == "250" | substr(sso$ip,7,9) == "251" |
       substr(sso$ip,7,9) == "252"] <- "G"

sso_NA <- subset(sso, is.na(sso$bd) == TRUE)

sso_NA <- subset(sso, is.na(sso$bd) == FALSE)
sso <- sso_NA
rm(sso_NA)
unique(sso$bd)

sso_o <- sso
sso <- sqldf("SELECT sso.*, LUT_loc.* FROM sso, LUT_loc WHERE sso.bd = LUT_loc.장소명")
sso <- sso[,-c(2,5,6,8)]
sso <- sso[,c(1:3,5,4)]
sso$Flag <- 11

all_data_2017 <- rbind(all_data_2017, sso)

write.csv(sso, "IPloc_2017.csv")
write.csv(all_data_2017, "all_data(2017_1,2).csv")

















































