library(tidyverse)

setwd("~/Desktop/HP")

Pilot.Data <- read.csv("Hiring+Perceptions_July+3%2C+2018_10.27.csv")
Pilot.Data.White <- Pilot.Data %>%
  filter(Q59 == "White")

Pilot.Data.White

Pilot.Data.White <- Pilot.Data.White[,-1:-11]

Pilot.Data.White[,2] <- as.numeric(Pilot.Data.White[,2])
Pilot.Data.White[,3] <- as.numeric(Pilot.Data.White[,3])
Pilot.Data.White[,4] <- as.numeric(Pilot.Data.White[,4])
Pilot.Data.White[,5] <- as.numeric(Pilot.Data.White[,5])
Pilot.Data.White[,6] <- as.numeric(Pilot.Data.White[,6])
Pilot.Data.White[,7] <- as.numeric(Pilot.Data.White[,7])
Pilot.Data.White[,8] <- as.numeric(Pilot.Data.White[,8])
Pilot.Data.White[,9] <- as.numeric(Pilot.Data.White[,9])
Pilot.Data.White[,10] <- as.numeric(Pilot.Data.White[,10])
Pilot.Data.White[,11] <- as.numeric(Pilot.Data.White[,11])
Pilot.Data.White[,12] <- as.numeric(Pilot.Data.White[,12])
Pilot.Data.White[,13] <- as.numeric(Pilot.Data.White[,13])
Pilot.Data.White[,14] <- as.numeric(Pilot.Data.White[,14])
 
Pilot.Data.White$Q32_1 <- recode(Pilot.Data.White$Q32_1, `8` = 7, `7` = 6, `6` = 5, `5` = 4, `4` = 3, `3` = 2, `2` = 1)
Pilot.Data.White$Q32_2 <- recode(Pilot.Data.White$Q32_2, `8` = 7, `7` = 6, `6` = 5, `5` = 4, `4` = 3, `3` = 2, `2` = 1)
Pilot.Data.White$Q32_3 <- recode(Pilot.Data.White$Q32_3, `8` = 7, `7` = 6, `6` = 5, `5` = 4, `4` = 3, `3` = 2, `2` = 1)
Pilot.Data.White$Q33_1 <- recode(Pilot.Data.White$Q33_1, `8` = 7, `7` = 6, `6` = 5, `5` = 4, `4` = 3, `3` = 2, `2` = 1)
Pilot.Data.White$Q33_2 <- recode(Pilot.Data.White$Q33_2, `8` = 7, `7` = 6, `6` = 5, `5` = 4, `4` = 3, `3` = 2, `2` = 1)
Pilot.Data.White$Q33_3 <- recode(Pilot.Data.White$Q33_3, `8` = 7, `7` = 6, `6` = 5, `5` = 4, `4` = 3, `3` = 2, `2` = 1)
Pilot.Data.White$Q34_1 <- recode(Pilot.Data.White$Q34_1, `8` = 7, `7` = 6, `6` = 5, `5` = 4, `4` = 3, `3` = 2, `2` = 1)
Pilot.Data.White$Q34_2 <- recode(Pilot.Data.White$Q34_2, `8` = 7, `7` = 6, `6` = 5, `5` = 4, `4` = 3, `3` = 2, `2` = 1)
Pilot.Data.White$Q34_3 <- recode(Pilot.Data.White$Q34_3, `8` = 7, `7` = 6, `6` = 5, `5` = 4, `4` = 3, `3` = 2, `2` = 1)
Pilot.Data.White$Q35_1 <- recode(Pilot.Data.White$Q35_1, `8` = 7, `7` = 6, `6` = 5, `5` = 4, `4` = 3, `3` = 2, `2` = 1)
Pilot.Data.White$Q35_2 <- recode(Pilot.Data.White$Q35_2, `8` = 7, `7` = 6, `6` = 5, `5` = 4, `4` = 3, `3` = 2, `2` = 1)
Pilot.Data.White$Q35_3 <- recode(Pilot.Data.White$Q35_3, `8` = 7, `7` = 6, `6` = 5, `5` = 4, `4` = 3, `3` = 2, `2` = 1)
Pilot.Data.White$Q35_4 <- recode(Pilot.Data.White$Q35_4, `8` = 7, `7` = 6, `6` = 5, `5` = 4, `4` = 3, `3` = 2, `2` = 1)

Pilot.Data.White[,16] <- as.numeric(Pilot.Data.White[,16])
Pilot.Data.White[,17] <- as.numeric(Pilot.Data.White[,17])
Pilot.Data.White[,18] <- as.numeric(Pilot.Data.White[,18])
Pilot.Data.White[,19] <- as.numeric(Pilot.Data.White[,19])
Pilot.Data.White[,20] <- as.numeric(Pilot.Data.White[,20])
Pilot.Data.White[,21] <- as.numeric(Pilot.Data.White[,21])
Pilot.Data.White[,22] <- as.numeric(Pilot.Data.White[,22])
Pilot.Data.White[,23] <- as.numeric(Pilot.Data.White[,23])
Pilot.Data.White[,24] <- as.numeric(Pilot.Data.White[,24])
Pilot.Data.White[,25] <- as.numeric(Pilot.Data.White[,25])
Pilot.Data.White[,26] <- as.numeric(Pilot.Data.White[,26])
Pilot.Data.White[,27] <- as.numeric(Pilot.Data.White[,27])
Pilot.Data.White[,28] <- as.numeric(Pilot.Data.White[,28])

Pilot.Data.White$Q37_1 <- recode(Pilot.Data.White$Q37_1, `8` = 7, `7` = 6, `6` = 5, `5` = 4, `4` = 3, `3` = 2, `2` = 1)
Pilot.Data.White$Q37_2 <- recode(Pilot.Data.White$Q37_2, `8` = 7, `7` = 6, `6` = 5, `5` = 4, `4` = 3, `3` = 2, `2` = 1)
Pilot.Data.White$Q37_3 <- recode(Pilot.Data.White$Q37_3, `8` = 7, `7` = 6, `6` = 5, `5` = 4, `4` = 3, `3` = 2, `2` = 1)
Pilot.Data.White$Q38_1 <- recode(Pilot.Data.White$Q38_1, `8` = 7, `7` = 6, `6` = 5, `5` = 4, `4` = 3, `3` = 2, `2` = 1)
Pilot.Data.White$Q38_2 <- recode(Pilot.Data.White$Q38_2, `8` = 7, `7` = 6, `6` = 5, `5` = 4, `4` = 3, `3` = 2, `2` = 1)
Pilot.Data.White$Q38_3 <- recode(Pilot.Data.White$Q38_3, `8` = 7, `7` = 6, `6` = 5, `5` = 4, `4` = 3, `3` = 2, `2` = 1)
Pilot.Data.White$Q39_1 <- recode(Pilot.Data.White$Q39_1, `8` = 7, `7` = 6, `6` = 5, `5` = 4, `4` = 3, `3` = 2, `2` = 1)
Pilot.Data.White$Q39_2 <- recode(Pilot.Data.White$Q39_2, `8` = 7, `7` = 6, `6` = 5, `5` = 4, `4` = 3, `3` = 2, `2` = 1)
Pilot.Data.White$Q39_3 <- recode(Pilot.Data.White$Q39_3, `8` = 7, `7` = 6, `6` = 5, `5` = 4, `4` = 3, `3` = 2, `2` = 1)
Pilot.Data.White$Q40_1 <- recode(Pilot.Data.White$Q40_1, `8` = 7, `7` = 6, `6` = 5, `5` = 4, `4` = 3, `3` = 2, `2` = 1)
Pilot.Data.White$Q40_2 <- recode(Pilot.Data.White$Q40_2, `8` = 7, `7` = 6, `6` = 5, `5` = 4, `4` = 3, `3` = 2, `2` = 1)
Pilot.Data.White$Q40_3 <- recode(Pilot.Data.White$Q40_3, `8` = 7, `7` = 6, `6` = 5, `5` = 4, `4` = 3, `3` = 2, `2` = 1)
Pilot.Data.White$Q40_4 <- recode(Pilot.Data.White$Q40_4, `8` = 7, `7` = 6, `6` = 5, `5` = 4, `4` = 3, `3` = 2, `2` = 1)

plyr::count(Pilot.Data.White$Q31)
table(Pilot.Data.White$Q31)
factor(Pilot.Data.White$Q31)

asian.names <- c("Chan Tran", "Chan Yan Li", "Chang Wei", "Chen", "chen chang", "Chen Lang", "Chen Yan", "chen yang", "Chen yang", "Chen Yang", "CHEN YANG", "Chen Yeng", "Chen Yjang", "chenyang", "Don Hoang", "Don Huang", "Don Huaong", "Don Hung", "Don Juang", "Dong haung", "Dong Haung", "Dong Huang", "DONG HUANG", "Dong Huang", "Dong Huong", "Doung Haung", "duan houng", "duon huong", "Houng", "Keny chang", "PHENG", "Waix", "WAN", "wang", "Wang", "wang", "Wang  Wong", "Wang fung chu", "wang xi wong?", "Wang xiu Wong", "Wang Xiu Wong", "WANG XIU WONG", "Wang Xiu Wong", "Wang Ziu Wong", "Wei", "Wong", "Wong Xi", "wung xungu", "Xi Wong", "Xian", "Xiang", "Xiao", "Xie Wu", "Xiu Xiang", "zang wei chen", "Zang Wi", "Zao", "Zengh Wei Chang", "Zhang Chan", "Zhang Wei Chan", "Zhang Wei Chang", "Zhang Wei Chen", "Zhang Wei Chen", "Zheang", "zheng", "Zheng Wei Chen", "zhou", "ZWANG")
Pilot.Data.White$res1_race <- ifelse(Pilot.Data.White$Q31 %in% asian.names, "AA", "AF")

Pilot.Data.White[124,]$res1_race <- "AA"
Pilot.Data.White[26,]$res1_race <- "AA"

Pilot.Data.White$res2_race <- ifelse(Pilot.Data.White$res1_race == "AA", "AF", "AA")

#Qualification Index
Pilot.Data.White$AF_Qual <- if_else(Pilot.Data.White$res1_race == "AF", rowMeans(Pilot.Data.White[,2:4], na.rm = T), 0)

Pilot.Data.White$AF_Qual <- if_else(Pilot.Data.White$res2_race == "AF", rowMeans(Pilot.Data.White[,16:18], na.rm = T), Pilot.Data.White$AF_Qual)

Pilot.Data.White$AF_Qual <- round(Pilot.Data.White$AF_Qual, 2)

label(Pilot.Data.White$AF_Qual) <- "Qualification (AF)"

Pilot.Data.White$AA_Qual <- if_else(Pilot.Data.White$res1_race == "AA", rowMeans(Pilot.Data.White[,2:4], na.rm = T), 0)

Pilot.Data.White$AA_Qual <- if_else(Pilot.Data.White$res2_race == "AA", rowMeans(Pilot.Data.White[,16:18], na.rm = T), Pilot.Data.White$AA_Qual)

Pilot.Data.White$AA_Qual <- round(Pilot.Data.White$AA_Qual, 2)

label(Pilot.Data.White$AA_Qual) <- "Qualification (AA)"

#Status Index
Pilot.Data.White$AF_Stat <- if_else(Pilot.Data.White$res1_race == "AF", rowMeans(Pilot.Data.White[,5:7], na.rm = T), 0)

Pilot.Data.White$AF_Stat <- if_else(Pilot.Data.White$res2_race == "AF", rowMeans(Pilot.Data.White[,19:21], na.rm = T), Pilot.Data.White$AF_Stat)

Pilot.Data.White$AF_Stat <- round(Pilot.Data.White$AF_Stat, 2)

label(Pilot.Data.White$AF_Stat) <- "Status (AF)"

Pilot.Data.White$AA_Stat <- if_else(Pilot.Data.White$res1_race == "AA", rowMeans(Pilot.Data.White[,5:7], na.rm = T), 0)

Pilot.Data.White$AA_Stat <- if_else(Pilot.Data.White$res2_race == "AA", rowMeans(Pilot.Data.White[,19:21], na.rm = T), Pilot.Data.White$AA_Stat)

Pilot.Data.White$AA_Stat <- round(Pilot.Data.White$AA_Stat, 2)

label(Pilot.Data.White$AA_Stat) <- "Status (AA)"

#American Index
Pilot.Data.White$AF_Amer <- if_else(Pilot.Data.White$res1_race == "AF", rowMeans(Pilot.Data.White[,8:10], na.rm = T), 0)

Pilot.Data.White$AF_Amer <- if_else(Pilot.Data.White$res2_race == "AF", rowMeans(Pilot.Data.White[,22:24], na.rm = T), Pilot.Data.White$AF_Amer)

Pilot.Data.White$AF_Amer <- round(Pilot.Data.White$AF_Amer, 2)

label(Pilot.Data.White$AF_Amer) <- "Americanness (AF)"

Pilot.Data.White$AA_Amer <- if_else(Pilot.Data.White$res1_race == "AA", rowMeans(Pilot.Data.White[,8:10], na.rm = T), 0)

Pilot.Data.White$AA_Amer <- if_else(Pilot.Data.White$res2_race == "AA", rowMeans(Pilot.Data.White[,22:24], na.rm = T), Pilot.Data.White$AA_Amer)

Pilot.Data.White$AA_Amer <- round(Pilot.Data.White$AA_Amer, 2)

label(Pilot.Data.White$AA_Amer) <- "Americanness (AA)"
