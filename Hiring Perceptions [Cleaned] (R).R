library(plyr)
library(tidyverse)
library(psych)
library(Hmisc)

Data <- read.csv("HiringPerceptionsData.csv")

#Greenwald Lab Room
Data$Room <- Data$Room %>%
  recode_factor("T" = 2, "C" = 3, "D" = 4, "E" = 5, "F" = 6)

Data$Room <- factor(Data$Room,
                         levels = c(2,3,4,5,6),
                         labels = c("T", "C", "D", "E", "F"))

label(Data$Room) <- "Greenwald Lab Room"

#Condition
Data$Condition <- Data$Condition %>%
  recode_factor("C" = 1, "A" = 2, "S" = 3)

Data$Condition <- factor(Data$Condition,
                         levels = c(1,2,3),
                         labels = c("Control", "American", "Status"))

label(Data$Condition) <- "Condition"

#Applicant 1 Race
Data$res1_race <- Data$res1_race %>% 
  recode_factor("AF" = 1, "AA" = 2)

Data$res1_race <- factor(Data$res1_race,
                         levels = c(1,2),
                         labels = c("AF", "AA"))

label(Data$res1_race) <- "Applicant 1 Race"

#Applican 2 Race
Data$res2_race <- Data$res2_race %>% 
  recode_factor("AF" = 1, "AA" = 2)

Data$res2_race <- factor(Data$res2_race,
                         levels = c(1,2),
                         labels = c("AF", "AA"))

label(Data$res2_race) <- "Applicant 2 Race"

#Applicant Names for Resume 1
Data$res1_name <- as.factor(Data$res1_name)
Data$res1_mancheck <- as.factor(Data$res1_mancheck)

Data$res1_name <- ifelse(Data$res1_race=="AA", dplyr::recode(Data$res1_name, `1` = 5, `2` = 6, `3` = 7, `4` = 8),  Data$res1_name)

Data$res1_name <- factor(Data$res1_name,
                         levels = c(1,2,3,4,5,6,7,8),
                         labels = c("Deshawn", "Terell", "Tyrone", "Lamar", "Zhang Wei", "Chen", "Dong", "Wang Xiu"))

label(Data$res1_name) <- "Applicant 1 Name"

#Data$res1_name[Data$res1_race=="AA"] <- recode(Data$res1_name[Data$res1_race=="AA"], `1` = 5, `2` = 6, `3` = 7, `4` = 8) 

#Applicant Names for Resume 2
Data$res2_name <- as.factor(Data$res2_name)
Data$res2_mancheck <- as.factor(Data$res2_mancheck)

Data$res2_name <- ifelse(Data$res2_race=="AA", dplyr::recode(Data$res2_name, `1` = 5, `2` = 6, `3` = 7, `4` = 8),  Data$res2_name)

Data$res2_name <- factor(Data$res2_name,
                         levels = c(1,2,3,4,5,6,7,8),
                         labels = c("Deshawn", "Terell", "Tyrone", "Lamar", "Zhang Wei", "Chen", "Dong", "Wang Xiu"))

label(Data$res2_name) <- "Applicant 2 Name"

#Resume 1 Version
Data$res1_ver <- Data$res1_ver %>%
  recode_factor("A" = 1, "B" = 2)

Data$res1_ver <- factor(Data$res1_ver,
                        levels = c(1,2),
                        labels = c("A", "B"))

label(Data$res1_ver) <- "Resume 1 Version"

#Resume 2 Version
Data$res2_ver <- Data$res2_ver %>%
  recode_factor("A" = 1, "B" = 2)

Data$res2_ver <- factor(Data$res2_ver,
                        levels = c(1,2),
                        labels = c("A", "B"))

label(Data$res2_ver) <- "Resume 2 Version"

#Hired Race Information
Data$hired_race <- Data$hired_race %>% 
  recode_factor("AF" = 1, "AA" = 2)

Data$hired_race <- factor(Data$hired_race,
                          levels = c(0,1,2,3,4),
                          labels = c("No Response","AF", "AA","Neither Selected","Both Selected"))

label(Data$hired_race) <- "Race of Hired Applicant"

#Recode Questions
Data$res1_Q9 <- Data$res1_Q9 %>%
  dplyr::recode("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5, "6" = 6, "7" = 7)


Data$res2_Q1 <- Data$res2_Q1 %>%
  dplyr::recode("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5, "6" = 6, "7" = 7)


#Order of Appearance
#This is kind of nonsense. I can't remember what I wanted to look into by creating this variable, but either way... here it is!
Data$AF_hire <- ifelse(Data$res1_race=="AF", dplyr::recode(Data$hired_race, "AF" = 1, "AA" = 2),  0)

Data$AF_hire <- factor(Data$AF_hire,
                          levels = c(0,1,2),
                          labels = c("Not First","Appeared 1st","AF 1st/AA hired"))

Data$AA_hire <- ifelse(Data$res1_race=="AA", dplyr::recode(Data$hired_race, "AA" = 1, "AF" = 2),  0)

Data$AA_hire <- factor(Data$AA_hire,
                       levels = c(0,1,2),
                       labels = c("Not First","Appeared 1st","AA 1st/AF hired"))

label(Data$AF_hire) <- "Order of Appearance (African American)"
label(Data$AA_hire) <- "Order of Appearance (Asian American)"

#Manipulation Check
Data$res1_mc <- ifelse(Data$res1_race=="AF", dplyr::recode(Data$res1racecheck, "1" = 1, "2" = 0, "3" = 0, "4" = 0, "5" = 0, "6" = 0, "7" = 0, "8" = 0),  0)

Data$res1_mc <- ifelse(Data$res1_race=="AA", dplyr::recode(Data$res1racecheck, "1" = 0, "2" = 1, "3" = 0, "4" = 0, "5" = 0, "6" = 0, "7" = 0, "8" = 0), Data$res1_mc)

label(Data$res1_mc) <- "Resume 1 Race Manipulation Check"

Data$res2_mc <- ifelse(Data$res2_race=="AF", dplyr::recode(Data$res2racecheck, "1" = 1, "2" = 0, "3" = 0, "4" = 0, "5" = 0, "6" = 0, "7" = 0, "8" = 0),  0)

Data$res2_mc <- ifelse(Data$res2_race=="AA", dplyr::recode(Data$res2racecheck, "1" = 0, "2" = 1, "3" = 0, "4" = 0, "5" = 0, "6" = 0, "7" = 0, "8" = 0), Data$res2_mc)

label(Data$res2_mc) <- "Resume 2 Race Manipulation Check"

#Adjust class of question variables
Data$res1_Q1 <- as.numeric(Data$res1_Q1)
Data$res1_Q2 <- as.numeric(Data$res1_Q2)
Data$res1_Q3 <- as.numeric(Data$res1_Q3)
Data$res1_Q4 <- as.numeric(Data$res1_Q4)
Data$res1_Q5 <- as.numeric(Data$res1_Q5)
Data$res1_Q6 <- as.numeric(Data$res1_Q6)
Data$res1_Q7 <- as.numeric(Data$res1_Q7)
Data$res1_Q8 <- as.numeric(Data$res1_Q8)
Data$res1_Q10 <- as.numeric(Data$res1_Q10)
Data$res1_Q11 <- as.numeric(Data$res1_Q11)
Data$res1_Q12 <- as.numeric(Data$res1_Q12)
Data$res1_Q13 <- as.numeric(Data$res1_Q13)

Data$res2_Q2 <- as.numeric(Data$res2_Q2)
Data$res2_Q3 <- as.numeric(Data$res2_Q3)
Data$res2_Q4 <- as.numeric(Data$res2_Q4)
Data$res2_Q5 <- as.numeric(Data$res2_Q5)
Data$res2_Q6 <- as.numeric(Data$res2_Q6)
Data$res2_Q7 <- as.numeric(Data$res2_Q7)
Data$res2_Q8 <- as.numeric(Data$res2_Q8)
Data$res2_Q9 <- as.numeric(Data$res2_Q9)
Data$res2_Q10 <- as.numeric(Data$res2_Q10)
Data$res2_Q11 <- as.numeric(Data$res2_Q11)
Data$res2_Q12 <- as.numeric(Data$res2_Q12)
Data$res2_Q13 <- as.numeric(Data$res2_Q13)

#Participant Race
Data$PRACE <- ifelse(Data$p_race_af == 1 & Data$p_race_w + Data$p_race_aa + Data$p_race_ha + Data$p_race_an + Data$p_race_pi + Data$p_race_mea + Data$p_race_o == 0, dplyr::recode(Data$p_race_af, "1" = 1), 9)

Data$PRACE <- ifelse(Data$p_race_aa == 1 & Data$p_race_w + Data$p_race_af + Data$p_race_ha + Data$p_race_an + Data$p_race_pi + Data$p_race_mea + Data$p_race_o == 0, dplyr::recode(Data$p_race_aa, "1" = 2), Data$PRACE)

Data$PRACE <- ifelse(Data$p_race_ha == 1 & Data$p_race_w + Data$p_race_af + Data$p_race_aa + Data$p_race_an + Data$p_race_pi + Data$p_race_mea + Data$p_race_o == 0, dplyr::recode(Data$p_race_ha, "1" = 3), Data$PRACE)

Data$PRACE <- ifelse(Data$p_race_an == 1 & Data$p_race_w + Data$p_race_af + Data$p_race_ha + Data$p_race_aa + Data$p_race_pi + Data$p_race_mea + Data$p_race_o == 0, dplyr::recode(Data$p_race_an, "1" = 4), Data$PRACE)

Data$PRACE <- ifelse(Data$p_race_pi == 1 & Data$p_race_w + Data$p_race_af + Data$p_race_ha + Data$p_race_an + Data$p_race_aa + Data$p_race_mea + Data$p_race_o == 0, dplyr::recode(Data$p_race_pi, "1" = 5), Data$PRACE)

Data$PRACE <- ifelse(Data$p_race_mea == 1 & Data$p_race_w + Data$p_race_af + Data$p_race_ha + Data$p_race_an + Data$p_race_pi + Data$p_race_aa + Data$p_race_o == 0, dplyr::recode(Data$p_race_mea, "1" = 6), Data$PRACE)

Data$PRACE <- ifelse(Data$p_race_w == 1 & Data$p_race_aa + Data$p_race_af + Data$p_race_ha + Data$p_race_an + Data$p_race_pi + Data$p_race_mea + Data$p_race_o == 0, dplyr::recode(Data$p_race_w, "1" = 7), Data$PRACE)

Data$PRACE <- ifelse(Data$p_race_o == 1 & Data$p_race_w + Data$p_race_af + Data$p_race_ha + Data$p_race_an + Data$p_race_pi + Data$p_race_mea + Data$p_race_aa == 0, dplyr::recode(Data$p_race_o, "1" = 8), Data$PRACE)

Data$PRACE <- factor(Data$PRACE,
                       levels = c(1,2,3,4,5,6,7,8,9),
                       labels = c("African American","Asian American","Hispanic American","Alaskan Native/Native American","Pacific Islander","Middle Eastern American","White American","Other",NA))

label(Data$PRACE) <- "Participant Race"

#Qualification Index
Data$AF_Qual <- if_else(Data$res1_race == "AF", rowMeans(Data[,11:13], na.rm = T), 0)

Data$AF_Qual <- if_else(Data$res2_race == "AF", rowMeans(Data[,28:30], na.rm = T), Data$AF_Qual)

Data$AF_Qual <- round(Data$AF_Qual, 2)

label(Data$AF_Qual) <- "Qualification (AF)"

Data$AA_Qual <- if_else(Data$res1_race == "AA", rowMeans(Data[,11:13], na.rm = T), 0)

Data$AA_Qual <- if_else(Data$res2_race == "AA", rowMeans(Data[,28:30], na.rm = T), Data$AA_Qual)

Data$AA_Qual <- round(Data$AA_Qual, 2)

label(Data$AA_Qual) <- "Qualification (AA)"

#Status Index
Data$AF_Stat <- if_else(Data$res1_race == "AF", rowMeans(Data[,14:16], na.rm = T), 0)

Data$AF_Stat <- if_else(Data$res2_race == "AF", rowMeans(Data[,31:33], na.rm = T), Data$AF_Stat)

Data$AF_Stat <- round(Data$AF_Stat, 2)

label(Data$AF_Stat) <- "Status (AF)"

Data$AA_Stat <- if_else(Data$res1_race == "AA", rowMeans(Data[,11:13], na.rm = T), 0)

Data$AA_Stat <- if_else(Data$res2_race == "AA", rowMeans(Data[,31:33], na.rm = T), Data$AA_Stat)

Data$AA_Stat <- round(Data$AA_Stat, 2)

label(Data$AA_Stat) <- "Status (AA)"

#American Index
Data$AF_Amer <- if_else(Data$res1_race == "AF", rowMeans(Data[,17:19], na.rm = T), 0)

Data$AF_Amer <- if_else(Data$res2_race == "AF", rowMeans(Data[,34:36], na.rm = T), Data$AF_Amer)

Data$AF_Amer <- round(Data$AF_Amer, 2)

label(Data$AF_Amer) <- "Americanness (AF)"

Data$AA_Amer <- if_else(Data$res1_race == "AA", rowMeans(Data[,17:19], na.rm = T), 0)

Data$AA_Amer <- if_else(Data$res2_race == "AA", rowMeans(Data[,34:36], na.rm = T), Data$AA_Amer)

Data$AA_Amer <- round(Data$AA_Amer, 2)

label(Data$AA_Amer) <- "Americanness (AA)"

#Recode Questions
Data$AF_Q1 <- if_else(Data$res1_race == "AF", Data$res1_Q1, Data$res2_Q1)
Data$AF_Q2 <- if_else(Data$res1_race == "AF", Data$res1_Q2, Data$res2_Q2)
Data$AF_Q3 <- if_else(Data$res1_race == "AF", Data$res1_Q3, Data$res2_Q3)
Data$AF_Q4 <- if_else(Data$res1_race == "AF", Data$res1_Q4, Data$res2_Q4)
Data$AF_Q5 <- if_else(Data$res1_race == "AF", Data$res1_Q5, Data$res2_Q5)
Data$AF_Q6 <- if_else(Data$res1_race == "AF", Data$res1_Q6, Data$res2_Q6)
Data$AF_Q7 <- if_else(Data$res1_race == "AF", Data$res1_Q7, Data$res2_Q7)
Data$AF_Q8 <- if_else(Data$res1_race == "AF", Data$res1_Q8, Data$res2_Q8)
Data$AF_Q9 <- if_else(Data$res1_race == "AF", Data$res1_Q9, Data$res2_Q9)
Data$AF_Q10 <- if_else(Data$res1_race == "AF", Data$res1_Q10, Data$res2_Q10)
Data$AF_Q11 <- if_else(Data$res1_race == "AF", Data$res1_Q11, Data$res2_Q11)
Data$AF_Q12 <- if_else(Data$res1_race == "AF", Data$res1_Q12, Data$res2_Q12)
Data$AF_Q13 <- if_else(Data$res1_race == "AF", Data$res1_Q13, Data$res2_Q13)

Data$AA_Q1 <- if_else(Data$res1_race == "AA", Data$res1_Q1, Data$res2_Q1)
Data$AA_Q2 <- if_else(Data$res1_race == "AA", Data$res1_Q2, Data$res2_Q2)
Data$AA_Q3 <- if_else(Data$res1_race == "AA", Data$res1_Q3, Data$res2_Q3)
Data$AA_Q4 <- if_else(Data$res1_race == "AA", Data$res1_Q4, Data$res2_Q4)
Data$AA_Q5 <- if_else(Data$res1_race == "AA", Data$res1_Q5, Data$res2_Q5)
Data$AA_Q6 <- if_else(Data$res1_race == "AA", Data$res1_Q6, Data$res2_Q6)
Data$AA_Q7 <- if_else(Data$res1_race == "AA", Data$res1_Q7, Data$res2_Q7)
Data$AA_Q8 <- if_else(Data$res1_race == "AA", Data$res1_Q8, Data$res2_Q8)
Data$AA_Q9 <- if_else(Data$res1_race == "AA", Data$res1_Q9, Data$res2_Q9)
Data$AA_Q10 <- if_else(Data$res1_race == "AA", Data$res1_Q10, Data$res2_Q10)
Data$AA_Q11 <- if_else(Data$res1_race == "AA", Data$res1_Q11, Data$res2_Q11)
Data$AA_Q12 <- if_else(Data$res1_race == "AA", Data$res1_Q12, Data$res2_Q12)
Data$AA_Q13 <- if_else(Data$res1_race == "AA", Data$res1_Q13, Data$res2_Q13)

#write.csv(Data, file = "Hiring Perceptions [Cleaned].csv")

###Analysis

##Reliability
#AF_Qual
psych::alpha(Data[,76:78], na.rm = T)
#AF_Stat
psych::alpha(Data[,79:81], na.rm = T)
#AF_Amer
psych::alpha(Data[,82:84], na.rm = T)

#AA_Qual
psych::alpha(Data[,89:91], na.rm = T)
#AA_Stat
psych::alpha(Data[,92:94], na.rm = T)
#AA_Amer
psych::alpha(Data[,95:97], na.rm = T)

##Exploratory
#First I'm going to check to see which distribution best fits the data that I'm working with

Data.Indeces <- cbind(Data$Condition, Data$AF_Qual, Data$AF_Stat, Data$AF_Amer, Data$AA_Qual, Data$AA_Stat, Data$AA_Amer)

Data.Indeces <- as.data.frame(Data.Indeces)
variablenames <- c("Condition", "AF_Qual", "AF_Stat", "AF_Amer", "AA_Qual", "AA_Stat", "AA_Amer")
names(Data.Indeces) <- variablenames

require(MASS)
require(car)
# This is so that distributions that must be non-zero can make sense of my
# data
Data.Indeces$AF_Qual.t <- Data.Indeces$AF_Qual + 1
qqp(Data.Indeces$AF_Qual.t, "norm")

# lnorm means lognormal
qqp(Data.Indeces$AF_Qual.t, "lnorm")

# qqp requires estimates of the parameters of the negative binomial, Poisson and gamma distributions. You can generate estimates using the fitdistr function. Save the output and extract the estimates of each parameter as I have shown below.
nbinom <- fitdistr(as.integer(Data.Indeces$AF_Qual.t), "Negative Binomial")
qqp(Data.Indeces$AF_Qual.t, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])

poisson <- fitdistr(as.integer(Data.Indeces$AF_Qual.t), "Poisson")
qqp(Data.Indeces$AF_Qual.t, "pois", poisson$estimate) 
#Not a good fit for our data

gamma <- fitdistr(as.integer(Data.Indeces$AF_Qual.t), "gamma")
qqp(Data.Indeces$AF_Qual.t, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])

#ANOVA
require(lme4)

anovaModelAFQualOnly <- aov(AF_Qual ~ Condition, Data)
summary(anovaModelAFQualOnly)

#Within Subjects Factor
QUAL <- factor(c("Qualification (AF)", "Qualification (AA)"))
AMER <- factor(c("Americanness (AF)", "Americanness (AA)"))
STAT <- factor(c("Status (AF)", "Status (AA)"))

#car options for analysis to have a default set so that it computes sums of squares properly for the mixed design
options(contrasts = c("contr.sum","contr.poly"))

#Between Subjects Factor
Condition <- factor(Data$Condition, labels=c("Control", "American", "Status"))

## run the ANOVA and print a summary
Anv1 <- lm(cbind(Data$AF_Qual, Data$AA_Qual, Data$AF_Amer, Data$AA_Amer, Data$AF_Stat, Data$AA_Stat)~Condition)
summary(Anv1, multivariate=F)

#Convert Data to long form
Data.Anv <- Data %>%
  dplyr::select(Subj, Condition, AF_Qual, AA_Qual, AF_Amer, AA_Amer, AF_Stat, AA_Stat)

library(reshape2)

Data.lng <- melt(Data.Anv, id=c("Subj", "Condition"), variable.name = "Index", value.name="Rating")

## code separate factors for Race and the Type of Measure
Data.lng$Race <- factor(substr(Data.lng$Index, 1, 2))
Data.lng$Measure <- factor(substr(Data.lng$Index, 4, 7))

## Make Subj into a factor
Data.lng$Subj <- factor(Data.lng$Subj)

## Since group is already a factor, does not need to be converted to factor

## load library and set options
library(devtools)
dev_mode()
#install_github('mike-lawrence/ez')
library(ez)
dev_mode()
options(contrasts = c("contr.sum","contr.poly"))

## run the ANOVA
ezANOVA(Data.lng, dv=Rating, wid=Subj, within=.(Race), between=Condition, detailed=T)

###Leaving the above code in because I have a sense of what's wrong but I'm not entirely sure whether or not there's anything useful in the code

#So far, it seems like by best bet is to put each within subject measure into it's own datafram and analyze them all separately.

#Qual
Data.Qual <- Data %>%
  dplyr::select(Subj, Condition, AF_Qual, AA_Qual)

Data.Qual.lng <- melt(Data.Qual, id=c("Subj", "Condition"), variable.name = "Index", value.name="Rating")

Data.Qual.lng$Race <- factor(substr(Data.Qual.lng$Index, 1, 2))
Data.Qual.lng$Measure <- factor(substr(Data.Qual.lng$Index, 4, 7))
Data.Qual.lng$Subj <- factor(Data.Qual.lng$Subj)

ezANOVA(Data.Qual.lng, dv=Rating, wid=Subj, within=.(Race), between=Condition, detailed=T)

#Amer
Data.Amer <- Data %>%
  dplyr::select(Subj, Condition, AF_Amer, AA_Amer)

Data.Amer.lng <- melt(Data.Amer, id=c("Subj", "Condition"), variable.name = "Index", value.name="Rating")

Data.Amer.lng$Race <- factor(substr(Data.Amer.lng$Index, 1, 2))
Data.Amer.lng$Measure <- factor(substr(Data.Amer.lng$Index, 4, 7))
Data.Amer.lng$Subj <- factor(Data.Amer.lng$Subj)

ezANOVA(Data.Amer.lng, dv=Rating, wid=Subj, within=.(Race), between=Condition, detailed=T)

#Stat
Data.Stat <- Data %>%
  dplyr::select(Subj, Condition, AF_Stat, AA_Stat)

Data.Stat.lng <- melt(Data.Stat, id=c("Subj", "Condition"), variable.name = "Index", value.name="Rating")

Data.Stat.lng$Race <- factor(substr(Data.Stat.lng$Index, 1, 2))
Data.Stat.lng$Measure <- factor(substr(Data.Stat.lng$Index, 4, 7))
Data.Stat.lng$Subj <- factor(Data.Stat.lng$Subj)

ezANOVA(Data.Stat.lng, dv=Rating, wid=Subj, within=.(Race), between=Condition, detailed=T)

#Plots
library(ggplot2)
library(ggthemes)

#simple ggplot
ggplot(data = Data.Qual.lng, aes(x = Condition, y = Rating)) +
  geom_point()

#base plot
plot(Rating ~ Condition, data = Data.Qual.lng, xlab = "Participant Condition", ylab = "Rating of Qualification", main = "How Qualified is the Applicant?", col = "red", cex = 1, pch = 16)

#more complex ggplot
ggplot(data = Data.Qual.lng, aes(x = Condition, y = Rating)) + #this is where you set the x and y
  geom_point(color = "red", size = 3) + #this is changing the color and size of the points on your plot. This draws points whereas other functions might draw lines
  scale_y_continuous(limits = c(0, NA)) + #tells ggplot to set the bottom boundary of the y axis at 0
  xlab("Condition") + ylab("Rating of Qualification") + #this provides labels
  ggtitle("How Qualified is the Applicant?") + #this gives your plot a title
  theme_excel()

#color the ggplot by race:
ggplot(data = Data.Qual.lng, aes(x = Condition, y = Rating, color = Race)) + #this is coloring the dots based on your factor variable
  geom_point() +
  scale_y_continuous(limits = c(0, NA)) +
  xlab("Condition") + ylab("Rating of Qualification") +
  ggtitle("Qualification for Each Job by Race") +
  theme_stata()

#Here's one with a few more arguments. the order doesn't really matter when you name them. However you don't have to name them as they have a default order that can be seen by asking ?ggplot or ?"argument." You might notice how there aren't many african american dots relative to the asian american dots...

#When dealing with categorical data you might want to "jitter" your data. This displays your data but takes each point and moves it randomly around it's set point. For example, you turn this:
ggplot(data = Data.Qual.lng, aes(x = Condition, y = Rating, color = Race)) +
  geom_point()

#which is pretty useless. 
#Into this:
ggplot(data = Data.Qual.lng, aes(x = Condition, y = Rating, color = Race)) +
  geom_point(position = position_jitter(width = 0.25, height = 0)) #for categorical stuff jitter .5 should be your maximum as the distance between one point and the next is 1.

#Creating a function that helps summarize data
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- plyr::ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- plyr::rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

Data.lng.c <- summarySE(data=Data.lng, measurevar="Rating", groupvars=c("Condition","Measure", "Race"))
Data.lng.c

Data.Qual.lng.c <- summarySE(Data.Qual.lng, measurevar="Rating", groupvars=c("Condition","Measure", "Race"))
Data.Amer.lng.c <- summarySE(Data.Amer.lng, measurevar="Rating", groupvars=c("Condition","Measure", "Race"))
Data.Stat.lng.c <- summarySE(Data.Stat.lng, measurevar="Rating", groupvars=c("Condition","Measure", "Race"))

##Line Graphs

# Standard error of the mean
ggplot(Data.lng.c, aes(x=Condition, y=Rating, colour=Race)) + 
  geom_errorbar(aes(ymin=Rating-se, ymax=Rating+se), width=.1) +
  geom_line() +
  geom_point() +
  facet_wrap(~Measure)


# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.25) # move them .05 to the left and right

ggplot(Data.lng.c, aes(x=Condition, y=Rating, colour=Race)) + 
  geom_errorbar(aes(ymin=Rating-se, ymax=Rating+se), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd) +
  facet_wrap(~Measure)


# Use 95% confidence interval instead of SEM
ggplot(Data.lng.c, aes(x=Condition, y=Rating, colour=Race)) + 
  geom_errorbar(aes(ymin=Rating-ci, ymax=Rating+ci), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd) +
  facet_wrap(~Measure)

# Black error bars - notice the mapping of 'group=Race' -- without it, the error
# bars won't be dodged!
ggplot(Data.lng.c, aes(x=Condition, y=Rating, colour=Race, group=Race)) + 
  #geom_errorbar(aes(ymin=Rating-ci, ymax=Rating+ci), colour="black", width=.1, position=pd) +
  geom_line() +
  geom_point(size=3) +
  scale_y_continuous(limits = c(0,7)) +
  facet_wrap(~Measure)

#A finished graph with error bars representing the standard error of the mean might look like this. The points are drawn last so that the white fill goes on top of the lines and error bars.
ggplot(Data.lng.c, aes(x=Condition, y=Rating, colour=Race, group=Race)) + 
  geom_errorbar(aes(ymin=Rating-se, ymax=Rating+se), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
  xlab("Condition") +
  ylab("Participant Ratings (1-7)") +
  scale_colour_hue(name="Race of Applicant",    # Legend label, use darker colors
                   breaks=c("AF", "AA"),
                   labels=c("African American", "Asian American"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Ratings of Applicants on Measures of Americanness, Qualification, and Status") +
  expand_limits(y=0) +                        # Expand y range
  scale_y_continuous(limits=c(0,7)) +         # can set # of ticks with break= argument (not used here)
  facet_wrap(~Measure) +
  theme_bw() +
  theme(legend.justification=c(1,0),
        legend.position=c(1,0))               # Position legend in bottom right

###Bar Graphs

#Bar plot with error bars
ggplot(Data.lng.c, aes(x=Condition, y=Rating, fill=Race, group=Race)) +
  geom_bar(stat="identity", position=position_dodge())  +
  geom_errorbar(aes(ymin=Rating-sd, ymax=Rating+sd), width=.2,
                position=position_dodge(.9)) +
  facet_wrap(~Measure) +
  scale_fill_brewer(palette="Paired") + theme_minimal()

#If you wnat to save all three plots separately to a list you can do that by splitting the data then applying the plot as a function
plist <- lapply(split(Data.lng.c, Data.lng.c$Measure), function(d) {
  ggplot(d, aes(Condition, Rating, fill=Race, group=Race)) + 
    geom_bar(stat="identity", position=position_dodge())  +
    geom_errorbar(aes(ymin=Rating-sd, ymax=Rating+sd), width=.2,
                  position=position_dodge(.9)) +
    facet_wrap(~ Measure) +
    scale_y_continuous(limits=c(0, 7))
    #theme_bw() +
    #theme(plot.margin=unit(rep(0.4,4),"lines"),
          #axis.title=element_blank())
})

#Example of a single bar plot
ggplot(Data.Qual.lng.c, aes(Condition, Rating)) + 
  geom_bar(aes(fill = Race), stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin=Rating-sd, ymax=Rating+sd), width=.2, position= position_dodge(width = .9)) +
  scale_fill_brewer(palette="Paired") + theme_minimal()
  #At some point I have to figure out how to center error bars without a bunch of extra steps


#A few other bar plots
# Check to make sure that Condition is a factor
Data.lng.c2 <- Data.lng.c
class(Data.lng.c2$Condition)

# Error bars represent standard error of the mean
ggplot(Data.lng.c2, aes(x=Condition, y=Rating, fill=Race)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Rating-se, ymax=Rating+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  facet_wrap(~Measure)


# Use 95% confidence intervals instead of SEM
ggplot(Data.lng.c2, aes(x=Condition, y=Rating, fill=Race)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Rating-ci, ymax=Rating+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  facet_wrap(~Measure)

#Note that in both cases there are multiple error bars. This is an easy fix but we'll address it later

#An example of one closer to being finished
ggplot(Data.lng.c2, aes(x=Condition, y=Rating, fill=Race)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=Rating-se, ymax=Rating+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Participant Condition") +
  ylab("Participant Ratings (1-7)") +
  scale_fill_hue(name="Applicant Race", # Legend label, use darker colors
                 breaks=c("AF", "AA"),
                 labels=c("African American", "Asian American")) +
  ggtitle("Ratings of Applicants on Measures of\nAmericanness, Qualification, and Status") +
  scale_y_continuous(limits = c(0,7)) +
  facet_wrap(~Measure) +
  theme_bw()

##Scatter

#Example of a jittered scatterplot
ggplot(Data.lng, aes(x= Condition, y = Rating, color = Race)) +
  geom_point(position = position_jitter(width = 0.25)) +
  facet_wrap(~ Measure) +
  scale_y_continuous(limits = c(0, NA))

##If you want to export these graphs into their own PDF files
library(gridExtra)

# 3 Single-page PDF file, each with 1 plot
for (i in seq(1, length(plist))) {
  pdf(paste0("Graph of Indeces",i,".pdf"), 7, 5)
  grid.arrange(grobs=plist[i:(i)], 
               ncol=1, left="Participant Rating", bottom="Condition")
  dev.off()
}

#describe(Data)
#summary(Data)

#... A few more things. First are two functions to help calculate within subject error bars:

## Norms the data within specified groups in a data frame; it normalizes each subject (identified by idvar) so that they have the same mean, within each group specified by betweenvars.
##   data: a data frame.
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   na.rm: a boolean that indicates whether to ignore NA's
normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE) {
  library(plyr)
  
  # Measure var on left, idvar + between vars on right of formula.
  data.subjMean <- plyr::ddply(data, c(idvar, betweenvars), .drop=.drop,
                         .fun = function(xx, col, na.rm) {
                           c(subjMean = mean(xx[,col], na.rm=na.rm))
                         },
                         measurevar,
                         na.rm
  )
  
  # Put the subject means with original data
  data <- merge(data, data.subjMean)
  
  # Get the normalized data in a new column
  measureNormedVar <- paste(measurevar, "_norm", sep="")
  data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
    mean(data[,measurevar], na.rm=na.rm)
  
  # Remove this subject mean column
  data$subjMean <- NULL
  
  return(data)
}


## Summarizes data, handling within-subjects variables by removing inter-subject variability. It will still work if there are no within-S variables. Gives count, un-normed mean, normed mean (with same between-group mean), standard deviation, standard error of the mean, and confidence interval.If there are within-subject variables, calculate adjusted values using method from Morey (2008).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   withinvars: a vector containing names of columns that are within-subjects variables
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  
  # Ensure that the betweenvars and withinvars are factors
  factorvars <- vapply(data[, c(betweenvars, withinvars), drop=FALSE],
                       FUN=is.factor, FUN.VALUE=logical(1))
  
  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ",
            paste(nonfactorvars, collapse = ", "))
    data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
  }
  
  # Get the means from the un-normed data
  datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars),
                     na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Drop all the unused columns (these will be calculated with normed data)
  datac$sd <- NULL
  datac$se <- NULL
  datac$ci <- NULL
  
  # Norm each subject's data
  ndata <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)
  
  # This is the name of the new column
  measurevar_n <- paste(measurevar, "_norm", sep="")
  
  # Collapse the normed data - now we can treat between and within vars the same
  ndatac <- summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),
                      na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Apply correction from Morey (2008) to the standard error and confidence interval
  #  Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels,
                                  FUN.VALUE=numeric(1)))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
  
  # Apply the correction factor
  ndatac$sd <- ndatac$sd * correctionFactor
  ndatac$se <- ndatac$se * correctionFactor
  ndatac$ci <- ndatac$ci * correctionFactor
  
  # Combine the un-normed means with the normed results
  merge(datac, ndatac)
}

#Now to collapse the data
Data.lng.wc <- summarySEwithin(Data.lng, measurevar="Rating", withinvars=c("Race","Measure"),
                        idvar=c("Subj"), na.rm=FALSE, conf.interval=.95)

Data.lng.wc

# Make the graph with the 95% confidence interval
ggplot(Data.lng.wc, aes(x=Measure, y=Rating, group=Race, colour=Race)) +
  geom_line() +
  geom_errorbar(width=.1, aes(ymin=Rating-ci, ymax=Rating+ci)) +
  geom_point(shape=21, size=3, fill="white") +
  ylim(0,7)

#Why is it important to calculate these differently? Take a looksee:
# Instead of summarySEwithin, use summarySE, which treats condition as though it were a between-subjects variable
Data.lng.wc_between <- summarySE(data=Data.lng, measurevar="Rating", groupvars=c("Measure","Race"), na.rm=FALSE, conf.interval=.95)
Data.lng.wc_between
#>   condition  N value       sd       se       ci
#> 1   pretest 10 47.74 8.598992 2.719240 6.151348
#> 2  posttest 10 51.43 7.253972 2.293907 5.189179

# Show the between-S CI's in black, and the within-S CI's in group color
ggplot(Data.lng.wc_between, aes(x=Measure, y=Rating, group=Race, color=Race)) +
  geom_line() +
  geom_errorbar(width=.1, aes(ymin=Rating-ci, ymax=Rating+ci), colour="black") +
  geom_errorbar(width=.1, aes(ymin=Rating-ci, ymax=Rating+ci), data=Data.lng.wc) +
  geom_point(shape=21, size=3, fill="white") +
  ylim(1,7)

#And one more graph for fun
#This time we'll add the between measure back in
Data.lng.wbc <- summarySEwithin(Data.lng, measurevar="Rating", betweenvars="Condition", withinvars=c("Race","Measure"),
                               idvar=c("Subj"), na.rm=FALSE, conf.interval=.95)


ggplot(Data.lng.wbc, aes(x=Measure, y=Rating, fill=Race)) +
  geom_bar(position=position_dodge(.9), colour="black", stat="identity") +
  geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=Rating-ci, ymax=Rating+ci)) +
  coord_cartesian(ylim=c(1,7)) +
  scale_fill_manual(values=c("#CCCCCC","#FFFFFF")) +
  scale_y_continuous(breaks=seq(1:100)) +
  facet_wrap(~Condition) +
  theme_bw()+
  geom_hline(yintercept=0) 

#Finally, onto the Chi-squared part of the code
# 2-Way Frequency Table 
#attach(Data)
mytable <- table(Data$hired_race,Data$Condition) # A will be rows, B will be columns 
mytable
mytable <- mytable[2:3,] #since the other rows are empty, we'll just select for the rows with data in them manually.
mytable

margin.table(mytable, 1) # A frequencies (summed over B) 
margin.table(mytable, 2) # B frequencies (summed over A)

prop.table(mytable) # cell percentages
prop.table(mytable, 1) # row percentages 
prop.table(mytable, 2) # column percentages

# 2-Way Frequency Table
mytable2 <- xtabs(~hired_race+Condition, data=Data)
mytable2 <- mytable2[2:3,]
ftable(mytable2) # print table 
summary(mytable2) # chi-square test of indepedence

# 2-Way Cross Tabulation (closest to spss Crosstabs)
#install.packages("gmodels")
library(gmodels)
CrossTable(Data$hired_race, Data$Condition)

#Chi-Square Test
chisq.test(mytable)
#test independence of the row and column variable. By default, the p-value is calculated from the asymptotic chi-squared distribution of the test statistic. Optionally, the p-value can be derived via Monte Carlo simultation.

#Fisher Exact Test
mytable.m <- as.matrix(mytable)
fisher.test(mytable.m)
#exact test of independence. x is a two dimensional contingency table in matrix form.

#Mantel-Haenszel test
mantelhaen.test(x)
#a Cochran-Mantel-Haenszel chi-squared test of the null hypothesis that two nominal variables are conditionally independent in each stratum, assuming that there is no three-way interaction. x is a 3 dimensional contingency table, where the last dimension refers to the strata.

# Grouped Bar Plot
counts <- table(Data$hired_race, Data$Condition)
counts <- counts[2:3,]
barplot(counts, main="Which Race is Hired by Condition",
        xlab="Condition", col=c("turquoise","blue"),
        legend = rownames(counts), beside=TRUE)

# Association Plot Example
library(vcd)
assoc(mytable, shade=TRUE)

# Mosaic Plot Example
mosaic(mytable, shade=TRUE, legend=TRUE)
