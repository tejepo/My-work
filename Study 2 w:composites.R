library(dplyr)
#install.packages("haven")
library(haven)

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

path <- file.path("~/Desktop/UW/Summer '18/CultureColab/Native Bias/Data/Study_2_w_composites.sav")
studytwodata <- read_sav(path)

lm1 <- lm(DV1_4 ~ SES + GenKnow_mean, studytwodata)
lm2 <- lm(DV1_4 ~ SES + GenKnow_mean + SES*GenKnow_mean, studytwodata)

summary(lm1)
summary(lm2)
  
studytwodata$DV1_4
studytwodata$Race

lm3 <- lm(DV1_5 ~ SES + GenKnow_mean, studytwodata)
lm4 <- lm(DV1_5 ~ SES + GenKnow_mean + SES*GenKnow_mean, studytwodata)

summary(lm3)
summary(lm4)

plot(studytwodata$DV1_4, studytwodata$DV1_5)

library(ggplot2)

ggplot(studytwodata, aes(x= DV1_5, y = DV1_4)) +
  geom_point(position = position_jitter(width = 0.25)) +
  ylab("Abolish Columbus Day") +
  xlab("Create Indigenous Peoples' Day") +
  scale_y_continuous(limits = c(0, NA))

tail(studytwodata, 15)
studytwonew <- studytwodata[,505:757]
View(studytwonew)

reduceddataset <- as.data.frame(cbind(studytwodata$DV1_4, studytwodata$DV1_5, studytwodata$CultRacism_composite, studytwodata$BrokenStereotypes, studytwodata$Colorblind, studytwodata$RomanticStereotypes, studytwodata$Contact, studytwodata$Extended, studytwodata$FamProducts_sum, studytwodata$ProductsUsed_sum, studytwodata$FamiliarExpressions_sum, studytwodata$Native_SDOadaptation, studytwodata$Conflict, studytwodata$SLB_comp, studytwodata$RacismMini_comp, studytwodata$Nationalism, studytwodata$Tough, studytwodata$Anti_fem, studytwodata$SES, studytwodata$Invisibility, studytwodata$PastGuilt, studytwodata$HistoricKnow_mean, studytwodata$ContempKnow_mean, studytwodata$GenKnow_mean, studytwodata$MaterialConcern, studytwodata$ContemporaryInvisibility, studytwodata$Race, studytwodata$Intimacy))

View(reduceddataset)
colnames(reduceddataset) <-  c("Columbus Day", "Indigenous Peoples' Day", "Cultural Racism", "Broken Stereotypes", "Colorblindness", "Romantic Stereotypes", "Contact", "Extended", "Familiar Products (sum)", "Products Used (sum)", "FamiliarExpressions (sum)","Native SDO adaptation", "Conflict", "SLB (comp)", "Racism Mini (comp)", "Nationalism", "Tough", "Anti Feminist", "College Education", "Invisibility", "Past Guilt", "Historic Knowledge (%)", "Contemporary Knowledge (%)", "General Knowledge (%)", "Material Concerns", "Contemporary Invisibility", "Race", "Intimacy")

#reduceddataset <- as.data.frame(reduceddataset)

my_num_data <- reduceddataset

res <- cor(my_num_data, use = "complete.obs")
round(res,2)

library(Hmisc)
res2 <- rcorr(as.matrix(my_num_data), type = c("pearson","spearman"))
res2

#r values 
res2$r
res2$r <- round(res2$r, 3)

#p values
res2$P
res2$P <- round(res2$P, 3)

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

matrix1 <- flattenCorrMatrix(res2$r, res2$P)

sig.matrix1 <- subset(matrix1, p < .051)

#generate the files
write.csv(res, file = "new results.csv")
write.csv(matrix1, file = "new all cor with p.csv")
write.csv(sig.matrix1, file = "new Only sig cor.csv")

#At this stage we can take what we have and look at the data and at relationships in the data. The following will be good for digging deeper on specific parts of the data set that we think are particularly interesting.


#Number coding matrix
symnum(res, abbr.colnames = FALSE)

#install.packages("corrplot")
library(corrplot)

#This is a nice visual aide but is better for a subset of the data
plot1 <- corrplot(res, type = "upper", order = "hclust", 
                  tl.col = "black", tl.srt = 45)

#Positive correlations are displayed in blue and negative correlations in red color. Color intensity and the size of the circle are proportional to the correlation coefficients. In the right side of the correlogram, the legend color shows the correlation coefficients and the corresponding colors.

# Insignificant correlation are crossed
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.01, insig = "blank")
# Insignificant correlations are leaved blank
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.01, insig = "blank")


#another good visualizatiojn tool for a subset of the data
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")

chart <- chart.Correlation(my_num_data, histogram=TRUE, pch=19)

plist <- list(chart, plot1)

for (i in seq(1, length(plist))) {
  pdf(paste0("Performance Analysis #",i,".pdf"), 7, 5)
  grid.arrange(grobs=chart[i:(i)], 
               ncol=1, left="Participant Rating", bottom="Condition")
  dev.off()
}

#a heatmap that works well with subsetted data
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = res, col = col, symm = TRUE)

lm5 <- lm(`Columbus Day` ~ Colorblindness + Contact + `SLB (comp)` + Inivisibility + `Historic Knowledge (%)`, reduceddataset)
summary(lm5)

lm6 <- lm(`Indigenous Peoples' Day` ~ Colorblindness + Contact + `SLB (comp)` + Inivisibility + `Historic Knowledge (%)`, reduceddataset)
summary(lm6)

lm7 <- lm(Contact ~ `Columbus Day` + `Indigenous Peoples' Day` + `Columbus Day`:`Indigenous Peoples' Day`, reduceddataset)
summary(lm7)

lm8 <- lm(`Indigenous Peoples' Day` ~ Contact + `SLB (comp)` + Invisibility, reduceddataset)
summary(lm8)

lm9 <- lm(`Columbus Day` ~ Contact + `SLB (comp)`, reduceddataset)
summary(lm9)



lm11 <- lm(`Columbus Day` ~ Contact + `SLB (comp)` + Invisibility, reduceddataset)
summary(lm11)

lm12 <- lm(`Indigenous Peoples' Day` ~ Contact + `SLB (comp)` + Invisibility, reduceddataset)
summary(lm12)

lm13 <- lm(Invisibility ~ Contact, reduceddataset)
summary(lm13)

lm14 <- lm(Invisibility ~ Contact + `SLB (comp)`, reduceddataset)
summary(lm14)

reduceddataset$Race <- factor(reduceddataset$Race,
       levels = c(1,2,3,4,5,6,7,8),
       labels = c("Asian", "Black", "White", "Latino", "Middle Eastern", "Native", "Other", "Multiracial"))

#######Graphs######

#Does race change the relationship between support for IPD and contact?
#Model

reduceddataset.sum <- summarySE(data=reduceddataset, measurevar="Indigenous Peoples' Day", groupvars=c("Race"))

reduceddataset %>% reduceddataset
  
lm10 <- lm(`Indigenous Peoples' Day` ~ Contact + Race = Race-1, reduceddataset)
summary(lm10)

ggplot(reduceddataset, aes(x=Contact, y=`Indigenous Peoples' Day`)) +
  #geom_errorbar(width=.1, aes(ymin=Rating-ci, ymax=Rating+ci)) +
  geom_point(shape=21, size=3, fill="white") +
  geom_smooth(method=lm) +
  facet_wrap(~Race) +
  ylim(0,7)

ggplot(reduceddataset.sum, aes(x=Race, y=`Indigenous Peoples' Day`, fill=Race, group=Race)) +
  geom_bar(stat="identity", position=position_dodge())  +
  geom_errorbar(aes(ymin=`Indigenous Peoples' Day`-ci, ymax=`Indigenous Peoples' Day`+ci), width=.2,
                position=position_dodge(.9)) +
  #facet_wrap(~Race) +
  scale_fill_brewer(palette="Paired") + theme_minimal()


#Columbus Day
reduceddataset.col <- summarySE(data=reduceddataset, measurevar="Columbus Day", groupvars=c("Race"))

ggplot(reduceddataset, aes(x=Contact, y=`Columbus Day`)) +
  #geom_errorbar(width=.1, aes(ymin=Rating-ci, ymax=Rating+ci)) +
  geom_point(shape=21, size=3, fill="white") +
  geom_smooth(method=lm) +
  facet_wrap(~Race) +
  ylim(0,7)


ggplot(reduceddataset.col, aes(x=Race, y=`Columbus Day`, fill=Race, group=Race)) +
  geom_bar(stat="identity", position=position_dodge())  +
  geom_errorbar(aes(ymin=`Columbus Day`-ci, ymax=`Columbus Day`+ci), width=.2,
                position=position_dodge(.9)) +
  #facet_wrap(~Race) +
  scale_fill_brewer(palette="Paired") + theme_minimal()
