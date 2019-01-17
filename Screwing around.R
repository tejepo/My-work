#Labor Market Data
#Test analysis 02/01/18

rm(list=ls())
require(ggplot2)
require(psych)
require(dplyr)
setwd("~/Desktop/UW/Winter '18/Linear Models & Data Analysis/Test")
lmarket <- read.csv("HiringPerceptionsData012618.csv")
hist(lmarket$Condition, lmarket$hired_race)
as.factor(lmarket$Condition)
lmarket$Condition
?hist
lmarket <- lmarket %>% na.fail(lmarket.Condition)
View(lmarket$Condition)
?na.omit
View(lmarket)

lmarket_cleaned <- lmarket[ -seq( from=60, length.out=58, by=1), ]
lmarket_cleaned

typeof(lmarket_cleaned$hired_race)
as.factor(lmarket_cleaned$hired_race)

ggplot() +
  geom_point(data=lmarket_cleaned, aes(x=AA_QUAL,y=AF_QUAL), size = .5)) +
  xlim(c(1,3)) +
  #annotate(geom = "point", x=0, y=mean(df_urban$bpm), color = "red", size=4) +
  #annotate(geom = "point", x=1, y=mean(df_rural$bpm), color = "red", size=4) +
  #geom_hline(yintercept=mean(df$bpm), color="blue") + #blue line is grand mean, this is anova
  geom_smooth(data=lmarket_cleaned,aes(x=AA_QUAL,y=AF_QUAL),method="lm", se=F) +
  theme_bw()

as.numeric(lmarket_cleaned$AF_QUAL)
as.numeric(lmarket_cleaned$AA_QUAL)

apatheme<-theme_bw()+
  theme(
    panel.grid.major=element_blank(), 
    panel.grid.minor=element_blank(), 
    panel.border=element_blank(), 
    axis.line=element_line(), 
    legend.title=element_blank()
  )

milestoneplot<-function(df, xvar, yvar){
  ggplot(data=df, aes(x=xvar,y=yvar)) +
    geom_point(shape=16, size = 1) +
    ylim(min(yvar,na.rm=T) - sd(yvar,na.rm=T),max(yvar,na.rm=T) + sd(yvar,na.rm=T)) + 
    xlim(min(xvar,na.rm=T) - sd(xvar,na.rm=T),max(xvar,na.rm=T) + sd(xvar,na.rm=T)) + 
    apatheme +
    labs(x = "African American Qual", y= "Asian American Qual") +
    ggtitle("Relationship of Qualification")
}

nicescatter<-milestoneplot(df=lmarket_cleaned,xvar=lmarket_cleaned$AF_QUAL,yvar=lmarket_cleaned$AA_QUAL) +
  stat_smooth(method = "lm", formula = y ~ x, color = "black", se=T) 

nicescatter

names(nicescatter)
nicescatter$plot_env

m1 <- AA_QUAL ~ AF_QUAL
qualipre <- lm(m1, data=lmarket_cleaned)

summary(qualipre)
hist(qualipre$residuals)
qual.res<- qualipre$residuals
scatter.hist(qualipre$AF_QUAL, y=qualipre$residuals, na.rm=T)

?scatter.smooth
qualipre[
scatter.smooth(lmarket_cleaned$AF_QUAL, y=qualipre$residuals,
               xlab = "African American Qualification",
               ylab = "Residuals")

plot(lmarket_cleaned$AF_QUAL, qual.res, na.rm=T)

length(lmarket_cleaned$AF_QUAL)
length(qual.res)

lmarket_cleaned <- lmarket[ -seq( from=60, length.out=58, by=1), ]
lmarket_cleaned$AF_QUAL

AFQUAL<-lmarket_cleaned$AF_QUAL

AFQUAL <- AFQUAL[ -seq( from=59, length.out=1, by=1), ]

