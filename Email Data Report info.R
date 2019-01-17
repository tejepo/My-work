#Analysis of Labor Market Field data
#11-9-18
#Terrence Pope
library(tidyverse)
library(vcd)
library(gmodels)
library(Hmisc)

setwd("~/Desktop/HP/Email Analysis/")
HPEmail <- read.csv("Summer Email Data Nov 18.csv")


#Recode Participant Race
HPEmail$Race <- HPEmail$User.Email %>%
  recode_factor("leroy.king195@gmail.com" = 1, "tremayne.jackson999@gmail.com" = 1, "tyrone.washington230@gmail.com" = 1, "zhiyuan.wang631@gmail.com" = 2, "chang.yang675@gmail.com" = 2, "tao.lee735@gmail.com" = 2)

HPEmail$Race <- factor(HPEmail$Race,
                    levels = c(1,2),
                    labels = c("Black", "Asian"))

label(HPEmail$Race) <- "Participant Race"


HPEmail <- HPEmail %>% 
  filter(Condition == "American" | Condition == "Status")

#Create Variable For 

#Creating a table
emailResponse <- table(HPEmail$Condition, HPEmail$Race)
emailResponse <- emailResponse[2:3,]

margin.table(emailResponse,1)
margin.table(emailResponse,2)

HP <- xtabs(~Condition+Race, data=HPEmail)
HP <- HP[2:3,]
ftable(HP)
summary(HP)

#Finally, onto the Chi-squared part of the code
# 2-Way Frequency Table 
#attach(Data)

recievedResponse <- HPEmail %>%
  filter(Replied == "Yes")

mytable <- table(recievedResponse$Race,recievedResponse$Condition) # A will be rows, B will be columns 
mytable
mytable <- mytable[,2:3] #since the other columns are empty, we'll just select for the column with data in them manually.
mytable

margin.table(mytable, 1) # A frequencies (summed over B) 
margin.table(mytable, 2) # B frequencies (summed over A)

prop.table(mytable) # cell percentages
prop.table(mytable, 1) # row percentages 
prop.table(mytable, 2) # column percentages

# 2-Way Frequency Table
mytable2 <- xtabs(~Race+Condition, data=recievedResponse)
mytable2 <- mytable2[,2:3]
ftable(mytable2) # print table 
summary(mytable2) # chi-square test of indepedence

# 2-Way Cross Tabulation (closest to spss Crosstabs)
#install.packages("gmodels")
CrossTable(recievedResponse$Condition, recievedResponse$Race)

#Chi-Square Test
chisq.test(mytable)
#test independence of the row and column variable. By default, the p-value is calculated from the asymptotic chi-squared distribution of the test statistic. Optionally, the p-value can be derived via Monte Carlo simultation.

#Fisher Exact Test
mytable.m <- as.matrix(mytable)
fisher.test(mytable.m)
#exact test of independence. x is a two dimensional contingency table in matrix form.

# Grouped Bar Plot
counts <- table(recievedResponse$Race, recievedResponse$Condition)
counts <- counts[,2:3]
barplot(counts, main="Which Race is Hired by Condition",
        xlab="Condition", col=c("turquoise","blue"),
        legend = rownames(counts), beside=TRUE)

# Association Plot Example
assoc(mytable, shade=TRUE)

# Mosaic Plot Example
mosaic(mytable, shade=TRUE, legend=TRUE)


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

#Figure out the chi squared test of fit:
asian.prop <- HPEmail %>% 
  filter(Race == "Asian") %>%
  select(Condition, Replied.) 

black.prop <- HPEmail %>% 
  filter(Race == "Black") %>%
  select(Condition, Replied.) 

setwd("~/Desktop")
write.csv(asian.prop, "data1.csv")
write.csv(black.prop, "data2.csv")

##THE FOLLOWING WONT WORK UNTIL I EDIT/MERGE THE CSV FILES AND ADD A RACE COLUM

aProp.sum <- summarySE(data=asian.prop, measurevar="Replied.", groupvars=c("Condition"))
bProp.sum <- summarySE(data=black.prop, measurevar="Replied.", groupvars=c("Condition"))

all.prop <- read.csv("data1.csv")

all.prop %>%
  filter(Race == "Asian") %>%
  lm(Replied. ~ Condition, .) %>%
  summary()

all.prop %>%
  filter(Race == "Black") %>%
  lm(Replied. ~ Condition, .) %>%
  summary()

all.prop.sum <- summarySE(data=all.prop, measurevar="Replied.", groupvars=c("Race", "Condition"))
all.prop.sum

pd <- position_dodge(0.25)

ggplot(all.prop.sum, aes(x=Condition, y=Replied., colour=Race, group=Race)) + 
  geom_errorbar(aes(ymin=Replied.-se, ymax=Replied.+se), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
  xlab("Condition") +
  ylab("% of Responses") +
  scale_colour_hue(name="Race of Applicant",    # Legend label, use darker colors
                   breaks=c("Black", "Asian"),
                   labels=c("African American", "Asian American"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Email Response Rates") +
  expand_limits(y=0) +                        # Expand y range
  scale_y_continuous(limits=c(0,1)) +         # can set # of ticks with break= argument (not used here)
  #facet_wrap(~Measure) +
  theme_bw() +
  theme(legend.justification=c(1,0),
        legend.position=c(1,0))      

####

ggplot(all.prop.sum, aes(x=Condition, y=Replied., fill=Race)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=Replied.-se, ymax=Replied.+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Job Type") +
  ylab("% of Responses") +
  scale_fill_hue(name="Applicant Race", # Legend label, use darker colors
                 breaks=c("Black", "Asian"),
                 labels=c("African American", "Asian American")) +
  ggtitle("Email Response Rates") +
  scale_y_continuous(limits = c(0,1)) +
  #facet_wrap(~Measure) +
  theme_bw()



#####
asian.prop <- HPEmail %>%
  filter(Race == "Asian") %>%
  select(Replied.) 

black.prop <- HPEmail %>% 
  filter(Race == "Black") %>%
  select(Replied.) 

asian.prop <- as.vector(asian.prop)
asian.prop <- as.numeric(asian.prop[,1])
apm <- mean(asian.prop)

black.prop <- as.vector(black.prop)
black.prop <- as.numeric(black.prop[,1])
bpm <- mean(black.prop)


tulip <- c(apm, bpm)
res <- chisq.test(tulip)
summary(res)

res$expected
