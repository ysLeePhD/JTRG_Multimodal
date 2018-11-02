
library(foreign)

data00 <- read.csv(file="M:/Millennial_CA/02_raw_data/11_latest_update/GenY_Syntax6_Step1_temp.csv")

colnames(data00)
data11 <- data00[, c(1, 37, 33, 7, 57, 19, 53, 116, 799, 674)]
colnames(data11)

data11$PID <- data11$ï..PID
data11$ï..PID <- NULL 

data11$Group <- NA 
data11$Group <- ifelse(data11$Age<=34 & data11$C6_ParentsDV==0, 1, data11$Group) # independent millennials 
data11$Group <- ifelse(data11$Age<=34 & data11$C6_ParentsDV==1, 2, data11$Group) # dependent millennials 
data11$Group <- ifelse(data11$Age>=35,                          3, data11$Group) # Gen Xers 
data11$Group <- factor(data11$Group, labels=c("IndMill", "DepMill", "GenXer"), ordered=TRUE)
table(data11$Group)

library(dplyr)

# Calculate percentages and label positions
group <- c("IndMill", "IndMill", "IndMill", "IndMill", "IndMill", 
          "DepMill", "DepMill", "DepMill", "DepMill", "DepMill", 
          "GenXer", "GenXer", "GenXer", "GenXer", "GenXer")
group <- factor(group, levels=c("GenXer", "DepMill", "IndMill"))
Scale <-c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree", 
          "Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree", 
          "Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree") 
Scale <- factor(Scale, levels=c("Strongly agree", "Agree", "Neutral", "Disagree", "Strongly disagree"))

wtsummary <- function(x){
  return (c(
    sum(data11[data11$Group=="IndMill" & data11[, x]==1, ]$Final_weights),
    sum(data11[data11$Group=="IndMill" & data11[, x]==2, ]$Final_weights),
    sum(data11[data11$Group=="IndMill" & data11[, x]==3, ]$Final_weights),
    sum(data11[data11$Group=="IndMill" & data11[, x]==4, ]$Final_weights),
    sum(data11[data11$Group=="IndMill" & data11[, x]==5, ]$Final_weights),
    sum(data11[data11$Group=="DepMill" & data11[, x]==1, ]$Final_weights),
    sum(data11[data11$Group=="DepMill" & data11[, x]==2, ]$Final_weights),
    sum(data11[data11$Group=="DepMill" & data11[, x]==3, ]$Final_weights),
    sum(data11[data11$Group=="DepMill" & data11[, x]==4, ]$Final_weights),
    sum(data11[data11$Group=="DepMill" & data11[, x]==5, ]$Final_weights),
    sum(data11[data11$Group=="GenXer" & data11[, x]==1, ]$Final_weights),
    sum(data11[data11$Group=="GenXer" & data11[, x]==2, ]$Final_weights),
    sum(data11[data11$Group=="GenXer" & data11[, x]==3, ]$Final_weights),
    sum(data11[data11$Group=="GenXer" & data11[, x]==4, ]$Final_weights),
    sum(data11[data11$Group=="GenXer" & data11[, x]==5, ]$Final_weights)
  ))
}

library(scales)

wtcase <- wtsummary(1)
df <- data.frame(group, scale, wtcase)
# https://stackoverflow.com/questions/33807624/understanding-ddply-error-message
pct <- ddply(df, .(group), plyr::summarize, pct=round(wtcase/sum(wtcase)*100, 1))
df <- data.frame(df, pct$pct)
df$pct <- paste0(as.character(df$pct), "%")
df$pct.pct <- NULL



# https://stackoverflow.com/questions/9563368/create-stacked-barplot-where-each-stack-is-scaled-to-sum-to-100
ggplot(df,aes(x = group, y = wtcase, fill = Scale)) + 
  geom_bar(position = "fill",stat = "identity", width=.7, colour="black", lwd=0.1) +
  coord_flip()+
  scale_y_continuous(labels = percent_format()) + 
  xlab("")+
  ylab("") 
#I am trying to figure out my career (e.g. what I want to to, where I will end up.).")

library(lattice)
library(Formula)
library(Hmisc)
# detach(package:Hmisc)

wtsummary2 <- function(x) {
  m1 <- wtd.mean(data11[data11$Group=="IndMill", ][, x], data11[data11$Group=="IndMill", ]$Final_weights)
  sd1 <- sqrt(wtd.var(data11[data11$Group=="IndMill", ][, x], data11[data11$Group=="IndMill", ]$Final_weights))
  m2 <- wtd.mean(data11[data11$Group=="DepMill", ][,x], data11[data11$Group=="DepMill", ]$Final_weights)
  sd2 <- sqrt(wtd.var(data11[data11$Group=="DepMill", ][,x], data11[data11$Group=="DepMill", ]$Final_weights))
  m3 <- wtd.mean(data11[data11$Group=="GenXer", ][,x], data11[data11$Group=="GenXer", ]$Final_weights)
  sd3 <- sqrt(wtd.var(data11[data11$Group=="GenXer", ][,x], data11[data11$Group=="GenXer", ]$Final_weights))
  wt.mean <- c(m1, m2, m3)
  wt.sd <- c(sd1, sd2, sd3)
  wt <- data.frame(wt.mean, wt.sd)
  return(wt)
}

wtsummary2(1)


# install.packages("gdata")
# install.packages("mice")
# install.packages("weights")

library(gdata)
library(lattice)
library(Formula)
library(Hmisc)
library(Formula)
library(ggplot2)
library(mice)
library(weights)

colnames(data11)
# https://cran.r-project.org/web/packages/weights/weights.pdf
wtd.chi.sq(data11$Group, data11[, 1], weight=data11$Final_weights, mean1=FALSE)
