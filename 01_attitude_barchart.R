
library(foreign)

data00 <- read.csv(file="M:/Millennial_CA/02_raw_data/11_latest_update/GenY_Syntax6_Step1_temp.csv")

colnames(data00)
data11 <- data00[, c(1, 7:34, 36:47, 49:60, 584:597, 799, 116, 674)]
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


sent00 <- read.csv(file="M:/Millennial_CA/17_JTRG_multimodal/JTRG_Multimodal/att_sentences.csv")
str(sent00)
sent01 <- as.character(sent00[, 1])

attfilenames <- paste0("attitude", 1:66, ".png")
plotnames <- paste0("p", 1:66)
plotnames
library(scales)
library(ggplot2)
library(RColorBrewer)
library(plyr)

#11, 13, 14, 15, 20, 23, 24, 25, 26, 27, 30,  
#34, 35, 37, 39, 42, 45, 48, 52, 56, 61, 64

for (i in c(11, 13, 14, 15, 20, 23, 24, 26, 27, 30,  
            34, 35, 37, 39, 42, 45, 48, 52, 56, 61, 64)) {
  wtcase <- wtsummary(i)
  df <- data.frame(group, Scale, wtcase)
  # https://stackoverflow.com/questions/33807624/understanding-ddply-error-message
  pct <- ddply(df, .(group), plyr::summarize, pct=round(wtcase/sum(wtcase)*100, 1))
  df <- data.frame(df, pct$pct)
  df$pct <- paste0(as.character(df$pct), "%")
  df$pct.pct <- NULL

  # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  # cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442")#, "#0072B2", "#D55E00", "#CC79A7")
  # https://stackoverflow.com/questions/9563368/create-stacked-barplot-where-each-stack-is-scaled-to-sum-to-100
  ggplot(df,aes(x = group, y = wtcase, fill = Scale))+ 
    geom_bar(position = "fill",stat = "identity", width=.7, colour="black", lwd=0.1) +
    scale_fill_brewer(palette="RdYlBu", direction=-1) + 
    # https://stackoverflow.com/questions/8750871/ggplot2-reverse-order-of-scale-brewer
    coord_flip()+
    scale_y_continuous(labels = percent_format()) + 
    xlab("")+
    ylab(sent01[i])+
    # http://felixfan.github.io/ggplot2-remove-grid-background-margin/
    theme(panel.background = element_blank())+
    theme(legend.position='none')
  
  ggsave(width = 8.5, height = 2, dpi = 300, attfilenames[i])
}


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
library(ggplot2)
library(mice)
library(weights)

# options(stringsAsFactors = FALSE)
# https://stackoverflow.com/questions/16819956/warning-message-in-invalid-factor-level-na-generated

table(data11$Group)
data12 <- data11[data11$Group != "GenXer", ]
data12$Group <- factor(data12$Group, levels=c("IndMill", "DepMill"), ordered=TRUE)
data13 <- data11[data11$Group != "DelMill", ]
data13$Group <- factor(data13$Group, levels=c("IndMill", "GenXer"), ordered=TRUE)
data14 <- data11[data11$Group != "IndMill", ]
data14$Group <- factor(data14$Group, levels=c("DepMill", "GenXer"), ordered=TRUE)


rm(wtd.chi.sq.results1, wtd.chi.sq.results2, wtd.chi.sq.results3)
wtd.chi.sq.results1 <- data.frame(Chisq1=as.numeric(), df1=as.integer(), p.value1=as.numeric(), sig1=as.character())
wtd.chi.sq.results1$sig1 <- as.character(wtd.chi.sq.results1$sig1)
# https://cran.r-project.org/web/packages/weights/weights.pdf
for (i in 1:66){
  a <- wtd.chi.sq(data12$Group, data12[, i], weight=data12$Final_weights, mean1=FALSE)
  wtd.chi.sq.results1[i, 1] <- a[1]
  wtd.chi.sq.results1[i, 2] <- a[2]
  wtd.chi.sq.results1[i, 3] <- a[3]
  wtd.chi.sq.results1[i, 4] <- ifelse(a[3]<0.01, "***", ifelse(a[3]<0.05, "**", ifelse(a[3]<0.1, "*", "")))
}
wtd.chi.sq.results1$varname <- colnames(data11)[1:66]
wtd.chi.sq.results1 <- wtd.chi.sq.results1[, c(5, 1:4)]
wtd.chi.sq.results1


wtd.chi.sq.results2 <- data.frame(Chisq2=as.numeric(), df2=as.integer(), p.value2=as.numeric(), sig2=as.character())
wtd.chi.sq.results2$sig2 <- as.character(wtd.chi.sq.results2$sig2)
# https://cran.r-project.org/web/packages/weights/weights.pdf
for (i in 1:66){
  a <- wtd.chi.sq(data13$Group, data13[, i], weight=data13$Final_weights, mean1=FALSE)
  wtd.chi.sq.results2[i, 1] <- a[1]
  wtd.chi.sq.results2[i, 2] <- a[2]
  wtd.chi.sq.results2[i, 3] <- a[3]
  wtd.chi.sq.results2[i, 4] <- ifelse(a[3]<0.01, "***", ifelse(a[3]<0.05, "**", ifelse(a[3]<0.1, "*", "")))
}
wtd.chi.sq.results2$varname <- colnames(data11)[1:66]
wtd.chi.sq.results2 <- wtd.chi.sq.results2[, c(5, 1:4)]
wtd.chi.sq.results2


wtd.chi.sq.results3 <- data.frame(Chisq3=as.numeric(), df3=as.integer(), p.value3=as.numeric(), sig3=as.character())
wtd.chi.sq.results3$sig3 <- as.character(wtd.chi.sq.results3$sig3)
# https://cran.r-project.org/web/packages/weights/weights.pdf
for (i in 1:66){
  a <- wtd.chi.sq(data14$Group, data14[, i], weight=data14$Final_weights, mean1=FALSE)
  wtd.chi.sq.results3[i, 1] <- a[1]
  wtd.chi.sq.results3[i, 2] <- a[2]
  wtd.chi.sq.results3[i, 3] <- a[3]
  wtd.chi.sq.results3[i, 4] <- ifelse(a[3]<0.01, "***", ifelse(a[3]<0.05, "**", ifelse(a[3]<0.1, "*", "")))
}
wtd.chi.sq.results3$varname <- colnames(data11)[1:66]
wtd.chi.sq.results3 <- wtd.chi.sq.results3[, c(5, 1:4)]
wtd.chi.sq.results3

wtd.chi.sq.results <- merge(wtd.chi.sq.results1[, c(1, 5)], wtd.chi.sq.results2[, c(1, 5)], by.x="varname", by.y="varname")
wtd.chi.sq.results <- merge(wtd.chi.sq.results, wtd.chi.sq.results3[, c(1, 5)], by.x="varname", by.y="varname")
# because the original SPSS table was not accurately sorted by alphabetical order, 
# there is inconsistency regarding the place of "J11k_noalternativecar" 

colnames(wtd.chi.sq.results) <- c("statement", "IM-DM", "IM-GX", "DM-GX")

write.csv(wtd.chi.sq.results, file="M:/Millennial_CA/17_JTRG_multimodal/JTRG_Multimodal/wtchisqresults.csv")
