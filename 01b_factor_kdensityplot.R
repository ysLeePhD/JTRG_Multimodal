
library(foreign)

data00 <- read.csv(file="M:/Millennial_CA/02_raw_data/11_latest_update/GenY_Syntax6_Step1_temp.csv")

colnames(data00)
data31 <- data00[, c(1,842:891, 799, 116, 674)]
colnames(data31)

data31$PID <- data31$ï..PID
data31$ï..PID <- NULL 

data31$Group <- NA 
data31$Group <- ifelse(data31$Age<=34 & data31$C6_ParentsDV==0, 1, data31$Group) # independent millennials 
data31$Group <- ifelse(data31$Age<=34 & data31$C6_ParentsDV==1, 2, data31$Group) # dependent millennials 
data31$Group <- ifelse(data31$Age>=35,                          3, data31$Group) # Gen Xers 
data31$Group <- factor(data31$Group, labels=c("IndMill", "DepMill", "GenXer"), ordered=TRUE)
table(data31$Group)

data31 <- data31[-912, ]
factorfilenames <- paste0("factor", 1:32, ".png")

library(ggplot2)

for (i in 19:50) {
  ggplot(data31, aes(x=data31[, i], fill=Group)) +
    geom_density(alpha=0.5) + 
    scale_fill_brewer(palette="RdYlBu", direction=-1) + 
    xlim(c(-4.5, 4.5))+ 
    ylim(c(0, 0.6))+
    ylab("")+
    xlab(colnames(data31)[i])
  
  ggsave(factorfilenames[i-18])
}

# ?density
# https://cran.r-project.org/web/packages/ggplot2/ggplot2.pdf
# http://www.sthda.com/english/wiki/ggplot2-density-plot-quick-start-guide-r-software-and-data-visualization#customized-density-plots

# http://www.sthda.com/english/wiki/one-way-anova-test-in-r

aov.results <- data.frame(var1=double(), var2=double(), var3=double())
colnames(aov.results) <- c("DepMill-IndMill", "GenXer-IndMill", "GenXer-DepMill")

for (i in 19:50) {
  a <- TukeyHSD(aov(data31[, i] ~ data31$Group))[[1]][, 4]
  aov.results[i-18, 1] <- a[1]
  aov.results[i-18, 2] <- a[2]
  aov.results[i-18, 3] <- a[3]
}
aov.results
aov.results$sig1 <- ifelse(aov.results[1]<0.01, "***", ifelse(aov.results[1]<0.05, "**", ifelse(aov.results[1]<0.1, "*", "")))
aov.results$sig2 <- ifelse(aov.results[2]<0.01, "***", ifelse(aov.results[2]<0.05, "**", ifelse(aov.results[2]<0.1, "*", "")))
aov.results$sig3 <- ifelse(aov.results[3]<0.01, "***", ifelse(aov.results[3]<0.05, "**", ifelse(aov.results[3]<0.1, "*", "")))
aov.results <- aov.results[, c(1, 4, 2, 5, 3, 6)]
rownames(aov.results) <- colnames(data31)[19:50]
write.csv(aov.results, file="M:/Millennial_CA/17_JTRG_multimodal/JTRG_Multimodal/anovaresults.csv")



