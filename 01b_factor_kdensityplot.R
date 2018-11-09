
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

table(data31$Group)
table(as.numeric(data31$Group))

data32 <- data31[data31$Group == "IndMill", ]
data33 <- data31[data31$Group == "DepMill", ]
data34 <- data31[data31$Group == "GenXer", ]

rm(table3) 
table3 <- data.frame("IndM-DepM"=double(), "sig1"=as.character(), 
                     "IndM-GenX"=double(), "sig2"=as.character(), 
                     "DepM-GenX"=double(), "sig3"=as.character(), stringsAsFactors = FALSE)
table3
for (i in 19:36) {
  table3[i-18, 1] <- wtd.t.test(data32[, i], data33[, i], weight=data32$Final_weights, weighty=data33$Final_weights, samedata=FALSE)$coefficients[3]
  table3[i-18, 2] <- ifelse(table3[i-18, 1]<0.01, "***", ifelse(table3[i-18, 1]<0.05, "**", ifelse(table3[i-18, 1]<0.1, "*", "")))
  table3[i-18, 3] <- wtd.t.test(data32[, i], data34[, i], weight=data32$Final_weights, weighty=data34$Final_weights, samedata=FALSE)$coefficients[3]
  table3[i-18, 4] <- ifelse(table3[i-18, 3]<0.01, "***", ifelse(table3[i-18, 3]<0.05, "**", ifelse(table3[i-18, 3]<0.1, "*", "")))
  table3[i-18, 5] <- wtd.t.test(data33[, i], data34[, i], weight=data33$Final_weights, weighty=data34$Final_weights, samedata=FALSE)$coefficients[3]
  table3[i-18, 6] <- ifelse(table3[i-18, 5]<0.01, "***", ifelse(table3[i-18, 5]<0.05, "**", ifelse(table3[i-18, 5]<0.1, "*", "")))
  rownames(table3)[i-18] <- colnames(data32)[i]
}

table3 
