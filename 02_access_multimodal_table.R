
library(foreign)

data00 <- read.csv(file="M:/Millennial_CA/02_raw_data/11_latest_update/GenY_Syntax6_Step1_temp.csv")
colnames(data00)

data21 <- data00[, c(1, 799, 116, 727:729, 794:796, 674)]
colnames(data21)

data21$PID <- data21$ï..PID
data21$ï..PID <- NULL 

data21$Group <- NA 
data21$Group <- ifelse(data21$Age<=34 & data21$C6_ParentsDV==0, 1, data21$Group) # independent millennials 
data21$Group <- ifelse(data21$Age<=34 & data21$C6_ParentsDV==1, 2, data21$Group) # dependent millennials 
data21$Group <- ifelse(data21$Age>=35,                          3, data21$Group) # Gen Xers 
data21$Group <- factor(data21$Group, labels=c("IndMill", "DepMill", "GenXer"), ordered=TRUE)

data21$bikescore <- ifelse(is.na(data21$bikescore)==TRUE, 0, data21$bikescore)
data21$transitscore <- ifelse(is.na(data21$transitscore)==TRUE, 0, data21$transitscore)

head(data21)


