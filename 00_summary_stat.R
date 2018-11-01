
library(foreign)

data00 <- read.csv(file="M:/Millennial_CA/02_raw_data/11_latest_update/GenY_Syntax6_Step1_temp.csv")

colnames(data00)
data01 <- data00[, c(1, 114:120, 599:643, 799, 426, 468, 181:334)]
colnames(data01)

data01$Group <- NA 
data01$Group <- ifelse(data01$Age<=34 & data01$C6_ParentsDV==0, 1, data01$Group) # independent millennials 
data01$Group <- ifelse(data01$Age<=34 & data01$C6_ParentsDV==1, 2, data01$Group) # dependent millennials 
data01$Group <- ifelse(data01$Age>=35,                          3, data01$Group) # Gen Xers 
data01$Group <- factor(data01$Group, labels=c("IndMill", "DepMill", "GenXer"), ordered=TRUE)

table(data01$K11_gender)
data01$Gender <- factor(data01$K11_gender, labels=c("male", "female", "trans", "decline"), ordered=TRUE)

data01$nchild <- data01$K14a_kidsunder6 + data01$K14b_kids6to12 + data01$K14c_kids13to17
data01$yeschild <- ifelse(data01$nchild>0, 1, 0)

table(data01$K17_hhincome)
data01$hhincome <- factor(data01$K17_hhincome, labels=c("Prefer not to answer", "less than $20,000", "$20,001 to $40,000", 
                                                       "$40,001 to $60,000", "$60,001 to $80,000", "$80,001 to $100,000", 
                                                       "$100,001 to $120,000", "$120,001 to $140,000", "$140,001 to $160,000", 
                                                       "More than $160,000"), ordered=TRUE) 
data01$AgeGroup <- NA 
data01$AgeGroup <- ifelse(data01$Age<=26,                  1, data01$AgeGroup)
data01$AgeGroup <- ifelse(data01$Age>=27 & data01$Age<=34, 2, data01$AgeGroup)
data01$AgeGroup <- ifelse(data01$Age>=35 & data01$Age<=43, 3, data01$AgeGroup)
data01$AgeGroup <- ifelse(data01$Age>=44,                  4, data01$AgeGroup)
table(data01$AgeGroup)

data01$Hispanic <- data01$K6d_hispanic

data01$Race <- NA 
data01$Race <- ifelse(data01$K6b_asian==1 & data01$K6e_white+data01$K6c_black+data01$K6a_nativeamerican==0, 1, data01$Race)
data01$Race <- ifelse(data01$K6e_white==1 & data01$K6b_asian+data01$K6c_black+data01$K6a_nativeamerican==0, 2, data01$Race)
data01$Race <- ifelse(data01$K6c_black==1 & data01$K6b_asian+data01$K6e_white+data01$K6a_nativeamerican==0, 3, data01$Race)
data01$Race <- ifelse(data01$K6a_nativeamerican==1 & data01$K6b_asian+data01$K6e_white+data01$K6c_black==0, 4, data01$Race)
data01$Race <- ifelse(is.na(data01$Race)==TRUE, 5, data01$Race)
table(data01$Race)
data01$Race <- factor(data01$Race, labels=c("Asian", "White", "Black", "Native American", "Others"), ordered=TRUE)
table(data01$Race)




