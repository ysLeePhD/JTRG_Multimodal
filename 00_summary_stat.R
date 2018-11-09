
library(foreign)

data00 <- read.csv(file="M:/Millennial_CA/02_raw_data/11_latest_update/GenY_Syntax6_Step1_temp.csv")

colnames(data00)
data01 <- data00[, c(1, 114:120, 599:643, 799, 420, 426, 468, 181:334, 674)]
colnames(data01)

data01$PID <- data01$ï..PID

data01$Group <- NA 
data01$Group <- ifelse(data01$Age<=34 & data01$C6_ParentsDV==0, 1, data01$Group) # independent millennials 
data01$Group <- ifelse(data01$Age<=34 & data01$C6_ParentsDV==1, 2, data01$Group) # dependent millennials 
data01$Group <- ifelse(data01$Age>=35,                          3, data01$Group) # Gen Xers 
data01$Group <- factor(data01$Group, labels=c("IndMill", "DepMill", "GenXer"), ordered=TRUE)


# Sociodemographc/economic characteristics  

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
data01$AgeGroup <- factor(data01$AgeGroup, labels=c("youngM", "olderM", "youngX", "olderX"), ordered=TRUE)

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

table(data01$K19_education)
data01$Education <- factor(data01$K19_education, labels=c(
  "Prefer not to answer", "Some grade/high school", "High school/GED", "Some college", 
  "Associate's degree", "Bachelor's degree", "Graduate degree", "Professional degree"), ordered=TRUE)
table(data01$Education)

data01$HHSize <- data01$K13_HHSize


# Travel outcomes  

data01$license <- factor(data01$H1_car, labels=c("No license", "With a driver's license"), ordered=TRUE)
data01$ncar <- data01$H4_NumCar
data01$carpdr <- ifelse(data01$K15_numdrivers>0, data01$H4_NumCar/data01$K15_numdrivers, 0)
data01$VMDpw <- ifelse(data01$H11car_VMT>=0, data01$H11car_VMT, 0) 
colnames(data01)

summary(data01$F1_commutedest)

summary(data01[is.na(data01$F7work_pmode)==TRUE, ]$F7school_pmode)
summary(data01[is.na(data01$F7work_pmode)==FALSE, ]$F7school_pmode)
summary(data01[is.na(data01$F7school_pmode)==TRUE, ]$F7work_pmode)
summary(data01[is.na(data01$F7school_pmode)==FALSE, ]$F7work_pmode)

data01$commute_pmode <- NA
data01$commute_pmode <- ifelse(is.na(data01$F7work_pmode)==FALSE, data01$F7work_pmode, data01$commute_pmode)
data01$commute_pmode <- ifelse(is.na(data01$F7school_pmode)==FALSE, data01$F7school_pmode, data01$commute_pmode)
data01$commute_pmode <- ifelse(data01$commute_pmode<0, NA, data01$commute_pmode)
table(data01$commute_pmode)

data01$lastcommute_drive   <- ifelse(data01$commute_pmode==1, 1, 0) 
data01$lastcommute_carpool <- 0 
data01$lastcommute_carpool <- ifelse(data01$commute_pmode==2, 1, 0) 
data01$lastcommute_carpool <- ifelse(data01$commute_pmode==3, 1, data01$lastcommute_carpool) 
data01$lastcommute_motor   <- ifelse(data01$commute_pmode==4, 1, 0) 
data01$lastcommute_shuttle <- ifelse(data01$commute_pmode==5, 1, 0) 
data01$lastcommute_transit <- 0 
data01$lastcommute_transit <- ifelse(data01$commute_pmode==6, 1, 0) 
data01$lastcommute_transit <- ifelse(data01$commute_pmode==7, 1, data01$lastcommute_transit) 
data01$lastcommute_transit <- ifelse(data01$commute_pmode==8, 1, data01$lastcommute_transit) 
data01$lastcommute_ridehail<- ifelse(data01$commute_pmode==10, 1, 0) 
data01$lastcommute_bike    <- ifelse(data01$commute_pmode==11, 1, 0) 
data01$lastcommute_walk    <- 0 
data01$lastcommute_walk    <- ifelse(data01$commute_pmode==12, 1, 0) 
data01$lastcommute_walk    <- ifelse(data01$commute_pmode==13, 1, data01$lastcommute_walk) 
data01$lastcommute_other   <- 0 
data01$lastcommute_other   <- ifelse(data01$commute_pmode==9, 1, 0) # taxi
data01$lastcommute_other   <- ifelse(data01$commute_pmode==14, 1, data01$lastcommute_other) 

modefreq <- function(x){
  a <- NA 
  for (i in 1:length(x)){
    if (is.na(x[i])==TRUE) {
      a[i] <- 0
    } else if (x[i]<3) {
      a[i] <- 0 
    } else if (x[i]==3) {
      a[i] <- 0.5 
    } else if (x[i]==4) {
      a[i] <- 2
    } else if (x[i]==5) {
      a[i] <- 6
    } else if (x[i]==6) {
      a[i] <- 14
    } else if (x[i]==7) {
      a[i] <- 20
    }
  }
  return(a) 
}

data01$commute_car <- modefreq(data01$F6school_Drivealone) + modefreq(data01$F6school_CarpoolD) + modefreq(data01$F6school_CarpoolP) + 
  modefreq(data01$F6school_Moto) + modefreq(data01$F6work_Drivealone) + modefreq(data01$F6work_CarpoolD) + 
  modefreq(data01$F6work_CarpoolP) + modefreq(data01$F6work_Moto) 
data01$commute_transit <- modefreq(data01$F6school_Bus) + modefreq(data01$F6school_LR) + modefreq(data01$F6school_Train) + 
  modefreq(data01$F6work_Bus) + modefreq(data01$F6work_LR) + modefreq(data01$F6work_Train)
data01$commute_active <- modefreq(data01$F6school_Bike) + modefreq(data01$F6school_Skateboard) + modefreq(data01$F6school_Walk) + 
  modefreq(data01$F6work_Bike) + modefreq(data01$F6work_Skateboard) + modefreq(data01$F6work_Walk)
data01$commute_ridehail <- modefreq(data01$F6school_Uber) + modefreq(data01$F6work_Uber)
data01$commute_other <- modefreq(data01$F6school_Shuttle) + modefreq(data01$F6school_Taxi) + modefreq(data01$F6school_Other) + 
  modefreq(data01$F6work_Shuttle) + modefreq(data01$F6work_Taxi) + modefreq(data01$F6work_Other)

data01$leisure_car <- modefreq(data01$F14leisure_Drivealone) + modefreq(data01$F14leisure_CarpoolD) + 
  modefreq(data01$F14leisure_CarpoolP) + modefreq(data01$F14leisure_Carsharing) + modefreq(data01$F14leisure_Moto) 
data01$leisure_transit <- modefreq(data01$F14leisure_Bus) + modefreq(data01$F14leisure_LR) + modefreq(data01$F14leisure_Train)
data01$leisure_active <- modefreq(data01$F14leisure_Bike) + modefreq(data01$F14leisure_Skateboard) + modefreq(data01$F14leisure_Walk)
data01$leisure_ridehail <- modefreq(data01$F14leisure_Uber)
data01$leisure_other <- modefreq(data01$F14leisure_Taxi) + modefreq(data01$F14leisure_Other) 
  
  
# Create a weighted summary table 

data02 <- data01[, c("PID", "Group", "Gender", "yeschild", "hhincome", "AgeGroup", 
                     "Hispanic", "Race", "Education", "license", "HHSize", "ncar", "carpdr", "VMDpw", 
                     "lastcommute_drive", "lastcommute_carpool", "lastcommute_motor", "lastcommute_shuttle", 
                     "lastcommute_transit", "lastcommute_ridehail", "lastcommute_bike", "lastcommute_walk", 
                     "lastcommute_other", "commute_car", "commute_transit", "commute_active", "commute_ridehail", 
                     "commute_other", "leisure_car", "leisure_transit", "leisure_active", "leisure_ridehail", "leisure_other", 
                     "Final_weights")]

sapply(data02, class)

data02$AgeGroup1 <- ifelse(as.numeric(data02$AgeGroup)==1, 1, 0)
data02$AgeGroup2 <- ifelse(as.numeric(data02$AgeGroup)==2, 1, 0)
data02$AgeGroup3 <- ifelse(as.numeric(data02$AgeGroup)==3, 1, 0)
data02$AgeGroup4 <- ifelse(as.numeric(data02$AgeGroup)==4, 1, 0)

data02$Gender1 <- ifelse(as.numeric(data02$Gender)==1, 1, 0)
data02$Gender2 <- ifelse(as.numeric(data02$Gender)==2, 1, 0)
data02$Gender3 <- ifelse(as.numeric(data02$Gender)==3, 1, 0)
data02$Gender4 <- ifelse(as.numeric(data02$Gender)==4, 1, 0)

data02$Race1 <- ifelse(as.numeric(data02$Race)==1, 1, 0)
data02$Race2 <- ifelse(as.numeric(data02$Race)==2, 1, 0)
data02$Race3 <- ifelse(as.numeric(data02$Race)==3, 1, 0)
data02$Race4 <- ifelse(as.numeric(data02$Race)==4, 1, 0)
data02$Race5 <- ifelse(as.numeric(data02$Race)==5, 1, 0)

data02$Education1 <- ifelse(as.numeric(data02$Education)==1, 1, 0)
data02$Education2 <- ifelse(as.numeric(data02$Education)==2, 1, 0)
data02$Education3 <- ifelse(as.numeric(data02$Education)==3, 1, 0)
data02$Education4 <- ifelse(as.numeric(data02$Education)==4, 1, 0)
data02$Education5 <- ifelse(as.numeric(data02$Education)==5, 1, 0)
data02$Education6 <- ifelse(as.numeric(data02$Education)==6, 1, 0)
data02$Education7 <- ifelse(as.numeric(data02$Education)==7, 1, 0)
data02$Education8 <- ifelse(as.numeric(data02$Education)==8, 1, 0)

data02$hhincome01 <- ifelse(as.numeric(data02$hhincome)==1, 1, 0)
data02$hhincome02 <- ifelse(as.numeric(data02$hhincome)==2, 1, 0)
data02$hhincome03 <- ifelse(as.numeric(data02$hhincome)==3, 1, 0)
data02$hhincome04 <- ifelse(as.numeric(data02$hhincome)==4, 1, 0)
data02$hhincome05 <- ifelse(as.numeric(data02$hhincome)==5, 1, 0)
data02$hhincome06 <- ifelse(as.numeric(data02$hhincome)==6, 1, 0)
data02$hhincome07 <- ifelse(as.numeric(data02$hhincome)==7, 1, 0)
data02$hhincome08 <- ifelse(as.numeric(data02$hhincome)==8, 1, 0)
data02$hhincome09 <- ifelse(as.numeric(data02$hhincome)==9, 1, 0)
data02$hhincome10 <- ifelse(as.numeric(data02$hhincome)==10, 1, 0)


library(plyr)

ddply(data02, .(Gender), summarize, sum(is.na(Gender)==FALSE))
#ddply(data02, .(Gendern), summarize, sum(is.na(Gendern)==FALSE))
ddply(data02, .(hhincome), summarize, sum(is.na(hhincome)==FALSE))
#ddply(data02, .(hhincomen), summarize, sum(is.na(hhincomen)==FALSE))
ddply(data02, .(Race), summarize, sum(is.na(Race)==FALSE))
#ddply(data02, .(Racen), summarize, sum(is.na(Racen)==FALSE))
ddply(data02, .(Education), summarize, sum(is.na(Education)==FALSE))
#ddply(data02, .(Educationn), summarize, sum(is.na(Educationn)==FALSE))

library(tableone)
library(grid) 
library(Matrix)
library(survival)
library(survey)

colnames(data02)

## Weighted table with tableone
xvars <- c("Gender1", "Gender2", "Gender3", "Gender4", "yeschild", 
           "hhincome01", "hhincome02", "hhincome03", "hhincome04", "hhincome05", 
           "hhincome06", "hhincome07", "hhincome08", "hhincome09", "hhincome10", 
           "AgeGroup1", "AgeGroup2", "AgeGroup3", "AgeGroup4", "Hispanic", 
           "Race1", "Race2", "Race3", "Race4", "Race5", 
           "Education1", "Education2", "Education3", "Education4", "Education5", 
           "Education6", "Education7", "Education8", "HHSize", 
           "license", "ncar", "carpdr", "VMDpw", 
           "lastcommute_drive", "lastcommute_carpool", "lastcommute_motor", "lastcommute_shuttle", 
           "lastcommute_transit", "lastcommute_ridehail", "lastcommute_bike", "lastcommute_walk", 
           "lastcommute_other", "commute_car", "commute_transit", "commute_active", "commute_ridehail", 
           "commute_other", "leisure_car", "leisure_transit", "leisure_active", "leisure_ridehail", "leisure_other")

xvars2 <- c("Gender", "AgeGroup", "Race", "Hispanic", "Education", "hhincome", "HHSize", "yeschild", 
            "license", "ncar", "carpdr", "VMDpw", 
            "lastcommute_drive", "lastcommute_carpool", "lastcommute_motor", "lastcommute_shuttle", 
           "lastcommute_transit", "lastcommute_ridehail", "lastcommute_bike", "lastcommute_walk", 
           "lastcommute_other", "commute_car", "commute_transit", "commute_active", "commute_ridehail", 
           "commute_other", "leisure_car", "leisure_transit", "leisure_active", "leisure_ridehail", "leisure_other")

wt.table1 <- svydesign(ids = ~ 1, data = data02, weights = ~ Final_weights)
wt.table2 <- svyCreateTableOne(vars = xvars2, strata ="Group", data = wt.table1)
print(wt.table2, catDigits=3, contDigits=3, test=TRUE, smd = TRUE)

library(lattice)
library(Formula)
library(Hmisc)

library(gdata)
library(lattice)
library(Formula)
library(Hmisc)
library(ggplot2)
library(mice)
library(weights)

#wtd.chi.sq(data02$Group, data02$Gender, weight=data02$Final_weights, mean1=FALSE)
data03 <- data02[data02$Group != "GenXer", ]
data04 <- data02[data02$Group != "DepMill", ]
data05 <- data02[data02$Group != "IndMill", ]

rm(table1)
table1 <- data.frame("IndM-DepM"=as.numeric(), "sig1"=as.character(), 
                     "IndM-GenX"=as.numeric(), "sig2"=as.character(), 
                     "DepM-GenX"=as.numeric(), "sig3"=as.character(), stringsAsFactors = FALSE)

for (i in 3:10) {
  table1[i-2, 1] <- wtd.chi.sq(data03$Group, data03[, i], weight=data03$Final_weights, mean1=TRUE)[3]
  table1[i-2, 2] <- ifelse(table1[i-2, 1]<0.01, "***", ifelse(table1[i-2, 1]<0.05, "**", ifelse(table1[i-2, 1]<0.1, "*", "")))
  table1[i-2, 3] <- wtd.chi.sq(data04$Group, data04[, i], weight=data04$Final_weights, mean1=TRUE)[3]
  table1[i-2, 4] <- ifelse(table1[i-2, 3]<0.01, "***", ifelse(table1[i-2, 3]<0.05, "**", ifelse(table1[i-2, 3]<0.1, "*", "")))
  table1[i-2, 5] <- wtd.chi.sq(data05$Group, data05[, i], weight=data05$Final_weights, mean1=TRUE)[3]
  table1[i-2, 6] <- ifelse(table1[i-2, 5]<0.01, "***", ifelse(table1[i-2, 5]<0.05, "**", ifelse(table1[i-2, 5]<0.1, "*", "")))
  rownames(table1)[i-2] <- colnames(data03)[i]
}

data06 <- data02[data02$Group == "IndMill", ]
data07 <- data02[data02$Group == "DepMill", ]
data08 <- data02[data02$Group == "GenXer", ]

# https://stackoverflow.com/questions/39646391/independent-sample-t-test-after-weighting
# https://cran.r-project.org/web/packages/weights/weights.pdf
for (i in 11:33) {
  table1[i-2, 1] <- wtd.t.test(data06[, i], data07[, i], weight=data06$Final_weights, weighty=data07$Final_weights, samedata=FALSE)$coefficients[3]
  table1[i-2, 2] <- ifelse(table1[i-2, 1]<0.01, "***", ifelse(table1[i-2, 1]<0.05, "**", ifelse(table1[i-2, 1]<0.1, "*", "")))
  table1[i-2, 3] <- wtd.t.test(data06[, i], data08[, i], weight=data06$Final_weights, weighty=data08$Final_weights, samedata=FALSE)$coefficients[3]
  table1[i-2, 4] <- ifelse(table1[i-2, 3]<0.01, "***", ifelse(table1[i-2, 3]<0.05, "**", ifelse(table1[i-2, 3]<0.1, "*", "")))
  table1[i-2, 5] <- wtd.t.test(data07[, i], data08[, i], weight=data07$Final_weights, weighty=data08$Final_weights, samedata=FALSE)$coefficients[3]
  table1[i-2, 6] <- ifelse(table1[i-2, 5]<0.01, "***", ifelse(table1[i-2, 5]<0.05, "**", ifelse(table1[i-2, 5]<0.1, "*", "")))
  rownames(table1)[i-2] <- colnames(data03)[i]
}

table1

