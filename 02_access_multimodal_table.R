
library(foreign)

data00 <- read.csv(file="M:/Millennial_CA/02_raw_data/11_latest_update/GenY_Syntax6_Step1_temp.csv")
colnames(data00)

data21 <- data00[, c(1, 181:334, 727:729, 794:796, 799, 116, 674)]
colnames(data21)

data21$PID <- data21$ï..PID
data21$ï..PID <- NULL 

data21$Group <- NA 
data21$Group <- ifelse(data21$Age<=34 & data21$C6_ParentsDV==0, 1, data21$Group) # independent millennials 
data21$Group <- ifelse(data21$Age<=34 & data21$C6_ParentsDV==1, 2, data21$Group) # dependent millennials 
data21$Group <- ifelse(data21$Age>=35,                          3, data21$Group) # Gen Xers 
data21$Group <- factor(data21$Group, labels=c("IndMill", "DepMill", "GenXer"), ordered=TRUE)
colnames(data21)

data21$bikescore <- ifelse(is.na(data21$bikescore)==TRUE, 0, data21$bikescore)
data21$transitscore <- ifelse(is.na(data21$transitscore)==TRUE, 0, data21$transitscore)

head(data21)

# the below scripts borrowed from 00_summary.stat.R  

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


# modality styles for commute trips 

data21$commute_car <- modefreq(data21$F6school_Drivealone) + modefreq(data21$F6school_CarpoolD) + modefreq(data21$F6school_CarpoolP) + 
  modefreq(data21$F6school_Moto) + modefreq(data21$F6work_Drivealone) + modefreq(data21$F6work_CarpoolD) + 
  modefreq(data21$F6work_CarpoolP) + modefreq(data21$F6work_Moto) 
data21$commute_transit <- modefreq(data21$F6school_Bus) + modefreq(data21$F6school_LR) + modefreq(data21$F6school_Train) + 
  modefreq(data21$F6work_Bus) + modefreq(data21$F6work_LR) + modefreq(data21$F6work_Train)
data21$commute_active <- modefreq(data21$F6school_Bike) + modefreq(data21$F6school_Skateboard) + modefreq(data21$F6school_Walk) + 
  modefreq(data21$F6work_Bike) + modefreq(data21$F6work_Skateboard) + modefreq(data21$F6work_Walk)
data21$commute_other <-  modefreq(data21$F6school_Uber) + modefreq(data21$F6school_Shuttle) + modefreq(data21$F6school_Taxi) + 
  modefreq(data21$F6school_Other) + modefreq(data21$F6work_Uber) + modefreq(data21$F6work_Shuttle) +  
  modefreq(data21$F6work_Taxi) + modefreq(data21$F6work_Other)

data21$commute_total <- data21$commute_car + data21$commute_transit + data21$commute_active + data21$commute_other
data21$pct_commute_car <- ifelse(data21$commute_total>0, data21$commute_car/data21$commute_total*100, 0)
data21$pct_commute_transit <- ifelse(data21$commute_total>0, data21$commute_transit/data21$commute_total*100, 0)
data21$pct_commute_active <- ifelse(data21$commute_total>0, data21$commute_active/data21$commute_total*100, 0)
data21$pct_commute_other <- ifelse(data21$commute_total>0, data21$commute_other/data21$commute_total*100, 0)

#data21$entropy_commutes <- ifelse(data21$commute_total>0, -data21$pct_commute_car/100*log(data21$pct_commute_car/100)
#                                  -data21$pct_commute_transit/100*log(data21$pct_commute_transit/100)
#                                  -data21$pct_commute_active/100*log(data21$pct_commute_active/100)
#                                  -data21$pct_commute_other/100*log(data21$pct_commute_other/100), NA) 
#hist(data21$entropy_commutes)

quantile(data21$pct_commute_car, probs=c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))
quantile(data21$pct_commute_transit, probs=c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))
quantile(data21$pct_commute_active, probs=c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))
quantile(data21$pct_commute_other, probs=c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))

hist(data21$pct_commute_car, freq=FALSE, breaks=seq(0, 100, by=1.25))
hist(data21$pct_commute_transit, freq=FALSE, breaks=seq(0, 100, by=1.25))
hist(data21$pct_commute_active, freq=FALSE, breaks=seq(0, 100, by=1.25))
hist(data21$pct_commute_other, freq=FALSE, breaks=seq(0, 100, by=1.25))

data21$modality_commutes <- NA 
data21$modality_commutes <- ifelse(data21$commute_total>0, "Other commuter", data21$modality_commutes) 
data21$modality_commutes <- ifelse(data21$pct_commute_car==100, "Only car commuter", data21$modality_commutes) 
data21$modality_commutes <- ifelse(data21$pct_commute_car>=75 & data21$pct_commute_car<100, "Mostly car commuter", data21$modality_commutes) 
data21$modality_commutes <- ifelse(data21$pct_commute_transit==100, "Only transit commuter", data21$modality_commutes) 
data21$modality_commutes <- ifelse(data21$pct_commute_transit>=50 & data21$pct_commute_transit<100, "Mostly transit commuter", data21$modality_commutes) 
data21$modality_commutes <- ifelse(data21$pct_commute_active==100, "Only active commuter", data21$modality_commutes) 
data21$modality_commutes <- ifelse(data21$pct_commute_active>=50 & data21$pct_commute_active<100, "Mostly active commuter", data21$modality_commutes) 
data21$modality_commutes <- factor(data21$modality_commutes, 
                                   levels=c("Only car commuter", "Mostly car commuter", 
                                            "Only transit commuter", "Mostly transit commuter", 
                                            "Only active commuter", "Mostly active commuter", "Other commuter"))
table(data21$modality_commutes)
sum(is.na(data21$modality_commutes)&data21$commute_total>0)



# modality styles for leisure trips 

data21$leisure_car <- modefreq(data21$F14leisure_Drivealone) + modefreq(data21$F14leisure_CarpoolD) + 
  modefreq(data21$F14leisure_CarpoolP) + modefreq(data21$F14leisure_Moto) + modefreq(data21$F14leisure_Carsharing)
data21$leisure_transit <- modefreq(data21$F14leisure_Bus) + modefreq(data21$F14leisure_LR) + modefreq(data21$F14leisure_Train)
data21$leisure_active <- modefreq(data21$F14leisure_Bike) + modefreq(data21$F14leisure_Skateboard) + modefreq(data21$F14leisure_Walk)
data21$leisure_other <- modefreq(data21$F14leisure_Uber) + modefreq(data21$F14leisure_Taxi) + modefreq(data21$F14leisure_Other) 

data21$leisure_total <- data21$leisure_car + data21$leisure_transit + data21$leisure_active + data21$leisure_other
data21$pct_leisure_car <- ifelse(data21$leisure_total>0, data21$leisure_car/data21$leisure_total*100, 0)
data21$pct_leisure_transit <- ifelse(data21$leisure_total>0, data21$leisure_transit/data21$leisure_total*100, 0)
data21$pct_leisure_active <- ifelse(data21$leisure_total>0, data21$leisure_active/data21$leisure_total*100, 0)
data21$pct_leisure_other <- ifelse(data21$leisure_total>0, data21$leisure_other/data21$leisure_total*100, 0)

quantile(data21$pct_leisure_car, probs=c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))
quantile(data21$pct_leisure_transit, probs=c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))
quantile(data21$pct_leisure_active, probs=c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))
quantile(data21$pct_leisure_other, probs=c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))

hist(data21$pct_leisure_car, freq=FALSE, breaks=seq(0, 100, by=1.25))
hist(data21$pct_leisure_transit, freq=FALSE, breaks=seq(0, 100, by=1.25))
hist(data21$pct_leisure_active, freq=FALSE, breaks=seq(0, 100, by=1.25))
hist(data21$pct_leisure_other, freq=FALSE, breaks=seq(0, 100, by=1.25))

data21$modality_leisure <- NA 
data21$modality_leisure <- ifelse(data21$leisure_total>0, "Other multimodals", data21$modality_leisure) 
data21$modality_leisure <- ifelse(data21$pct_leisure_car==100, "Only car user", data21$modality_leisure) 
data21$modality_leisure <- ifelse(data21$pct_leisure_car>=75 & data21$pct_leisure_car<100, "Mostly car user", data21$modality_leisure) 
data21$modality_leisure <- ifelse(data21$pct_leisure_transit==100, "Only transit rider", data21$modality_leisure) 
data21$modality_leisure <- ifelse(data21$pct_leisure_transit>=50 & data21$pct_leisure_transit<100, "Mostly transit rider", data21$modality_leisure) 
data21$modality_leisure <- ifelse(data21$pct_leisure_active==100, "Only active traveler", data21$modality_leisure) 
data21$modality_leisure <- ifelse(data21$pct_leisure_active>=50 & data21$pct_leisure_active<100, "Mostly active traveler", data21$modality_leisure) 
data21$modality_leisure <- factor(data21$modality_leisure, levels=c("Only car user", "Mostly car user", 
                                                                    "Only transit rider", "Mostly transit rider", 
                                                                    "Only active traveler", "Mostly active traveler", "Other multimodals"))
table(data21$modality_leisure)
sum(data21$leisure_total>0) #1913 
sum(data21$leisure_total==0) #no leisure trips reported 
sum(is.na(data21$modality_leisure)==TRUE) #no leisure trips reported 
sum(is.na(data21$modality_leisure)==TRUE & data21$leisure_total>0)


# mytable <- table(data21$modality_commutes, data21$modality_leisure)
# mytable

# prop.table(mytable)
# prop.table(mytable, 1) # row percentage 
# prop.table(mytable, 2) # column percentage 
# https://www.statmethods.net/stats/frequencies.html

# library(gmodels)
# CrossTable(data21$modality_commutes, data21$modality_leisure)

# mytable.IndMill <- table(data21[data21$Group=="IndMill", ]$modality_commutes, data21[data21$Group=="IndMill", ]$modality_leisure)
# mytable.IndMill

# round(prop.table(mytable.IndMill), 2)
# round(prop.table(mytable.IndMill, 1), 2) # row percentage 
# round(prop.table(mytable.IndMill, 2), 2) # column percentage 

# mytable.DepMill <- table(data21[data21$Group=="DepMill", ]$modality_commutes, data21[data21$Group=="DepMill", ]$modality_leisure)
# mytable.DepMill

# round(prop.table(mytable.DepMill), 2)
# round(prop.table(mytable.DepMill, 1), 2) # row percentage 
# round(prop.table(mytable.DepMill, 2), 2) # column percentage 

# mytable.GenXer <- table(data21[data21$Group=="GenXer", ]$modality_commutes, data21[data21$Group=="GenXer", ]$modality_leisure)
# mytable.GenXer

# round(prop.table(mytable.GenXer), 2)
# round(prop.table(mytable.GenXer, 1), 2) # row percentage 
# round(prop.table(mytable.GenXer, 2), 2) # column percentage 


# modality styles for the last commute trips 

data21$last_car <- ifelse(is.na(data21$F7school_pmode)==FALSE & data21$F7school_pmode>=1 & data21$F7school_pmode<=4, 1, 0)
data21$last_transit <- ifelse(is.na(data21$F7school_pmode)==FALSE & data21$F7school_pmode>=6 & data21$F7school_pmode<=8, 1, 0)
data21$last_active <- ifelse(is.na(data21$F7school_pmode)==FALSE & data21$F7school_pmode>=11 & data21$F7school_pmode<=13, 1, 0) 
data21$last_other <- ifelse(is.na(data21$F7school_pmode)==FALSE & (data21$F7school_pmode==5 | data21$F7school_pmode==9 |
                              data21$F7school_pmode==10 | data21$F7school_pmode==14), 1, 0) 
table(data21$last_car)
table(data21$last_transit)
table(data21$last_active)
table(data21$last_other)

data21$last_car <- ifelse(is.na(data21$F7work_pmode)==FALSE & data21$F7work_pmode>=1 & data21$F7work_pmode<=4, 1, data21$last_car)
data21$last_transit <- ifelse(is.na(data21$F7work_pmode)==FALSE & data21$F7work_pmode>=6 & data21$F7work_pmode<=8, 1, data21$last_transit)
data21$last_active <- ifelse(is.na(data21$F7work_pmode)==FALSE & data21$F7work_pmode>=11 & data21$F7work_pmode<=13, 1, data21$last_active) 
data21$last_other <- ifelse(is.na(data21$F7work_pmode)==FALSE & (data21$F7work_pmode==5 | data21$F7work_pmode==9 |
                              data21$F7work_pmode==10 | data21$F7work_pmode==14), 1, data21$last_other) 
table(data21$last_car+
        data21$last_transit+
        data21$last_active+
        data21$last_other)

data21$last_car2     <- ifelse(data21$F8school_Drivealone==1 | data21$F8school_CarpoolD==1 | data21$F8school_CarpoolP==1 | data21$F8school_Moto==1, 1, 0)
data21$last_car2     <- ifelse(is.na(data21$last_car2)==TRUE, 0, data21$last_car2)
data21$last_transit2 <- ifelse(data21$F8school_Bus==1 | data21$F8school_LR==1 | data21$F8school_Train==1, 1, 0)
data21$last_transit2 <- ifelse(is.na(data21$last_transit2)==TRUE, 0, data21$last_transit2)
data21$last_active2  <- ifelse(data21$F8school_Bike==1 | data21$F8school_Skateboard==1 | data21$F8school_Walk==1, 1, 0) 
data21$last_active2  <- ifelse(is.na(data21$last_active2)==TRUE, 0, data21$last_active2) 
data21$last_other2   <- ifelse(data21$F8school_Shuttle==1 | data21$F8school_Taxi==1 | data21$F8school_Uber==1 | data21$F8school_Other==1, 1, 0) 
data21$last_other2   <- ifelse(is.na(data21$last_other2)==TRUE, 0, data21$last_other2)

table(data21$last_car2)

data21$last_car2     <- ifelse(data21$last_car2==0 & (data21$F8work_Drivealone==1 | data21$F8work_CarpoolD==1 | data21$F8work_CarpoolP==1 | data21$F8work_Moto==1, 1, data21$last_car2)
data21$last_transit2 <- ifelse(data21$F8work_Bus==1 | data21$F8work_LR==1 | data21$F8work_Train==1, 1, data21$last_transit2)
data21$last_active2  <- ifelse(data21$F8work_Bike==1 | data21$F8work_Skateboard==1 | data21$F8work_Walk==1, 1, data21$last_active2) 
data21$last_other2   <- ifelse(data21$F8work_Shuttle==1 | data21$F8work_Taxi==1 | data21$F8work_Uber==1 | data21$F8work_Other==1, 1, data21$last_other2) 

table(data21$last_car2+
        data21$last_transit2+
        data21$last_active2+
        data21$last_other2)
