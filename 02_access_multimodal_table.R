
library(foreign)

data00 <- read.csv(file="M:/Millennial_CA/02_raw_data/11_latest_update/GenY_Syntax6_Step1_temp.csv")
colnames(data00)

data21 <- data00[, c(1, 181:334, 727:729, 794:796, 799, 803, 116, 674)]
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

quantile(data21$pct_commute_car, probs=c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))
quantile(data21$pct_commute_transit, probs=c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))
quantile(data21$pct_commute_active, probs=c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))
quantile(data21$pct_commute_other, probs=c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))

table(data21$RegionHome)
hist(data21[data21$RegionHome=="MTC", ]$pct_commute_car, freq=FALSE, breaks=seq(0, 100, by=1.25))
hist(data21[data21$RegionHome=="SCAG", ]$pct_commute_car, freq=FALSE, breaks=seq(0, 100, by=1.25))
hist(data21[data21$RegionHome=="Central Valley", ]$pct_commute_car, freq=FALSE, breaks=seq(0, 100, by=1.25))
hist(data21[data21$RegionHome=="NorCal and Others", ]$pct_commute_car, freq=FALSE, breaks=seq(0, 100, by=1.25))
hist(data21[data21$RegionHome=="SANDAG", ]$pct_commute_car, freq=FALSE, breaks=seq(0, 100, by=1.25))

hist(data21$pct_commute_transit, freq=FALSE, breaks=seq(0, 100, by=1.25))
hist(data21$pct_commute_active, freq=FALSE, breaks=seq(0, 100, by=1.25))
hist(data21$pct_commute_other, freq=FALSE, breaks=seq(0, 100, by=1.25))

data21$modality_commutes <- NA 
data21$modality_commutes <- ifelse(data21$pct_commute_car>=90, "Monomodal driver", data21$modality_commutes) 
data21$modality_commutes <- ifelse(data21$pct_commute_car<90 & data21$pct_commute_car>=10, 
                                   "Multimodal driver", data21$modality_commutes) 
data21$modality_commutes <- ifelse(data21$pct_commute_car<10 & data21$pct_commute_car>=0, "Multimodal non-driver", data21$modality_commutes) 
data21$modality_commutes <- ifelse(data21$commute_total==0, NA, data21$modality_commutes) 
data21$modality_commutes <- factor(data21$modality_commutes, 
                                   levels=c("Monomodal driver", "Multimodal driver", 
                                            "Multimodal non-driver"), ordered=TRUE)
table(data21$modality_commutes) #sum-1340 (unweighted)



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
data21$modality_leisure <- ifelse(data21$pct_leisure_car>=90, "Monomodal driver", data21$modality_leisure) 
data21$modality_leisure <- ifelse(data21$pct_leisure_car<90 & data21$pct_leisure_car>=10, 
                                   "Multimodal driver", data21$modality_leisure) 
data21$modality_leisure <- ifelse(data21$pct_leisure_car<10 & data21$pct_leisure_car>=0, "Multimodal non-driver", 
                                  data21$modality_leisure) 
data21$modality_leisure <- ifelse(data21$leisure_total==0, NA, data21$modality_leisure) 
data21$modality_leisure <- factor(data21$modality_leisure, 
                                   levels=c("Monomodal driver", "Multimodal driver", 
                                            "Multimodal non-driver"), ordered=TRUE)
table(data21$modality_leisure) #sum-1913 (unweighted)



# modality styles for the last commute trip

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

data21$last_car2a     <- ifelse(data21$F8school_Drivealone==1 | data21$F8school_CarpoolD==1 | data21$F8school_CarpoolP==1 | data21$F8school_Moto==1, 1, 0)
data21$last_car2a     <- ifelse(is.na(data21$last_car2a)==TRUE, 0, data21$last_car2a)
data21$last_transit2a <- ifelse(data21$F8school_Bus==1 | data21$F8school_LR==1 | data21$F8school_Train==1, 1, 0)
data21$last_transit2a <- ifelse(is.na(data21$last_transit2a)==TRUE, 0, data21$last_transit2a)
data21$last_active2a  <- ifelse(data21$F8school_Bike==1 | data21$F8school_Skateboard==1 | data21$F8school_Walk==1, 1, 0) 
data21$last_active2a  <- ifelse(is.na(data21$last_active2a)==TRUE, 0, data21$last_active2a) 
data21$last_other2a   <- ifelse(data21$F8school_Shuttle==1 | data21$F8school_Taxi==1 | data21$F8school_Uber==1 | data21$F8school_Other==1, 1, 0) 
data21$last_other2a   <- ifelse(is.na(data21$last_other2a)==TRUE, 0, data21$last_other2a)

table(data21$last_car2a)
table(data21$last_transit2a)
table(data21$last_active2a)
table(data21$last_other2a)

data21$last_car2b     <- ifelse(data21$F8work_Drivealone==1 | data21$F8work_CarpoolD==1 | data21$F8work_CarpoolP==1 | data21$F8work_Moto==1, 1, 0)
data21$last_car2b     <- ifelse(is.na(data21$last_car2b)==TRUE, 0, data21$last_car2b)
data21$last_transit2b <- ifelse(data21$F8work_Bus==1 | data21$F8work_LR==1 | data21$F8work_Train==1, 1, 0)
data21$last_transit2b <- ifelse(is.na(data21$last_transit2b)==TRUE, 0, data21$last_transit2b)
data21$last_active2b  <- ifelse(data21$F8work_Bike==1 | data21$F8work_Skateboard==1 | data21$F8work_Walk==1, 1, 0) 
data21$last_active2b  <- ifelse(is.na(data21$last_active2b)==TRUE, 0, data21$last_active2b) 
data21$last_other2b   <- ifelse(data21$F8work_Shuttle==1 | data21$F8work_Taxi==1 | data21$F8work_Uber==1 | data21$F8work_Other==1, 1, 0) 
data21$last_other2b   <- ifelse(is.na(data21$last_other2b)==TRUE, 0, data21$last_other2b)

table(data21$last_car2b)
table(data21$last_transit2b)
table(data21$last_active2b)
table(data21$last_other2b)

data21$last_car2     <- ifelse(data21$last_car2a==1 | data21$last_car2b==1, 1, 0)
data21$last_transit2 <- ifelse(data21$last_transit2a==1 | data21$last_transit2b==1, 1, 0)
data21$last_active2  <- ifelse(data21$last_active2a==1 | data21$last_active2b==1, 1, 0)
data21$last_other2   <- ifelse(data21$last_other2a==1 | data21$last_other2b==1, 1, 0)

table(data21$last_car2)
table(data21$last_transit2)
table(data21$last_active2)
table(data21$last_other2)

data21$lc.mono_car    <- ifelse(data21$last_car==1 & data21$last_transit2==0 & data21$last_active2==0 & data21$last_other2==0, 1, 0) 
data21$lc.mono_noncar <- ifelse((data21$last_transit==1 | data21$last_active==1 | data21$last_other==1) & data21$last_car2==0, 1, 0)
data21$lc.multi       <- ifelse(data21$last_car+data21$last_transit+data21$last_active+data21$last_other>0 & data21$lc.mono_car==0 & 
                                  data21$lc.mono_noncar==0, 1, 0)
data21$modality_last <- ifelse(data21$lc.mono_car==1, 1, 0)
data21$modality_last <- ifelse(data21$lc.mono_noncar==1, 2, data21$modality_last)
data21$modality_last <- ifelse(data21$lc.multi==1, 3, data21$modality_last)
data21$modality_last <- ifelse(data21$modality_last==0, NA, data21$modality_last)
data21$modality_last <- as.factor(data21$modality_last)
levels(data21$modality_last) <- c("Mono car", "Mono non-car", "Multimodal") 
table(data21$modality_last)

data22 <- data21[, c("PID", "D1A", "D1B", "D1C", "walkscore", "bikescore", "transitscore", 
                     "modality_commutes", "modality_leisure", "modality_last", "Group", "RegionHome", "Final_weights")]
colnames(data22)


library(tableone)
library(grid) 
library(Matrix)
library(survival)
library(survey)

xvars4 <- c("D1A", "D1B", "D1C", "walkscore", "bikescore", "transitscore", 
            "modality_commutes", "modality_leisure", "modality_last")

wt.table4a <- svydesign(ids = ~ 1, data = data22, weights = ~ Final_weights)
wt.table4b <- svyCreateTableOne(vars = xvars4, strata ="Group", data = wt.table4a)
write.csv(print(wt.table4b, catDigits=3, contDigits=3, test=TRUE, smd = FALSE), 
          file="M:/Millennial_CA/17_JTRG_multimodal/JTRG_Multimodal/table4.csv")

wt.table4c <- svyCreateTableOne(vars = xvars4, strata ="RegionHome", data = wt.table4a)
write.csv(print(wt.table4c, catDigits=3, contDigits=3, test=TRUE, smd = FALSE), 
          file="M:/Millennial_CA/17_JTRG_multimodal/JTRG_Multimodal/table5.csv")

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

rm(table4)
table4 <- data.frame("IndM-DepM"=as.numeric(), "sig1"=as.character(), 
                     "IndM-GenX"=as.numeric(), "sig2"=as.character(), 
                     "DepM-GenX"=as.numeric(), "sig3"=as.character(), stringsAsFactors = FALSE)

data26 <- data22[data22$Group == "IndMill", ]
data27 <- data22[data22$Group == "DepMill", ]
data28 <- data22[data22$Group == "GenXer", ]

for (i in 2:7) {
  table4[i-1, 1] <- wtd.t.test(data26[, i], data27[, i], weight=data26$Final_weights, weighty=data27$Final_weights, samedata=FALSE)$coefficients[3]
  table4[i-1, 2] <- ifelse(table4[i-1, 1]<0.01, "***", ifelse(table4[i-1, 1]<0.05, "**", ifelse(table4[i-1, 1]<0.1, "*", "")))
  table4[i-1, 3] <- wtd.t.test(data26[, i], data28[, i], weight=data26$Final_weights, weighty=data28$Final_weights, samedata=FALSE)$coefficients[3]
  table4[i-1, 4] <- ifelse(table4[i-1, 3]<0.01, "***", ifelse(table4[i-1, 3]<0.05, "**", ifelse(table4[i-1, 3]<0.1, "*", "")))
  table4[i-1, 5] <- wtd.t.test(data27[, i], data28[, i], weight=data27$Final_weights, weighty=data28$Final_weights, samedata=FALSE)$coefficients[3]
  table4[i-1, 6] <- ifelse(table4[i-1, 5]<0.01, "***", ifelse(table4[i-1, 5]<0.05, "**", ifelse(table4[i-1, 5]<0.1, "*", "")))
  rownames(table4)[i-1] <- colnames(data26)[i]
}

data23 <- data22[data22$Group != "GenXer", ]  # between IndMill vs. DepMill 
data24 <- data22[data22$Group != "DepMill", ] # between IndMill vs. GenXer 
data25 <- data22[data22$Group != "IndMill", ] # between DepMill vs. GenXer

for (i in 8:10) {
  table4[i-1, 1] <- wtd.chi.sq(data23$Group, data23[, i], weight=data23$Final_weights, mean1=TRUE)[3] 
  table4[i-1, 2] <- ifelse(table4[i-1, 1]<0.01, "***", ifelse(table4[i-1, 1]<0.05, "**", ifelse(table4[i-1, 1]<0.1, "*", "")))
  table4[i-1, 3] <- wtd.chi.sq(data24$Group, data24[, i], weight=data24$Final_weights, mean1=TRUE)[3]
  table4[i-1, 4] <- ifelse(table4[i-1, 3]<0.01, "***", ifelse(table4[i-1, 3]<0.05, "**", ifelse(table4[i-1, 3]<0.1, "*", "")))
  table4[i-1, 5] <- wtd.chi.sq(data25$Group, data25[, i], weight=data25$Final_weights, mean1=TRUE)[3] 
  table4[i-1, 6] <- ifelse(table4[i-1, 5]<0.01, "***", ifelse(table4[i-1, 5]<0.05, "**", ifelse(table4[i-1, 5]<0.1, "*", "")))
  rownames(table4)[i-1] <- colnames(data23)[i]
}

table4
