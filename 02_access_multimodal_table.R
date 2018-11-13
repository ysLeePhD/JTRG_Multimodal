
library(foreign)

data00 <- read.csv(file="M:/Millennial_CA/02_raw_data/11_latest_update/GenY_Syntax6_Step1_temp.csv")
colnames(data00)

data21 <- data00[, c(1, 181:334, 727:729, 794:796, 799, 803, 468, 116, 674)]
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

data21$H11car_VMT <- ifelse(data21$H11car_VMT<0, NA, data21$H11car_VMT)
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

max.value <- 90
min.value <- 10 

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
data21$modality_commutes <- ifelse(data21$pct_commute_car>=max.value, "Monomodal driver", data21$modality_commutes) 
data21$modality_commutes <- ifelse(data21$pct_commute_car<max.value & data21$pct_commute_car>=min.value, 
                                   "Multimodal driver", data21$modality_commutes) 
data21$modality_commutes <- ifelse(data21$pct_commute_car<min.value & data21$pct_commute_car>=0, "Multimodal non-driver", data21$modality_commutes) 
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
data21$modality_leisure <- ifelse(data21$pct_leisure_car>=max.value, "Monomodal driver", data21$modality_leisure) 
data21$modality_leisure <- ifelse(data21$pct_leisure_car<max.value & data21$pct_leisure_car>=min.value, 
                                   "Multimodal driver", data21$modality_leisure) 
data21$modality_leisure <- ifelse(data21$pct_leisure_car<min.value & data21$pct_leisure_car>=0, "Multimodal non-driver", 
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
                     "modality_commutes", "modality_leisure", "modality_last", "H11car_VMT", "Group", "RegionHome", "Final_weights")]


alltr <- read.csv("M:/Millennial_CA/17_JTRG_multimodal/04_Alltransit/TRscore_full.csv")
alltr <- alltr[, c("pid", "TQ1")]
alltr$PID <- alltr$pid
alltr$pid <- NULL
alltr$TQ1 <- as.numeric(as.character(alltr$TQ1))

data22 <- merge(data22, alltr, by.x="PID", by.y="PID")
colnames(data22)

library(tableone)
library(grid) 
library(Matrix)
library(survival)
library(survey)

xvars4 <- c("D1A", "D1B", "D1C", "walkscore", "bikescore", "TQ1", 
            "modality_commutes", "modality_leisure", "modality_last", "H11car_VMT")

myfunction01 <- function(x){
  a <- as.numeric(gregexpr(pattern="\\(", x))
  b <- as.numeric(gregexpr(pattern="\\)", x))
  c <- round(as.numeric(substring(x, a+1, b-1)), 1)
  return(c)
}
myfunction02 <- function(x){
  a <- as.numeric(gregexpr(pattern="\\(", x))
  b <- round(as.numeric(substring(x, 1, a-2)), 1)
  return(b)
}

wt.table4a <- svydesign(ids = ~ 1, data = data22, weights = ~ Final_weights)
wt.table4b <- svyCreateTableOne(vars = xvars4, strata ="Group", data = wt.table4a)
print(wt.table4b, catDigits=3, contDigits=3, test=TRUE, smd = FALSE)
#write.csv(print(wt.table4b, catDigits=3, contDigits=3, test=TRUE, smd = FALSE), 
#          file="M:/Millennial_CA/17_JTRG_multimodal/JTRG_Multimodal/table4.csv")

table4c <- print(wt.table4b, catDigits=3, contDigits=3, test=TRUE, smd = FALSE)[c(9:11, 13:15, 17:19), 1:3]
table4c[, 1:3] <- myfunction01(table4c[, 1:3])
table4c <- as.data.frame(table4c, stringsAsFactors=FALSE)
table4c

wt.table5a <- svydesign(ids = ~ 1, data = data22, weights = ~ Final_weights)
wt.table5b <- svyCreateTableOne(vars = xvars4, strata ="RegionHome", data = wt.table5a)
print(wt.table5b, catDigits=3, contDigits=3, test=TRUE, smd = FALSE) 
#write.csv(print(wt.table5b, catDigits=3, contDigits=3, test=TRUE, smd = FALSE), 
#          file="M:/Millennial_CA/17_JTRG_multimodal/JTRG_Multimodal/table5.csv")

table5c <- print(wt.table5b, catDigits=3, contDigits=3, test=TRUE, smd = FALSE)[c(9:11, 13:15, 17:19), 1:6]
table5c[, 1:6] <- myfunction01(table5c[, 1:6])
table5c <- as.data.frame(table5c, stringsAsFactors=FALSE)
table5c 

temp01 <- cbind(table4c, table5c)
rownames(temp01) <- c("comm_monodr", "comm_multidr", "comm_multi", "leis_monodr", "leis_multidr", "leis_multi", "last_car", "last_noncar", "last_muulti" )
write.csv(as.data.frame(t(temp01), stringsAsFactors=FALSE), 
          file="M:/Millennial_CA/17_JTRG_multimodal/JTRG_Multimodal/table5.csv")

# Figure 3. Accessibility/multimodality for two regions, MTC and SCAG. 

table(data22$RegionHome)

myfunction01 <- function(x){
  a <- as.numeric(gregexpr(pattern="\\(", x))
  b <- as.numeric(gregexpr(pattern="\\)", x))
  c <- round(as.numeric(substring(x, a+1, b-1)), 1)
  return(c)
}
myfunction02 <- function(x){
  a <- as.numeric(gregexpr(pattern="\\(", x))
  b <- round(as.numeric(substring(x, 1, a-2)), 1)
  return(b)
}

rm(fig4, temp01, temp02, temp03, temp04, temp05, temp06)
fig4 <- data.frame(RegionHome=character(), Group=character(), 
                   walkscore=double(), multimodal=double(), VMD=double(), stringsAsFactors=FALSE)

data22sf <- data22[data22$RegionHome=="MTC",]
wt.figure3sf1 <- svydesign(ids = ~ 1, data = data22sf, weights = ~ Final_weights)
wt.figure3sf2 <- svyCreateTableOne(vars = xvars4, strata ="Group", data = wt.figure3sf1)
print(wt.figure3sf2, catDigits=3, contDigits=3, test=TRUE, smd = FALSE)
write.csv(print(wt.figure3sf2, catDigits=3, contDigits=3, test=TRUE, smd = FALSE), 
          file="M:/Millennial_CA/17_JTRG_multimodal/JTRG_Multimodal/figure3sf2.csv")

temp00 <- print(wt.figure3sf2, catDigits=3, contDigits=3, test=TRUE, smd = FALSE)[c(9:11, 13:15, 17:19), ]
fig3a <- data.frame(comm_monodr=double(), comm_multidr=double(), comm_multi=double(), 
                    leis_monodr=double(), leis_multidr=double(), leis_multi=double(), 
                    last_car=double(),    last_noncar=double(),  last_muulti=double(), 
                    stringsAsFactors=FALSE)
fig3a[1, 1:9] <- myfunction01(temp00[1:9, 1])  
fig3a[2, 1:9] <- myfunction01(temp00[1:9, 2])  
fig3a[3, 1:9] <- myfunction01(temp00[1:9, 3])  
rownames(fig3a) <- c("IndMill", "DepMill", "GenXer")
write.csv(fig3a, file="M:/Millennial_CA/17_JTRG_multimodal/JTRG_Multimodal/figure3a.csv")

temp01 <- print(wt.figure3sf2, catDigits=3, contDigits=3, test=TRUE, smd = FALSE)[c(5, 19, 20), ]
fig4[1:3, 1] <- "MTC"
fig4[1:3, 2] <- c("IndMill", "DepMill", "GenXer")
fig4[1:3, 3] <- myfunction02(temp01[1, 1:3])  
fig4[1:3, 4] <- myfunction01(temp01[2, 1:3])  
fig4[1:3, 5] <- myfunction02(temp01[3, 1:3])  

data22la <- data22[data22$RegionHome=="SCAG",]
wt.figure3la1 <- svydesign(ids = ~ 1, data = data22la, weights = ~ Final_weights)
wt.figure3la2 <- svyCreateTableOne(vars = xvars4, strata ="Group", data = wt.figure3la1)
print(wt.figure3la2, catDigits=3, contDigits=3, test=TRUE, smd = FALSE)
write.csv(print(wt.figure3la2, catDigits=3, contDigits=3, test=TRUE, smd = FALSE), 
          file="M:/Millennial_CA/17_JTRG_multimodal/JTRG_Multimodal/figure3la2.csv")

temp00 <- print(wt.figure3la2, catDigits=3, contDigits=3, test=TRUE, smd = FALSE)[c(9:11, 13:15, 17:19), ]
fig3b <- data.frame(comm_monodr=double(), comm_multidr=double(), comm_multi=double(), 
                    leis_monodr=double(), leis_multidr=double(), leis_multi=double(), 
                    last_car=double(),    last_noncar=double(),  last_muulti=double(), 
                    stringsAsFactors=FALSE)
fig3b[1, 1:9] <- myfunction01(temp00[1:9, 1])  
fig3b[2, 1:9] <- myfunction01(temp00[1:9, 2])  
fig3b[3, 1:9] <- myfunction01(temp00[1:9, 3])  
rownames(fig3b) <- c("IndMill", "DepMill", "GenXer")
write.csv(fig3b, file="M:/Millennial_CA/17_JTRG_multimodal/JTRG_Multimodal/figure3b.csv")

temp02 <- print(wt.figure3la2, catDigits=3, contDigits=3, test=TRUE, smd = FALSE)[c(5, 19, 20), ]
fig4[4:6, 1] <- "SCAG"
fig4[4:6, 2] <- c("IndMill", "DepMill", "GenXer")
fig4[4:6, 3] <- myfunction02(temp02[1, 1:3])  
fig4[4:6, 4] <- myfunction01(temp02[2, 1:3])  
fig4[4:6, 5] <- myfunction02(temp02[3, 1:3])

data22scg <- data22[data22$RegionHome=="SACOG",]
wt.figure3scg1 <- svydesign(ids = ~ 1, data = data22scg, weights = ~ Final_weights)
wt.figure3scg2 <- svyCreateTableOne(vars = xvars4, strata ="Group", data = wt.figure3scg1)
print(wt.figure3scg2, catDigits=3, contDigits=3, test=TRUE, smd = FALSE)
temp03 <- print(wt.figure3scg2, catDigits=3, contDigits=3, test=TRUE, smd = FALSE)[c(5, 19, 20), ]
fig4[7:9, 1] <- "SACOG"
fig4[7:9, 2] <- c("IndMill", "DepMill", "GenXer")
fig4[7:9, 3] <- myfunction02(temp03[1, 1:3])  
fig4[7:9, 4] <- myfunction01(temp03[2, 1:3])  
fig4[7:9, 5] <- myfunction02(temp03[3, 1:3])

data22sd <- data22[data22$RegionHome=="SANDAG",]
wt.figure3sd1 <- svydesign(ids = ~ 1, data = data22sd, weights = ~ Final_weights)
wt.figure3sd2 <- svyCreateTableOne(vars = xvars4, strata ="Group", data = wt.figure3sd1)
print(wt.figure3sd2, catDigits=3, contDigits=3, test=TRUE, smd = FALSE)
temp04 <- print(wt.figure3sd2, catDigits=3, contDigits=3, test=TRUE, smd = FALSE)[c(5, 19, 20), ]
fig4[10:12, 1] <- "SANDAG"
fig4[10:12, 2] <- c("IndMill", "DepMill", "GenXer")
fig4[10:12, 3] <- myfunction02(temp04[1, 1:3]) # walkscore 
fig4[10:12, 4] <- myfunction01(temp04[2, 1:3]) # last commute multimodal 
fig4[10:12, 5] <- myfunction02(temp04[3, 1:3])

data22cv <- data22[data22$RegionHome=="Central Valley",]
wt.figure3cv1 <- svydesign(ids = ~ 1, data = data22cv, weights = ~ Final_weights)
wt.figure3cv2 <- svyCreateTableOne(vars = xvars4, strata ="Group", data = wt.figure3cv1)
print(wt.figure3cv2, catDigits=3, contDigits=3, test=TRUE, smd = FALSE)
temp05 <- print(wt.figure3cv2, catDigits=3, contDigits=3, test=TRUE, smd = FALSE)[c(5, 19, 20), ]
fig4[13:15, 1] <- "Central Valley"
fig4[13:15, 2] <- c("IndMill", "DepMill", "GenXer")
fig4[13:15, 3] <- myfunction02(temp05[1, 1:3]) # walkscore 
fig4[13:15, 4] <- myfunction01(temp05[2, 1:3]) # last commute multimodal 
fig4[13:15, 5] <- myfunction02(temp05[3, 1:3])

data22nth <- data22[data22$RegionHome=="NorCal and Others",]
wt.figure3nth1 <- svydesign(ids = ~ 1, data = data22nth, weights = ~ Final_weights)
wt.figure3nth2 <- svyCreateTableOne(vars = xvars4, strata ="Group", data = wt.figure3nth1)
print(wt.figure3nth2, catDigits=3, contDigits=3, test=TRUE, smd = FALSE)
temp06 <- print(wt.figure3nth2, catDigits=3, contDigits=3, test=TRUE, smd = FALSE)[c(5, 19, 20), ]
fig4[16:18, 1] <- "NorCal and Others"
fig4[16:18, 2] <- c("IndMill", "DepMill", "GenXer")
fig4[16:18, 3] <- myfunction02(temp06[1, 1:3]) # walkscore 
fig4[16:18, 4] <- myfunction01(temp06[2, 1:3]) # last commute multimodal 
fig4[16:18, 5] <- myfunction02(temp06[3, 1:3]) 

fig4$RegionHome <- factor(fig4$RegionHome, ordered=TRUE)
fig4$Group <- factor(fig4$Group, levels=c("IndMill", "DepMill", "GenXer"), ordered=TRUE)
#fig4$rowno <- factor(rownames(fig4), ordered=TRUE) # need to have a unique ID column in the factor class
#fig4 <- fig4[, c(6, 1:5)]
fig4$VMDsq <- fig4$VMD*fig4$VMD*fig4$VMD*fig4$VMD*fig4$VMD*fig4$VMD

# http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
#library(tidyr)
#colnames(fig4)
#fig4_long <- tidyr::gather(fig4, Group, value, IndMill, DepMill, GenXer, factor_key=TRUE)
#fig4_long$rowno <- NULL
#fig4_long$Group <- factor(fig4_long$Group, ordered=TRUE)

fig4
fig4[fig4$Group=="DepMill", ]

#https://ggplot2.tidyverse.org/reference/scale_size.html
#http://t-redactyl.io/blog/2016/02/creating-plots-in-r-using-ggplot2-part-6-weighted-scatterplots.html
ggplot(data=fig4, aes(x=fig4$walkscore, y=fig4$multimodal, size=fig4$VMD)) +
  geom_point(aes(colour=fig4$Group), pch=16, alpha=1) + 
  geom_point(shape=21, colour="black", alpha=1) + 
  geom_text(aes(label=fig4$RegionHome), size=2.5) + 
  scale_color_brewer(palette="RdYlGn", direction=-1) + 
  labs(x = "Walkscore", y = "Percent multimodal travelers for the last commute") +
  ylim(0, 25) + 
  xlim(30, 65) + 
  scale_size(range = c(10, 45)) +
  theme_bw() +
  guides(size=F,alpha=F) + 
  theme(legend.position = "bottom", legend.direction = "horizontal", 
        legend.text=element_text(size=10)) + 
  labs(colour = "Demographic groups")

ggsave("Fig4.jpg", device="jpeg", width=9, height=6, units="in", dpi=600)

  #scale_size_identity(aes=(size=fig4$VMDsq))
  #scale_size_manual(values=c(2,3,4))
  #scale_color_manual(values=c("green", "yellow", "red"))





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
data26 <- data26[, c(1:6, 15, 8:14)]
data27 <- data22[data22$Group == "DepMill", ]
data27 <- data27[, c(1:6, 15, 8:14)]
data28 <- data22[data22$Group == "GenXer", ]
data28 <- data28[, c(1:6, 15, 8:14)]

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
