
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

#for (i in 19:50) {
#  ggplot(data31, aes(x=data31[, i], fill=Group)) +
#    geom_density(alpha=0.5) + 
#    scale_fill_brewer(palette="RdYlBu", direction=-1) + 
#    xlim(c(-4.5, 4.5))+ 
#    ylim(c(0, 0.6))+
#    ylab("")+
#    xlab(colnames(data31)[i])
#  
#  ggsave(factorfilenames[i-18])
#}

# ?density
# https://cran.r-project.org/web/packages/ggplot2/ggplot2.pdf
# http://www.sthda.com/english/wiki/ggplot2-density-plot-quick-start-guide-r-software-and-data-visualization#customized-density-plots

# http://www.sthda.com/english/wiki/one-way-anova-test-in-r

colnames(data31)

xvars2 <- c("Ztraditional_shopper_18F", "Zpro_exercise_18F",                                      
            "Zpro_env_policies_18F", "Zmaterialism_18F",                                       
            "Zpleasent_commute_reversed_18F", "Zclimate_change_concern_18F",                            
            "Zpro_suburban_18F", "Zstablished_in_life_18F",                                
            "Zlong_term_suburbanite_18F", "Zmust_own_car_reversed_18F",                             
            "Ztime_mode_constrained_18F", "Zresponsive_to_env_effects_price_of_travel_reversed_18F",
            "Ztech_embracing_reversed_18F", "Zadv_var_seeker_18F",                                    
            "Zmonochronic_18F", "Zpro_social_18F",                                        
            "Zcar_as_atool_18F", "Zinternet_smarthphone_lover_18F")

wt.table2a <- svydesign(ids = ~ 1, data = data31, weights = ~ Final_weights)
wt.table2b <- svyCreateTableOne(vars = xvars2, strata ="Group", data = wt.table2a)
write.csv(print(wt.table2b, catDigits=3, contDigits=3, test=TRUE, smd = FALSE), 
          file="M:/Millennial_CA/17_JTRG_multimodal/JTRG_Multimodal/table2a.csv")


table(data31$Group)
table(as.numeric(data31$Group))

data32 <- data31[data31$Group == "IndMill", ]
data33 <- data31[data31$Group == "DepMill", ]
data34 <- data31[data31$Group == "GenXer", ]

rm(table2) 
table2 <- data.frame("IndM-DepM"=double(), "sig1"=as.character(), 
                     "IndM-GenX"=double(), "sig2"=as.character(), 
                     "DepM-GenX"=double(), "sig3"=as.character(), stringsAsFactors = FALSE)
table2
for (i in 19:36) {
  table2[i-18, 1] <- wtd.t.test(data32[, i], data33[, i], weight=data32$Final_weights, weighty=data33$Final_weights, samedata=FALSE)$coefficients[3]
  table2[i-18, 2] <- ifelse(table2[i-18, 1]<0.01, "***", ifelse(table2[i-18, 1]<0.05, "**", ifelse(table2[i-18, 1]<0.1, "*", "")))
  table2[i-18, 3] <- wtd.t.test(data32[, i], data34[, i], weight=data32$Final_weights, weighty=data34$Final_weights, samedata=FALSE)$coefficients[3]
  table2[i-18, 4] <- ifelse(table2[i-18, 3]<0.01, "***", ifelse(table2[i-18, 3]<0.05, "**", ifelse(table2[i-18, 3]<0.1, "*", "")))
  table2[i-18, 5] <- wtd.t.test(data33[, i], data34[, i], weight=data33$Final_weights, weighty=data34$Final_weights, samedata=FALSE)$coefficients[3]
  table2[i-18, 6] <- ifelse(table2[i-18, 5]<0.01, "***", ifelse(table2[i-18, 5]<0.05, "**", ifelse(table2[i-18, 5]<0.1, "*", "")))
  rownames(table2)[i-18] <- colnames(data32)[i]
}

table2
write.csv(table2, file="M:/Millennial_CA/17_JTRG_multimodal/JTRG_Multimodal/table2b.csv")