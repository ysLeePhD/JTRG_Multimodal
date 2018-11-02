
# https://stackoverflow.com/questions/34903368/how-to-center-stacked-percent-barchart-labels

# Question 

library('plyr')
library('ggplot2')
library('scales')
set.seed(1992)
n=68

Category <- sample(c("Black", "Red", "Blue", "Cyna", "Purple"), n, replace = TRUE, prob = NULL)
Brand <- sample("Brand", n, replace = TRUE, prob = NULL)
Brand <- paste0(Brand, sample(1:5, n, replace = TRUE, prob = NULL))
USD <- abs(rnorm(n))*100
df <- data.frame(Category, Brand, USD)

# Calculate the percentages
df = ddply(df, .(Brand), transform, percent = USD/sum(USD) * 100)

# Format the labels and calculate their positions
df = ddply(df, .(Brand), transform, pos = (cumsum(USD) - 0.5 * USD))

#create nice labes
df$label = paste0(sprintf("%.0f", df$percent), "%")  

head(df)

ggplot(df, aes(x=reorder(Brand, USD,
                         function(x) + sum(x)),  y=percent, fill=Category))+
  geom_bar(position = "fill", stat='identity',  width = .7)+
  #geom_text(aes(label=label, ymax=100, ymin=0), vjust=0, hjust=0,color = "white",  position=position_fill())+
  coord_flip()+
  scale_y_continuous(labels = percent_format())+
  ylab("")+
  xlab("")

# Answer 

library(dplyr)

# Initial data frame   
df <- data.frame(Category, Brand, USD)

# Calculate percentages and label positions
df.summary = df %>% group_by(Brand, Category) %>% 
  summarise(USD = sum(USD)) %>%   # Within each Brand, sum all values in each Category
  mutate(percent = USD/sum(USD),
         pos = cumsum(percent) - 0.5*percent)

ggplot(df.summary, aes(x=reorder(Brand,USD,function(x)+sum(x)), y=percent, fill=Category)) +
  geom_bar(stat='identity',  width = .7, colour="black", lwd=0.1) +
  geom_text(aes(label=ifelse(percent >= 0.07, paste0(sprintf("%.0f", percent*100),"%"),""),
                y=pos), colour="white") +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  labs(y="", x="")