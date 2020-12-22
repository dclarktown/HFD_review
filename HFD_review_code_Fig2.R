#Danielle Clarkson-Townsend August 2020
#HFD review 
#code for figure 2
setwd("yourworkingdirectory")
load("HFD_reviewdata.RData")
names(HFD_reviewdata)
#note: one study has some NA values for age information
#"disease" refers to model category, "genetic_KO" refers to whether genetic knockouts or knockins were used, 
#"diet_description" refers to approximate % fat kcal in the diet, "IMCD" refers to whether an ingredient-matched control diet was used"
#"varied_ages" refers to whether the study used mice or rats of varied ages
#load libraries
library(gmodels)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(scales)
#making diet category a factor variable
HFD_reviewdata$Diet_cat <- as.factor(HFD_reviewdata$Diet_cat)
# plotting how duration differs by HFD model
p <- ggplot(HFD_reviewdata, aes(x=wk_diff, y=Diet_cat, color=Disease)) + 
  geom_boxplot(outlier.size=3) + scale_color_manual(values=c("#fd8d3c","#d94801", "#7f2704"))+
  geom_point(position=position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), aes(fill=Disease), size=3) +
  coord_flip() 
p
p <- p + ylab('HFD % Fat kCal') +
  xlab('Weeks on Diet') +
  theme(text = element_text(size = 18)) 
p

#geom_jitter(shape=16, size=3, position=position_jitter(0.5))
#plotting how %HFD differs by disease model
m <- ggplot(HFD_reviewdata, aes(x=Diet_description, y=Disease, color=Disease)) + 
  geom_boxplot(outlier.size=3) + scale_color_manual(values=c("#fd8d3c","#d94801", "#7f2704"))+
  geom_point(position=position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9),aes(color=Disease), size=3) +
  coord_flip() 
m
m <- m + ylab('Disease Model') +
  xlab('HFD % Fat kCal') +
  theme(text = element_text(size = 18))
m
#plotting how age start differs by disease model
HFD_reviewdata$age_start_wks
r <- ggplot(HFD_reviewdata, aes(x=age_start_wks, y=Disease, color=Disease)) + 
  geom_boxplot(outlier.size=3) +scale_color_manual(values=c("#fd8d3c","#d94801", "#7f2704")) +
  geom_point(position=position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9),aes(fill=Disease), size=3) +
  coord_flip() 
r
r <- r + ylab('Disease Model') +
  xlab('Age Diet Treatment Started') +
  theme(text = element_text(size = 18))
r
#timeline of start and end dates for HFD by disease group
#first reorder to grouping by disease model and start age:
x3 <- HFD_reviewdata[order(HFD_reviewdata$Disease, HFD_reviewdata$age_start_wks), ]
#now plot timelines
t <-ggplot(transform(x3, y=order(Disease, age_start_wks)),
           aes(x=age_start_wks, xend=age_end_wks, y=y, yend=y, color=Disease)) + scale_color_manual(values=c("#fd8d3c","#d94801", "#7f2704"))+
  geom_segment(size=3) + 
  scale_y_discrete(breaks=NULL) 
t
t <- t + ylab('Disease Model') +
  xlab('Timeline Diet Treatment (Age in Weeks)') +
  theme(text = element_text(size = 18))
t


#plotting frequency of rodent type for HFD
agg <- count(HFD_reviewdata, Rodent)
ecols <- c(Mice="#bf812d",  Rats = "#8c510a", 'Rats and Mice' = "black")
hfd_ord <- mutate(agg, Rodent = reorder(Rodent, -n, sum))
c <- ggplot(hfd_ord) +
  geom_col(aes(x = Rodent, y = n, fill = Rodent)) +
  scale_fill_manual(values = ecols)
c
c <- c + ylab('Frequency (Count)') +
  xlab('Rodent') +
  theme(text = element_text(size = 18))
c
# make column graphing what rodents are used for disease model
agg <- count(HFD_reviewdata, Disease, Rodent)
head(agg)
ecols <- c(Mice="#bf812d",  Rats = "#8c510a", 'Rats and Mice' = "black")
hfd_ord <- mutate(agg,
                  Disease = reorder(Disease, -n, sum),
                  Rodent = reorder(Rodent, -n, sum))
p2 <- ggplot(hfd_ord) +
  geom_col(aes(x = Disease, y = n, fill = Rodent)) +
  scale_fill_manual(values = ecols)
p2
p2 <- p2 + ylab('Frequency (Count)') +
  xlab('Disease Model') +
  theme(text = element_text(size = 18))
p2
#make column graphing sex and disease model
agg <- count(HFD_reviewdata, Disease, Sex)
head(agg)
ecols <- c(Females="#08519c",  Males = "#4292c6", Both = "black", 'Not specified' = "grey")
hfd_ord <- mutate(agg,
                  Disease = reorder(Disease, -n, sum),
                  Sex = reorder(Sex, -n, sum))
p1 <- ggplot(hfd_ord) +
  geom_col(aes(x = Disease, y = n, fill = Sex)) +
  scale_fill_manual(values = ecols)
p1
p1 <- p1 + ylab('Frequency (Count)') +
  xlab('Disease Model') +
  theme(text = element_text(size = 18))
p1

#make column graphing genetic KO and disease model
a2 <- count(HFD_reviewdata, Disease, Genetic_KO)
head(a2)
ecols <- c(Yes="#d94801",  No = "grey76")
ord2 <- mutate(a2,
               Disease = reorder(Disease, -n, sum),
               Genetic_KO = reorder(Genetic_KO, -n, sum))
p1 <- ggplot(ord2) +
  geom_col(aes(x = Disease, y = n, fill = Genetic_KO)) +
  scale_fill_manual(values = ecols)
p1
p1 <- p1 + ylab('Frequency (Count)') +
  xlab('Disease Model') +
  theme(text = element_text(size = 18))
p1

##################
#pie chart
df <- count(HFD_reviewdata, IMCD)
bp<- ggplot(df, aes(x="", y=n, fill=IMCD))+
  geom_bar(width = 1, stat = "identity")
bp
pie <- bp + coord_polar("y", start=0)
pie
pie + scale_fill_manual(values=c("grey76", "#4292c6"))
#make blank theme to get rid of degrees around pie chart
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )
# Apply blank theme
pie + scale_fill_manual(values=c("grey76", "#4292c6")) + blank_theme +
  theme(axis.text.x=element_blank()) +
  theme(text = element_text(size = 18)) +
  geom_text(aes(y = n/2, label = percent(n/66)), size=8)

#N's on the figure were done by hand
