#########################################
# Covid descriptive analysis         ####
# Maria Fernanda Ibarra Gutierrez    ###
# DMEG Group                         ###
# 2020                               ###
########################################

#.libPaths(c("C:/Program Files/R/R-3.6.1/library"))

rm(list=ls())
setwd("~")

#install.packages(c("tidyverse")) 
require(tidyverse)

## Directory ###
input = "\\Users\\ferna\\OneDrive - The University of Manchester\\Data Analytics and Society\\Seminar DMEG\\3 Covid Analysis\\input"
grap = "\\Users\\ferna\\OneDrive - The University of Manchester\\Data Analytics and Society\\Seminar DMEG\\3 Covid Analysis\\graphs"
output = "\\Users\\ferna\\OneDrive - The University of Manchester\\Data Analytics and Society\\Seminar DMEG\\3 Covid Analysis\\output"

#Covid database
covid = read.csv(paste(input, "full_data.csv", sep="\\"), as.is = T, stringsAsFactors = F)

#Stock market indixes databases 
dax = read.csv(paste(input, "dax.csv", sep="\\"), as.is = T, stringsAsFactors = F)
dow = read.csv(paste(input, "dow.csv", sep="\\"), as.is = T, stringsAsFactors = F)
ftse = read.csv(paste(input, "ftse.csv", sep="\\"), as.is = T, stringsAsFactors = F)
ipc = read.csv(paste(input, "ipc.csv", sep="\\"), as.is = T, stringsAsFactors = F)
nasdaq = read.csv(paste(input, "nasdaq.csv", sep="\\"), as.is = T, stringsAsFactors = F)
nikkei = read.csv(paste(input, "nikkei.csv", sep="\\"), as.is = T, stringsAsFactors = F)
shanghai = read.csv(paste(input, "shanghai.csv", sep="\\"), as.is = T, stringsAsFactors = F)
sp = read.csv(paste(input, "sp.csv", sep="\\"), as.is = T, stringsAsFactors = F)
topix = read.csv(paste(input, "topix.csv", sep="\\"), as.is = T, stringsAsFactors = F)
smi = read.csv(paste(input, "smi.csv", sep="\\"), as.is = T, stringsAsFactors = F)

#Foreign exchange databases
usdeur = read.csv(paste(input, "usdeur.csv", sep="\\"), as.is = T, stringsAsFactors = F)
usdgbp = read.csv(paste(input, "usdgbp.csv", sep="\\"), as.is = T, stringsAsFactors = F)
usdjpy = read.csv(paste(input, "usdjpy.csv", sep="\\"), as.is = T, stringsAsFactors = F)
usdmxn = read.csv(paste(input, "usdmxn.csv", sep="\\"), as.is = T, stringsAsFactors = F)
usdcny = read.csv(paste(input, "usdcny.csv", sep="\\"), as.is = T, stringsAsFactors = F)
usdchf = read.csv(paste(input, "usdchf.csv", sep="\\"), as.is = T, stringsAsFactors = F)

#Foreign exchange databases (1 day)
usdeur1 = read.csv(paste(input, "usdeur1.csv", sep="\\"), as.is = T, stringsAsFactors = F)
usdgbp1 = read.csv(paste(input, "usdgbp1.csv", sep="\\"), as.is = T, stringsAsFactors = F)
usdjpy1 = read.csv(paste(input, "usdjpy1.csv", sep="\\"), as.is = T, stringsAsFactors = F)
usdmxn1 = read.csv(paste(input, "usdmxn1.csv", sep="\\"), as.is = T, stringsAsFactors = F)
usdcny1 = read.csv(paste(input, "usdcny1.csv", sep="\\"), as.is = T, stringsAsFactors = F)
usdchf1 = read.csv(paste(input, "usdchf1.csv", sep="\\"), as.is = T, stringsAsFactors = F)

#Covid
str(covid)
names(covid)
head(covid)
min(covid$date)
max(covid$date)

#Cleaning the date
table(covid$date)
covid$date <- gsub("-","",covid$date)

covid$year = substr(covid$date ,1,4)
table(covid$year)

covid$month = substr(covid$date,5,6)
table(covid$month)

covid$day = substr(covid$date,7,8)
table(covid$day)

covid$date1<- paste(covid$day, covid$month, covid$year, sep = "/")
table(covid$date1)

covid$date1 = as.Date(covid$date1,format="%d/%m/%Y")

head(covid)

tc<-aggregate(covid$new_cases, by=list(covid$location), sum)
mar<-subset(covid, covid$date%in%"20200311")
mar1<-subset(mar, mar$location%in%"Japan" | mar$location%in%"United States" | 
                  mar$location%in%"China" | mar$location%in%"United Kingdom"|
                  mar$location%in%"Mexico" | mar$location%in%"Switzerland" |
                  mar$location%in%"Germany" )


#Indixes####
names(dax) = c("date", "price","open", "high", "low", "vol", "change")
names(dow) = c("date", "price","open", "high", "low", "vol", "change")
names(ftse) = c("date", "price","open", "high", "low", "vol", "change")
names(ipc) = c("date", "price","open", "high", "low", "vol", "change")
names(nasdaq) = c("date", "price","open", "high", "low", "vol", "change")
names(nikkei) = c("date", "price","open", "high", "low", "vol", "change")
names(shanghai) = c("date", "price","open", "high", "low", "vol", "change")
names(sp) = c("date", "price","open", "high", "low", "vol", "change")
names(topix) = c("date", "price","open", "high", "low", "vol", "change")
names(smi) = c("date", "price","open", "high", "low", "vol", "change")

#Type of index variables
dax$ind<- c("Dax")
dow$ind<- c("Dow Jones")
ftse$ind<- c("FTSE")
ipc$ind<- c("IPC")
nasdaq$ind<- c("Nasdaq")
nikkei$ind<- c("Nikkei")
shanghai$ind<- c("Shanghai")
sp$ind<- c("S&P")
topix$ind<- c("Topix")
smi$ind<- c("Smi")

#Join the databases
index<-rbind(dax, dow, ftse, ipc, nasdaq, nikkei, shanghai, sp, topix,smi)

#Cleaning the date
#Eliminate NULL values
index<-subset(index, !index$date%in%"")

table(index$date)
index$date <- gsub(" ","",index$date)
index$date <- gsub(",","",index$date)

index$month = substr(index$date,1,3)
table(index$month)

index$month1 = ifelse(index$month%in%"Dec", 12,
                     ifelse(index$month%in%"Feb", 02,
                            ifelse(index$month%in%"Jan", 01,
                                   ifelse(index$month%in%"Mar", 03,
                                          ifelse(index$month%in%"Apr", 04,"ERROR")))))

table(index$month,index$month1)

index$day = substr(index$date,4,5)
table(index$day)

index$year = substr(index$date ,6,9)
table(index$year)

index$date1<- paste(index$day, index$month1, index$year, sep = "/")
table(index$date1)

index$date1 = as.Date(index$date1,format="%d/%m/%Y")

head(index)

#Cumulative change per index and date
library(dplyr)

sub_ind <- index %>%
  select(ind, date1, change) %>%
  group_by (ind,date1) %>%
  summarise(change=sum(change)) %>%
  mutate(cum_chan=cumsum(change))

sub_ind<- as.data.frame(sub_ind)

#Extract databse with new format on dates
#write.csv(index, paste(output,"index.csv",sep="\\"), row.names = F, fileEncoding = "UTF-8")

rm(dax, dow, ftse, ipc, nasdaq, nikkei, shanghai, sp, topix , smi)

#index = read.csv(paste(output, "index.csv", sep="\\"), as.is = T, stringsAsFactors = F)

str(index)

#Change the type of variables from character to numeric
table(index$price)
head(index$price)
index$price <- gsub(",","",index$price)
index$price= as.numeric(index$price)

table(index$open)
head(index$open)
index$open <- gsub(",","",index$open)
index$open = as.numeric(index$open)

table(index$high)
head(index$high)
index$high <- gsub(",","",index$high)
index$high = as.numeric(index$high)

table(index$low)
head(index$low)
index$low <- gsub(",","",index$low)
index$low = as.numeric(index$low)

#Transform the index units to USD currency
#Lets join an clean the fix exchange value
names(usdeur1) = c("date", "price1","open1", "high1", "low1", "change")
names(usdgbp1) = c("date", "price1","open1", "high1", "low1", "change")
names(usdjpy1) = c("date", "price1","open1", "high1", "low1", "change")
names(usdmxn1) = c("date", "price1","open1", "high1", "low1", "change")
names(usdcny1) = c("date", "price1","open1", "high1", "low1", "change")
names(usdchf1) = c("date", "price1","open1", "high1", "low1", "change")

usdeur1$ind<- c("euro")
usdgbp1$ind<- c("gbp")
usdjpy1$ind<- c("jpy")
usdmxn1$ind<- c("mxn")
usdcny1$ind<- c("cny")
usdchf1$ind<- c("chf")

exch_f = rbind(usdeur1,usdgbp1,usdjpy1,usdmxn1,usdcny1,usdchf1)
rm(usdeur1,usdgbp1,usdjpy1,usdmxn1,usdcny1,usdchf1)

exch_f = subset(exch_f, !exch_f$date%in%"")
head(exch_f)
exch_f = select(exch_f, -open1, -high1, -low1, -change)
exch_f1 = spread(exch_f, ind, price1)
exch_f1$kaux = 1

#Merging database index with database fix foreign change
index$kaux<-1

subindex<-merge(index, exch_f1[c("chf","cny","euro","gbp","jpy","mxn","kaux")], by="kaux", all.x = TRUE)

str(subindex)
subindex$jpy = as.numeric(subindex$jpy)
subindex$cny = as.numeric(subindex$cny)
subindex$euro = as.numeric(subindex$euro)
subindex$gbp = as.numeric(subindex$gbp)
subindex$mxn = as.numeric(subindex$mxn)
subindex$chf = as.numeric(subindex$chf)

subindex$price_us<-ifelse(subindex$ind%in%"Topix" | subindex$ind%in%"Nikkei", subindex$price/subindex$jpy,
                          ifelse(subindex$ind%in%"Shanghai", subindex$price/subindex$cny,
                                 ifelse(subindex$ind%in%"Dax", subindex$price/subindex$euro,
                                        ifelse(subindex$ind%in%"FTSE", subindex$price/subindex$gbp,
                                               ifelse(subindex$ind%in%"IPC", subindex$price/subindex$mxn,
                                                      ifelse(subindex$ind%in%"Smi", subindex$price/subindex$chf,
                                                             subindex$price))))))

#Exchange database
names(usdeur) = c("date", "price","open", "high", "low", "change")
names(usdgbp) = c("date", "price","open", "high", "low", "change")
names(usdjpy) = c("date", "price","open", "high", "low", "change")
names(usdmxn) = c("date", "price","open", "high", "low", "change")
names(usdcny) = c("date", "price","open", "high", "low", "change")
names(usdchf) = c("date", "price","open", "high", "low", "change")

usdeur$ind<- c("euro")
usdgbp$ind<- c("gbp")
usdjpy$ind<- c("jpy")
usdmxn$ind<- c("mxn")
usdcny$ind<- c("cny")
usdchf$ind<- c("chf")

exch<-rbind(usdeur , usdgbp ,usdjpy ,usdmxn ,usdcny ,usdchf)
rm(usdeur , usdgbp ,usdjpy ,usdmxn ,usdcny ,usdchf)

str(exch)

#Cleaning the date
#Eliminate NULL values
exch<-subset(exch, !exch$date%in%"")

table(exch$date)
exch$date <- gsub(" ","",exch$date)
exch$date <- gsub(",","",exch$date)

exch$month = substr(exch$date,1,3)
table(exch$month)

exch$month1 = ifelse(exch$month%in%"Dec", 12,
                      ifelse(exch$month%in%"Feb", 02,
                             ifelse(exch$month%in%"Jan", 01,
                                    ifelse(exch$month%in%"Mar", 03,
                                           ifelse(exch$month%in%"Apr", 04,"ERROR")))))

table(exch$month,exch$month1)

exch$day = substr(exch$date,4,5)
table(exch$day)

exch$year = substr(exch$date ,6,9)
table(exch$year)

exch$date1<- paste(exch$day, exch$month1, exch$year, sep = "/")
table(exch$date1)

exch$date1 = as.Date(exch$date1,format="%d/%m/%Y")

head(exch)

#Change the type of variables from character to numeric
str(exch)

table(exch$price)
head(exch$price)
exch$price= as.numeric(exch$price)

table(exch$open)
head(exch$open)
exch$open = as.numeric(exch$open)

table(exch$high)
head(exch$high)
exch$high = as.numeric(exch$high)

table(exch$low)
head(exch$low)
exch$low = as.numeric(exch$low)

table(exch$change)
head(exch$change)
exch$change = as.numeric(exch$change)

str(exch)

#Plots
library("RColorBrewer")
display.brewer.all()

#Covid
require(ggrepel)

s1<- subset(covid, !covid$location%in%"World" & !covid$location%in%"International")
summary(s1)

g1<-ggplot(data=s1) +
  geom_line(aes(x=date1, y=total_cases, group=location, colour=location), size=0.9) +
  labs(title ="Covid-19", 
       subtitle="Total cases by location", x = "Date", 
       y = "Number of cases", caption = "Source: Our world in Data. April 22, 2020 (15:00, London time)")+
  geom_point(data=filter(s1, date1 == max(date1) & total_cases > 70000 ),
             aes(x = date1 , y = total_cases, label=location, color=location, fill=location), shape=19, size=2) + 
  geom_text_repel(data=filter(s1, date1 == max(date1) & total_cases > 70000 ),
                  aes(x = date1 , y = total_cases, label=location), size=5, fontface=2, color="black", nudge_x = 1) + 
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank()) +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16, face = "bold"),
        axis.text.x=element_text(colour="black", size = 12),
        axis.text.y=element_text(colour="black", size = 12),
        axis.title.x = element_text(color="black", size = 12, vjust=-0.35),
        axis.title.y = element_text(color="black", size = 12 , vjust=0.35),legend.position = "none")+
  scale_y_continuous(breaks=seq(from=0, to=830000, by=25000))
 
#ggsave(paste(grap, "covid1.png", sep="\\"), width=12, height=12)
#ggsave(paste(grap, "covid1p.png", sep="\\"), g1, width=8, height=8)

s2<- subset(covid, covid$location%in%"World" )
summary(s2)

g2<-ggplot(data=s2) +
  geom_line(aes(x=date1, y=total_cases, group=location), size=1, colour="#D55E00") +
  geom_text_repel(data=filter(s2, date1 == max(date1) & total_cases == max(total_cases)),
                  aes(x = date1 , y = total_cases, label=total_cases), size=5, fontface=2, color="black")+
  labs(title ="Covid-19", 
       subtitle="Total cases in the World", x = "Date", 
       y = "Number of cases", caption = "Source: Our world in data. April 22, 2020 (15:00, London time)") +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank()) +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16, face = "bold"),
        axis.text.x=element_text(colour="black", size = 12),
        axis.text.y=element_text(colour="black", size = 12),
        axis.title.x = element_text(color="black", size = 12, vjust=-0.35),
        axis.title.y = element_text(color="black", size = 12 , vjust=0.35))+
  scale_y_continuous(breaks=seq(from=0, to=2600000, by=100000)) 

#ggsave(paste(grap, "covid2.png", sep="\\"), width=12, height=12)  

require(gridExtra)
p3 <-grid.arrange(g2, g1, nrow = 1)

#ggsave(paste(grap, "fig1.png", sep="\\"), p3, width=20, height=10)  

#Stock Market indixes US DOLLARS
names(subindex)
summary(subindex)

g3<-ggplot(data=subindex) +
  geom_line(aes(x=date1, y=price_us, group=ind, color=ind), size=0.8) +
  scale_colour_manual(values=brewer.pal(n =10, name = "Paired"))+
  labs(title ="Stock Market Indexes", 
       subtitle="Daily prices in US Dollars", x = "Date", 
       y = "US Dollars", caption = "Source: UK Investing. April 22, 2020 (15:00, London time) ", color ="Indexes") +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank()) +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16, face = "bold"),
        axis.text.x=element_text(colour="black", size = 12),
        axis.text.y=element_text(colour="black", size = 12),
        axis.title.x = element_text(color="black", size = 12, vjust=-0.35),
        axis.title.y = element_text(color="black", size = 12 , vjust=0.35))+
  scale_y_continuous(breaks=seq(from=0, to=30000, by=3000)) 

#ggsave(paste(grap, "index_us.png", sep="\\"), width=12, height=12)  

#Stock Market indixes cumulative change
library(scales)
names(sub_ind)
summary(sub_ind)

g4<-ggplot(data=sub_ind) +
  geom_line(aes(x=date1, y=(cum_chan/100), group=ind, color=ind), size=0.8) +
  scale_colour_manual(values=brewer.pal(n =10, name = "Paired"))+
  labs(title ="Stock Market Indexes", 
       subtitle="Percentage of cumulative changes", x = "Date", 
       y = "Percentage", caption = "Source: UK Investing. April 22, 2020 (15:00, London time)", color ="Indexes") +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank()) +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16, face = "bold"),
        axis.text.x=element_text(colour="black", size = 12),
        axis.text.y=element_text(colour="black", size = 12),
        axis.title.x = element_text(color="black", size = 12, vjust=-0.35),
        axis.title.y = element_text(color="black", size = 12 , vjust=0.35))+
  scale_y_continuous(labels = percent)

#ggsave(paste(grap, "index_ch.png", sep="\\"), width=12, height=12)  

#Covid outbreak vs index US DOLLARS
names(covid)

cov_china<-subset(covid, covid$location%in%"China")
cov_china$minaux<-min(cov_china$total_cases)
cov_china$mindate<-min(cov_china$date)
cov_china<-subset(cov_china, cov_china$date==cov_china$mindate)

cov_jap<-subset(covid, covid$location%in%"Japan")
cov_jap<-subset(cov_jap, !cov_jap$total_cases%in%0)
cov_jap$minaux<-min(cov_jap$total_cases)
cov_jap$mindate<-min(cov_jap$date)
cov_jap<-subset(cov_jap, cov_jap$date==cov_jap$mindate)

cov_us<-subset(covid, covid$location%in%"United States")
cov_us<-subset(cov_us, !cov_us$total_cases%in%0)
cov_us$minaux<-min(cov_us$total_cases)
cov_us$mindate<-min(cov_us$date)
cov_us<-subset(cov_us, cov_us$date==cov_us$mindate)

cov_ger<-subset(covid,covid$location%in%"Germany")
cov_ger<-subset(cov_ger, !cov_ger$total_cases%in%0)
cov_ger$minaux<-min(cov_ger$total_cases)
cov_ger$mindate<-min(cov_ger$date)
cov_ger<-subset(cov_ger, cov_ger$date==cov_ger$mindate)

cov_uk<-subset(covid, covid$location%in%"United Kingdom" )
cov_uk<-subset(cov_uk, !cov_uk$total_cases%in%0)
cov_uk$minaux<-min(cov_uk$total_cases)
cov_uk$mindate<-min(cov_uk$date)
cov_uk<-subset(cov_uk, cov_uk$date==cov_uk$mindate)

cov_mxn<-subset(covid, covid$location%in%"Mexico")
cov_mxn<-subset(cov_mxn, !cov_mxn$total_cases%in%0)
cov_mxn$minaux<-min(cov_mxn$total_cases)
cov_mxn$mindate<-min(cov_mxn$date)
cov_mxn<-subset(cov_mxn, cov_mxn$date==cov_mxn$mindate)
cov_mxn$date1<-cov_mxn$date1 - 1

cov_sw<-subset(covid, covid$location%in%"Switzerland")
cov_sw<-subset(cov_sw, !cov_sw$total_cases%in%0)
cov_sw$minaux<-min(cov_sw$total_cases)
cov_sw$mindate<-min(cov_sw$date)
cov_sw<-subset(cov_sw, cov_sw$date==cov_sw$mindate)

outb<-rbind(cov_china,cov_jap,cov_us,cov_ger,cov_uk,cov_mxn,cov_sw)

rm(cov_china,cov_jap,cov_us,cov_ger,cov_uk,cov_mxn,cov_sw)

names(outb)

outb1<-select(outb, -new_cases,-new_deaths,-total_deaths,-minaux,-mindate, -year,-month,-day, -date)
subindex<-select(subindex, -kaux,-date,-price,-open,-high,-low,-vol,-change,-month,-month1,-day,-year,-chf,-cny,-euro,-gbp,
                           -jpy,-mxn)

names(subindex)<-c("ind","date","price_us")
names(outb1)<-c("location","total_cases","date" )

#Database of outbreak VS indexes prices
outb1<-merge(subindex, outb1[c("total_cases","location", "date")], by="date", all.x =  TRUE)

head(outb1)
require(ggrepel)
g5<-ggplot(data=outb1) +
  geom_line(aes(x=date, y=price_us, group=ind, color=ind), size=0.8) +
  
  scale_colour_manual(values=brewer.pal(n =10, name = "Paired"))+
  
  geom_point(data=filter(outb1, (ind%in%"Topix" | ind%in%"Nikkei") & location%in%"Japan" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=23, fill="darkred", color="darkred", size=2) +
  geom_text_repel(data=filter(outb1, (ind%in%"Topix" | ind%in%"Nikkei") & location%in%"Japan" & !total_cases%in%NA),
            aes(x = date , y = price_us, label="Japan outbreak"), size=5, fontface=2, position = position_nudge(y = -0.1)) + 
  
  geom_point(data=filter(outb1, (ind%in%"Shanghai") & location%in%"China" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=23, fill="darkred", color="darkred", size=2) +
  geom_text_repel(data=filter(outb1, (ind%in%"Shanghai") & location%in%"China" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="China outbreak"), size=5, fontface=2, position = position_nudge(y = -0.1)) + 
  
  geom_point(data=filter(outb1, (ind%in%"S&P" | ind%in%"Dow Jones"| ind%in%"Nasdaq") & location%in%"United States" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=23, fill="darkred", color="darkred", size=2) +
  geom_text_repel(data=filter(outb1, (ind%in%"S&P" | ind%in%"Dow Jones"| ind%in%"Nasdaq") & location%in%"United States" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="US outbreak"), size=5, fontface=2, position = position_nudge(y = -0.1)) + 
   
   geom_point(data=filter(outb1, (ind%in%"Dax") & location%in%"Germany" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=23, fill="darkred", color="darkred", size=2) +
   geom_text_repel(data=filter(outb1,  (ind%in%"Dax") & location%in%"Germany" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="Germany outbreak"), size=5, fontface=2, position = position_nudge(y = -0.1)) + 
    
   geom_point(data=filter(outb1, (ind%in%"FTSE") & location%in%"United Kingdom" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=23, fill="darkred", color="darkred", size=2) +
   geom_text_repel(data=filter(outb1,  (ind%in%"FTSE") & location%in%"United Kingdom" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="UK outbreak"), size=5, fontface=2, position = position_nudge(y = -0.1)) + 
   
   geom_point(data=filter(outb1, (ind%in%"IPC") & location%in%"Mexico" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=23, fill="darkred", color="darkred", size=2) +
  geom_text_repel(data=filter(outb1,  (ind%in%"IPC") & location%in%"Mexico" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="Mexico outbreak"), size=5, fontface=2, position = position_nudge(y = -0.1)) + 
    
  geom_point(data=filter(outb1, (ind%in%"Smi") & location%in%"Switzerland" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=23, fill="darkred", color="darkred", size=2) +
  geom_text_repel(data=filter(outb1, (ind%in%"Smi") & location%in%"Switzerland" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="Switzerland outbreak"), size=5, fontface=2, position = position_nudge(y = -0.1)) + 
  labs(title ="Covid-19 outbreak vs Stock Market Indexes", 
      subtitle="Daily comparison ", x = "Date", 
      y = "US Dollars", caption = "Source: UK Investing. April 22, 2020 (15:00, London time)", color ="Indexes") +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank()) +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16, face = "bold"),
        axis.text.x=element_text(colour="black", size = 12),
        axis.text.y=element_text(colour="black", size = 12),
        axis.title.x = element_text(color="black", size = 12),
        axis.title.y = element_text(color="black", size = 12 ))+
  scale_y_continuous(breaks=seq(from=0, to=30000, by=3000)) 

#ggsave(paste(grap, "cov_indus.png", sep="\\"), width=12, height=12)  

#Database of outbreak VS indexes changes
names(sub_ind)<-c("ind","date","change", "cum_chan")

names(outb)

outb2<-select(outb, -new_cases,-new_deaths,-total_deaths,-minaux,-mindate, -year,-month,-day, -date)

names(outb2)<-c("location","total_cases","date" )

outb2<-merge(sub_ind, outb2[c("total_cases","location", "date")], by="date", all.x =  TRUE)

head(outb2)
require(ggrepel)
g6<-ggplot(data=outb2) +
  geom_line(aes(x=date, y=(cum_chan/100), group=ind, color=ind), size=0.8) +
  
  scale_colour_manual(values=brewer.pal(n =10, name = "Paired"))+
  
  geom_point(data=filter(outb2, (ind%in%"Topix" | ind%in%"Nikkei") & location%in%"Japan" & !total_cases%in%NA),
             aes(x=date, y=(cum_chan/100)), shape=23, fill="darkred", color="darkred", size=3) +
  geom_text_repel(data=filter(outb2, (ind%in%"Nikkei" ) & location%in%"Japan" & !total_cases%in%NA),
    aes(x = date , y = (cum_chan/100), label="Japan outbreak"), size=5, fontface=2, hjust = 0.5, vjust = 1,
    nudge_x = 0,nudge_y = -0.2)+
    
  geom_point(data=filter(outb2, (ind%in%"Shanghai") & location%in%"China" & !total_cases%in%NA),
             aes(x=date, y=(cum_chan/100)), shape=23, fill="darkred", color="darkred", size=3) +
  geom_text_repel(data=filter(outb2, (ind%in%"Shanghai") & location%in%"China" & !total_cases%in%NA),
                  aes(x = date , y = (cum_chan/100), label="China outbreak"), size=5, fontface=2, hjust = 0.5, vjust = 1,
                  nudge_x = 0, nudge_y = 0) + 
  
  geom_point(data=filter(outb2, (ind%in%"S&P" | ind%in%"Dow Jones"| ind%in%"Nasdaq") & location%in%"United States" & !total_cases%in%NA),
             aes(x=date, y=(cum_chan/100)), shape=23, fill="darkred", color="darkred", size=3) +
  geom_text_repel(data=filter(outb2, ( ind%in%"Dow Jones") & location%in%"United States" & !total_cases%in%NA),
                  aes(x = date , y = (cum_chan/100), label="US outbreak"), size=5, fontface=2, hjust = 0.5, vjust = 1,
                  nudge_x = 0, nudge_y = 0.1) + 
  
  geom_point(data=filter(outb2, (ind%in%"Dax") & location%in%"Germany" & !total_cases%in%NA),
             aes(x=date, y=(cum_chan/100)), shape=23, fill="darkred", color="darkred", size=3) +
  geom_text_repel(data=filter(outb2,  (ind%in%"Dax") & location%in%"Germany" & !total_cases%in%NA),
                  aes(x = date , y = (cum_chan/100), label="Germany outbreak"), size=5, fontface=2, hjust = 0.5, vjust = 1,
                  nudge_x = 0,nudge_y = -0.2) + 
  
  geom_point(data=filter(outb2, (ind%in%"FTSE") & location%in%"United Kingdom" & !total_cases%in%NA),
             aes(x=date, y=(cum_chan/100)), shape=23, fill="darkred", color="darkred", size=3) +
  geom_text_repel(data=filter(outb2,  (ind%in%"FTSE") & location%in%"United Kingdom" & !total_cases%in%NA),
                  aes(x = date , y = (cum_chan/100), label="UK outbreak"), size=5, fontface=2, hjust = 0.5, vjust = 1,
                  nudge_x = 5,nudge_y = -0.2) + 
  
  geom_point(data=filter(outb2, (ind%in%"IPC") & location%in%"Mexico" & !total_cases%in%NA),
             aes(x=date, y=(cum_chan/100)), shape=23, fill="darkred", color="darkred", size=3) +
  geom_text_repel(data=filter(outb2,  (ind%in%"IPC") & location%in%"Mexico" & !total_cases%in%NA),
                  aes(x = date , y = (cum_chan/100), label="Mexico outbreak"), size=5, fontface=2, hjust = 0.5, vjust = 1,
                  nudge_x = -3,nudge_y = -0.2) + 
  
  geom_point(data=filter(outb2, (ind%in%"Smi") & location%in%"Switzerland" & !total_cases%in%NA),
             aes(x=date, y=(cum_chan/100)), shape=23, fill="darkred", color="darkred", size=3) +
  geom_text_repel(data=filter(outb2, (ind%in%"Smi") & location%in%"Switzerland" & !total_cases%in%NA),
                  aes(x = date , y = (cum_chan/100), label="Switzerland outbreak"), size=5, fontface=2, hjust = 0.5, vjust = 1,
                  nudge_x = 2,nudge_y = .10) + 
  
  labs(title ="Covid-19 outbreak vs Stock Market Indexes", 
       subtitle="Daily comparison ", x = "Date", 
       y = "Percentage", caption = "Source: UK Investing. April 22, 2020 (15:00, London time)", color ="Indexes") +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank()) +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16, face = "bold"),
        axis.text.x=element_text(colour="black", size = 12),
        axis.text.y=element_text(colour="black", size = 12),
        axis.title.x = element_text(color="black", size = 12),
        axis.title.y = element_text(color="black", size = 12 ))+
  scale_y_continuous(labels = percent)

#ggsave(paste(grap, "cov_ind_ch.png", sep="\\"), width=12, height=12)  

#Database of outbreak VS US indexes changes
head(outb2)
require(ggrepel)
g6_1<-ggplot(data=filter(outb2, (ind%in%"S&P" | ind%in%"Dow Jones"| ind%in%"Nasdaq"))) +
  geom_line(aes(x=date, y=(cum_chan/100), group=ind, color=ind), size=0.8) +
  
  scale_colour_manual(values=brewer.pal(n =10, name = "Paired"))+
  
  labs(y = "Percentage", x="Date", caption = "Source: UK Investing. April 22, 2020 (15:00, London time)", color ="Indexes") +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank()) +
  theme(axis.text.x=element_text(colour="black", size = 10),
        axis.text.y=element_text(colour="black", size = 10),
        axis.title.x = element_text(color="black", size = 12, vjust=-0.35),
        axis.title.y = element_text(color="black", size = 12 , vjust=0.35))+
  scale_y_continuous(labels = percent)


#Foreign Exchange 1 USD VS ANOTHER CURRENCY
display.brewer.all()
names(exch)

table(exch$ind)

exch$ind<-ifelse(exch$ind%in%"cny", "1.cny",
                 ifelse(exch$ind%in%"jpy", "2.jpy",
                        ifelse(exch$ind%in%"euro", "3.eur",
                               ifelse(exch$ind%in%"gbp", "4.gbp",
                                      ifelse(exch$ind%in%"chf", "5.chf",
                                             ifelse(exch$ind%in%"mxn", "6.mxn","ERROR"))))))


g7<-ggplot(data=exch) +
  geom_line(aes(x=date1, y=price, group=ind, color=ind), size=0.8) +
  scale_colour_manual(values=brewer.pal(n =6, name = "Dark2"))+
  facet_grid(ind~., scales = "free")+
  labs(title ="Foreign Exchange", 
       subtitle="Daily prices of Currencies vs US Dollar", x = "Date", 
       y = "Currencies units", caption = "Source: UK Investing. April 22, 2020 (15:00, London time)) ", color ="Currencies") +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#CCCCCC"), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank()) +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16, face = "bold"),
        axis.text.x=element_text(colour="black", size = 10),
        axis.text.y=element_text(colour="black", size = 10),
        axis.title.x = element_text(color="black", size = 12, vjust=-0.35),
        axis.title.y = element_text(color="black", size = 12 , vjust=0.35))

#ggsave(paste(grap, "exch.png", sep="\\"), width=12, height=12)  

#Covid outbreak VS Foreign Exchange 1 USD VS ANOTHER CURRENCY
names(covid)

cov_china<-subset(covid, covid$location%in%"China")
cov_china$minaux<-min(cov_china$total_cases)
cov_china$mindate<-min(cov_china$date)
cov_china<-subset(cov_china, cov_china$date==cov_china$mindate)

cov_jap<-subset(covid, covid$location%in%"Japan")
cov_jap<-subset(cov_jap, !cov_jap$total_cases%in%0)
cov_jap$minaux<-min(cov_jap$total_cases)
cov_jap$mindate<-min(cov_jap$date)
cov_jap<-subset(cov_jap, cov_jap$date==cov_jap$mindate)

cov_us<-subset(covid, covid$location%in%"United States")
cov_us<-subset(cov_us, !cov_us$total_cases%in%0)
cov_us$minaux<-min(cov_us$total_cases)
cov_us$mindate<-min(cov_us$date)
cov_us<-subset(cov_us, cov_us$date==cov_us$mindate)

cov_ger<-subset(covid,covid$location%in%"Germany")
cov_ger<-subset(cov_ger, !cov_ger$total_cases%in%0)
cov_ger$minaux<-min(cov_ger$total_cases)
cov_ger$mindate<-min(cov_ger$date)
cov_ger<-subset(cov_ger, cov_ger$date==cov_ger$mindate)

cov_uk<-subset(covid, covid$location%in%"United Kingdom" )
cov_uk<-subset(cov_uk, !cov_uk$total_cases%in%0)
cov_uk$minaux<-min(cov_uk$total_cases)
cov_uk$mindate<-min(cov_uk$date)
cov_uk<-subset(cov_uk, cov_uk$date==cov_uk$mindate)

cov_mxn<-subset(covid, covid$location%in%"Mexico")
cov_mxn<-subset(cov_mxn, !cov_mxn$total_cases%in%0)
cov_mxn$minaux<-min(cov_mxn$total_cases)
cov_mxn$mindate<-min(cov_mxn$date)
cov_mxn<-subset(cov_mxn, cov_mxn$date==cov_mxn$mindate)
cov_mxn$date1<-cov_mxn$date1 - 1

cov_sw<-subset(covid, covid$location%in%"Switzerland")
cov_sw<-subset(cov_sw, !cov_sw$total_cases%in%0)
cov_sw$minaux<-min(cov_sw$total_cases)
cov_sw$mindate<-min(cov_sw$date)
cov_sw<-subset(cov_sw, cov_sw$date==cov_sw$mindate)

outb<-rbind(cov_china,cov_jap,cov_us,cov_ger,cov_uk,cov_mxn,cov_sw)

rm(cov_china,cov_jap,cov_us,cov_ger,cov_uk,cov_mxn,cov_sw)

names(outb)
names(exch)

outb3<-select(outb, -new_cases,-new_deaths,-total_deaths,-minaux,-mindate, -date, -year, -month, -day, -date)
names(outb3)<-c("location","total_cases","date")

sub_exch<-select(exch, -date, -open, -high, -low, -change, -month, -month1, -day, -year)

names(sub_exch)<-c("price_us","ind","date")

outb3<-merge(sub_exch, outb3[c("total_cases","location", "date")], by="date", all.x = TRUE)

head(outb3)

#Graphic of Covid-19 vs Foreign Exchange without US outbreak
require(ggrepel)
g8<-ggplot(data=outb3) +
  geom_line(aes(x=date, y=price_us, group=ind, color=ind), size=0.8) +
  
  scale_colour_manual(values=brewer.pal(n =6, name = "Dark2"))+
  facet_grid(ind~., scales = "free")+
  
  geom_point(data=filter(outb3, (ind%in%"2.jpy") & location%in%"Japan" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=23, fill="darkred", color="darkred", size=2) +
  geom_text_repel(data=filter(outb3, (ind%in%"2.jpy") & location%in%"Japan" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="Japan outbreak"), size=5, fontface=2) + 
  
  geom_point(data=filter(outb3, (ind%in%"1.cny") & location%in%"China" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=23, fill="darkred", color="darkred", size=2) +
  geom_text_repel(data=filter(outb3, (ind%in%"1.cny") & location%in%"China" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="China outbreak"), size=5, fontface=2) +
  
  geom_point(data=filter(outb3, (ind%in%"3.eur") & location%in%"Germany" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=23, fill="darkred", color="darkred", size=2) +
  geom_text_repel(data=filter(outb3,  (ind%in%"3.eur") & location%in%"Germany" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="Germany outbreak"), size=5, fontface=2) + 
  
  geom_point(data=filter(outb3, (ind%in%"4.gbp") & location%in%"United Kingdom" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=23, fill="darkred", color="darkred", size=2) +
  geom_text_repel(data=filter(outb3,  (ind%in%"4.gbp") & location%in%"United Kingdom" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="UK outbreak"), size=5, fontface=2) + 
  
  geom_point(data=filter(outb3, (ind%in%"6.mxn") & location%in%"Mexico" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=23, fill="darkred", color="darkred", size=2) +
  geom_text_repel(data=filter(outb3,  (ind%in%"6.mxn") & location%in%"Mexico" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="Mexico outbreak"), size=5, fontface=2) + 
  
  geom_point(data=filter(outb3, (ind%in%"5.chf") & location%in%"Switzerland" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=23, fill="darkred", color="darkred", size=2) +
  geom_text_repel(data=filter(outb3, (ind%in%"5.chf") & location%in%"Switzerland" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="Switzerland outbreak"), size=5, fontface=2) + 
  
  labs(title ="Covid-19 outbreak vs Foreign Exchanges", 
       subtitle="Daily comparison ", x = "Date", 
       y = "Currencies units", caption = "Source: UK Investing. April 22, 2020 (15:00, London time)", color ="Currencies") +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#CCCCCC"), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank()) +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16, face = "bold"),
        axis.text.x=element_text(colour="black", size = 10),
        axis.text.y=element_text(colour="black", size = 10),
        axis.title.x = element_text(color="black", size = 12, vjust=-0.35),
        axis.title.y = element_text(color="black", size = 12 , vjust=0.35))

#ggsave(paste(grap, "cov_exch.png", sep="\\"), width=12, height=12) 

#Graphic of Covid-19 vs Foreign Exchange with US outbreak

require(ggrepel)
g9<-ggplot(data=outb3) +
  geom_line(aes(x=date, y=price_us, group=ind, color=ind), size=0.8) +
  
  scale_colour_manual(values=brewer.pal(n =6, name = "Dark2"))+
  facet_grid(ind~., scales = "free")+
  
  geom_point(data=filter(outb3, (ind%in%"2.jpy") & location%in%"Japan" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=23, fill="darkred", color="darkred", size=2) +
  geom_text_repel(data=filter(outb3, (ind%in%"2.jpy") & location%in%"Japan" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="Japan outbreak"), size=5, fontface=2, 
                  nudge_x = -5, nudge_y =-2) + 
  
  geom_point(data=filter(outb3, (ind%in%"1.cny") & location%in%"China" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=23, fill="darkred", color="darkred", size=2) +
  geom_text_repel(data=filter(outb3, (ind%in%"1.cny") & location%in%"China" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="China outbreak"), size=5, fontface=2,
                  nudge_x = -5, nudge_y =0) +
  
  geom_point(data=filter(outb3, (ind%in%"3.eur") & location%in%"Germany" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=23, fill="darkred", color="darkred", size=2) +
  geom_text_repel(data=filter(outb3,  (ind%in%"3.eur") & location%in%"Germany" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="Germany outbreak"), size=5, fontface=2,
                  nudge_x = 5, nudge_y =-3) + 
  
  geom_point(data=filter(outb3, (ind%in%"4.gbp") & location%in%"United Kingdom" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=23, fill="darkred", color="darkred", size=2) +
  geom_text_repel(data=filter(outb3,  (ind%in%"4.gbp") & location%in%"United Kingdom" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="UK outbreak"), size=5, fontface=2,
                  nudge_x = 1, nudge_y =0) + 
  
  geom_point(data=filter(outb3, (ind%in%"6.mxn") & location%in%"Mexico" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=23, fill="darkred", color="darkred", size=2) +
  geom_text_repel(data=filter(outb3,  (ind%in%"6.mxn") & location%in%"Mexico" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="Mexico outbreak"), size=5, fontface=2,
                  nudge_x = 5, nudge_y =0) + 
  
  geom_point(data=filter(outb3, (ind%in%"5.chf") & location%in%"Switzerland" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=23, fill="darkred", color="darkred", size=2) +
  geom_text_repel(data=filter(outb3, (ind%in%"5.chf") & location%in%"Switzerland" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="Switzerland outbreak"), size=5, fontface=2,
                  nudge_x = 5, nudge_y =2) + 
  
  geom_point(data=filter(outb3, location%in%"United States" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=19, fill="darkred", color="darkblue", size=2) +
  geom_text_repel(data=filter(outb3, location%in%"United States" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="US outbreak"), size=5, fontface=2, nudge_x = 0,
                  nudge_y =2) +
  
  labs(title ="Covid-19 outbreak vs Foreign Exchanges", 
       subtitle="Daily comparison ", 
       y = "Currencies units", x = NULL, color ="Currencies") +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#CCCCCC"), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank()) +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16, face = "bold"),
        axis.text.x=element_text(colour="black", size = 10),
        axis.text.y=element_text(colour="black", size = 10),
        axis.title.x = element_text(color="black", size = 12, vjust=-0.35),
        axis.title.y = element_text(color="black", size = 12 , vjust=0.35))

#ggsave(paste(grap, "cov_exch1.png", sep="\\"), width=12, height=12)  

#Add a date with the importance among Coronavirus started
#WHO report
#China lockdown:23/01/20
#US lockdown: FROM THE 4 MARCH OFFICAL 16/03/20 
outb4<-select(outb, -date, -new_cases, -new_deaths, -total_deaths, -year, -month, -day, -minaux, -mindate)

outb_aux<-data.frame("world",1,"20/01/2020")
outb_aux1<-data.frame("pandemic",1,"11/03/2020")
outb_aux2<-data.frame("chinalock",1,"23/01/2020")
outb_aux3<-data.frame("uslock",1,"16/03/2020")

names(outb_aux)<-c("location",  "total_cases", "date1")
outb_aux$date1 = as.Date(outb_aux$date1,format="%d/%m/%Y")

names(outb_aux1)<-c("location",  "total_cases", "date1")
outb_aux1$date1 = as.Date(outb_aux1$date1,format="%d/%m/%Y")

names(outb_aux2)<-c("location",  "total_cases", "date1")
outb_aux2$date1 = as.Date(outb_aux2$date1,format="%d/%m/%Y")

names(outb_aux3)<-c("location",  "total_cases", "date1")
outb_aux3$date1 = as.Date(outb_aux3$date1,format="%d/%m/%Y")

outb_aux<-rbind(outb4,outb_aux,outb_aux1,outb_aux2,outb_aux3)
names(outb_aux)<-c("location",  "total_cases", "date")

#Acknowledge DATA
#Database of outbreak acknowledge VS indexes prices
outb4<-merge(subindex, outb_aux[c("total_cases","location", "date")], by="date", all.x =  TRUE)

head(outb4)
require(ggrepel)
g10<-ggplot(data=outb4) +
  geom_line(aes(x=date, y=price_us, group=ind, color=ind), size=0.8) +
  
  scale_colour_manual(values=brewer.pal(n =10, name = "Paired"))+
  
  geom_point(data=filter(outb4, (ind%in%"Topix" | ind%in%"Nikkei") & location%in%"Japan" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=23, fill="darkred", color="darkred", size=2) +
  geom_text_repel(data=filter(outb4, (ind%in%"Topix" | ind%in%"Nikkei") & location%in%"Japan" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="Japan outbreak"), size=5, fontface=2, position = position_nudge(y = -0.1)) +

    geom_point(data=filter(outb4, (ind%in%"Shanghai") & location%in%"China" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=23, fill="darkred", color="darkred", size=2) +
  geom_text_repel(data=filter(outb4, (ind%in%"Shanghai") & location%in%"China" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="China outbreak"), size=5, fontface=2, position = position_nudge(y = -0.1)) + 
  
  geom_point(data=filter(outb4, (ind%in%"S&P" | ind%in%"Dow Jones"| ind%in%"Nasdaq") & location%in%"United States" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=23, fill="darkred", color="darkred", size=2) +
  geom_text_repel(data=filter(outb4, (ind%in%"S&P" | ind%in%"Dow Jones"| ind%in%"Nasdaq") & location%in%"United States" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="US outbreak"), size=5, fontface=2, 
                  nudge_x = 5, nudge_y = -1) + 
  
  geom_point(data=filter(outb4, (ind%in%"Dax") & location%in%"Germany" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=23, fill="darkred", color="darkred", size=2) +
  geom_text_repel(data=filter(outb4,  (ind%in%"Dax") & location%in%"Germany" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="Germany outbreak"), size=5, fontface=2, position = position_nudge(y = -0.1)) + 
  
  geom_point(data=filter(outb4, (ind%in%"FTSE") & location%in%"United Kingdom" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=23, fill="darkred", color="darkred", size=2) +
  geom_text_repel(data=filter(outb4,  (ind%in%"FTSE") & location%in%"United Kingdom" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="UK outbreak"), size=5, fontface=2, position = position_nudge(y = -0.1)) + 
  
  geom_point(data=filter(outb4, (ind%in%"IPC") & location%in%"Mexico" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=23, fill="darkred", color="darkred", size=2) +
  geom_text_repel(data=filter(outb4,  (ind%in%"IPC") & location%in%"Mexico" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="Mexico outbreak"), size=5, fontface=2, position = position_nudge(y = -0.1)) + 
  
  geom_point(data=filter(outb4, (ind%in%"Smi") & location%in%"Switzerland" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=23, fill="darkred", color="darkred", size=2) +
  geom_text_repel(data=filter(outb4, (ind%in%"Smi") & location%in%"Switzerland" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="Switzerland outbreak"), size=5, fontface=2, position = position_nudge(y = -0.1)) + 
  
  geom_vline(data=filter(outb4, location%in%"world"),
            aes(xintercept=date), linetype="dashed", size=0.5, colour="#660033")+
  geom_text(data=filter(outb4, location%in%"world"),
            aes(x = date , y = 30000, label="First WHO report"), size=4, fontface=2, colour="#660033",
            nudge_x = -12, nudge_y = 1) + 
  
  geom_vline(data=filter(outb4, location%in%"chinalock"),
             aes(xintercept=date), linetype="dashed", size=0.5, colour="#660033")+
  geom_text(data=filter(outb4, location%in%"chinalock"),
            aes(x = date , y = 30000, label="China lockdown"), size=4, fontface=2, colour="#660033",
            nudge_x = 12, nudge_y = 1) + 
  
  geom_vline(data=filter(outb4, location%in%"pandemic"),
             aes(xintercept=date), linetype="dashed", size=0.5, colour="#660033")+
  geom_text(data=filter(outb4, location%in%"pandemic"),
            aes(x = date , y = 30000, label="Pandemic declaration"), size=4, fontface=2, colour="#660033",
            nudge_x = -12, nudge_y = -10) + 
  
  geom_vline(data=filter(outb4, location%in%"uslock"),
             aes(xintercept=date), linetype="dashed", size=0.5, colour="#660033")+
  geom_text(data=filter(outb4, location%in%"uslock"),
            aes(x = date , y = 30000, label="US lockdown"), size=4, fontface=2, colour="#660033",
            nudge_x = 12, nudge_y = -10) + 
  
  labs(title ="Covid-19 outbreak vs Stock Market Indexes", 
       subtitle="Daily comparison ", x = "Date", 
       y = "US Dollars", caption = "Source: UK Investing. April 22, 2020 (15:00, London time)", color ="Indexes") +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank()) +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16, face = "bold"),
        axis.text.x=element_text(colour="black", size = 12),
        axis.text.y=element_text(colour="black", size = 12),
        axis.title.x = element_text(color="black", size = 12),
        axis.title.y = element_text(color="black", size = 12 ))+
  scale_y_continuous(breaks=seq(from=0, to=30000, by=3000)) 

#ggsave(paste(grap, "cov_indusw.png", sep="\\"), width=12, height=12)  

#Database of outbreak acknowledge VS Indexes prices (WHO AND PANDEMIC ALONE)

g11<-ggplot(data=outb4) +
  geom_line(aes(x=date, y=price_us, group=ind, color=ind), size=0.8) +
  
  scale_colour_manual(values=brewer.pal(n =10, name = "Paired"))+
  
  geom_vline(data=filter(outb4, location%in%"world"),
             aes(xintercept=date), linetype="dashed", size=0.5, colour="#660033")+
  geom_text(data=filter(outb4, location%in%"world"),
            aes(x = date , y = 30000, label="WHO report"), size=3, fontface=2, colour="#660033",
            nudge_x = -12, nudge_y = 1) + 
  
  geom_vline(data=filter(outb4, location%in%"chinalock"),
             aes(xintercept=date), linetype="dashed", size=0.5, colour="#660033")+
  geom_text(data=filter(outb4, location%in%"chinalock"),
            aes(x = date , y = 30000, label="China lockdown"), size=3, fontface=2, colour="#660033",
            nudge_x = 10, nudge_y = 1) + 
  
  geom_vline(data=filter(outb4, location%in%"pandemic"),
             aes(xintercept=date), linetype="dashed", size=0.5, colour="#660033")+
  geom_text(data=filter(outb4, location%in%"pandemic"),
            aes(x = date , y = 30000, label="Pandemic declaration"), size=3, fontface=2, colour="#660033",
            nudge_x = -12, nudge_y = 1) + 
  
    geom_vline(data=filter(outb4, location%in%"uslock"),
             aes(xintercept=date), linetype="dashed", size=0.5, colour="#660033")+
  geom_text(data=filter(outb4, location%in%"uslock"),
            aes(x = date , y = 30000, label="US lockdown"), size=3, fontface=2, colour="#660033",
            nudge_x = 10, nudge_y = 1) + 
  
  labs(title ="Covid-19 outbreak vs Stock Market Indexes", 
       subtitle="Daily comparison ", x = "Date", 
       y = "US Dollars", caption = "Source: UK Investing. April 22, 2020 (15:00, London time)", color ="Indexes") +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank()) +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16, face = "bold"),
        axis.text.x=element_text(colour="black", size = 12),
        axis.text.y=element_text(colour="black", size = 12),
        axis.title.x = element_text(color="black", size = 12),
        axis.title.y = element_text(color="black", size = 12 ))+
  scale_y_continuous(breaks=seq(from=0, to=30000, by=3000)) 

#ggsave(paste(grap, "cov_indusw2.png", sep="\\"), width=12, height=12)  

#Database of outbreak VS indexes changes
names(sub_ind)
names(outb_aux)

outb2<-merge(sub_ind, outb_aux[c("total_cases","location", "date")], by="date", all.x =  TRUE)

head(outb2)
require(ggrepel)
g12<-ggplot(data=outb2) +
  geom_line(aes(x=date, y=(cum_chan/100), group=ind, color=ind), size=0.8) +
  
  scale_colour_manual(values=brewer.pal(n =10, name = "Paired"))+
  
  geom_point(data=filter(outb2, (ind%in%"Topix" | ind%in%"Nikkei") & location%in%"Japan" & !total_cases%in%NA),
             aes(x=date, y=(cum_chan/100)), shape=23, fill="darkred", color="darkred", size=3) +
  geom_text_repel(data=filter(outb2, (ind%in%"Nikkei" ) & location%in%"Japan" & !total_cases%in%NA),
                  aes(x = date , y = (cum_chan/100), label="Japan outbreak"), size=5, fontface=2, hjust = 0.5, vjust = 1,
                  nudge_x = 0, nudge_y = -0.2)+
  
  geom_point(data=filter(outb2, (ind%in%"Shanghai") & location%in%"China" & !total_cases%in%NA),
             aes(x=date, y=(cum_chan/100)), shape=23, fill="darkred", color="darkred", size=3) +
  geom_text_repel(data=filter(outb2, (ind%in%"Shanghai") & location%in%"China" & !total_cases%in%NA),
                  aes(x = date , y = (cum_chan/100), label="China outbreak"), size=5, fontface=2, hjust = 0.5, vjust = 1,
                  nudge_x = 0,nudge_y = 9) + 
  
  geom_point(data=filter(outb2, (ind%in%"S&P" | ind%in%"Dow Jones"| ind%in%"Nasdaq") & location%in%"United States" & !total_cases%in%NA),
             aes(x=date, y=(cum_chan/100)), shape=23, fill="darkred", color="darkred", size=3) +
  geom_text_repel(data=filter(outb2, ( ind%in%"Dow Jones") & location%in%"United States" & !total_cases%in%NA),
                  aes(x = date , y = (cum_chan/100), label="US outbreak"), size=5, fontface=2, hjust = 0.5, vjust = 1,
                  nudge_x = 0,nudge_y = 15) + 
  
  geom_point(data=filter(outb2, (ind%in%"Dax") & location%in%"Germany" & !total_cases%in%NA),
             aes(x=date, y=(cum_chan/100)), shape=23, fill="darkred", color="darkred", size=3) +
  geom_text_repel(data=filter(outb2,  (ind%in%"Dax") & location%in%"Germany" & !total_cases%in%NA),
                  aes(x = date , y = (cum_chan/100), label="Germany outbreak"), size=5, fontface=2, hjust = 0.5, vjust = 1,
                  nudge_x = 0, nudge_y = -0.2) + 
  
  geom_point(data=filter(outb2, (ind%in%"FTSE") & location%in%"United Kingdom" & !total_cases%in%NA),
             aes(x=date, y=(cum_chan/100)), shape=23, fill="darkred", color="darkred", size=3) +
  geom_text_repel(data=filter(outb2,  (ind%in%"FTSE") & location%in%"United Kingdom" & !total_cases%in%NA),
                  aes(x = date , y = (cum_chan/100), label="UK outbreak"), size=5, fontface=2, hjust = 0.5, vjust = 1,
                  nudge_x = 5,nudge_y = -0.21) + 
  
  geom_point(data=filter(outb2, (ind%in%"IPC") & location%in%"Mexico" & !total_cases%in%NA),
             aes(x=date, y=(cum_chan/100)), shape=23, fill="darkred", color="darkred", size=3) +
  geom_text_repel(data=filter(outb2,  (ind%in%"IPC") & location%in%"Mexico" & !total_cases%in%NA),
                  aes(x = date , y = (cum_chan/100), label="Mexico outbreak"), size=5, fontface=2, hjust = 0.5, vjust = 1,
                  nudge_x = -3, nudge_y = -0.2) + 
  
  geom_point(data=filter(outb2, (ind%in%"Smi") & location%in%"Switzerland" & !total_cases%in%NA),
             aes(x=date, y=(cum_chan/100)), shape=23, fill="darkred", color="darkred", size=3) +
  geom_text_repel(data=filter(outb2, (ind%in%"Smi") & location%in%"Switzerland" & !total_cases%in%NA),
                  aes(x = date , y = (cum_chan/100), label="Switzerland outbreak"), size=5, fontface=2, hjust = 0.5, vjust = 1,
                  nudge_x = 2,nudge_y = 14) + 
  
  geom_vline(data=filter(outb2, location%in%"world"),
             aes(xintercept=date), linetype="dashed", size=0.5, colour="#660033")+
  geom_text(data=filter(outb2, location%in%"world"),
            aes(x = date , y = 0.1, label="WHO report"), size=5, fontface=2, colour="#660033",
            nudge_x = -7, nudge_y = 0.10) + 
  
  geom_vline(data=filter(outb2, location%in%"chinalock"),
             aes(xintercept=date), linetype="dashed", size=0.5, colour="#660033")+
  geom_text(data=filter(outb2, location%in%"chinalock"),
            aes(x = date , y = 0.1, label="China lockdown"), size=5, fontface=2, colour="#660033",
            nudge_x = 7, nudge_y = 0.10) + 
  
  geom_vline(data=filter(outb2, location%in%"pandemic"),
             aes(xintercept=date), linetype="dashed", size=0.5, colour="#660033")+
  geom_text(data=filter(outb2, location%in%"pandemic"),
            aes(x = date , y = 0.1, label="Pandemic declaration"), size=5, fontface=2, colour="#660033",
            nudge_x = -9, nudge_y = 0.10) + 
  
  geom_vline(data=filter(outb2, location%in%"uslock"),
             aes(xintercept=date), linetype="dashed", size=0.5, colour="#660033")+
  geom_text(data=filter(outb2, location%in%"uslock"),
            aes(x = date , y = 0.1, label="US lockdown"), size=5, fontface=2, colour="#660033",
            nudge_x = 7, nudge_y = 0.10) + 
  
  labs(title ="Covid-19 outbreak vs Stock Market Indexes", 
       subtitle="Daily comparison ", x = "Date", 
       y = "Percentage", caption = "Source: UK Investing. April 22, 2020 (15:00, London time)", color ="Indexes") +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank()) +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16, face = "bold"),
        axis.text.x=element_text(colour="black", size = 12),
        axis.text.y=element_text(colour="black", size = 12),
        axis.title.x = element_text(color="black", size = 12),
        axis.title.y = element_text(color="black", size = 12 ))+
  scale_y_continuous(labels = percent)

#ggsave(paste(grap, "cov_ind_chw.png", sep="\\"), width=12, height=12)  

#Database of outbreak VS indexes changes (WHO and pandemic alone)
head(outb2)
require(ggrepel)

g13<-ggplot(data=outb2) +
  geom_line(aes(x=date, y=(cum_chan/100), group=ind, color=ind), size=0.8) +
  
  scale_colour_manual(values=brewer.pal(n =10, name = "Paired"))+
  
  geom_vline(data=filter(outb2, location%in%"world"),
             aes(xintercept=date), linetype="dashed", size=0.5, colour="#660033")+
  geom_text(data=filter(outb2, location%in%"world"),
            aes(x = date , y = 0.1, label="WHO report"), size=3, fontface=2, colour="#660033",
            nudge_x = -10, nudge_y = 0.10) + 
  
  geom_vline(data=filter(outb2, location%in%"chinalock"),
             aes(xintercept=date), linetype="dashed", size=0.5, colour="#660033")+
  geom_text(data=filter(outb2, location%in%"chinalock"),
            aes(x = date , y = 0.1, label="China lockdown"), size=3, fontface=2, colour="#660033",
            nudge_x = 10, nudge_y = 0.10) + 
  
  geom_vline(data=filter(outb2, location%in%"pandemic"),
             aes(xintercept=date), linetype="dashed", size=0.5, colour="#660033")+
  geom_text(data=filter(outb2, location%in%"pandemic"),
            aes(x = date , y = 0.1, label="Pandemic declaration"), size=3, fontface=2, colour="#660033",
            nudge_x = -10, nudge_y = 0.10) + 
  
  geom_vline(data=filter(outb2, location%in%"uslock"),
             aes(xintercept=date), linetype="dashed", size=0.5, colour="#660033")+
  geom_text(data=filter(outb2, location%in%"uslock"),
            aes(x = date , y = 0.1, label="US lockdown"), size=3, fontface=2, colour="#660033",
            nudge_x = 10, nudge_y = 0.10) + 
  
  labs(title ="Covid-19 outbreak vs Stock Market Indexes", 
       subtitle="Daily comparison ", x = "Date", 
       y = "Percentage", caption = "Source: UK Investing. April 22, 2020 (15:00, London time)", color ="Indexes") +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank()) +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16, face = "bold"),
        axis.text.x=element_text(colour="black", size = 12),
        axis.text.y=element_text(colour="black", size = 12),
        axis.title.x = element_text(color="black", size = 12),
        axis.title.y = element_text(color="black", size = 12 ))+
  scale_y_continuous(labels = percent)

#ggsave(paste(grap, "cov_ind_chw1.png", sep="\\"), width=12, height=12) 

#Foreign Exchange 1 USD VS ANOTHER CURRENCY
names(exch)

table(exch$ind)

#Covid outbreak VS Foreign Exchange 1 USD VS ANOTHER CURRENCY
names(outb_aux)
names(exch)
names(sub_exch)

outb3<-merge(sub_exch, outb_aux[c("total_cases","location", "date")], by="date", all.x = TRUE)

head(outb3)

#Graphic of Covid-19 vs Foreign Exchange with US outbreak

require(ggrepel)
g14<-ggplot(data=outb3) +
  geom_line(aes(x=date, y=price_us, group=ind, color=ind), size=0.8) +
  
  scale_colour_manual(values=brewer.pal(n =6, name = "Dark2"))+
  facet_grid(ind~., scales = "free")+
  
  geom_point(data=filter(outb3, (ind%in%"2.jpy") & location%in%"Japan" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=23, fill="darkred", color="darkred", size=2) +
  geom_text_repel(data=filter(outb3, (ind%in%"2.jpy") & location%in%"Japan" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="Japan outbreak"), size=5, fontface=2, 
                  nudge_x = 0, nudge_y =-4) + 
  
  geom_point(data=filter(outb3, (ind%in%"1.cny") & location%in%"China" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=23, fill="darkred", color="darkred", size=2) +
  geom_text_repel(data=filter(outb3, (ind%in%"1.cny") & location%in%"China" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="China outbreak"), size=5, fontface=2) +
  
  geom_point(data=filter(outb3, (ind%in%"3.eur") & location%in%"Germany" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=23, fill="darkred", color="darkred", size=2) +
  geom_text_repel(data=filter(outb3,  (ind%in%"3.eur") & location%in%"Germany" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="Germany outbreak"), size=5, fontface=2,
                  nudge_x = 5, nudge_y =1) + 
  
  geom_point(data=filter(outb3, (ind%in%"4.gbp") & location%in%"United Kingdom" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=23, fill="darkred", color="darkred", size=2) +
  geom_text_repel(data=filter(outb3,  (ind%in%"4.gbp") & location%in%"United Kingdom" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="UK outbreak"), size=5, fontface=2,
                  nudge_x = 5, nudge_y =1) + 
  
  geom_point(data=filter(outb3, (ind%in%"6.mxn") & location%in%"Mexico" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=23, fill="darkred", color="darkred", size=2) +
  geom_text_repel(data=filter(outb3,  (ind%in%"6.mxn") & location%in%"Mexico" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="Mexico outbreak"), size=5, fontface=2) + 
  
  geom_point(data=filter(outb3, (ind%in%"5.chf") & location%in%"Switzerland" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=23, fill="darkred", color="darkred", size=2) +
  geom_text_repel(data=filter(outb3, (ind%in%"5.chf") & location%in%"Switzerland" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="Switzerland outbreak"), size=5, fontface=2) + 
  
  geom_point(data=filter(outb3, location%in%"United States" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=19, fill="darkred", color="darkblue", size=2) +
  geom_text_repel(data=filter(outb3, location%in%"United States" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="US outbreak"), size=5, fontface=2, nudge_x = 0,
                  nudge_y =2) +
  
  geom_point(data=filter(outb3, location%in%"world" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=19, fill="#660033", color="#660033", size=2) +
  geom_text_repel(data=filter(outb3, location%in%"world" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="WHO report"), size=5, fontface=2, color="#660033",
                  nudge_x = -8,
                  nudge_y = 0) +
  
  geom_point(data=filter(outb3, location%in%"chinalock" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=19, fill="#660033", color="#660033", size=2) +
  geom_text_repel(data=filter(outb3, location%in%"chinalock" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="China lockdown"), size=5, fontface=2, color="#660033",
                  nudge_x = 1,
                  nudge_y = 0) +
  
  geom_point(data=filter(outb3, location%in%"pandemic" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=19, fill="#660033", color="#660033", size=2) +
  geom_text_repel(data=filter(outb3, location%in%"pandemic" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="Pandemic declaration"), size=5, fontface=2, color="#660033",
                  nudge_x = -1,
                  nudge_y = 0) +
  
  geom_point(data=filter(outb3, location%in%"uslock" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=19, fill="#660033", color="#660033", size=2) +
  geom_text_repel(data=filter(outb3, location%in%"uslock" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="US lockdown"), size=5, fontface=2, color="#660033",
                  nudge_x = 1,
                  nudge_y = 0) +
  
  labs(title ="Covid-19 outbreak vs Foreign Exchanges", 
       subtitle="Daily comparison ", x = "Date", 
       y = "Currencies units", caption = "Source: UK Investing. April 22, 2020 (15:00, London time)", color ="Currencies") +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#CCCCCC"), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank()) +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16, face = "bold"),
        axis.text.x=element_text(colour="black", size = 10),
        axis.text.y=element_text(colour="black", size = 10),
        axis.title.x = element_text(color="black", size = 12, vjust=-0.35),
        axis.title.y = element_text(color="black", size = 12 , vjust=0.35))

#ggsave(paste(grap, "cov_exch_w.png", sep="\\"), width=12, height=12)  

#Graphic of Covid-19 vs Foreign Exchange with US outbreak (WHO AND PANDEMIC ALONE)

require(ggrepel)
g15<-ggplot(data=outb3) +
  geom_line(aes(x=date, y=price_us, group=ind, color=ind), size=0.8) +
  
  scale_colour_manual(values=brewer.pal(n =6, name = "Dark2"))+
  facet_grid(ind~., scales = "free")+
  
  geom_point(data=filter(outb3, location%in%"world" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=19, fill="#660033", color="#660033", size=2) +
  geom_text_repel(data=filter(outb3, location%in%"world" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="WHO report"), size=3, fontface=2, color="#660033",
                  nudge_x = -8,
                  nudge_y = 0) +
  
  geom_point(data=filter(outb3, location%in%"chinalock" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=19, fill="#660033", color="#660033", size=2) +
  geom_text_repel(data=filter(outb3, location%in%"chinalock" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="China lockdown"), size=3, fontface=2, color="#660033",
                  nudge_x = 1,
                  nudge_y = 0) +
  
  geom_point(data=filter(outb3, location%in%"pandemic" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=19, fill="#660033", color="#660033", size=2) +
  geom_text_repel(data=filter(outb3, location%in%"pandemic" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="Pandemic declaration"), size=3, fontface=2, color="#660033",
                  nudge_x = -1,
                  nudge_y = 0) +
  
  geom_point(data=filter(outb3, location%in%"uslock" & !total_cases%in%NA),
             aes(x=date, y=price_us), shape=19, fill="#660033", color="#660033", size=2) +
  geom_text_repel(data=filter(outb3, location%in%"uslock" & !total_cases%in%NA),
                  aes(x = date , y = price_us, label="US lockdown"), size=3, fontface=2, color="#660033",
                  nudge_x = 1,
                  nudge_y = 0) +

  labs(title ="Covid-19 outbreak vs Foreign Exchanges", 
       subtitle="Daily comparison ", x = "Date", 
       y = "Currencies units", caption = "Source: UK Investing. April 22, 2020 (15:00, London time)", color ="Currencies") +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#CCCCCC"), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank()) +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16, face = "bold"),
        axis.text.x=element_text(colour="black", size = 10),
        axis.text.y=element_text(colour="black", size = 10),
        axis.title.x = element_text(color="black", size = 12, vjust=-0.35),
        axis.title.y = element_text(color="black", size = 12 , vjust=0.35))

#ggsave(paste(grap, "cov_exch_w1.png", sep="\\"), width=12, height=12)  



