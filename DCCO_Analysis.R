
# Load librarys -----------------------------------------------------------


library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)


# Load data ---------------------------------------------------------------


nests<-read_excel("Data/DCCO_Nest_Data_All_Years_04-27-17.xlsx",sheet=3)
head(nests)
# Renames and sort by date
nests<-nests %>% rename(Count=`Active Nests`) %>% arrange(date)

# Calculate days from 1 Sep
nests$DOS=as.numeric((as.Date(nests$date)-ymd(paste(nests$Season,"09","01"))))


# Summarize by season using top 5 counts ----------------------------------

Counts<-nests %>% group_by(Season) %>%top_n(n = 5,wt = Count) %>%
  summarise(max=max(Count),mean=mean(Count),median=median(Count),sd=sd(Count),n=n())

Counts

# Summary figures ---------------------------------------------------------

# Max count by year
ggplot(Counts,aes(Season,max,group=Season))+geom_bar(stat="identity",fill="dodgerblue")+theme_bw(base_size = 24)
ggsave("Plots/max_nest_top_5.jpg")

# median count by year
ggplot(Counts,aes(Season,median,group=Season))+geom_bar(stat="identity",fill="dodgerblue")+theme_bw(base_size = 24)
ggsave("Plots/median_nest_top_5.jpg")

# Mean count by year
ggplot(Counts,aes(Season,mean,group=Season))+geom_bar(stat="identity",fill="dodgerblue")+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd))+theme_bw(base_size = 24)
ggsave("Plots/mean_nest_top_5.jpg")

nests$Date<-ymd("2000-09-01")+days(nests$DOS)
# Seasonal figures --------------------------------------------------------

ggplot(nests,aes(Date,Count,group=Season,color=factor(Season)))+
  geom_path()+
  geom_point()+
  facet_grid(Season~.)+
  scale_x_date(date_breaks = "1 month",  date_labels = "%b")

ggsave("Plots/season_Count.jpg",width = 5,height = 12,units = "in")

top5Count<-nests %>% group_by(Season) %>% top_n(n = 5,wt = Count)

ggplot(nests,aes(Date,Count,group=Season,color=factor(Season)))+
  geom_path()+
  geom_point()+
  facet_grid(Season~.)+
  geom_point(data=top5Count,aes(DOS,Count,group=Season),color='red')+
  scale_x_date(date_breaks = "1 month",  date_labels = "%b")

ggsave("Plots/season_Count_with_top5.jpg",width = 5,height = 12,units = "in")


top3Count<-nests %>% group_by(Season) %>% top_n(n = 3,wt = Count)
ggplot(nests,aes(Date,Count,group=Season,color=factor(Season)))+
  geom_path()+
  geom_point()+
  facet_grid(Season~.)+
  geom_point(data=top3Count,aes(DOS,Count,group=Season),color='red')+
  scale_x_date(date_breaks = "1 month",  date_labels = "%b")

ggsave("Plots/season_Count_with_top3.jpg",width = 5,height = 12,units = "in")


# Rolling Mean figures ----------------------------------------------------

roll<-nests %>% group_by(Season) %>% mutate(rollCount2=rollmean(Count,k=2,align = "right", fill = NA),
                                            rollCount3=rollmean(Count,k=3,align = "right", fill = NA))

ggplot(roll,aes(DOS,rollCount2,group=Season,color=factor(Season)))+
  geom_path()+
  geom_point()+
  facet_grid(Season~.)
ggsave("Plots/season_Count_2date_rolling_mean.jpg",width = 5,height = 12,units = "in")

ggplot(roll,aes(DOS,rollCount3,group=Season,color=factor(Season)))+
  geom_path()+
  geom_point()+
  facet_grid(Season~.)
ggsave("Plots/season_Count_3date_rolling_mean.jpg",width = 5,height = 12,units = "in")
