
# Load librarys -----------------------------------------------------------


library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(tidyr)
library(stringr)
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
top5Count<-nests %>% group_by(Season) %>% top_n(n = 5,wt = Count)

summary(aov(Count~Season,data=top5Count))
pairwise.t.test(top5Count$Count,top5Count$Season,p.adjust.method = "bonferroni")


# Phenology ---------------------------------------------------------------

Start33<-nests %>%
  group_by(Season) %>%
  filter(Count>.33*max(Count)) %>%
  summarise(DOS_First33=min(DOS),Date_First33=min(Date),DOS_Last33=max(DOS),Date_Last33=max(Date)) %>%
  ungroup() %>%
  mutate(DOSA_First33=DOS_First33-mean(DOS_First33),DOSA_Last33=DOS_Last33-mean(DOS_Last33))

End<-nests %>%
  group_by(Season) %>%
  filter(row_number()==n()) %>%
  summarise(DOS_end=min(DOS),Date_end=min(Date)) %>%
  ungroup() %>%
  mutate(DOSA_end=DOS_end-mean(DOS_end))

Start<-nests %>%
  group_by(Season) %>%
  filter(row_number()==1) %>%
  summarise(DOS_start=min(DOS),Date_start=min(Date)) %>%
  ungroup() %>%
  mutate(DOSA_start=DOS_start-mean(DOS_start))

Mid<-nests %>%
  group_by(Season) %>%
  summarise(DOS_mid=mean(max(DOS)-min(DOS)),Date_mid=ymd("2000-09-01")+days(mean(max(Date)-min(Date)))) %>%
  ungroup() %>%
  mutate(DOSA_mid=DOS_mid-mean(DOS_mid))

Pheno<-left_join(Start,Start33) %>%
  left_join(Mid) %>%
  left_join(End) %>%
  dplyr::select(Season,starts_with("DO")) %>%
  gather(Type,DOS,DOS_start:DOSA_end) %>%
  arrange(Season,Type) %>%
  mutate(Anomoly=str_detect(Type,"A"),
         Type=str_replace(Type,"DOSA_|DOS_",""),
         Anomoly=ifelse(Anomoly==TRUE,"Anomoly","DOS"),
         Duation=end-start)

Pheno$Type<-factor(Pheno$Type,levels=c("start","First33","mid","Last33","end"))

head(Pheno)
# Phenology plot
ggplot(Pheno,aes(Season,DOS))+
  geom_bar(stat="identity",fill="dodgerblue",color="black")+
  theme_bw(base_size = 24)+theme(axis.text.x = element_text(angle=90,vjust = .5),panel.grid = element_blank())+
  ylab("Day of Season")+
  facet_grid( Anomoly~Type,scales = "free_y")

ggsave("Plots/Phenology.jpg",width = 9,height=6)

# Relationship between phenology and year?
ggplot(Pheno,aes(Season,DOS))+
  geom_point()+geom_smooth(method="lm")+
  facet_grid( Anomoly~Type,scales = "free_y")

library(broom)
# There is a negitive relationship between last33 and year. not sure if valid since I have not throughly checked last33 for sanity
Pheno %>%
  group_by(Type,Anomoly) %>%
  do(glance(lm(DOS~Season,data=.)))

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

ggplot(nests,aes(Date,Count,group=Season,color=factor(Season)))+
  geom_path()+
  geom_point()+
  facet_grid(Season~.)+
  geom_vline(data=Start,aes(xintercept=Date,group=Season),color='red')+
  scale_x_date(date_breaks = "1 month",  date_labels = "%b")

ggsave("Plots/season_Count_with_top5.jpg",width = 5,height = 12,units = "in")


top5Count<-nests %>% group_by(Season) %>% top_n(n = 5,wt = Count)

ggplot(nests,aes(Date,Count,group=Season,color=factor(Season)))+
  geom_path()+
  geom_point()+
  facet_grid(Season~.)+
  geom_point(data=top5Count,aes(Date,Count,group=Season),color='red')+
  scale_x_date(date_breaks = "1 month",  date_labels = "%b")

ggsave("Plots/season_Count_with_top5.jpg",width = 5,height = 12,units = "in")


top3Count<-nests %>% group_by(Season) %>% top_n(n = 3,wt = Count)
ggplot(nests,aes(Date,Count,group=Season,color=factor(Season)))+
  geom_path()+
  geom_point()+
  facet_grid(Season~.)+
  geom_point(data=top3Count,aes(Date,Count,group=Season),color='red')+
  scale_x_date(date_breaks = "1 month",  date_labels = "%b")

ggsave("Plots/season_Count_with_top3.jpg",width = 5,height = 12,units = "in")

top50PctCount<-nests %>% group_by(Season) %>% filter(Count>.5*max(Count))
ggplot(nests,aes(Date,Count,group=Season,color=factor(Season)))+
  geom_path()+
  geom_point()+
  facet_grid(Season~.)+
  geom_point(data=top50PctCount,aes(Date,Count,group=Season),color='red')+
  scale_x_date(date_breaks = "1 month",  date_labels = "%b")

ggsave("Plots/season_Count_with_greaterThen0.5TimesMax.jpg",width = 5,height = 12,units = "in")

top75PctCount<-nests %>% group_by(Season) %>% filter(Count>.75*max(Count))
ggplot(nests,aes(Date,Count,group=Season,color=factor(Season)))+
  geom_path()+
  geom_point()+
  facet_grid(Season~.)+
  geom_point(data=top75PctCount,aes(Date,Count,group=Season),color='red')+
  scale_x_date(date_breaks = "1 month",  date_labels = "%b")

ggsave("Plots/season_Count_with_greaterThen0.75TimesMax.jpg",width = 5,height = 12,units = "in")


# Rolling Mean figures ----------------------------------------------------

roll<-nests %>% group_by(Season) %>% mutate(rollCount2=rollmean(Count,k=2,align = "right", fill = NA),
                                            rollCount3=rollmean(Count,k=3,align = "right", fill = NA))

ggplot(roll,aes(Date,rollCount2,group=Season,color=factor(Season)))+
  geom_path()+
  geom_point()+
  facet_grid(Season~.)+
  scale_x_date(date_breaks = "1 month",  date_labels = "%b")

ggsave("Plots/season_Count_2date_rolling_mean.jpg",width = 5,height = 12,units = "in")

ggplot(roll,aes(Date,rollCount3,group=Season,color=factor(Season)))+
  geom_path()+
  geom_point()+
  facet_grid(Season~.)+
  scale_x_date(date_breaks = "1 month",  date_labels = "%b")

ggsave("Plots/season_Count_3date_rolling_mean.jpg",width = 5,height = 12,units = "in")

