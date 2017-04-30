
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

nests$Date<-ymd("2000-09-01")+days(nests$DOS)

# Summarize by season using top 5 counts ----------------------------------

Counts<-nests %>% group_by(Season) %>%top_n(n = 3,wt = Count) %>%
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
  mutate(DOS_duration=DOS_end-DOS_start,DOSA_duration=DOS_duration-mean(DOS_duration)) %>%
  gather(Type,DOS,DOS_start:DOSA_duration) %>%
  arrange(Season,Type) %>%
  mutate(Anomoly=str_detect(Type,"A"),
         Type=str_replace(Type,"DOSA_|DOS_",""),
         Anomoly=ifelse(Anomoly==TRUE,"Anomoly","DOS"))

Pheno$Type<-factor(Pheno$Type,levels=c("start","First33","mid","Last33","end","duration"))

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



# Oceanography - SOI, ONI, MEI --------------------------------------------

SOI<-read_csv("Data/Oceanographic_data/SOI.csv")
SOI$Date<-ymd(paste(SOI$Date,"01"))

ONI<-read_csv("Data/Oceanographic_data/ONI.csv")
ONI$Date<-ymd(paste(ONI$PeriodNum,"01"))
ONI<- ONI %>% select(year=Year,Date,ONI)

MEI<-read_csv("Data/Oceanographic_data/MEI.csv")
MEI<-gather(MEI, Months,MEI,DECJAN:NOVDEC)
MEI$Month<-str_sub(MEI$Months,4,6)
MEI$Date<-ymd(paste(MEI$YEAR,MEI$Month,"01"))
MEI<- MEI %>% select(year=YEAR,Date,MEI)

ENSO<-SOI %>% left_join(ONI) %>% left_join(MEI) %>%
  filter(year>1995)
ENSO$Season <- c(rep(1995,6),
  rep(1996,12),
  rep(1997,12),
  rep(1998,12),
  rep(1999,12),
  rep(2000,12),
  rep(2001,12),
  rep(2002,12),
  rep(2003,12),
  rep(2004,12),
  rep(2005,12),
  rep(2006,12),
  rep(2007,12),
  rep(2008,12),
  rep(2009,12),
  rep(2010,12),
  rep(2011,12),
  rep(2012,12),
  rep(2013,12),
  rep(2014,12),
  rep(2015,12),
  rep(2016,8))
ggplot(ENSO,aes(x=Date))+
  geom_path(aes(y=SOI),color="blue")+
  geom_path(aes(y=MEI),color="red")+
  geom_path(aes(y=ONI),color="green")

ENSO_start <- ENSO %>%
  filter(month(Date)%in%c(8,9,10)) %>%
  group_by(Season) %>%
  summarise(MEI=mean(MEI),
            SOI=mean(SOI),
            ONI=mean(ONI))

  ENSO_pre <- ENSO %>%
    filter(month(Date)%in%c(6,7,8)) %>%
  group_by(Season) %>%
    summarise(MEI=mean(MEI),
              SOI=mean(SOI),
              ONI=mean(ONI))

  ENSO_mid <- ENSO %>%
    filter(month(Date)%in%c(10,11,12)) %>%
    group_by(Season) %>%
    summarise(MEI=mean(MEI),
              SOI=mean(SOI),
              ONI=mean(ONI))

  ENSO_all <- ENSO %>%
    filter(month(Date)%in%c(9,10,11,12,1,2,3)) %>%
    group_by(Season) %>%
    summarise(MEI=mean(MEI),
              SOI=mean(SOI),
              ONI=mean(ONI))


  ENSO_end <- ENSO %>%
    filter(month(Date)%in%c(1,2,3)) %>%
    group_by(Season) %>%
    summarise(MEI=mean(MEI),
              SOI=mean(SOI),
              ONI=mean(ONI))
  phenoENSO<-Pheno %>% left_join(ENSO_start)

  ggsave("Plots/Phenology.jpg",width = 9,height=6)

  # Relationship between phenology and year?
  ggplot(phenoENSO,aes(MEI,DOS))+
    geom_point()+geom_smooth(method="lm")+
    facet_grid( Anomoly~Type,scales = "free_y")
  ggplot(phenoENSO,aes(SOI,DOS))+
    geom_point()+geom_smooth(method="lm")+
    facet_grid( Anomoly~Type,scales = "free_y")
  ggplot(phenoENSO,aes(ONI,DOS))+
    geom_point()+geom_smooth(method="lm")+
    facet_grid( Anomoly~Type,scales = "free_y")

  library(broom)
  # There is a negitive relationship between last33 and year. not sure if valid since I have not throughly checked last33 for sanity
  Pheno %>%
    left_join(ENSO_start) %>%
    group_by(Type,Anomoly) %>%
    do(glance(lm(DOS~MEI,data=.))) %>% View
  Pheno %>%
    left_join(ENSO_start) %>%
    group_by(Type,Anomoly) %>%
    do(glance(lm(DOS~SOI,data=.))) %>% View
  Pheno %>%
    left_join(ENSO_start) %>%
    group_by(Type,Anomoly) %>%
    do(glance(lm(DOS~ONI,data=.))) %>% View

  # End is corrilated#
  Pheno %>%
    left_join(ENSO_end) %>%
    group_by(Type,Anomoly) %>%
    do(glance(lm(DOS~MEI,data=.))) %>% View
  Pheno %>%
    left_join(ENSO_end) %>%
    group_by(Type,Anomoly) %>%
    do(glance(lm(DOS~SOI,data=.))) %>% View
  Pheno %>%
    left_join(ENSO_end) %>%
    group_by(Type,Anomoly) %>%
    do(glance(lm(DOS~ONI,data=.))) %>% View


  Pheno %>%
    left_join(ENSO_mid) %>%
    group_by(Type,Anomoly) %>%
    do(glance(lm(DOS~MEI,data=.))) %>% View
  Pheno %>%
    left_join(ENSO_mid) %>%
    group_by(Type,Anomoly) %>%
    do(glance(lm(DOS~SOI,data=.))) %>% View
  Pheno %>%
    left_join(ENSO_mid) %>%
    group_by(Type,Anomoly) %>%
    do(glance(lm(DOS~ONI,data=.))) %>% View

  Pheno %>%
    left_join(ENSO_all) %>%
    group_by(Type,Anomoly) %>%
    do(glance(lm(DOS~MEI,data=.))) %>% View
  Pheno %>%
    left_join(ENSO_all,) %>%
    group_by(Type,Anomoly) %>%
    do(glance(lm(DOS~SOI,data=.))) %>% View
  Pheno %>%
    left_join(ENSO_all) %>%
    group_by(Type,Anomoly) %>%
    do(glance(lm(DOS~ONI,data=.))) %>% View

  head(ENSO)
hmm<-  Pheno %>%
    left_join(ENSO) %>%
    group_by(Type,month=month(Date),Anomoly) %>%
    do(glance(lm(DOS~SOI,data=.)))
ggplot(hmm,aes(x=month,y=r.squared))+geom_point() +facet_grid(Anomoly~Type)
hmm


# Counts vs index ---------------------------------------------------------

Pheno %>%
  left_join(ENSO) %>%
  left_join(Counts) %>%
  group_by(Type, Anomoly,month=month(Date)) %>%
  do(tidy(lm(max~MEI+DOS,data=.))) %>% View


# get oceanography vars ---------------------------------------------------

# 28.811285,-111.970331
#
# 29.312622, -112.693975
# 28.458908, -111.710355
library(xtractomatic)
a<- searchData(searchList=list(list("dtypename","MW")))


erdMWsstdmday<-xtracto_3D(xpos = c(-112.693975,-111.710355)+360,
                           ypos = c(28.458908,29.312622),
                           tpos = c("2002-06-24","2017-03-16"), dtype ="erdMWsstdmday" )



dims <- dim(erdMWsstdmday$data)
dataOut<-NULL
for(i in 1:174){
  temp<-data.frame(sst =c(erdMWsstdmday$data[,,i]))
  temp$Date<-erdMWsstdmday$time[i]
  dataOut<-bind_rows(dataOut,temp)
}
head(dataOut)

sst<-dataOut %>%
  group_by(Date) %>%
  summarise(sst=mean(sst,na.rm=T),sd=sd(sst,na.rm=T))

ggplot(sst,aes(x=ymd(Date),y=sst))+geom_line()



erdSW1chlamday<-xtracto_3D(xpos = c(-112.693975,-111.710355),
                           ypos = c(28.458908,29.312622),
                           tpos = c("2000-05-01","2010-12-16"), dtype ="erdSW1chlamday" )
erdMWchla8day<-xtracto_3D(xpos = c(-112.693975,-111.710355),
                           ypos = c(28.458908,29.312622),
                           tpos = c("2002-07-05","2017-04-25"), dtype ="erdMWchla8day" )

dims <- dim(erdSW1chlamday$data)
dataOut<-NULL
for(i in 1:126){
  temp<-data.frame(chl =c(erdSW1chlamday$data[,,i]))
  temp$Date<-erdSW1chlamday$time[i]
  dataOut<-bind_rows(dataOut,temp)
}
head(dataOut)

chl<-dataOut %>%
  group_by(Date) %>%
  summarise(chl=mean(chl,na.rm=T),sd=sd(chl,na.rm=T))

ggplot(chl,aes(x=ymd(Date),y=log(chl)))+geom_line()



