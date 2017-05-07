
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

# raw counts data ---------------------------------------------------------

nd<-read_csv("Data/DCCO_Nest_Data/DCCO_Nest_2000-2017.csv",trim_ws = T)
head(nd)
nd$Date<-dmy(nd$Date)
nd$Nests<-str_replace(nd$Nests,"~","")
nd<- nd %>% select(Date,StartTime,WindStart,WindStartDir,WindEnd,WindEndDir,StartSky, StopSky,
                   Section,Nests,Notes ) %>% mutate(Nests=as.numeric(Nests))
table(nd$Section)
nd$Section<-gsub("A,\\\xc9, G","AG",nd$Section)
nd$Section<-gsub("A,\\\xc4, G","AG",nd$Section)
nd$Section<-gsub("A,\\\x80, G","AG",nd$Section)
nd$Section<-toupper(nd$Section)
nd$Section<-str_trim(nd$Section)

nd$Section<-str_replace(nd$Section,"/","")
nd$Section<-str_replace(nd$Section,"-","")
nd$Section<-str_replace(nd$Section,"\\&","")
nd$Section<-gsub(" ","",nd$Section)
nd$Section<-gsub("D,E,F,G","DG",nd$Section)
table(nd$Section,year(nd$Date))

nd$SectionAF<-str_replace(nd$Section,"^A$|^E$|^B$|^BC$|^C$|^CD$|^DE$|^C$|^DE$|^D$","North")
nd$SectionAF<-str_replace(nd$SectionAF,"^G$|^FG$|^F$","East")
nd$SectionAF<-str_replace(nd$SectionAF,"^H$|^I$","West")

head(nd)
unique(nd$SectionAF)

nd1<-nd %>% group_by(Date,SectionAF) %>% summarise(Nests=ceiling(sum(Nests,na.rm=T)))

# write.csv(nd1,"DCCO_Nest_sections_AF.csv",row.names = F)

nd1<-read_csv("DCCO_Nest_sections_AF.csv")

# Calculate days from 1 Sep
nd1$DOS=as.numeric((mdy(nd1$Date)-ymd(paste(nd1$Season,"09","01"))))
nd1$date<-mdy(nd1$Date)
nd1$Date<-ymd("2000-09-01")+days(nd1$DOS)

CountsSec<-nd1 %>% filter(SectionAF%in%c("West","East","North")) %>%
  group_by(Season,Section=SectionAF) %>%top_n(n = 3,wt = Nests) %>%
  summarise(max=max(Nests),mean=mean(Nests),median=median(Nests),sd=sd(Nests),n=n())


# Summary figures ---------------------------------------------------------

# Max count by year
ggplot(Counts,aes(Season,max,group=Season))+geom_bar(stat="identity",fill="dodgerblue")+theme_bw(base_size = 24)
ggsave("Plots/max_nest_top_3.jpg")

ggplot(CountsSec,aes(Season,max,group=Section,fill=Section))+geom_bar(stat="identity",show.legend = T)+theme_bw(base_size = 24)+theme(legend.position = "bottom")
ggsave("Plots/max_nest_sec_top_3.jpg")

# median count by year
ggplot(Counts,aes(Season,median,group=Season))+geom_bar(stat="identity",fill="dodgerblue")+theme_bw(base_size = 24)
ggsave("Plots/median_nest_top_3.jpg")

ggplot(CountsSec,aes(Season,median,fill=Section))+geom_bar(stat="identity")+theme_bw(base_size = 24)
ggsave("Plots/median_nest_sec_top_3.jpg")
# Mean count by year
ggplot(Counts,aes(Season,mean,group=Season))+
  geom_bar(stat="identity",fill="dodgerblue",position = position_dodge(width = .9))+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),position = position_dodge(width = .9))+theme_bw(base_size = 24)
ggsave("Plots/mean_nest_top_3.jpg")

ggplot(CountsSec,aes(Season,mean,fill=Section))+geom_bar(stat="identity",position = position_dodge(width = .9))+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),position = position_dodge(width = .9))+theme_bw(base_size = 24)
ggsave("Plots/mean_nest_sec_top_3.jpg")

ggplot(CountsSec,aes(Season,median,fill=Section))+geom_point(stat="identity")+geom_smooth(method="lm")+theme_bw(base_size = 24)
summary(lm(max~Season+Section,CountsSec))
ggsave("Plots/max_trend_nest_sec_top_3.jpg")
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

# Seasonal figures Sections ----------------------------------------------------
nd1$Count<-nd1$Nests
nd1$Section<-nd1$SectionAF
nd1<-nd1 %>% group_by(Season,Section) %>% filter(Section%in%c("East","West","North")) %>% arrange(Date)
head(nd1)

ggplot(nd1,aes(Date,Count,group=Section,color=Section))+
  geom_path()+
  geom_point()+
  facet_grid(Season~.)+
  scale_x_date(date_breaks = "1 month",  date_labels = "%b")

ggsave("Plots/season_Count_sec.jpg",width = 5,height = 12,units = "in")


top5Count<-nd1 %>% group_by(Season,Section) %>% top_n(n = 1,wt = Count)
# East section peaks late in 2013

ggplot(nd1,aes(Date,Count,group=Section,color=Section))+
  geom_path()+
  geom_point()+
  facet_grid(Season~.)+
  geom_point(data=top5Count,aes(Date,Count,group=Season),color='red')+
  scale_x_date(date_breaks = "1 month",  date_labels = "%b")

ggsave("Plots/season_Count_with_top1_sec.jpg",width = 5,height = 12,units = "in")

top50PctCount<-nd1 %>% group_by(Season,Section) %>% filter(Count>.75*max(Count))
ggplot(nd1,aes(Date,Count,group=Section,color=Section))+
  geom_path()+
  geom_point()+
  facet_grid(Season~.)+
  geom_point(data=top50PctCount,aes(Date,Count,group=Season),color='red')+
  scale_x_date(date_breaks = "1 month",  date_labels = "%b")

ggsave("Plots/season_Count_with_greaterThen0.99TimesMax_sec.jpg",width = 5,height = 12,units = "in")


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



# Phenology ---------------------------------------------------------------

Start33<-nests %>%
  group_by(Season) %>%
  filter(Count>.33*max(Count)) %>%
  summarise(DOS_First33=min(DOS),Date_First33=min(Date),DOS_Last33=max(DOS),Date_Last33=max(Date)) %>%
  ungroup() %>%
  mutate(DOSA_First33=DOS_First33-mean(DOS_First33),DOSA_Last33=DOS_Last33-mean(DOS_Last33))

End<-nests %>%
  group_by(Season) %>%
  filter(Count>0) %>%
  filter(row_number()==n()) %>%
  summarise(DOS_end=min(DOS),Date_end=min(Date)) %>%
  ungroup() %>%
  mutate(DOSA_end=DOS_end-mean(DOS_end))

Start<-nests %>%
  group_by(Season) %>%
  filter(Count>max(Count)*.1) %>%
  filter(row_number()==1) %>%
  summarise(DOS_start=min(DOS),Date_start=min(Date)) %>%
  ungroup() %>%
  mutate(DOSA_start=DOS_start-mean(DOS_start))

Mid<-nests %>%
  group_by(Season) %>%
  filter(Count>max(Count)*.1) %>%
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
  mutate(Anomaly=str_detect(Type,"A"),
         Type=str_replace(Type,"DOSA_|DOS_",""),
         Anomaly=ifelse(Anomaly==TRUE,"Anomaly","DOS"))

Pheno$Type<-factor(Pheno$Type,levels=c("start","First33","mid","Last33","end","duration"))
library(broom)
summary(aov(DOS~Season, data=Pheno[Pheno$Anomaly=="DOS"&Pheno$Type=="Last33",]))
head(Pheno)
# Phenology plot
ggplot(Pheno,aes(Season,DOS))+
  geom_bar(stat="identity",fill="dodgerblue",color="black")+
  theme_bw(base_size = 24)+theme(axis.text.x = element_text(angle=90,vjust = .5),panel.grid = element_blank())+
  ylab("Day of Season")+
  facet_grid( Anomaly~Type,scales = "free_y")

ggsave("Plots/Phenology.jpg",width = 9,height=6)
# Phenology Sec------------------------------------------------------------

Start33<-nd1 %>%
  group_by(Season,Section) %>%
  filter(Count>.33*max(Count)) %>%
  summarise(DOS_First33=min(DOS),Date_First33=min(Date),DOS_Last33=max(DOS),Date_Last33=max(Date)) %>%
  ungroup() %>%
  mutate(DOSA_First33=DOS_First33-mean(DOS_First33),DOSA_Last33=DOS_Last33-mean(DOS_Last33))

End<-nd1 %>%
  group_by(Season,Section) %>%
  filter(Count>0) %>%
  filter(row_number()==n()) %>%
  summarise(DOS_end=min(DOS),Date_end=min(Date)) %>%
  ungroup() %>%
  mutate(DOSA_end=DOS_end-mean(DOS_end))

Start<-nd1 %>%
  group_by(Season,Section) %>%
  filter(Count>max(Count)*.1) %>%
  filter(row_number()==1) %>%
  summarise(DOS_start=min(DOS),Date_start=min(Date)) %>%
  ungroup() %>%
  mutate(DOSA_start=DOS_start-mean(DOS_start))

Mid<-nd1 %>%
  group_by(Season,Section) %>%
  filter(Count>max(Count)*.1) %>%
  summarise(DOS_mid=mean(max(DOS)-min(DOS)),Date_mid=ymd("2000-09-01")+days(mean(max(Date)-min(Date)))) %>%
  ungroup() %>%
  mutate(DOSA_mid=DOS_mid-mean(DOS_mid))

Pheno<-left_join(Start,Start33) %>%
  left_join(Mid) %>%
  left_join(End) %>%
  dplyr::select(Season,Section,starts_with("DO")) %>%
  mutate(DOS_duration=DOS_end-DOS_start,DOSA_duration=DOS_duration-mean(DOS_duration)) %>%
  gather(Type,DOS,DOS_start:DOSA_duration) %>%
  arrange(Season,Type) %>%
  mutate(Anomaly=str_detect(Type,"A"),
         Type=str_replace(Type,"DOSA_|DOS_",""),
         Anomaly=ifelse(Anomaly==TRUE,"Anomaly","DOS"))

Pheno$Type<-factor(Pheno$Type,levels=c("start","First33","mid","Last33","end","duration"))
library(broom)
summary(aov(DOS~Season+Section, data=Pheno[Pheno$Anomaly=="DOS"&Pheno$Type=="Last33",]))
head(Pheno)
# Phenology plot
ggplot(Pheno,aes(Season,DOS,fill=Section))+
  geom_bar(stat="identity",color="black")+
  theme_bw(base_size = 24)+theme(axis.text.x = element_text(angle=90,vjust = .5),panel.grid = element_blank())+
  ylab("Day of Season")+
  facet_grid( Anomaly+Section~Type,scales = "free_y")

ggsave("Plots/Phenology_sec.jpg",width = 9,height=6)

# Relationship between phenology and year?
ggplot(Pheno,aes(Season,DOS))+
  geom_point()+geom_smooth(method="lm")+
  facet_grid( Anomaly+Section~Type,scales = "free_y")

library(broom)
# There is a negitive relationship between last33 and year. not sure if valid since I have not throughly checked last33 for sanity
Pheno %>%
  group_by(Type,Anomaly) %>%
  do(glance(lm(DOS~Season+Section,data=.))) %>% data.frame

# Oceanography - SOI, ONI, MEI --------------------------------------------

SOI<-read_csv("Data/Oceanographic_data/SOI.csv")
SOI$Date<-ymd(paste(SOI$Date,"01"))

ONI<-read_csv("Data/Oceanographic_data/ONI.csv")
ONI$Date<-ymd(paste(ONI$PeriodNum,"01"))
ONI<- ONI %>% dplyr::select(year=Year,Date,ONI)

MEI<-read_csv("Data/Oceanographic_data/MEI.csv")
MEI<-gather(MEI, Months,MEI,DECJAN:NOVDEC)
MEI$Month<-str_sub(MEI$Months,4,6)
MEI$Date<-ymd(paste(MEI$YEAR,MEI$Month,"01"))
MEI<- MEI %>% select(year=YEAR,Date,MEI)

ENSO<-SOI %>% left_join(ONI) %>% left_join(MEI) %>%
  filter(year>1995) %>%
  mutate(Season=year(Date-months(6)))

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
  facet_grid( Anomaly~Type,scales = "free_y")
ggplot(phenoENSO,aes(SOI,DOS))+
  geom_point()+geom_smooth(method="lm")+
  facet_grid( Anomaly~Type,scales = "free_y")
ggplot(phenoENSO,aes(ONI,DOS))+
  geom_point()+geom_smooth(method="lm")+
  facet_grid( Anomaly~Type,scales = "free_y")

library(broom)
# There is a negitive relationship between last33 and year. not sure if valid since I have not throughly checked last33 for sanity
Pheno %>%
  left_join(ENSO_start) %>%
  group_by(Type,Anomaly) %>%
  do(glance(lm(DOS~MEI+Section,data=.))) %>% View
Pheno %>%
  left_join(ENSO_start) %>%
  group_by(Type,Anomaly) %>%
  do(glance(lm(DOS~SOI+Section,data=.))) %>% View
Pheno %>%
  left_join(ENSO_start) %>%
  group_by(Type,Anomaly) %>%
  do(glance(lm(DOS~ONI+Section,data=.))) %>% View

# End is corrilated#
Pheno %>%
  left_join(ENSO_end) %>%
  group_by(Type,Anomaly) %>%
  do(glance(lm(DOS~MEI+Section,data=.))) %>% View

# End is corrilated#
Pheno %>%
  left_join(ENSO_end) %>%
  group_by(Type,Anomaly) %>%
  do(glance(lm(DOS~SOI+Section,data=.))) %>% View

# End is corrilated#
Pheno %>%
  left_join(ENSO_end) %>%
  group_by(Type,Anomaly) %>%
  do(glance(lm(DOS~ONI+Section,data=.))) %>% View

# End is corrilated#
Pheno %>%
  left_join(ENSO_mid) %>%
  group_by(Type,Anomaly) %>%
  do(glance(lm(DOS~MEI+Section,data=.))) %>% View
Pheno %>%
  left_join(ENSO_mid) %>%
  group_by(Type,Anomaly) %>%
  do(glance(lm(DOS~SOI+Section,data=.))) %>% View
Pheno %>%
  left_join(ENSO_mid) %>%
  group_by(Type,Anomaly) %>%
  do(glance(lm(DOS~ONI+Section,data=.))) %>% View

Pheno %>%
  left_join(ENSO_all) %>%
  group_by(Type,Anomaly) %>%
  do(glance(lm(DOS~MEI+Section,data=.))) %>% View
Pheno %>%
  left_join(ENSO_all) %>%
  group_by(Type,Anomaly) %>%
  do(glance(lm(DOS~SOI+Section,data=.))) %>% View
Pheno %>%
  left_join(ENSO_all) %>%
  group_by(Type,Anomaly) %>%
  do(glance(lm(DOS~ONI+Section,data=.))) %>% View

head(ENSO)
hmm<-  Pheno %>%
  left_join(ENSO) %>%
  group_by(Type,month=month(Date),Anomaly) %>%
  do(glance(lm(DOS~SOI+Section,data=.)))
ggplot(hmm,aes(x=month,y=r.squared))+geom_point() +facet_grid(Anomaly~Type)
View(hmm)

hmm<-  Pheno %>%
  left_join(ENSO) %>%
  group_by(Type,month=month(Date),Anomaly) %>%
  do(glance(lm(DOS~ONI+Section,data=.)))
ggplot(hmm,aes(x=month,y=r.squared))+geom_point() +facet_grid(Anomaly~Type)
View(hmm)

hmm<-  Pheno %>%
  left_join(ENSO) %>%
  group_by(Type,month=month(Date),Anomaly) %>%
  do(glance(lm(DOS~MEI+Section,data=.)))
ggplot(hmm,aes(x=month,y=r.squared))+geom_point() +facet_grid(Anomaly~Type)
View(hmm)
# Counts vs index ---------------------------------------------------------

Pheno %>%
  left_join(ENSO) %>%
  left_join(Counts) %>%
  group_by(Type, Anomaly,month=month(Date)) %>%
  do(glance(lme(max~MEI+Season+Section,data=.))) %>%
  mutate(p.value=round(p.value,5))%>% View


# get oceanography vars ---------------------------------------------------

# 28.811285,-111.970331
#
# 29.312622, -112.693975
# 28.458908, -111.710355
library(xtractomatic)
a<- searchData(searchList=list(list("dtypename","pp")))

#
erdMWsstdmday<-xtracto_3D(xpos = c(-112.693975,-111.710355)+360,
                           ypos = c(28.458908,29.312622),
                           tpos = c("2002-06-24","2017-03-16"), dtype ="erdMWsstdmday" )
#
#
#
dims <- dim(erdMWsstdmday$data)
dataOut<-NULL
for(i in 1:174){
  temp<-data.frame(sst =c(erdMWsstdmday$data[,,i]))
  temp$Date<-erdMWsstdmday$time[i]
  dataOut<-bind_rows(dataOut,temp)
}
head(dataOut)
#
library(raster)
table(is.infinite(dataOut$sst))
max(filter(dataOut,sst<13)$sst)

sst<-dataOut %>%
  filter(sst>13,!is.na(sst)) %>%
  group_by(Date) %>%
  summarise(sstM=mean(sst,na.rm=T),
            med=median(sst,na.rm=T),
            min=min(sst,na.rm=T),
            max=max(sst,na.rm=T),
            sd=sd(sst,na.rm=T)) %>%
  rename(sst=sstM)
#
# ggplot(sst,aes(x=ymd(Date),y=sst))+geom_line()
# saveRDS(sst,"sst_erdMWsstdmday.rds")
#
#
# erdSW1chlamday<-xtracto_3D(xpos = c(-112.693975,-111.710355),
#                            ypos = c(28.458908,29.312622),
#                            tpos = c("2000-05-01","2010-12-16"), dtype ="erdSW1chlamday" )
erdMWchla8day<-xtracto_3D(xpos = c(-112.693975,-111.710355),
                          ypos = c(28.458908,29.312622),
                          tpos = c("2002-07-05","2017-04-25"), dtype ="erdMWchla8day" )
# saveRDS(erdSW1chlamday,"erdSW1chlamday.rds")
# # saveRDS(erdMWchla8day,"erdMWchla8day.rds")
# erdSW1chlamday<-readRDS("erdSW1chlamday.rds")
# dims <- dim(erdSW1chlamday$data)
# dataOut<-NULL
# for(i in 1:126){
#   temp<-data.frame(chl =c(erdSW1chlamday$data[,,i]))
#   temp$Date<-erdSW1chlamday$time[i]
#   dataOut<-bind_rows(dataOut,temp)
# }
# head(dataOut)
#
# chl<-dataOut %>%
#   group_by(Date) %>%
#   summarise(chl=mean(chl,na.rm=T),sd=sd(chl,na.rm=T))
# saveRDS(chl,"chl_erdSW1chlamday.rds")
# ggplot(chl,aes(x=ymd(Date),y=log(chl)))+geom_line()
#
# dims <- dim(erdMWchla8day$data)
# dataOut<-NULL
# for(i in 1:5188){
#   temp<-data.frame(chl =c(erdMWchla8day$data[,,i]))
#   temp$Date<-erdMWchla8day$time[i]
#   dataOut<-bind_rows(dataOut,temp)
# }
# head(dataOut)
# saveRDS(dataOut,"erdMWchla8day_table.rds")
# dataOut<-readRDS("erdMWchla8day_table.rds")
# dataOut$Date<-ymd(dataOut$Date)
# dataOut$month<-month(dataOut$Date)
# dataOut$year<-year(dataOut$Date)
#
# chl1<-dataOut %>%
#   group_by(year,month) %>%
#   summarise(chl=mean(chl,na.rm=T),sd=sd(chl,na.rm=T))
# saveRDS(chl1,"chl_erdMWchla8day.rds")

sst<-readRDS("sst_erdMWsstdmday.rds")
sst<-sst %>%
  mutate(Date=ymd(Date),year=year(Date),month=month(Date),
         Season=year(Date-months(6))) %>%
  dplyr::select(-Date)
chl<-readRDS("chl_erdSW1chlamday.rds")
chl<-chl %>%
  mutate(Date=ymd(Date),year=year(Date),month=month(Date),
         Season=year(Date-months(6))) %>%
  select(-Date)
chl1<-readRDS("chl_erdMWchla8day.rds")
chl1<-chl1 %>% ungroup() %>%
  mutate(Date=ymd(paste(year,month,"01")),year=year(Date),month=month(Date),
         Season=year(Date-months(6))) %>%
  select(-Date)
# ggplot()+
#   geom_line(data=chl1,aes(x=ymd(paste(year,month,16)),y=(chl)),color="blue")+
#   geom_line(data=chl,aes(x=ymd(Date),y=(chl)),color="red")



head(chl1)
head(Pheno)
all<-Pheno %>%
  left_join(ENSO) %>%
  left_join(Counts) %>%
  mutate(month=month(Date)) %>%
  rename(sd_count=sd) %>%
  left_join(chl1) %>%
  left_join(sst)

Ocean<-sst %>%
  rename(sd_sst=sd) %>%
  left_join(chl1) %>%
  rename(sd_chl=sd) %>%filter(!is.na(sst))


all<-Counts %>%
  rename(sd_count=sd) %>%
  left_join(chl1) %>%
  rename(sd_chl=sd,max_c=max) %>%
  left_join(sst) %>% filter(!is.na(sst))

ggplot(all[!is.na(all$sst)&all$Season!=2005,],aes(x=chl,y=max_c))+
  geom_text(aes(label=Season))+
  facet_wrap(~month)+
  geom_smooth(method="lm")

ENSO

ggplot(all,aes(x=sst))+geom_density(aes(group=factor(month),fill=factor(month)))+facet_grid(month~.)
ggplot(all,aes(x=month,y=sst))+geom_bar(stat ="identity",aes())+facet_grid(Season~.)

Ocean$month<-factor(Ocean$month,levels=c(7,8,9,10,11,12,1,2,3,4,5,6))
Ocean <- Ocean %>% filter(Season %in%c(2002,2009,2010:2016))
Sums<-Ocean %>%
  group_by(month) %>%
  summarise(sstM=mean(sst, na.rm=T),sst_sd=sd(sst,na.rm=T),
       chlM=mean(chl, na.rm=T),chl_sd=sd(chl,na.rm=T))

ggplot(Sums,aes(x=month,y=sstM))+geom_bar(stat ="identity")+geom_errorbar(aes(ymin=sstM-sst_sd,ymax=sstM+sst_sd))
ggplot(Ocean,aes(x=month,y=sd_sst))+
  geom_path(aes(group=Season,colour=factor(Season)),size=1)

ggplot(Sums,aes(x=month,y=chlM))+geom_bar(stat ="identity")+geom_errorbar(aes(ymin=chlM-chl_sd,ymax=chlM+chl_sd))
ggplot(Ocean,aes(x=month,y=log(chl),group=Season,colour=factor(Season)))+geom_path(size=1)+geom_hline(yintercept =log(mean(Ocean$chl)))

all %>%
  filter(!is.na(sst),Season!=2005) %>%
  group_by(month) %>%
  do(glance(lm(max_c~chl+sd,data=.))) %>%
  mutate(p.value=round(p.value,5)) %>% View

library(lme4)
sst_m<-sst %>%
  dplyr::select(Season,month,sst) %>% filter(month%in%7:12) %>%
  mutate(month=paste0("sst_",month)) %>%
  spread(month,value = sst)
chl_m<-chl1 %>%
  dplyr::select(Season,month,chl) %>% filter(month%in%7:12) %>%
  mutate(month=paste0("chl_",month)) %>%
  spread(month,value = chl)
ENSO
lmer(max_c~chl+sst+(1|Season),data=all)
