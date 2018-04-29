
# Load librarys -----------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(tidyr)
library(stringr)
library(broom)

# Read DCCO prod
p13<-read_excel("Data/Productivity/DCCO_Productivity_2013_2014.xlsx") %>%
  mutate(Adult_Activity=ifelse(Adult_Activity=="1","I",Adult_Activity),
         Adult_Activity=ifelse(Adult_Activity=="0","A",Adult_Activity),
         Date=dmy(Date),
         Adults=ifelse(Adults=="not counted",NA,Adults),
         Season=2013,
         fledge=as.numeric(fledge))

p14<-read_excel("Data/Productivity/DCCO_Productivity_2014_2015.xlsx") %>%
  mutate(Date=dmy(Date),
         Adult_Activity=ifelse(Adult_Activity=="1","I",Adult_Activity),
         Adult_Activity=ifelse(Adult_Activity=="0","A",Adult_Activity),
         Adult_Activity=ifelse(Adult_Activity=="no view",NA,Adult_Activity),
         Adult_Activity=ifelse(Adult_Activity=="na",NA,Adult_Activity),
         Adult_Activity=ifelse(Adult_Activity=="no nest","X",Adult_Activity),
         Adult_Activity=ifelse(Adult_Activity=="no photo",NA,Adult_Activity),
         Season=2014)

p1314<-bind_rows(p13,p14) %>% arrange(Plot,Nest,Date) %>%
  mutate(Chick_Stage=ifelse(na.fill(fledge_partial,0)==1,"1",NA),
         Chick_Stage=ifelse(na.fill(fledge,0)==1,"1",Chick_Stage)  ,
         Chick_Stage=ifelse(na.fill(fledge,0)==2,"2",Chick_Stage)  ,
         Chick_Stage=ifelse(na.fill(Nacked,F)==T,"NF",Chick_Stage),
         Chick_Stage=ifelse(na.fill(Downy,F)==T,"DF",Chick_Stage),
         Chick_Stage=ifelse(na.fill(Part_feathered,F)==T,"PF",Chick_Stage),
         Chick_Stage=ifelse(na.fill(Most_feathered,F)==T,"MF",Chick_Stage),
         Chick_Stage=ifelse(na.fill(Full_feathered,F)==T,"FF",Chick_Stage),
         Nest=as.character(Nest),
         Chick=as.character(Chick))

p15<-read_excel("Data/Productivity/DCCO_Productivity 2015_2016.xlsx") %>%
  select(Date,Plot, Nest, Adult, Adult_Activity, Chick, Add_Chick=Add_Chcik, Chick_Stage=Chick_stage) %>%
  mutate(Chick=as.character(Chick))%>%
  mutate(Season=2015,Date=as.Date(Date))

p16<-read_excel("Data/Productivity/DCCO_Productivity 2016_2017.xlsx")%>%
  select(Date,Plot, Nest, Adult, Adult_Activity, Chick, Add_Chick, Chick_Stage)%>%
  mutate(Season=2016,Date=as.Date(Date))

p17<-read_excel("Data/Productivity/DCCO_Productivity 2017_2018.xlsx")%>%
  select(Date,Plot, Nest, Adult, Adult_Activity, Chick, Add_Chick, Chick_Stage) %>%
  mutate(Season=2017,Date=as.Date(Date))

# Bind and recode chick stage
prod<-bind_rows(p1314 %>% select(Season,Date,Plot, Nest, Adult=Adults, Adult_Activity, Chick, Chick_Stage),
                   p15,p16,p17) %>%arrange(Season, Plot, Nest) %>%
  mutate(Add_Chick=ifelse(Add_Chick=="+",1,Add_Chick),
         Chick_Stage=gsub("FULL SIZE|full size","FF",Chick_Stage),
         Chick_Stage=gsub("DF|2 DF, 1 NF|DF/PF|1/4 SIZE","DF",Chick_Stage),
         Chick_Stage=gsub("PF|PF/MF|1/2 SIZE|1/2 Size|1/2 feathered","PF",Chick_Stage),
         Chick_Stage=gsub("3/4 SIZE|PF, FF","MF",Chick_Stage))

# Add day of season
prod$DOS=as.numeric((as.Date(prod$Date)-ymd(paste(prod$Season,"09","01"))))

# Recode adult activity
prod$Adult_Activity<-toupper(prod$Adult_Activity)
prod$Adult_Activity1<-gsub("N/A",NA,prod$Adult_Activity)
prod$Adult_Activity1<-gsub("A/B","B",prod$Adult_Activity1)
prod$Adult_Activity1<-gsub("ATT","A",prod$Adult_Activity1)
prod$Adult_Activity1<-gsub("A,I","I",prod$Adult_Activity1)
prod$Adult_Activity1<-gsub("0","X",prod$Adult_Activity1)
prod$Adult_Activity1<-gsub("I/B","I",prod$Adult_Activity1)
prod$Adult_Activity1<-gsub("I\\?","I",prod$Adult_Activity1)
prod$Adult_Activity1<-recode(prod$Adult_Activity1,A="Attend",B="Brood",C="Chick",D="Display",I="Inc",X="Empty")

# make a cat for when adult of chick is indicating reproduciton
prod$Adult_Activity2<-prod$Adult_Activity1=="Inc"|prod$Adult_Activity1=="Brood"|prod$Adult_Activity1=="Chick"|(prod$Chick!="A"&prod$Chick!="0"|!is.na(prod$Chick))

# get the min chick count and fill gaps in chick obs of 2 visits
# chick_diff is change in number of chicks
# get the DOS for days that the # chicks changes
# Calculate Nest Attempts and consecutive checks with Adult_Activity2 >2 checks
prod1<-prod %>%
  group_by(Season, Plot, Nest) %>%
  arrange(Season, Plot, Nest) %>%
  mutate(Chick_min=na.fill(na.approx(as.numeric(gsub("+","",Chick)),na.rm=F,maxgap = 2),0),
         chick_diff=Chick_min-lag(Chick_min),
         chick_date=ifelse(chick_diff>=1,DOS,NA))%>%
  mutate(NestAttempt=data.table::rleid(!is.na(Adult_Activity2)),
         NestAttempt=ifelse(is.na(Adult_Activity2),NA,NestAttempt)) %>%
  group_by(Season, Plot, Nest,NestAttempt) %>%
  mutate(con_check=n(),
         con_check=ifelse(is.na(Adult_Activity2),NA,con_check))

head(prod1)

# Summarize info by plot
prod2<-prod1 %>%
  group_by(Season, Plot, Nest,con_check) %>%
  summarise(max_chicks=max(Chick_min,na.rm=T),
            attempt=max(con_check,na.rm=T)>=2) %>%
  group_by(Season, Plot) %>%
  summarise(chicks=sum(max_chicks),
            nest_attempts=sum(attempt,na.rm=T),
            nests=n())

ggplot(prod2,aes(x=Season,y=chicks/nest_attempts))+
  geom_boxplot()+
  labs(y="Chicks/Nest Attempt",title="Chicks Per Nest Attempt",
     subtitle="Nest attempts are nests with chicks or adults incubating or brooding \non 2 or more consecutive checks") +
  theme_classic(base_size = 18)+
  theme(axis.text = element_text(color="black"))

ggsave(filename = "Plots/chick_per_attempt_box.jpg")

ggplot(prod2,aes(y=Season,x=chicks/nest_attempts))+
  geom_density_ridges()+
  labs(x="Chicks/Nest Attempt",title="Chicks Per Nest Attempt",
       subtitle="Nest attempts are nests with chicks or adults incubating or brooding \non 2 or more consecutive checks") +
  theme_classic(base_size = 18)+
  theme(axis.text = element_text(color="black"))

ggsave(filename = "Plots/chick_per_attempt_ridge.jpg")

ggplot(prod2,aes(x=Season,y=chicks/nests))+geom_boxplot()

nestsWChick<-prod %>%
  mutate(Season=factor(Season)) %>%
  filter(Chick!=0,!is.na(Chick),Chick!="A") %>%
  select(Season, Date, DOS, Nest, Plot, Chick) %>%
  distinct() %>%
  group_by(Season, DOS,Nest, Plot) %>%
  summarise(date_min=ymd("2001-09-01")+days(min(DOS,na.rm=T)),
            date_max=ymd("2001-09-01")+days(max(DOS,na.rm=T)),
            n=n(),
            DOS_min=min(DOS,na.rm=T),
            DOS_max=max(DOS,na.rm=T))

summary(aov(DOS_min~factor(Season),data = nestsWChick))
TukeyHSD(aov(DOS_min~factor(Season),data = nestsWChick))

ntab<-nestsWChick %>% ungroup %>%
  mutate(Season=factor(Season)) %>%
  group_by(Season) %>%
  summarise(first_hatch=min(DOS),
            last_hatch=max(DOS),
            mean_hatch=mean(DOS),
            sd=sd(DOS),n=n())




library(multcompView)
enerate_label_df <- function(Tuke, group="c_pc_h4"){

  # Extract labels and factor levels from Tukey post-hoc
  Tukey.levels <- Tuke[[group]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])

  #I need to put the labels in the same order as in the boxplot :
  Tukey.labels$colClust=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$colClust) , ]
  return(Tukey.labels)
}


TukeC<-enerate_label_df(Tuke = TukeyHSD(aov(DOS_min~Season,data = nestsWChick)), group = "Season")
ggplot(nestsWChick %>% group_by(Season),aes(x=Season,y=date_min))+geom_boxplot()+
  scale_y_date(date_labels = "%b")+
  geom_text(data=ntab,aes(label=paste("N =",n),x=Season,group=Season),y=ymd("2001-10-30"),vjust=0)+
  annotate(geom = "text",x=TukeC$colClust,
           label=TukeC$Letters,size = 8,vjust=1 ,col="black", y=ymd("2002-03-01"))+
  labs(y="Date Chick First Observed",title="Date that a chick was first observed in each nest",
       subtitle="Box Plot. \nLetters above show significant differences (TukeyHSD)") +
  theme_classic(base_size = 18)+theme(axis.text = element_text(color="black"))
ggsave(filename = "Plots/First_Chick_date_box.jpg")

library(ggridges)
ggplot(nestsWChick %>% group_by(Season),aes(y=Season,x=date_min))+
  geom_density_ridges()+
  scale_x_date(date_labels = "%b")+
  geom_text(data=ntab,aes(label=paste("N =",n),y=Season,group=Season),x=ymd("2001-10-30"),vjust=0)+
  annotate(geom = "text",y=TukeC$colClust,
           label=TukeC$Letters,size = 8,vjust=-0.3 ,col="black", x=ymd("2002-03-01"))+
  labs(y="Season", x="Date Chick First Observed",title="Date that a chick was first observed in each nest",
       subtitle="Density distribution. \nLetters on left show significant differences (TukeyHSD)") +
  theme_classic(base_size = 18)+theme(axis.text = element_text(color="black"))
ggsave(filename = "Plots/First_Chick_date_ridge.jpg")
