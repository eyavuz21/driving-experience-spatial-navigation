#install relevant packages

#install.packages("openxlsx"); 
#install.packages("readxl"); 
#install.packages("data.table"); 
#install.packages("tidyr"); 
#install.packages("tidyverse"); 
#install.packages("zoo"); 
#install.packages("purrr"); 
#install.packages("car"); 
#install.packages("readr");
#install.packages("lm.beta");
#install.packages("ggplot2")

library(car);library(openxlsx);library(ggplot2);library(readr);library(lm.beta);library(readxl); library(data.table); library(tidyr); library(tidyverse); library(zoo); library(dplyr);library(purrr);library(car)

#upload dataframes and z-score variables 

correcteddf <- read_csv("/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/correcteddfEDfinalmodel.csv")

#summary tables of demographic variables 

correcteddfsummaryed <- data.frame(correcteddf$distance,correcteddf$age,correcteddf$gender,correcteddf$hours_of_phone_use_per_week,correcteddf$video_game_all_devices_hours_per_week)
colnames(correcteddfsummaryed) <- c("distance","age","gender","hours_of_phone_use_per_week","video_game_all_devices_hours_per_week")
correcteddfsummaryfemaleed <- correcteddfsummaryed[correcteddfsummaryed$gender=='Female',]
correcteddfsummarymaleed <- correcteddfsummaryed[correcteddfsummaryed$gender=='Male',]
tab2female <- CreateTableOne(vars = c("distance","age","gender","hours_of_phone_use_per_week","video_game_all_devices_hours_per_week"), data = correcteddfsummaryfemaleed, factorVars = c("gender"))
tab2male <- CreateTableOne(vars = c("distance","age","gender","hours_of_phone_use_per_week","video_game_all_devices_hours_per_week"), data = correcteddfsummarymaleed, factorVars = c("gender"))
tab2female <- print(tab2female,printToggle=FALSE)
tab2male <- print(tab2male,printToggle=FALSE)
write.csv(tab2female,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/summaryfemaleED.csv")
write.csv(tab2male,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/summarymaleED.csv")
correcteddfsummaryed <- data.frame(correcteddf$distance,correcteddf$age,correcteddf$gender,correcteddf$hours_of_phone_use_per_week,correcteddf$video_game_all_devices_hours_per_week)
colnames(correcteddfsummaryed) <- c("distance","age","gender","hours_of_phone_use_per_week","video_game_all_devices_hours_per_week")
tab2 <- CreateTableOne(vars = c("distance","age","gender","hours_of_phone_use_per_week","video_game_all_devices_hours_per_week"), data = correcteddfsummaryed, factorVars = c("gender"))
tab2 <- print(tab2,printToggle=FALSE)
write.csv(tab2,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/summaryED.csv")

#visualisations

#age one started to drive alone plot

correcteddf = read_csv("/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/correcteddfEDfinalmodel.csv")

correcteddf$binlearndrivealone[correcteddf$binlearndrivealone=='<18 yrs']='<18 yrs'
correcteddf$binlearndrivealone[correcteddf$binlearndrivealone=='>=18 yrs']='>=18 yrs'

binlearndrivealoneSE <- correcteddf %>%
  group_by(binlearndrivealone) %>%
  summarise( 
    n=n(),
    mean=mean(distance),
    sd=sd(distance)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

binlearndrivealoneSE$n_label <- paste0("n = ", binlearndrivealoneSE$n)
x1  = factor(binlearndrivealoneSE$binlearndrivealone, levels=c("<18 yrs",">=18 yrs"))
p = ggplot(data=binlearndrivealoneSE, aes(x=x1, y=mean)) + geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=0,size=0.5) + geom_point(size=2) + ylim(11,13.5) + geom_text(aes(label = n_label, y = mean + se + 0.2), size = 6)+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Age at which one starts to drive solo") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.title.x=element_text(size=20,face="bold",hjust=0.5,vjust=-2,margin=margin(t = 0, r = 0, b = 20, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 
png("agedrivealonebox.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "agedrivealonebox.png")
dev.off()

#age one started to drive alone raw with mean + SE

correcteddf = read_csv("/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/correcteddfEDfinalmodel.csv")

correcteddf$binlearndrivealone[correcteddf$binlearndrivealone=='<18 yrs']='<18 yrs'
correcteddf$binlearndrivealone[correcteddf$binlearndrivealone=='>=18 yrs']='>=18 yrs'

binlearndrivealoneSE <- correcteddf %>%
  group_by(binlearndrivealone) %>%
  summarise( 
    n=n(),
    mean=mean(distance),
    sd=sd(distance)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

binlearndrivealoneSE$n_label <- paste0("n = ", binlearndrivealoneSE$n)
correcteddf$x1  = factor(correcteddf$binlearndrivealone, levels=c("<18 yrs",">=18 yrs"))
p = ggplot(data=binlearndrivealoneSE, aes(x=x1, y=mean)) + geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=0,size=0.5) + geom_point(data = correcteddf, aes(x = x1, y = distance), color = "grey", alpha = 0.7, position = position_jitter(width = 0.1)) + geom_point(size=2, color="black") + ylim(11,13.5) + geom_text(aes(label = n_label, y = mean + se + 0.2), size = 6)+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Age at which one starts to drive solo") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.title.x=element_text(size=20,face="bold",hjust=0.5,vjust=-2,margin=margin(t = 0, r = 0, b = 20, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 
png("agedrivealonebox.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "agedrivealonebox.png")
dev.off()

#age one started to drive alone raw plot

correcteddf$x1  = factor(correcteddf$binlearndrivealone, levels=c("<18 yrs",">=18 yrs"))

p <- ggplot(correcteddf, aes(x = x1, y = distance)) +
  guides(color=guide_legend("Gender",override.aes = list(fill = NA))) + 
  theme(legend.title=element_text(size=20,face="bold"),legend.text=element_text(size=20,face="bold"),legend.key = element_rect(fill = NA, colour = NA)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE, colour = "black") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  labs(
    x = "Age at which one starts to drive solo",
    y = "Weighted wayfinding distance (VR-m)"
  ) +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20, face = "bold")
  )

ggsave(
  filename = "drive_linearinteraction.png",
  path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/",
  plot = p
)

#weekly biking plot

correcteddf$weeklybikingyesno[correcteddf$weeklybikingyesno=='Weekly biking yes']='Yes'
correcteddf$weeklybikingyesno[correcteddf$weeklybikingyesno=='Weekly biking no']='No'

bikingSE <- correcteddf %>%
  group_by(weeklybikingyesno) %>%
  summarise( 
    n=n(),
    mean=mean(distance),
    sd=sd(distance)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

bikingSE$n_label <- paste0("n = ", bikingSE$n)
x1  = factor(bikingSE$weeklybikingyesno, levels=c("No","Yes"))
p = ggplot(data=bikingSE, aes(x=x1, y=mean)) + geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=0,size=0.5) + geom_point(size=2) + ylim(11,13.5) + geom_text(aes(label = n_label, y = mean + se + 0.2), size = 6) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Weekly biking") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.title.x=element_text(size=20,face="bold",hjust=0.5,vjust=-2,margin=margin(t = 0, r = 0, b = 20, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 
png("bikeyesno.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "bikeyesno.png")
dev.off()

#other travel-related/demographic covariates

#weekly boating

correcteddf$weeklyboatingyesno[correcteddf$weeklyboatingyesno==0]='No'
correcteddf$weeklyboatingyesno[correcteddf$weeklyboatingyesno==1] = 'Yes'

boatSE <- correcteddf %>%
  group_by(weeklyboatingyesno) %>%
  summarise( 
    n=n(),
    mean=mean(distance),
    sd=sd(distance)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

x1  = factor(boatSE$weeklyboatingyesno, levels=c("No","Yes"))
p = ggplot(boatSE,aes(x=x1, y=mean)) + geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=0,size=0.5) + geom_point(size=2)  + ylim(8,14) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Weekly boating") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.text.x = element_text(angle=45,vjust=0.5),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 
png("boatEDbin.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "boatEDbin.png")
dev.off()

#weekly public transport 

correcteddf$weeklypublictransportyesno[correcteddf$weeklypublictransportyesno==0]='No'
correcteddf$weeklypublictransportyesno[correcteddf$weeklypublictransportyesno==1]='Yes'

publictranSE <- correcteddf %>%
  group_by(weeklypublictransportyesno) %>%
  summarise( 
    n=n(),
    mean=mean(distance),
    sd=sd(distance)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

x1  = factor(publictranSE$weeklypublictransportyesno, levels=c("No","Yes"))
p = ggplot(publictranSE,aes(x=x1, y=mean)) + geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=0,size=0.5) + geom_point(size=2)  + ylim(8,14) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Weekly public transport") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.text.x = element_text(angle=45,vjust=0.5),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 
png("PBEDbin.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "PBEDbin.png")
dev.off()

#average GPS reliance

p = ggplot(data=correcteddf, aes(x=GPSaverage, y=distance, group=1)) + geom_point() + geom_smooth(level=.6827,method="lm") + ylim(5,20) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Average GPS reliance score") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 
png("GPSED.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "GPSED.png")
dev.off()

#weekly hours of walking

p = ggplot(data=correcteddf, aes(x=walkinghours_per_week, y=distance, group=1)) + geom_point() + geom_smooth(level=.6827,method="lm") + ylim(5,20) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Hours of weekly walking") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 
png("HourswalkED.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "HourswalkED.png")
dev.off()

#weekly hours of driving

p = ggplot(data=correcteddf, aes(x=drivinghours_per_week, y=distance, group=1)) + geom_point() + ylim(5,20) + geom_smooth(level=.6827,method="lm") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Hours of weekly driving") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 
png("HoursdriveED.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "HoursdriveED.png")
dev.off()

#hours of weekly exercise 

p = ggplot(data=correcteddf, aes(x=hpwexercise, y=distance, group=1)) + geom_point() + geom_smooth(level=.6827,method="lm") + ylim(5,20) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Weekly hours of exercise") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 
png("HoursexperweekED.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "HoursexperweekED.png")
dev.off()

#daily travel time 

correcteddf$traveltimedaily[correcteddf$traveltimedaily=='0 - 0.5 hours']='>=0 and <0.5'
correcteddf$traveltimedaily[correcteddf$traveltimedaily=='0.5 - 1 hours']='>=0.5 and <1'
correcteddf$traveltimedaily[correcteddf$traveltimedaily=='1 - 1.5 hours']='>=1 and <1.5'
correcteddf$traveltimedaily[correcteddf$traveltimedaily=='1.5+']='>=1.5'

travelSE <- correcteddf %>%
  group_by(traveltimedaily) %>%
  summarise( 
    n=n(),
    mean=mean(distance),
    sd=sd(distance)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

x1  = factor(travelSE$traveltimedaily, levels=c(">=0 and <0.5",">=0.5 and <1",">=1 and <1.5",">=1.5"))
p = ggplot(travelSE,aes(x=x1, y=mean)) + geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=0,size=0.5) + geom_point(size=2)  + ylim(8,14) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Hours of daily travel") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.text.x = element_text(angle=45,vjust=0.5),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) + theme(plot.margin = unit(c(2.5, 0.5, 0.5, 0.5), "cm"))
png("DailytravelbinED.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "DailytravelbinED.png")
dev.off()

#daily travel time coded bin

correcteddf$traveltimedailycoded[correcteddf$traveltimedailycoded==0]='Up to 30 minutes'
correcteddf$traveltimedailycoded[correcteddf$traveltimedailycoded==1]='30 minutes or more'

travelSEcode <- correcteddf %>%
  group_by(traveltimedailycoded) %>%
  summarise( 
    n=n(),
    mean=mean(distance),
    sd=sd(distance)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

x1  = factor(travelSEcode$traveltimedailycoded, levels=c("Up to 30 minutes","30 minutes or more"))
p = ggplot(travelSEcode,aes(x=x1, y=mean)) + geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=0,size=0.5) + geom_point(size=2)  + ylim(10,13) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Hours of daily travel") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.text.x = element_text(angle=45,vjust=0.5),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) + theme(plot.margin = unit(c(2.5, 0.5, 0.5, 0.5), "cm"))
png("DailytravelbinED2.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "DailytravelbinED2.png")
dev.off()

#thinking modality 

correcteddf$thinkingmodality[correcteddf$thinkingmodality=='distance (e.g. 10 miles / km) away']='Distance'
correcteddf$thinkingmodality[correcteddf$thinkingmodality=='time (30mins away)']='Time'
correcteddf$thinkingmodality[(correcteddf$thinkingmodality!='Time') & (correcteddf$thinkingmodality!='Distance')]='Blocks'

thinkSE <- correcteddf %>%
  group_by(thinkingmodality) %>%
  summarise( 
    n=n(),
    mean=mean(distance),
    sd=sd(distance)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

x1  = factor(thinkSE$thinkingmodality, levels=c("Blocks","Distance","Time"))
p = ggplot(thinkSE,aes(x=x1, y=mean)) + geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=0,size=0.5) + geom_point(size=2)  + ylim(8,14) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Mode of thinking about space") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.text.x = element_text(angle=45,vjust=0.5),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 
png("ThinkingmodalityED.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "ThinkingmodalityED.png")
dev.off()

#hours per week exercise last 6 months

correcteddf$hpwexerciselast6mos[correcteddf$hpwexerciselast6mos==1]='Up to 1 hour'
correcteddf$hpwexerciselast6mos[correcteddf$hpwexerciselast6mos==2]='>= 1 and <2 hours'
correcteddf$hpwexerciselast6mos[correcteddf$hpwexerciselast6mos==3]='>= 2 and <3 hours'
correcteddf$hpwexerciselast6mos[correcteddf$hpwexerciselast6mos==4]='3 or more hours'
correcteddf$hpwexerciselast6mos[correcteddf$hpwexerciselast6mos==5]='3 or more hours'

hpwexercise6moSE <- correcteddf %>%
  group_by(hpwexerciselast6mos) %>%
  summarise( 
    n=n(),
    mean=mean(distance),
    sd=sd(distance)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

x1  = factor(hpwexercise6moSE$hpwexerciselast6mos, levels=c("Up to 1 hour",">= 1 and <2 hours",">= 2 and <3 hours","3 or more hours"))
p = ggplot(hpwexercise6moSE,aes(x=x1, y=mean)) + geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=0,size=0.5) + geom_point(size=2)  + ylim(8,14) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Weekly hours of exercise over the last 6 months") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.text.x = element_text(angle=45,vjust=0.5),axis.title.x=element_text(size=20,face="bold",hjust=1.0,margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) + theme(plot.margin = unit(c(2.5, 0.5, 0.5, 0.5), "cm"))
png("HPWexercise6mosED.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "HPWexercise6mosED.png")
dev.off()

#hours per week exercise last 6 months binned

correcteddf$hpwexerciselast6moscoded[correcteddf$hpwexerciselast6moscoded==0]='Up to 2 hours'
correcteddf$hpwexerciselast6moscoded[correcteddf$hpwexerciselast6moscoded==1]='2 or more hours'

hpwexercise6moSEcode <- correcteddf %>%
  group_by(hpwexerciselast6moscoded) %>%
  summarise( 
    n=n(),
    mean=mean(distance),
    sd=sd(distance)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

x1  = factor(hpwexercise6moSEcode$hpwexerciselast6moscoded, levels=c("Up to 2 hours","2 or more hours"))
p = ggplot(hpwexercise6moSEcode,aes(x=x1, y=mean)) + geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=0,size=0.5) + geom_point(size=2)  + ylim(10,13.5) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Weekly hours of exercise over the last 6 months") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.text.x = element_text(angle=45,vjust=0.5),axis.title.x=element_text(size=20,face="bold",hjust=1.0,margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) + theme(plot.margin = unit(c(2.5, 0.5, 0.5, 0.5), "cm"))
png("HPWexercise6mosEDbin.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "HPWexercise6mosEDbin.png")
dev.off()

#weekly hours of walking

p = ggplot(data=correcteddf, aes(x=walkinghours_per_week, y=distance, group=1)) + geom_point() + geom_smooth(level=.6827,method="lm") + ylim(5,20) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Weekly hours of walking") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 
png("HourswalkED.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "HourswalkED.png")
dev.off()

#age

p = ggplot(data=correcteddf, aes(x=age, y=distance, group=1)) + geom_point() + geom_smooth(level=.6827,method="lm") + ylim(5,20) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Age") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0)))  
png("ageED.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "ageED.png")
dev.off()

ageSE <- correcteddf %>%
  group_by(binage) %>%
  summarise( 
    n=n(),
    mean=mean(distance),
    sd=sd(distance)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

p = ggplot(ageSE,aes(x=binage, y=mean)) + geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=0,size=0.5) + geom_point(size=2)  + ylim(8,14) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Age") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.text.x = element_text(angle=45,vjust=0.5),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 
png("ageboxplotED.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "ageboxplotED.png")
dev.off()

#age line plot

p = ggplot(data=correcteddf, aes(x=age, y=distance, group=1)) + geom_point() + geom_smooth(level=.6827,method="lm") + ylim(5,20) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Age") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0)))  
png("ageED.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "ageED.png")
dev.off()

#weekly hours of video gaming binned

correcteddf$binvideo[correcteddf$binvideo=='>=1 and <5 hpw gaming on all devices']='>=1 and <5'
correcteddf$binvideo[correcteddf$binvideo=='>=5 and <10 hpw gaming on all devices']='>=5 and <10'
correcteddf$binvideo[correcteddf$binvideo=='>=10 hpw gaming on all devices']= '>=10'

binvideoSE <- correcteddf %>%
  group_by(binvideo) %>%
  summarise( 
    n=n(),
    mean=mean(distance),
    sd=sd(distance)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

x1  = factor(binvideoSE$binvideo, levels=c(">=1 and <5", ">=5 and <10", ">=10"))
p = ggplot(binvideoSE,aes(x=x1, y=mean)) + geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=0,size=0.5) + geom_point(size=2)  + ylim(8,14) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Weekly hours of video gaming on all devices") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.text.x = element_text(angle=45,vjust=0.5),axis.title.x=element_text(size=20,face="bold",hjust=1,margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 
png("videoboxplotED.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "videoboxplotED.png")
dev.off()

#weekly hours of video gaming line plot 

p = ggplot(data=correcteddf, aes(x=video_game_all_devices_hours_per_week, y=distance, group=1)) + geom_point() + geom_smooth(level=.6827,method="lm") + ylim(5,20) + scale_x_continuous(breaks=c(1, 5,10,15, 20),labels=c("1","5","10","15","20+"))  + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Weekly hours of video gaming on all devices") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.title.x=element_text(size=20,face="bold",hjust=1,margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 
png("VideoED.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "VideoED.png")
dev.off()

#gender

correcteddf <- correcteddf[(correcteddf$gender!="3"),]
correcteddf$gender[correcteddf$gender==1.0]='Male'
correcteddf$gender[correcteddf$gender==2.0]='Female'

genderSE <- correcteddf %>%
  group_by(gender) %>%
  summarise( 
    n=n(),
    mean=mean(distance),
    sd=sd(distance)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

p = ggplot(genderSE,aes(x=gender, y=mean)) + geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=0,size=0.5) + geom_point(size=2) + ylim(8,14) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Gender") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.text.x = element_text(angle=0,vjust=0.5),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 
png("genderboxplotED.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "genderboxplotED.png")
dev.off()

#weekly hours of phone use binned

phoneSE <- correcteddf %>%
  group_by(binphone) %>%
  summarise( 
    n=n(),
    mean=mean(distance),
    sd=sd(distance)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

phoneSE$binphone[phoneSE$binphone=='<15 hpw phone use']='>=1 and <15'
phoneSE$binphone[phoneSE$binphone=='>=15 and <28 hpw phone use']='>=15 and <28'
phoneSE$binphone[phoneSE$binphone=='>=28 and <41 hpw phone use']= '>=28 and <41'
phoneSE$binphone[phoneSE$binphone=='>=41 hpw phone use']= '>=41'

x1  = factor(phoneSE$binphone, levels=c(">=1 and <15",">=15 and <28",">=28 and <41",">=41"))
p = ggplot(data=phoneSE, aes(x=x1, y=mean, group=1)) + geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=0,size=0.5) + geom_point(size=2) + ylim(8,14) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Weekly hours of phone use") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.text.x=element_text(angle=35,vjust=0.5),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 
png("hoursphoneuseED.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "hoursphoneuseED.png")
dev.off()

#weekly hours of phone use line plot 

p = ggplot(data=correcteddf, aes(x=hours_of_phone_use_per_week, y=distance, group=1)) + geom_smooth(level=.6827,method="lm") + geom_point() + ylim(5,20) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Weekly hours of phone use") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.text.x=element_text(angle=35,vjust=0.5),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 
png("hoursphoneuselinearED.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "hoursphoneuselinearED.png")
dev.off()

#age one learnt to drive

p = ggplot(data=correcteddf, aes(x=agelearntodrive, y=distance, group=1)) + geom_point() + geom_smooth(level=.6827,method="lm") + ylim(5,20) + scale_x_continuous(breaks=c(12,15,20,25),labels=c("12","15","20","25")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Age one learnt to drive") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 
png("agelearndriveED.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "agelearndriveED.png")
dev.off()

#highest education level achieved plot 

correcteddf$education_coded <- ifelse(correcteddf$education %in% c("Some formal education", "High School"), 0, 1)

educationSE <- correcteddf %>%
  group_by(education_coded) %>%
  summarise( 
    n = n(),
    mean = mean(distance),
    sd = sd(distance)
  ) %>%
  mutate( se = sd / sqrt(n))  %>%
  mutate( ic = se * qt((1-0.05)/2 + .5, n-1))

educationSE$education_coded[educationSE$education_coded == 0] = "High School or below"
educationSE$education_coded[educationSE$education_coded == 1] = "2-Year College/Uni or above"
x1  = factor(educationSE$education_coded, levels=c("High School or below","2-Year College/Uni or above"))

p <- ggplot(data = educationSE, aes(x = x1, y = mean, group = 1)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0, size = 0.5) +
  geom_point(size = 2) +
  ylim(8, 14) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 25, hjust = 0.5,vjust=0.5)) +
  labs(x = "Highest education level achieved",
       y = "Weighted Wayfinding Distance (VR-m)") +
  theme(axis.text = element_text(size = 20),
        axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 20, face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0))) + theme(plot.margin = unit(c(2.5, 0.5, 0.5, 0.5), "cm"))

png("educationED.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "educationED.png")
dev.off()

#main linear model 

correcteddf <- read_csv('/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/correcteddfEDfinalmodel.csv')
correcteddf$video_game_all_devices_hours_per_week = scale(correcteddf$video_game_all_devices_hours_per_week)
correcteddf$video_game_phone_tablet_hours_per_week = scale(correcteddf$video_game_phone_tablet_hours_per_week)
correcteddf$hours_of_phone_use_per_week = scale(correcteddf$hours_of_phone_use_per_week)
correcteddf$age = scale(correcteddf$age)
correcteddf$hpwexercise = scale(correcteddf$hpwexercise)
correcteddf$hpwexerciselast6mos = as.factor(correcteddf$hpwexerciselast6mos)
correcteddf$GPSaverage = scale(correcteddf$GPSaverage)
correcteddf$walkinghours_per_week = scale(correcteddf$walkinghours_per_week)
correcteddf$drivinghours_per_week = scale(correcteddf$drivinghours_per_week)
correcteddf$gender[correcteddf$gender=="Female"]=0
correcteddf$gender[correcteddf$gender=="Male"]=1
correcteddf$gender <- as.factor(correcteddf$gender)
correcteddf$weeklybikingyesno <- as.factor(correcteddf$weeklybikingyesno)
correcteddf$agelearntodrive <- scale(correcteddf$agelearntodrive)
correcteddf$agestartdrivealone <- scale(correcteddf$agestartdrivealone)
correcteddf$highest_education_level_acheived <- as.factor(correcteddf$highest_education_level_acheived)
correcteddf$education_coded <- ifelse(correcteddf$education %in% c("Some formal education", "High School"), 0, 1)
correcteddf$drivinghours_per_week <- scale(correcteddf$drivinghours_per_week)
correcteddf$traveltimedaily <- as.factor(correcteddf$traveltimedaily)
correcteddf$thinkingmodality <- as.factor(correcteddf$thinkingmodality)
correcteddf$traveltimedailycoded <- ifelse(correcteddf$traveltimedaily %in% c("0 - 0.5 hours"), 0, 1)
correcteddf$thinkingmodalitycoded <- ifelse(correcteddf$thinkingmodality %in% c("‘blocks’"), 0, 1)
correcteddf$hpwexerciselast6moscoded <- ifelse(correcteddf$hpwexerciselast6mos %in% c(1,2), 0, 1)
write.csv(correcteddf, file = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/extradrive.csv", row.names = FALSE)

sink("/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/EDlatestdrive.csv")
sleepoutputall <- lm(zscore ~ agelearntodrive + agestartdrivealone + thinkingmodalitycoded + drivinghours_per_week + hpwexercise + walkinghours_per_week + weeklybikingyesno + weeklyboatingyesno + traveltimedailycoded + hpwexerciselast6moscoded + GPSaverage + age + gender + video_game_all_devices_hours_per_week + hours_of_phone_use_per_week + education_coded,data=correcteddf)
print(summary(sleepoutputall))
output <- summary(sleepoutputall)
effect_sizes <- cohens_f(sleepoutputall)
cohen_f_squared_values <- (effect_sizes$Cohens_f_partial)^2
variable_names <- effect_sizes$Parameter
cohen_f_df <- data.frame(Variable = variable_names,
                         Cohen_f = cohen_f_squared_values,
                         stringsAsFactors = FALSE)
colnames(cohen_f_df)[1] = 'Variable'
colnames(cohen_f_df)[2] = 'Cohens f2' 
cohen_f_df$`Cohens f2` <- round(cohen_f_df$`Cohens f2`,2)
write.csv(cohen_f_df,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/summaryCohensf2mainmodelED.csv")

sink()
closeAllConnections() 
confinter <- confint(sleepoutputall)
confinter <- as.data.frame(confinter)
confinter$`2.5 %` <- round(confinter$`2.5 %`, digits = 2)
confinter$`97.5 %` <- round(confinter$`97.5 %`, digits = 2)
confinter$uplow <- paste("[",confinter$`2.5 %`,",",confinter$`97.5 %`,"]")
confinter$names <- rownames(confinter) 
confinter <- subset(confinter, select = -c(1,2))
write.csv(confinter,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/CIdrive.csv", row.names=FALSE)
anovaoutput <- Anova(sleepoutputall,type=2)
vifoutputnew <- vif(sleepoutputall)
vifoutputnew <- as.data.frame(vifoutputnew)
vifoutputnew <- cbind(newColName = rownames(vifoutputnew), vifoutputnew)
rownames(vifoutputnew) <- 1:nrow(vifoutputnew)
colnames(vifoutputnew)[1] <- "Variables"
colnames(vifoutputnew)[2] <- "VIF"
write.csv(vifoutputnew,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/VIFEDnewalldrive.csv", row.names=FALSE)

#mediation analysis 

correcteddf <- read_csv('/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/correcteddfEDfinalmodel.csv')
correcteddf$highest_education_level_acheived <- as.numeric(correcteddf$highest_education_level_acheived)
correcteddf <- correcteddf[correcteddf$highest_education_level_acheived != 1.0,]
correcteddf$education_coded <- ifelse(correcteddf$education %in% c("Some formal education", "High School"), 0, 1)
correcteddf$traveltimedailycoded <- ifelse(correcteddf$traveltimedaily %in% c("0 - 0.5 hours"), 0, 1)
correcteddf$thinkingmodalitycoded <- ifelse(correcteddf$thinkingmodality %in% c("‘blocks’"), 0, 1)
correcteddf$hpwexerciselast6moscoded <- ifelse(correcteddf$hpwexerciselast6mos %in% c(1,2), 0, 1)

extract_mediation_summary <- function (x) { 
  
  clp <- 100 * x$conf.level
  isLinear.y <- ((class(x$model.y)[1] %in% c("lm", "rq")) || 
                   (inherits(x$model.y, "glm") && x$model.y$family$family == 
                      "gaussian" && x$model.y$family$link == "identity") || 
                   (inherits(x$model.y, "survreg") && x$model.y$dist == 
                      "gaussian"))
  
  printone <- !x$INT && isLinear.y
  
  if (printone) {
    
    smat <- c(x$d1, x$d1.ci, x$d1.p)
    smat <- rbind(smat, c(x$z0, x$z0.ci, x$z0.p))
    smat <- rbind(smat, c(x$tau.coef, x$tau.ci, x$tau.p))
    smat <- rbind(smat, c(x$n0, x$n0.ci, x$n0.p))
    
    rownames(smat) <- c("ACME", "ADE", "Total Effect", "Prop. Mediated")
    
  } else {
    smat <- c(x$d0, x$d0.ci, x$d0.p)
    smat <- rbind(smat, c(x$d1, x$d1.ci, x$d1.p))
    smat <- rbind(smat, c(x$z0, x$z0.ci, x$z0.p))
    smat <- rbind(smat, c(x$z1, x$z1.ci, x$z1.p))
    smat <- rbind(smat, c(x$tau.coef, x$tau.ci, x$tau.p))
    smat <- rbind(smat, c(x$n0, x$n0.ci, x$n0.p))
    smat <- rbind(smat, c(x$n1, x$n1.ci, x$n1.p))
    smat <- rbind(smat, c(x$d.avg, x$d.avg.ci, x$d.avg.p))
    smat <- rbind(smat, c(x$z.avg, x$z.avg.ci, x$z.avg.p))
    smat <- rbind(smat, c(x$n.avg, x$n.avg.ci, x$n.avg.p))
    
    rownames(smat) <- c("ACME (control)", "ACME (treated)", 
                        "ADE (control)", "ADE (treated)", "Total Effect", 
                        "Prop. Mediated (control)", "Prop. Mediated (treated)", 
                        "ACME (average)", "ADE (average)", "Prop. Mediated (average)")
    
  }
  
  colnames(smat) <- c("Estimate", paste(clp, "% CI Lower", sep = ""), 
                      paste(clp, "% CI Upper", sep = ""), "p-value")
  smat
  
}

install.packages("mediation")
library(mediation)

#mediation models for the age one started to drive alone

#mediation model with city one grew up in as mediator 

correcteddf$agestartdrivealone <- as.numeric(correcteddf$agestartdrivealone)
correcteddf$cityornot <- correcteddf$environment
correcteddf$cityornot[correcteddf$environment=='City'] = 1
correcteddf$cityornot[correcteddf$environment!='City'] = 0
correcteddf$cityornot <- as.numeric(correcteddf$cityornot)
sleepoutputallmed <- glm(cityornot ~  agelearntodrive + agestartdrivealone + thinkingmodalitycoded + drivinghours_per_week + hpwexercise + walkinghours_per_week + weeklybikingyesno + weeklyboatingyesno + traveltimedailycoded + hpwexerciselast6moscoded + GPSaverage + age + gender + video_game_all_devices_hours_per_week + hours_of_phone_use_per_week + education_coded, family=binomial(link='logit'),data=correcteddf)
sleepoutputallpredict <- lm(zscore ~ agelearntodrive + agestartdrivealone + cityornot + thinkingmodalitycoded + drivinghours_per_week + hpwexercise + walkinghours_per_week + weeklybikingyesno + weeklyboatingyesno + traveltimedailycoded + hpwexerciselast6moscoded + GPSaverage + age + gender + video_game_all_devices_hours_per_week + hours_of_phone_use_per_week + education_coded,data=correcteddf)
results = mediate(sleepoutputallmed, sleepoutputallpredict, treat='agestartdrivealone',mediator='cityornot',boot=T,sims=5000)
results = summary(results)
results = extract_mediation_summary(results)
results = as.data.frame(results)
write.csv(results,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/VIFEDnewalldrivemediationcityornot.csv", row.names=FALSE)

#mediation model with age as mediator 

correcteddf$age = as.numeric(correcteddf$age)
sleepoutputallmed <- lm(age ~  agelearntodrive + agestartdrivealone + thinkingmodalitycoded + drivinghours_per_week + hpwexercise + walkinghours_per_week + weeklybikingyesno + weeklyboatingyesno + traveltimedailycoded + hpwexerciselast6moscoded + GPSaverage + gender + video_game_all_devices_hours_per_week + hours_of_phone_use_per_week + education_coded,data=correcteddf)
sleepoutputallpredict <- lm(zscore ~ agelearntodrive + agestartdrivealone + thinkingmodalitycoded + drivinghours_per_week + hpwexercise + walkinghours_per_week + weeklybikingyesno + weeklyboatingyesno + traveltimedailycoded + hpwexerciselast6moscoded + GPSaverage + age + gender + video_game_all_devices_hours_per_week + hours_of_phone_use_per_week + education_coded,data=correcteddf)
results = mediate(sleepoutputallmed, sleepoutputallpredict, treat='agestartdrivealone',mediator='age',boot=T,sims=5000)
results = summary(results)
results = extract_mediation_summary(results)
results = as.data.frame(results)
write.csv(results,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/VIFEDnewalldrivemediationage.csv", row.names=FALSE)

#mediation model with gender as mediator 

correcteddf$gender[correcteddf$gender=="Male"] = 0
correcteddf$gender[correcteddf$gender=="Female"] = 1
correcteddf$gender <- as.numeric(correcteddf$gender)
sleepoutputallmed <- glm(gender ~  agelearntodrive + agestartdrivealone + thinkingmodalitycoded + drivinghours_per_week + hpwexercise + walkinghours_per_week + weeklybikingyesno + weeklyboatingyesno + traveltimedailycoded + hpwexerciselast6moscoded + GPSaverage + age + video_game_all_devices_hours_per_week + hours_of_phone_use_per_week + education_coded,family=binomial(link='logit'),data=correcteddf)
sleepoutputallpredict <- lm(zscore ~ agelearntodrive + agestartdrivealone + thinkingmodalitycoded + drivinghours_per_week + hpwexercise + walkinghours_per_week + weeklybikingyesno + weeklyboatingyesno + traveltimedailycoded + hpwexerciselast6moscoded + GPSaverage + age + gender + video_game_all_devices_hours_per_week + hours_of_phone_use_per_week + education_coded,data=correcteddf)
results = mediate(sleepoutputallmed, sleepoutputallpredict, treat='agestartdrivealone',mediator='gender',boot=T,sims=5000)
results = summary(results)
results = extract_mediation_summary(results)
results = as.data.frame(results)
write.csv(results,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/VIFEDnewalldrivemediationgender.csv", row.names=FALSE)

#mediation model with video game use as a mediator

correcteddf$video_game_all_devices_hours_per_week <- as.numeric(correcteddf$video_game_all_devices_hours_per_week)
sleepoutputallmed <- lm(video_game_all_devices_hours_per_week ~  agelearntodrive + agestartdrivealone + thinkingmodalitycoded + drivinghours_per_week + hpwexercise + walkinghours_per_week + weeklybikingyesno + weeklyboatingyesno + traveltimedailycoded + hpwexerciselast6moscoded + GPSaverage + age + gender + hours_of_phone_use_per_week + education_coded,data=correcteddf)
sleepoutputallpredict <- lm(zscore ~ agelearntodrive + agestartdrivealone + thinkingmodalitycoded + drivinghours_per_week + hpwexercise + walkinghours_per_week + weeklybikingyesno + weeklyboatingyesno + traveltimedailycoded + hpwexerciselast6moscoded + GPSaverage + age + gender + video_game_all_devices_hours_per_week + hours_of_phone_use_per_week + education_coded,data=correcteddf)
results = mediate(sleepoutputallmed, sleepoutputallpredict, treat='agestartdrivealone',mediator='video_game_all_devices_hours_per_week',boot=T,sims=5000)
results = summary(results)
results = extract_mediation_summary(results)
results = as.data.frame(results)
write.csv(results,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/VIFEDnewalldrivemediationvideogameusealldevices.csv", row.names=FALSE)

#mediation model with current environment as a mediator

correcteddf$citycurrent <- correcteddf$'city_current_type-quantised'
correcteddf$citycurrent[correcteddf$citycurrent==1]=1
correcteddf$citycurrent[correcteddf$citycurrent!=1]=0
correcteddf$citycurrent <- as.numeric(correcteddf$citycurrent)
sleepoutputallmed <- glm(citycurrent ~  agelearntodrive + agestartdrivealone + thinkingmodalitycoded + drivinghours_per_week + hpwexercise + walkinghours_per_week + weeklybikingyesno + weeklyboatingyesno + traveltimedailycoded + hpwexerciselast6moscoded + GPSaverage + age + gender + video_game_all_devices_hours_per_week + hours_of_phone_use_per_week + education_coded,family=binomial(link='logit'),data=correcteddf)
sleepoutputallpredict <- lm(zscore ~ agelearntodrive + agestartdrivealone + citycurrent + thinkingmodalitycoded + drivinghours_per_week + hpwexercise + walkinghours_per_week + weeklybikingyesno + weeklyboatingyesno + traveltimedailycoded + hpwexerciselast6moscoded + GPSaverage + age + gender + video_game_all_devices_hours_per_week + hours_of_phone_use_per_week + education_coded,data=correcteddf)
results = mediate(sleepoutputallmed, sleepoutputallpredict, treat='agestartdrivealone',mediator='citycurrent',boot=T,sims=5000)
results = summary(results)
results = extract_mediation_summary(results)
results = as.data.frame(results)
write.csv(results,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/VIFEDnewalldrivemediationcitycurrent.csv", row.names=FALSE)

#mediation models for the effect of weekly biking 

correcteddf$weeklybikingyesno <- as.character(correcteddf$weeklybikingyesno)
correcteddf$weeklybikingyesno[correcteddf$weeklybikingyesno=='Weekly biking yes'] = 1 
correcteddf$weeklybikingyesno[correcteddf$weeklybikingyesno=='Weekly biking no'] = 0
correcteddf$weeklybikingyesno <- as.numeric(correcteddf$weeklybikingyesno)

#mediation model with city one grew up in as mediator 

correcteddf$cityornot <- correcteddf$environment
correcteddf$cityornot[correcteddf$environment=='City'] = 1
correcteddf$cityornot[correcteddf$environment!='City'] = 0
correcteddf$cityornot <- as.numeric(correcteddf$cityornot)
correcteddf$cityornot <- as.numeric(correcteddf$cityornot)
sleepoutputallmed <- glm(cityornot ~  agelearntodrive + agestartdrivealone + thinkingmodalitycoded + drivinghours_per_week + hpwexercise + walkinghours_per_week + weeklybikingyesno + weeklyboatingyesno + traveltimedailycoded + hpwexerciselast6moscoded + GPSaverage + age + gender + video_game_all_devices_hours_per_week + hours_of_phone_use_per_week + education_coded,family=binomial(link='logit'),data=correcteddf)
sleepoutputallpredict <- lm(zscore ~ agelearntodrive + cityornot + agestartdrivealone + thinkingmodalitycoded + drivinghours_per_week + hpwexercise + walkinghours_per_week + weeklybikingyesno + weeklyboatingyesno + traveltimedailycoded + hpwexerciselast6moscoded + GPSaverage + age + gender + video_game_all_devices_hours_per_week + hours_of_phone_use_per_week + education_coded,data=correcteddf)
results = mediate(sleepoutputallmed, sleepoutputallpredict, treat='weeklybikingyesno',mediator='cityornot',boot=T,sims=5000)
results = summary(results)
results = extract_mediation_summary(results)
results = as.data.frame(results)
write.csv(results,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/VIFEDnewalldrivemediationcityornotonbike.csv", row.names=FALSE)

#mediation model with age as mediator 

correcteddf$age = as.numeric(correcteddf$age)
sleepoutputallmed <- lm(age ~  agelearntodrive + agestartdrivealone + thinkingmodalitycoded + drivinghours_per_week + hpwexercise + walkinghours_per_week + weeklybikingyesno + weeklyboatingyesno + traveltimedailycoded + hpwexerciselast6moscoded + GPSaverage + gender + video_game_all_devices_hours_per_week + hours_of_phone_use_per_week + education_coded,data=correcteddf)
sleepoutputallpredict <- lm(zscore ~ agelearntodrive + agestartdrivealone + thinkingmodalitycoded + drivinghours_per_week + hpwexercise + walkinghours_per_week + weeklybikingyesno + weeklyboatingyesno + traveltimedailycoded + hpwexerciselast6moscoded + GPSaverage + age + gender + video_game_all_devices_hours_per_week + hours_of_phone_use_per_week + education_coded,data=correcteddf)
results = mediate(sleepoutputallmed, sleepoutputallpredict, treat='weeklybikingyesno',mediator='age',boot=T,sims=5000)
results = summary(results)
results = extract_mediation_summary(results)
results = as.data.frame(results)
write.csv(results,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/VIFEDnewalldrivemediationageonbike.csv", row.names=FALSE)

#mediation model with gender as mediator 

correcteddf$gender <- as.numeric(correcteddf$gender)
sleepoutputallmed <- glm(gender ~  agelearntodrive + agestartdrivealone + thinkingmodalitycoded + drivinghours_per_week + hpwexercise + walkinghours_per_week + weeklybikingyesno + weeklyboatingyesno + traveltimedailycoded + hpwexerciselast6moscoded + GPSaverage + age + video_game_all_devices_hours_per_week + hours_of_phone_use_per_week + education_coded,family=binomial(link='logit'),data=correcteddf)
sleepoutputallpredict <- lm(zscore ~ agelearntodrive + agestartdrivealone + thinkingmodalitycoded + drivinghours_per_week + hpwexercise + walkinghours_per_week + weeklybikingyesno + weeklyboatingyesno + traveltimedailycoded + hpwexerciselast6moscoded + GPSaverage + age + gender + video_game_all_devices_hours_per_week + hours_of_phone_use_per_week + education_coded,data=correcteddf)
results = mediate(sleepoutputallmed, sleepoutputallpredict, treat='weeklybikingyesno',mediator='gender',boot=T,sims=5000)
results = summary(results)
results = extract_mediation_summary(results)
results = as.data.frame(results)
results = round(results,digits=7)
write.csv(results,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/VIFEDnewalldrivemediationgenderonbike.csv", row.names=FALSE)

sink("/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/EDmoderategenderbike.csv")
output <- summary(sleepoutputallmed)
print(output)
sink()
closeAllConnections() 

coef_table <- output$coefficients
odds_ratios <- exp(coef_table[, "Estimate"])
variable_names <- rownames(coef_table)
odds_ratio_df <- data.frame(
  Variable = variable_names,
  OddsRatio = odds_ratios,
  stringsAsFactors = FALSE
)
colnames(odds_ratio_df)[1] = 'Variable'
colnames(odds_ratio_df)[2] = 'Odds ratio' 
odds_ratio_df$`Odds ratio` <- round(odds_ratio_df$`Odds ratio`,2)
write.csv(odds_ratio_df,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/summaryODDsratiogendermediationED.csv")

confinter <- confint(sleepoutputallmed)
confinter <- as.data.frame(confinter)
confinter$`2.5 %` <- round(confinter$`2.5 %`, digits = 2)
confinter$`97.5 %` <- round(confinter$`97.5 %`, digits = 2)
confinter$uplow <- paste("[",confinter$`2.5 %`,",",confinter$`97.5 %`,"]")
confinter$names <- rownames(confinter) 
confinter <- subset(confinter, select = -c(1,2))
write.csv(confinter,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/modelEDgendermediatebikingCI.csv", row.names=FALSE)

#mediation model with video game use as a mediator

correcteddf$video_game_all_devices_hours_per_week <- as.numeric(correcteddf$video_game_all_devices_hours_per_week)
sleepoutputallmed <- lm(video_game_all_devices_hours_per_week ~  agelearntodrive + agestartdrivealone + thinkingmodalitycoded + drivinghours_per_week + hpwexercise + walkinghours_per_week + weeklybikingyesno + weeklyboatingyesno + traveltimedailycoded + hpwexerciselast6moscoded + GPSaverage + age + gender + hours_of_phone_use_per_week + education_coded,data=correcteddf)
sleepoutputallpredict <- lm(zscore ~ agelearntodrive + agestartdrivealone + thinkingmodalitycoded + drivinghours_per_week + hpwexercise + walkinghours_per_week + weeklybikingyesno + weeklyboatingyesno + traveltimedailycoded + hpwexerciselast6moscoded + GPSaverage + age + gender + video_game_all_devices_hours_per_week + hours_of_phone_use_per_week + education_coded,data=correcteddf)
results = mediate(sleepoutputallmed, sleepoutputallpredict, treat='weeklybikingyesno',mediator='video_game_all_devices_hours_per_week',boot=T,sims=5000)
results = summary(results)
results = extract_mediation_summary(results)
results = as.data.frame(results)
write.csv(results,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/VIFEDnewalldrivemediationvideogameusealldevicesonbike.csv", row.names=FALSE)

#mediation model with current environment as a mediator

correcteddf$citycurrent <- correcteddf$'city_current_type-quantised'
correcteddf$citycurrent[correcteddf$citycurrent==1]=1
correcteddf$citycurrent[correcteddf$citycurrent!=1]=0
correcteddf$citycurrent <- as.numeric(correcteddf$citycurrent)
sleepoutputallmed <- glm(citycurrent ~ agelearntodrive + agestartdrivealone + thinkingmodalitycoded + drivinghours_per_week + hpwexercise + walkinghours_per_week + weeklybikingyesno + weeklyboatingyesno + traveltimedailycoded + hpwexerciselast6moscoded + GPSaverage + age + gender + video_game_all_devices_hours_per_week + hours_of_phone_use_per_week + education_coded,family=binomial(link='logit'),data=correcteddf)
sleepoutputallpredict <- lm(zscore ~ agelearntodrive + citycurrent + agestartdrivealone + thinkingmodalitycoded + drivinghours_per_week + hpwexercise + walkinghours_per_week + weeklybikingyesno + weeklyboatingyesno + traveltimedailycoded + hpwexerciselast6moscoded + GPSaverage + age + gender + video_game_all_devices_hours_per_week + hours_of_phone_use_per_week + education_coded,data=correcteddf)
results = mediate(sleepoutputallmed, sleepoutputallpredict, treat='weeklybikingyesno',mediator='citycurrent',boot=T,sims=5000)
results = summary(results)
results = extract_mediation_summary(results)
results = as.data.frame(results)
write.csv(results,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/VIFEDnewalldrivemediationcitycurrentonbike.csv", row.names=FALSE)
