#Install Packages
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)

#Create working directories
#peds_seq is data table of children participants only
pdemo<-peds_seq
pdemo$virus<-mapvalues(pdemo$virus, from = c(1,2,3), to=c("SARS-CoV-2","RSV","IAV"))
pdemo$cat<-pdemo$virus
pdemo$cat<-mapvalues(pdemo$cat, from = c("SARS-CoV-2","RSV", "IAV"), to=c("SARS-CoV-2 \n Children","RSV \n Children", "IAV \n Children"))
pdemo$supp<-mapvalues(pdemo$supp, from=c(0,1,2,3,4),to=c("None","Low Flow NC/Mask","HFNC","NIPPV","IMV"))
pdemo$coll_supp<-mapvalues(pdemo$coll_supp, from=c(0,1,2,3,4),to=c("None","Low Flow NC/Mask","HFNC","NIPPV","IMV"))
pdemo$ards<-mapvalues(pdemo$ards, from=c(0,1), to=c("No ARDS","ARDS"))
pdemo$vaso<-mapvalues(pdemo$vaso, from=c(0,1), to=c("None","Vasoactive Medications"))
pdemo$gender<-mapvalues(pdemo$gender, from=c(0,1), to=c("Female","Male"))
pdemo$race<-mapvalues(pdemo$race, from=c(1,2,3,4,5,6,7,8,9), to=c("White","Black/African American","American Indian","Native Alaskan","Native Hawaiian","Other Pacific Islander","Asian","Other","Unknown or Not Reported"))
pdemo$ethnicity<-mapvalues(pdemo$ethnicity, from=c(1,2,3), to=c("Hispanic or Latino","Non-Hispanic or Latino","Unknown or Not Reported"))
pdemo$present<-mapvalues(pdemo$present, from=c(1,2,3,4,5,6,7,8), to=c("Respiratory Infection","CNS Infection","Shock","Cardiac Arrest","Gastrointestinal","Asymptomatic","Fever Only","Non-Resp Viral Syndrome"))
pdemo$complex<-pdemo$mhx_cat
pdemo$complex<-mapvalues(pdemo$complex, from=c(0,1,2,3),to=c("No","Yes","Yes","Yes"))
pdemo$vent<-pdemo$supp
pdemo$vent<-mapvalues(pdemo$vent,from=c("None","Low Flow NC/Mask","HFNC","NIPPV","IMV"),to=c("No","No","No","No","Yes"))
icu<-subset(pdemo, icu %in% "1")
mv<-subset(pdemo,supp %in% "IMV")
oxy<-subset(pdemo,oxy_duration >0)
resp<-subset(pdemo,supp %in% c("HFNC","NIPPV","IMV"))
nis<-subset(pdemo,supp %in% c("HFNC","NIPPV"))
sx<-subset(pdemo, present %in% c("Respiratory Infection","CNS Infection","Shock","Cardiac Arrest","Gastrointestinal","Fever Only","Non-Resp Viral Syndrome"))
plot<-pdemo

#Describing Data
#Median, quantile, table, prop.table commands used to describe data
median(subset(pdemo, virus %in% "SARS-CoV-2")$age_yrs)
quantile(subset(pdemo, virus %in% "SARS-CoV-2")$age_yrs)
table(subset(pdemo, virus %in% "SARS-CoV-2")$gender)
prop.table(table(subset(pdemo, virus %in% "SARS-CoV-2")$gender))

#Statistical Analysis
#wilcox.test, fisher.test, kruskal.test, and p.adjust commands used to describe data
kruskal.test(age_yrs~virus,data=pdemo)
fish<-xtabs(~pdemo$virus+pdemo$gender)
fisher.test(fish)
wilcox.test(subset(pdemo,virus %in% "RSV" | virus %in% "IAV")$age_yrs~subset(pdemo,virus %in% "RSV" | virus %in% "IAV")$virus)
p_vals$BH=p.adjust(p_vals$p,method="BH")

#Plotting Data
#Bar graphs and box plots for clinical data created using ggplot2. Representative examples.
#Box Plot of children ages
ggplot(plot, aes (x=cat, y=age_yrs, fill=cat))+
  geom_boxplot(outlier.shape=NA)+geom_jitter(shape=16, position=position_jitter(0.1))+
  scale_fill_manual(values=c("#5DADE2","#48C9B0","#EB984E","#EC7063","#AF7AC5","#FCEE74"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),
        axis.line=element_line(color="black"),legend.position="none",axis.title.x=element_blank(),
        plot.title=element_text(hjust=0.5))+
  ggtitle("Age")+ylab("Age at Collection (yrs)")+scale_y_continuous(expand = c(0, 0), limits = c(0, 20))+
  scale_x_discrete(limits=c("Uninfected \n Children", "SARS-CoV-2 \n Children","RSV \n Children", "IAV \n Children"))
#Stacked Bar Graph of respiratory support
ggplot(plot,aes(x=cat,fill=factor(supp,levels=c("IMV", "NIPPV","HFNC","Low Flow NC/Mask","None"))))+
  geom_bar(position=position_fill())+scale_fill_manual(values=c("#48C9B0","#EC7063","#5DADE2","#AF7AC5","#EB984E"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),
        axis.line=element_line(color="black"),legend.title=element_blank(),axis.title.x=element_blank(),legend.position="bottom",
        plot.title=element_text(hjust=0.5))+
  ggtitle("Peak Respiratory Support")+ylab("Proportion")+scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  scale_x_discrete(limits=c("SARS-CoV-2 \n Children","RSV \n Children", "IAV \n Children"))