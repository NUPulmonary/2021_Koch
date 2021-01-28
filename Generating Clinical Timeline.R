#Install Packages
library(ggplot2)
library(plyr)

#Create working directories
#timeline is a table of participants, virus, and relative days for key events sorted by length of stay
#time_events is a table of partcipants, relative days, and event descriptions
timeplot<-timeline
timeplot2<-timeline
timeplot3<- timeline


#RSV Plot
timeplot<-subset(timeplot,virus %in% "2")
los_timeline<-ggplot(time_events,aes(x=rel_day,y=rsv_id))+
  geom_segment(data=timeplot,aes(x=admit,xend=discharge,y=rsv_id,yend=rsv_id),color="grey60",size=0.5)
dur_timeline<-los_timeline+geom_segment(data=timeplot,aes(x=resp_start,xend=resp_end,y=rsv_id,yend=rsv_id),
                                        color="#EC7063",alpha=0.5,lineend="round",size=2)
dur_timeline+geom_point(aes(color=event,shape=event),alpha=0.5,size=2.5)+xlab("Days")+ylab("")+
  scale_color_manual(name = "",
                     values = c("sx" = "#AF7AC5",
                                "admit" = "#5DADE2",
                                "samp" = "black",
                                "discharge" = "#48C9B0"),
                     labels = c("sx" = "Symptom Onset",
                                "admit" = "Admission",
                                "samp" = "Sample Collection",
                                "discharge" = "Discharged"))+
  scale_shape_manual(name = "",
                     values = c("sx" = 18,
                                "admit" = 15,
                                "samp" = 17,
                                "discharge" = 19),
                     labels = c("sx" = "Symptom Onset",
                                "admit" = "Admission",
                                "samp" = "Sample Collection",
                                "discharge" = "Discharged"))+
  theme(legend.position="bottom",panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),
        axis.line=element_line(color="black"),legend.title=element_blank(),legend.key=element_rect(fill="white"),
        axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  scale_x_continuous(expand = c(0, 0), limits = c(-20, 45),breaks=seq(-20,40,by=10))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 36))

#IAV Plot
timeplot2<-subset(timeplot2,virus %in% "3")
los_timeline<-ggplot(time_events,aes(x=rel_day,y=iav_id))+
  geom_segment(data=timeplot2,aes(x=admit,xend=discharge,y=iav_id,yend=iav_id),color="grey60",size=0.5)
dur_timeline<-los_timeline+geom_segment(data=timeplot2,aes(x=resp_start,xend=resp_end,y=iav_id,yend=iav_id),
                                        color="#EC7063",alpha=0.5,lineend="round",size=2)
dur_timeline+geom_point(aes(color=event,shape=event),alpha=0.5,size=2.5)+xlab("Days")+ylab("")+
  scale_color_manual(name = "",
                     values = c("sx" = "#AF7AC5",
                                "admit" = "#5DADE2",
                                "samp" = "black",
                                "discharge" = "#48C9B0"),
                     labels = c("sx" = "Symptom Onset",
                                "admit" = "Admission",
                                "samp" = "Sample Collection",
                                "discharge" = "Discharged"))+
  scale_shape_manual(name = "",
                     values = c("sx" = 18,
                                "admit" = 15,
                                "samp" = 17,
                                "discharge" = 19),
                     labels = c("sx" = "Symptom Onset",
                                "admit" = "Admission",
                                "samp" = "Sample Collection",
                                "discharge" = "Discharged"))+
  theme(legend.position="bottom",panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),
        axis.line=element_line(color="black"),legend.title=element_blank(),legend.key=element_rect(fill="white"),
        axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  scale_x_continuous(expand = c(0, 0), limits = c(-20, 45),breaks=seq(-20,40,by=10))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 36))

#SARS-coV-2 Plot
timeplot3<-subset(timeplot3,virus %in% "1")
los_timeline<-ggplot(time_events,aes(x=rel_day,y=sars_id))+
  geom_segment(data=timeplot3,aes(x=admit,xend=discharge,y=sars_id,yend=sars_id),color="grey60",size=0.5)
dur_timeline<-los_timeline+geom_segment(data=timeplot3,aes(x=resp_start,xend=resp_end,y=sars_id,yend=sars_id),
                                        color="#EC7063",alpha=0.5,lineend="round",size=2)
dur_timeline+geom_point(aes(color=event,shape=event),alpha=0.5,size=2.5)+xlab("Days")+ylab("")+
  scale_color_manual(name = "",
                     values = c("sx" = "#AF7AC5",
                                "admit" = "#5DADE2",
                                "samp" = "black",
                                "discharge" = "#48C9B0"),
                     labels = c("sx" = "Symptom Onset",
                                "admit" = "Admission",
                                "samp" = "Sample Collection",
                                "discharge" = "Discharged"))+
  scale_shape_manual(name = "",
                     values = c("sx" = 18,
                                "admit" = 15,
                                "samp" = 17,
                                "discharge" = 19),
                     labels = c("sx" = "Symptom Onset",
                                "admit" = "Admission",
                                "samp" = "Sample Collection",
                                "discharge" = "Discharged"))+
  theme(legend.position="bottom",panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),
        axis.line=element_line(color="black"),legend.title=element_blank(),legend.key=element_rect(fill="white"),
        axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  scale_x_continuous(expand = c(0, 0), limits = c(-20, 45),breaks=seq(-20,40,by=10))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 36))
