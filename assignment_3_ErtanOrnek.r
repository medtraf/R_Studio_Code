# Visualizing Time Jump-Start Code for Financial Time Series

# begin by installing the packages quantmod, lubridate, latticeExtra, and zoo 
library(quantmod) # use for gathering and charting economic data
library(lubridate) # date functions
library(latticeExtra) # package used for horizon plot
library(zoo)  # utilities for working with time series
library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)
library(svglite)
setwd("D:/Backup/Dropbox/Canvas/455/Week6/Assignment3")


#read traffic data
trafficData<-read.csv(file = "VolumeData.csv", 
         stringsAsFactors = FALSE)

#load to data frame
trafficData$Volume..VPL.=trafficData$Volume..VPL.*12.0

trafficDF<-na.omit(data.frame(trafficData))
# trafficDF<-trafficDF%>%
#   mutate(       Time = hm(Time)
trafficDF$Time<-as.POSIXct(hm(trafficDF$Time),origin = "2015-12-31")




occsummary<-aggregate(trafficDF$Occupancy...., list(Time = trafficDF$Time), mean)
names(occsummary)[2]='averageocc'
occmaxsummary<-aggregate(trafficDF$Occupancy...., list(Time = trafficDF$Time), max)
names(occmaxsummary)[2]='maxocc'
occminsummary<-aggregate(trafficDF$Occupancy...., list(Time = trafficDF$Time), sd)
names(occminsummary)[2]='minocc'

speedsummary<-aggregate(trafficDF$Speed..MPH., list(Time = trafficDF$Time), mean)
names(speedsummary)[2]='averagespeed'
speedmaxsummary<-aggregate(trafficDF$Speed..MPH., list(Time = trafficDF$Time), max)
names(speedmaxsummary)[2]='maxspeed'
speedminsummary<-aggregate(trafficDF$Speed..MPH., list(Time = trafficDF$Time), sd)
names(speedminsummary)[2]='minspeed'

volumesummary<-aggregate(trafficDF$Volume..VPL., list(Time = trafficDF$Time), mean)
names(volumesummary)[2]='averagevol'
volumemaxsummary<-aggregate(trafficDF$Volume..VPL., list(Time = trafficDF$Time), max)
names(volumemaxsummary)[2]='maxvol'
volumeminsummary<-aggregate(trafficDF$Volume..VPL., list(Time = trafficDF$Time), sd)
names(volumeminsummary)[2]='minvol'




volumedata<-volumesummary %>% inner_join(volumemaxsummary, by='Time')%>% inner_join(volumeminsummary, by='Time')
speeddata<-speedsummary %>% inner_join(speedmaxsummary, by='Time')%>% inner_join(speedminsummary, by='Time')
occdata<-occsummary %>% inner_join(occmaxsummary, by='Time')%>% inner_join(occminsummary, by='Time')




#Volume Graph
plotting_object <- ggplot(volumedata,aes(Time, group = 1)) +
  
  geom_line( aes(x = volumedata$Time, y = volumedata$averagevol,colour="Average Volume"),size=1) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),legend.key=element_blank(),legend.position="top",
        legend.spacing.y = unit(0.01,"cm"),legend.margin=margin(c(0,0,0,0)))+
  scale_colour_manual("",values="black")+
  ylab("Volume (vehicles/hr)") +
  xlab("")+
  #ggtitle("Impact of Roadway Conditions on Driving Experience")
  scale_x_datetime( breaks = date_breaks("1 hour"), 
                    labels = date_format("%H:%M"), 
                    expand = c(.04,-.040))+
  #scale_x_datetime(limits =lims, breaks=date_breaks("4 hour"), labels=date_format("%H:%M"))+
  #scale_x_discrete()+
  geom_ribbon(data=volumedata,aes(ymin=volumedata$averagevol-volumedata$minvol*1.96,
                               ymax=volumedata$averagevol+volumedata$minvol*1.96,fill="Volume Range"),alpha=0.3
  )+
  
  scale_fill_manual("",values="gold1")+
 
  theme(axis.text.x=element_blank(), panel.background = element_blank())+
  annotate("rect", xmin = volumesummary$Time[190], xmax = volumesummary$Time[212], 
           ymin = min(volumesummary$averagevol)*2, ymax = max(volumesummary$averagevol*1.4), 
           alpha = 0.3, fill = "red")+
  annotate("text",label = "PM",
           y=max(volumesummary$averagevol)*1.8,x= volumesummary$Time[212-10])+
 
annotate("rect", xmin = volumesummary$Time[100-22], xmax = volumesummary$Time[122-22], 
         ymin = min(volumesummary$averagevol)*2, ymax = max(volumesummary$averagevol*1.4), 
         alpha = 0.3, fill = "red")+
  annotate("text",label = "AM",
           y=max(volumesummary$averagevol)*1.8,x= volumesummary$Time[122-10-20])+


# annotate("rect", xmin = volumesummary$Time[17], xmax = volumesummary$Time[24], 
#          ymin = 1840, ymax = 2040, 
#          alpha = 0.3, fill = "red")+
  annotate("text",label = "Peak",
           y=min(volumesummary$averagevol)*2.75,x= volumesummary$Time[122-10-22],fontface =1,size=2.3)+

annotate("text",label = "Peak",
         y=min(volumesummary$averagevol)*2.75,x= volumesummary$Time[212-10],fontface =1,size=2.3)
print (plotting_object)
ggsave("Volume.svg", width=10, height=2.5)


#Occupancy Graph
plotting_object <- ggplot(occdata,aes(Time, group = 1)) +
  
  geom_line( aes(x = occdata$Time, y = occdata$averageocc,colour="Average Occupancy"),size=1) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),legend.key=element_blank(),legend.position="top",
        legend.spacing.y = unit(0.01,"cm"),legend.margin=margin(c(0,0,0,0)))+
  scale_colour_manual("",values="black")+
  ylab("Occupancy (% of time)") +
  #xlab("")+
  #ggtitle("Impact of Roadway Conditions on Driving Experience")
  scale_x_datetime( breaks = date_breaks("1 hour"), 
                    labels = date_format("%H:%M"), 
                    expand = c(.04,-.040))+
  #scale_x_datetime(limits =lims, breaks=date_breaks("4 hour"), labels=date_format("%H:%M"))+
  #scale_x_discrete()+
  geom_ribbon(data=occdata,aes(ymin=occdata$averageocc-occdata$minocc*1.96,
                                  ymax=occdata$averageocc+occdata$minocc*1.96,fill="Occupancy Range"),alpha=0.3
  )+
  
  scale_fill_manual("",values="gold1")+
  
  theme(axis.text.x=element_blank(), panel.background = element_blank())+
  annotate("rect", xmin = occsummary$Time[190], xmax = occsummary$Time[212], 
           ymin = min(occsummary$averageocc)*2, ymax = max(occsummary$averageocc*1.4), 
           alpha = 0.3, fill = "red")+
  annotate("text",label = "PM",
           y=max(occsummary$averageocc)*1.8,x= occsummary$Time[212-10])+
  
  annotate("rect", xmin = occsummary$Time[100-22], xmax = occsummary$Time[122-22], 
           ymin = min(occsummary$averageocc)*2, ymax = max(occsummary$averageocc*1.4), 
           alpha = 0.3, fill = "red")+
  annotate("text",label = "AM",
           y=max(occsummary$averageocc)*1.8,x= occsummary$Time[122-10-20])+
  
  
  # annotate("rect", xmin = occsummary$Time[17], xmax = occsummary$Time[24], 
  #          ymin = 1840, ymax = 2040, 
  #          alpha = 0.3, fill = "red")+
  annotate("text",label = "Peak",
           y=min(occsummary$averageocc)*2.75,x= occsummary$Time[122-10-22],fontface =1,size=2.3)+
  
  annotate("text",label = "Peak",
           y=min(occsummary$averageocc)*2.75,x= occsummary$Time[212-10],fontface =1,size=2.3)
print (plotting_object)
ggsave("Occupancy.svg", width=10, height=2.5)




#Speed Graph
plotting_object <- ggplot(speeddata,aes(Time, group = 1)) +
  
  geom_line( aes(x = speeddata$Time, y = speeddata$averagespeed,colour="Average Speed"),size=1) +
  theme(legend.key=element_blank(),legend.position="top",
        legend.spacing.y = unit(0.01,"cm"),legend.margin=margin(c(0,0,0,0)))+
  scale_colour_manual("",values="black")+
  scale_y_continuous(breaks=seq(0, 100, 10))+
  ylab("Average Speed (mph)") +
  xlab("Time of Day")+
  #ggtitle("Average Speed at Capitol Drive 41 Southbound")+
  scale_x_datetime( breaks = date_breaks("1 hour"), 
                    labels = date_format("%H:%M"), 
                    expand = c(.04,-.040))+
  #scale_x_datetime(limits =lims, breaks=date_breaks("4 hour"), labels=date_format("%H:%M"))+
  #scale_x_discrete()+
  geom_ribbon(data=speeddata,aes(ymin=speeddata$averagespeed-speeddata$minspeed*1.96,
                               ymax=speeddata$averagespeed+speeddata$minspeed*1.96,fill="Speed Range"),alpha=0.3
  )+
  
  scale_fill_manual("",values="gold1")+
  
  theme(axis.text.x=element_text(angle=90), panel.background = element_blank())+
  annotate("rect", xmin = speedsummary$Time[190], xmax = speedsummary$Time[212], 
           ymin = 25, ymax =85, 
           alpha = 0.3, fill = "red")+
  annotate("text",label = "PM",
           y=87,x= speedsummary$Time[212-11])+
  
  annotate("rect", xmin = speedsummary$Time[100-22], xmax = speedsummary$Time[122-22], 
           ymin = 25, ymax = 85, 
           alpha = 0.3, fill = "red")+
  annotate("text",label = "AM",
           y=87,x= speedsummary$Time[122-10-22])+
  
  
  # annotate("rect", xmin = speedsummary$Time[17], xmax = speedsummary$Time[24], 
  #          ymin = 1840, ymax = 2040, 
  #          alpha = 0.3, fill = "red")+
  annotate("text",label = "Peak",
           y=30,x= speedsummary$Time[122-10-22],fontface =1,size=2.3)+
  
  annotate("text",label = "Peak",
           y=30,x= speedsummary$Time[212-10],fontface =1,size=2.3)
print (plotting_object)
ggsave("Speed.svg", width=10, height=2.5)


############################################################################################################
##How are the speeds during dry vs wet conditions
drydata<-trafficDF[which(trafficDF$Conditions=="Dry"),]

speedsummary<-aggregate(drydata$Speed..MPH., list(Time = drydata$Time), mean)
names(speedsummary)[2]='averagespeed'
speedmaxsummary<-aggregate(drydata$Speed..MPH., list(Time = drydata$Time), max)
names(speedmaxsummary)[2]='maxspeed'
speedminsummary<-aggregate(drydata$Speed..MPH., list(Time = drydata$Time), sd)
names(speedminsummary)[2]='minspeed'

speeddata<-speedsummary %>% inner_join(speedmaxsummary, by='Time')%>% inner_join(speedminsummary, by='Time')


##How are the speeds during dry vs wet conditions
wetdata<-trafficDF[which(trafficDF$Conditions=="Wet" | trafficDF$Conditions=="Chemically Wet"),]


speedsummaryW<-aggregate(wetdata$Speed..MPH., list(Time = wetdata$Time), mean)
names(speedsummaryW)[2]='averagespeed'
speedmaxsummaryW<-aggregate(wetdata$Speed..MPH., list(Time = wetdata$Time), max)
names(speedmaxsummaryW)[2]='maxspeed'
speedminsummaryW<-aggregate(wetdata$Speed..MPH., list(Time = wetdata$Time), sd)
names(speedminsummaryW)[2]='minspeed'

speeddataW<-speedsummaryW %>% inner_join(speedmaxsummaryW, by='Time')%>% inner_join(speedminsummaryW, by='Time')
speeddata<-speeddata %>% inner_join(speeddataW, by='Time')


#Speed Graph
plotting_object <- ggplot(speeddata,aes(Time, group = 1)) +
  
  geom_line( aes(x = speeddata$Time, y = speeddata$averagespeed.x,colour="Average Speed  (Dry)"),size=1) +
  theme(legend.key=element_blank(),legend.position="top",
        legend.spacing.y = unit(0.01,"cm"),legend.margin=margin(c(0,0,0,0)))+
  scale_colour_manual("",values=c("black","blue"))+
  scale_y_continuous(breaks=seq(0, 100, 10))+
  
  scale_x_datetime( breaks = date_breaks("1 hour"), 
                    labels = date_format("%H:%M"), 
                    expand = c(.04,-.040))+
  
 # scale_y_continuous(breaks=seq(0, 100, 10))+
  ylab("Average Speed (mph)") +
  xlab("Time of Day")+
  #ggtitle("Average Speed at Capitol Drive 41 Southbound")+
  # scale_x_datetime( breaks = date_breaks("1 hour"), 
  #                   labels = date_format("%H:%M"), 
  #                   expand = c(.04,-.040))+
  #scale_x_datetime(limits =lims, breaks=date_breaks("4 hour"), labels=date_format("%H:%M"))+
  #scale_x_discrete()+
  geom_ribbon(data=speeddata,aes(ymin=speeddata$averagespeed.x-speeddata$minspeed.x*1.96,
                                 ymax=speeddata$averagespeed.x+speeddata$minspeed.x*1.96,fill="Speed Range (Dry)"),alpha=0.3
  )+
  
  scale_fill_manual("",values=c("gold1","steelblue2"))+
  
  theme(axis.text.x=element_text(angle=90), panel.background = element_blank())+
  annotate("rect", xmin = speedsummary$Time[190], xmax = speedsummary$Time[212], 
           ymin = 25, ymax =85, 
           alpha = 0.3, fill = "red")+
  annotate("text",label = "PM",
           y=87,x= speedsummary$Time[212-11])+
  
  annotate("rect", xmin = speedsummary$Time[100-22], xmax = speedsummary$Time[122-22], 
           ymin = 25, ymax = 85, 
           alpha = 0.3, fill = "red")+
  annotate("text",label = "AM",
           y=87,x= speedsummary$Time[122-10-22])+
  
  
  # annotate("rect", xmin = speedsummary$Time[17], xmax = speedsummary$Time[24], 
  #          ymin = 1840, ymax = 2040, 
  #          alpha = 0.3, fill = "red")+
  annotate("text",label = "Peak",
           y=30,x= speedsummary$Time[122-10-22],fontface =1,size=2.3)+
  
  annotate("text",label = "Peak",
           y=30,x= speedsummary$Time[212-10],fontface =1,size=2.3)+
  
  geom_line( aes(x = speeddata$Time, y = speeddata$averagespeed.y,colour="Average Speed  (Wet)"),size=1) +
  theme(legend.key=element_blank(),legend.position="top",
        legend.spacing.y = unit(0.01,"cm"),legend.margin=margin(c(0,0,0,0)))+
  geom_ribbon(data=speeddata,aes(ymin=speeddata$averagespeed.y-speeddata$minspeed.y*1.96,
                                 ymax=speeddata$averagespeed.y+speeddata$minspeed.y*1.96,fill="Speed Range (Wet)"),alpha=0.3
  )
  #scale_colour_manual("",values=c("black","blue")
  #scale_y_continuous(breaks=seq(0, 100, 10))+
  
  # scale_x_datetime( breaks = date_breaks("1 hour"), 
  #                   labels = date_format("%H:%M"), 
  #                   expand = c(.04,-.040))
print (plotting_object)
ggsave("SpeedDryWet.svg", width=10, height=8)