library(ggplot2)
library(data.table)
library(dplyr)
library(patchwork)
library(reshape2)
library(viridis)
library(nlme)
library(lme4)
library(stringr)

# Load required libraries
library(lubridate)
library(tidyr)
library(readxl)

#Taylor version: https://github.com/taylorhatcher/WARP2024/tree/main

#toggle between desktop (y) and laptop (n)
desktop<- "y"

if(desktop=="y") setwd("/Users/laurenbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/data/WeatherData/")
if(desktop=="n") setwd("/Users/lbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/data/WeatherData/")

#1997
temp.1997= read.csv("CUH.metdata.1997_viii12_19.csv") #8/12 - 8/26
temp.1997$Period<- 1
temp2.1997= read.csv("CUH.metdata.1997_viii20_26.csv")
temp2.1997$Period<- 2
temp.1997= rbind(temp.1997,temp2.1997)
#format time
min <- str_sub(temp.1997$Time,-2,-1)
min[min %in% c("0","5")]<- paste("0", str_sub(min[min %in% c("0","5")],-1,-1), sep="")
hr<- str_sub(temp.1997$Time,-4,-3)
hr[hr==""]<- "0"
temp.1997$hm<- paste(hr, min, sep=":")
temp.1997$hr<- as.numeric(hr); temp.1997$min<- as.numeric(min)
temp.1997$dt<- temp.1997$Date + temp.1997$hr/24 + temp.1997$min/(24*60)

#1999
temp.1999= read.csv("FormattedHistoricMetDataFieldSln.csv") #8/18-8/25
temp.1999$hr<- as.numeric(str_sub(temp.1999$LONGTIME,-5,-4))
temp.1999$dt<- temp.1999$JDATE + as.numeric(str_sub(temp.1999$LONGTIME,-5,-4))/24 +as.numeric(str_sub(temp.1999$LONGTIME,-2,-1))/(24*60)

#2024
temp.2024= read.csv("combined_loggers_2024.csv") #6/21 - 8/18
#format time  
jday<- as.numeric(format(as.Date(temp.2024$datetime, format = "%Y-%m-%d %H:%M:%S"), "%j"))
temp.2024$hr<- as.numeric(format(strptime(temp.2024$datetime, format = "%Y-%m-%d %H:%M:%S"), "%H"))
min<- as.numeric(format(strptime(temp.2024$datetime, format = "%Y-%m-%d %H:%M:%S"), "%M"))
temp.2024$dt= jday +temp.2024$hr/24 + min/(24*60)
temp.2024$Date<- format(strptime(temp.2024$datetime, format = "%Y-%m-%d %H:%M:%S"), "%Y-%m-%d")
temp.2024$Time<- format(strptime(temp.2024$datetime, format = "%Y-%m-%d %H:%M:%S"), "%H:%M")

#change to long format and combine
t1997.l <- melt(temp.1997[,c("dt","Date","Time","hr","Tm1","Tm2","Tm3","Tm4","Tm5","Tm6","Tm7","Tm8","Tm9","Tm10")], id.vars = c("dt","Date","Time","hr"), variable.name = "T")
t1997.l$Year<- 1997

t1999.l <-  temp.1999[,c("dt","DATE","TIME","hr","Variable", "Value","YEAR")]
names(t1999.l) <- c("dt","Date","Time","hr","T", "value","Year")

t2024.l <- melt(temp.2024[,c("dt","Date","Time", "hr", "Logger1.T1","Logger1.T2","Logger1.T3","Logger1.T4","Logger2.T1","Logger2.T2","Logger2.T3","Logger2.T4","Logger3.T1","Logger3.T2","Logger3.T3","Logger3.T4.shadedT")], id.vars = c("dt","Date","Time","hr"), variable.name = "T")
#drop NAs
t2024.l<- t2024.l[which(!is.na(t2024.l$value)),]
t2024.l$Year<- 2024

#combine
tdat<- rbind(t1997.l, t1999.l, t2024.l)

#----------------
#Plot time series
ggplot(tdat, aes(x=dt,y=value, color=T)) + 
  geom_line()+facet_grid(Year~.) + guides(color="none")

#drop 2024 air temperature
tdat<- tdat[-which(tdat$T=="Logger3.T4.shadedT"),]

#average across sensors
tdat.mean <- tdat %>%
  group_by(dt, Date, Time, Year) %>%
  summarise(Tmean = mean(value, na.rm = TRUE), n=length(value))

#-----
#plot distributions for overlapping data, #1997: 224-238; 1999: 227:237
Tdist.plot <- ggplot(tdat.mean[which(tdat.mean$dt>223 & tdat.mean$dt<238),], aes(x=Tmean, color=factor(Year), fill=factor(Year), group=factor(Year))) +  geom_density(alpha=0.5)+
  scale_fill_viridis_d() +scale_color_viridis_d()+
  theme_classic(base_size = 18)+
  labs(x = "T mean (°C)", color = "Year", fill="Year") 

#restrict to daylight 223 238 Aug 11-26; Aug 11: 6,8:30; Aug 26: 6:20-8
tdat.mean$dec.dt<- tdat.mean$dt - floor(tdat.mean$dt)
tdat.day<- tdat.mean[which(tdat.mean$dec.dt>0.25 & tdat.mean$dec.dt<0.85),]
tdat.day<- tdat.day[which(tdat.day$dt>223 & tdat.day$dt<238),]

Tdist.day.plot <- ggplot(tdat.day, aes(x=Tmean, color=factor(Year), fill=factor(Year), group=factor(Year))) +  geom_density(alpha=0.5)+
  scale_fill_viridis_d() +scale_color_viridis_d()+
  theme_classic(base_size = 18)+ xlim(0,40)+
  labs(title="doy 223-238, daytime" , x = "T mean (°C)", color = "Year", fill="Year") 

#hourly max, min
tdat.mean.hr <- tdat %>%
  group_by(Date, hr, Year) %>%
  summarise(Tmin = min(value, na.rm = TRUE), Tmax = max(value, na.rm = TRUE), Tmean = mean(value, na.rm = TRUE), n=length(value), .groups = 'drop')

Tdist.hr.plot <- ggplot(tdat.mean.hr, aes(x=Tmax, color=factor(Year), fill=factor(Year), group=factor(Year))) +  geom_density(alpha=0.5)+
  scale_fill_viridis_d() +scale_color_viridis_d()

#-------------------------
#temperatures during experiments
# 1999: 15-25 Aug 1999; doy 227-237
# 2024: June 22-July 4; July 28-Aug 9; doys 173-185, 209-221
tdat.mean$doy<- floor(tdat.mean$dt)
inds1<- which(tdat.mean$Year==1999 & tdat.mean$doy %in% 227:237)
inds2<- which(tdat.mean$Year==2024 & tdat.mean$doy %in% 173:185)
inds3<- which(tdat.mean$Year==2024 & tdat.mean$doy %in% 209:221)
#add study
tdat.mean$study<- NA
tdat.mean$study[inds1]<- "Aug 1999"
tdat.mean$study[inds2]<- "June 2024"
tdat.mean$study[inds3]<- "July 2024"

Tdist.exp.plot <- ggplot(tdat.mean[which(!is.na(tdat.mean$study)),], aes(x=Tmean, color=factor(study), fill=factor(study), group=factor(study))) +  geom_density(alpha=0.5)+
  scale_fill_viridis_d() +scale_color_viridis_d()

# just daylight
tdat.day<- tdat.mean[which(tdat.mean$dec.dt>0.25 & tdat.mean$dec.dt<0.85),]

Tdist.expday.plot <- ggplot(tdat.day[which(!is.na(tdat.day$study)),], aes(x=Tmean, color=factor(study), fill=factor(study), group=factor(study))) +  geom_density(alpha=0.5)+
  scale_fill_viridis_d() +scale_color_viridis_d()+
  theme_classic(base_size = 18)+ xlim(0,40)+
  labs(title="experiments, daytime" , x = "T mean (°C)", color = "Study", fill="Study") 

#----------------
#Save figures
  if(desktop=="y") setwd("/Users/laurenbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/figures/")
  if(desktop=="n") setwd("/Users/lbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/figures/")
  
pdf("Fig_Tdist.pdf",height = 6, width = 12)
Tdist.day.plot + Tdist.expday.plot
  dev.off()

  #==================
#Plot temperature distribution with selection 
  Tplot<- tdat.mean[which(tdat.mean$dt>227 & tdat.mean$dt<238 & tdat.mean$Year %in% c(1999,2024)),]
  
  Tdist.plot <- ggplot(Tplot, aes(x=Tmean, color=factor(Year), fill=factor(Year), group=factor(Year))) +  geom_density(alpha=0.5)+
    scale_fill_viridis_d() +scale_color_viridis_d()+
    theme_classic(base_size = 18)+
    labs(x = "T mean (°C)", color = "Year", fill="Year") 
  
  
  #compute from GardenExpt2024
  #tpc.agg.h
  
  #Figure 2, mean short term mass-specific growth rate, from kingsolver 2000, 
  #selection gradient
  #Figure 7
  temps= c(11,17,23,29,35)
  pm.sg= c(0.338,0.182, 0.190, -0.233, -0.217) #selection gradient for pupal mass
  sg= as.data.frame(cbind(temps, pm.sg))
  #add y values
  sg$ys= tpc.agg.h$mean
  sg$Year=1999
  
  plot.sel<- Tdist.plot +
    #add TPC means
    geom_point(data=sg, aes(x=temps, y=ys))+
    geom_line(data=sg, aes(x=temps, y=ys))+
  #add selection arrows
  geom_segment(data=sg, aes(x = temps, y = ys, xend = temps, yend = ys+pm.sg/20),
                        arrow = arrow(length = unit(0.5, "cm")), lwd=1.2)
  
  #==================
  #Temp distributions over time  
  
  #GHCND date
  #find stations: https://ncics.org/portfolio/monitor/ghcn-d-station-data/ 
  # SEATTLE SAND POINT WEATHER FORECAST OFFICE, WA US (USW00094290)
  
  if(desktop=="y") setwd("/Users/laurenbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/data/GHCNdata/")
  if(desktop=="n") setwd("/Users/lbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/data/GHCNdata/")
  
  t.dat<- read.csv("USW00094290.csv")
  t.dat$site="Seattle"
  
  t.dat$tmin= t.dat$TMIN/10 #divide by ten
  t.dat$tmax= t.dat$TMAX/10
  t.dat$month= round(month(as.POSIXlt(t.dat$DATE)))
  t.dat$year= year(as.POSIXlt(t.dat$DATE))
  #restrict to growing season
  t.dat= t.dat[which(t.dat$month %in% c(4,5,7,8)),] 
  
  #code season
  #month 3,4 vs 7,8
  t.dat$season<- NA
  t.dat$season[which(t.dat$month %in% c(4:5))] ="spring"
  t.dat$season[which(t.dat$month %in% c(7:8))] ="summer"
  
  #restrict years
  t.dat= t.dat[which(t.dat$year %in% c(1987:2021)),]
  t.dat1= t.dat[which(t.dat$year %in% c(1991:1994)),]
  #or 87-92, 1990:1994
  t.dat1$period="initial"
  t.dat2= t.dat[which(t.dat$year %in% c(2016,2018,2019,2020)),]
  t.dat2$period="recent"
  #combine
  t.dat= rbind(t.dat1,t.dat2)
  
  #just day
  dtr=function(T_max, T_min, t=7:18){
    gamma= 0.44 - 0.46* sin(0.9 + pi/12 * t)+ 0.11 * sin(0.9 + 2 * pi/12 * t);   # (2.2) diurnal temperature function
    T = T_max*gamma + T_min - T_min*gamma
    return(T)
  }
  
  temps= sapply(t.dat$tmax, FUN="dtr", T_min=t.dat$tmin)
  temps= as.data.frame(t(temps))
  temps$period= t.dat$period
  temps$site= t.dat$site
  temps$season= t.dat$season
  
  #to long format
  temps1<- melt(temps, id.vars=c("period","site","season"))
  
  #density plot
  ggplot(temps1, aes(x=value, lty=period,color=site))+
    facet_wrap(~season)+
    geom_density(lwd=1.2)+scale_color_manual(values=c("orange","blue","darkgreen"))+
    xlim(0,45)+
    xlab("Temperature (°C)")+
    theme_bw(base_size = 18)+theme(legend.position = c(0.6, 0.7))
  
  #min, max distributions
  ggplot(t.dat, aes(x=tmin, lty=period,color=site))+
    facet_wrap(~season)+
    geom_density()+scale_color_manual(values=c("blue","orange","darkgreen"))+
    xlim(-20,30)+
    xlab("Temperature (°C)")+
    theme_bw(base_size = 18)
  
  ggplot(t.dat, aes(x=tmax, lty=period,color=site))+
    facet_wrap(~season)+
    geom_density()+scale_color_manual(values=c("blue","orange","darkgreen"))+
    xlim(0,45)+
    xlab("Temperature (°C)")+
    theme_bw(base_size = 18)
  
  

 