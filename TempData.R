library(ggplot2)
library(data.table)
library(patchwork)
library(reshape2)
library(viridis)
library(nlme)
library(lme4)
library(ggridges)
#library(rstatix) #seems to mess up dplyr
library(lubridate)
library(readxl)
library(dplyr)
library(rTPC)

colm<- viridis_pal(option = "mako")(8)
cols<- colm[c(2,4,7)]
cols2<- colm[c(2,6)]

# #toggle between desktop (y) and laptop (n)
# desktop<- "y"
# if(desktop=="y") setwd("/Users/laurenbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/")
# if(desktop=="n") setwd("/Users/lbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/")

tdat<- read.csv("./data/PrapaeGardenTemps_WARP.csv")

#----------------
#Plot time series
ggplot(tdat, aes(x=dt,y=value, color=T)) + 
  geom_line()+facet_grid(Year~.) + guides(color="none")

#drop 2024 air temperature
tdat<- tdat[-which(tdat$T=="Logger3.T4.shadedT"),]

#average across sensors
tdat.mean <- tdat %>%
  dplyr::group_by(dt, Date, Time, Year) %>%
  dplyr::summarise(Tmean = mean(value, na.rm = TRUE), n=length(value)) 

#-----
#plot distributions for overlapping data, #1997: 224-238; 1999: 227:237
Tdist.plot <- ggplot(tdat.mean[which(tdat.mean$dt>223 & tdat.mean$dt<238),], aes(x=Tmean, color=factor(Year), fill=factor(Year), group=factor(Year))) +  geom_density(alpha=0.5)+
  scale_fill_viridis_d() +scale_color_viridis_d()+
  theme_classic(base_size = 18)+
  labs(x = "Temperature (°C)", color = "Year", fill="Year") 

#restrict to daylight 223 238 Aug 11-26; Aug 11: 6,8:30; Aug 26: 6:20-8
#tdat.mean$dec.dt<- tdat.mean$dt - floor(tdat.mean$dt)
#tdat.day<- tdat.mean[which(tdat.mean$dec.dt>0.25 & tdat.mean$dec.dt<0.85),]
#tdat.day<- tdat.day[which(tdat.day$dt>223 & tdat.day$dt<238),]

Tdist.day.plot <- ggplot(tdat.mean, aes(x=Tmean, color=factor(Year), fill=factor(Year), group=factor(Year))) +  geom_density(alpha=0.5)+
  scale_fill_viridis_d() +scale_color_viridis_d()+
  theme_classic(base_size = 18)+ xlim(0,40)+
  labs(title="doy 223-238, daytime" , x = "Temperature (°C)", color = "Year", fill="Year") 

#hourly max, min
tdat.mean.hr <- tdat %>%
  dplyr::group_by(Date, hr, Year) %>%
  dplyr::summarise(Tmin = min(value, na.rm = TRUE), Tmax = max(value, na.rm = TRUE), Tmean = mean(value, na.rm = TRUE), n=length(value), .groups = 'drop')

Tdist.hr.plot <- ggplot(tdat.mean.hr, aes(x=Tmax, color=factor(Year), fill=factor(Year), group=factor(Year))) +  geom_density(alpha=0.5)+
  scale_fill_viridis_d() +scale_color_viridis_d()

#-------------------------
#temperatures during experiments
# OPUS 1999: 15-25 Aug 1999; doy 227-237, 
#From Joel March 24: The selection experiment in 1999 ran from Aug 11-25, with most individuals first placed in the garden on aug 12-13. We reset and monitored the field temperature measurements on Aug 11, but there a logging issue so that the data are only reliable starting Aug 15

# KingsolverGenetica2001 says July 28-Aug 5; doy 209-217 (Fig 8)
# KingsolverGomulkiewicz2003 uses 195 to 217; check Fig 3 distribution

# 2024: June 22-July 4; July 28-Aug 9; doys 173-185, 209-221
# 2023: 190-202, 212-226
tdat.mean$doy<- floor(tdat.mean$dt)
inds1<- which(tdat.mean$Year==1999 & tdat.mean$doy %in% 227:237)
#inds1<- which(tdat.mean$Year==1999 & tdat.mean$doy %in% 209:217)
inds2<- which(tdat.mean$Year==2024 & tdat.mean$doy %in% 173:185)
inds3<- which(tdat.mean$Year==2024 & tdat.mean$doy %in% 209:221)
#inds4<- which(tdat.mean$Year==2023 & tdat.mean$doy %in% 190:202)
#inds5<- which(tdat.mean$Year==2023 & tdat.mean$doy %in% 212:226)

#add study
tdat.mean$study<- NA
tdat.mean$study[inds1]<- "Aug 1999"
tdat.mean$study[inds2]<- "June 2024"
tdat.mean$study[inds3]<- "July 2024"
#tdat.mean$study[inds4]<- "July 2023"
#tdat.mean$study[inds5]<- "Aug 2023"
# temporally order studies
tdat.mean$study<- factor(tdat.mean$study, levels=c("Aug 1999", "June 2024", "July 2024"), ordered=TRUE)

Tdist.exp.plot <- ggplot(tdat.mean[which(!is.na(tdat.mean$study)),], aes(x=Tmean, color=factor(study), fill=factor(study), group=factor(study))) +  geom_density(alpha=0.5)+
  scale_fill_viridis_d() +scale_color_viridis_d()

# just daylight
#tdat.day<- tdat.mean[which(tdat.mean$dec.dt>0.25 & tdat.mean$dec.dt<0.85),]

Tdist.exp.plot <- ggplot(tdat.mean[which(!is.na(tdat.mean$study)),], aes(x=Tmean, color=factor(study), fill=factor(study), group=factor(study), lty=factor(Year))) +  geom_density(alpha=0.5)+
  scale_fill_manual(values=cols) +scale_color_manual(values=cols)+
  #scale_fill_viridis_d(option = "G") +scale_color_viridis_d(option = "G")+
  theme_classic(base_size = 18)+ xlim(0,42)+
  labs(x = "Temperature (°C)", color = "Study", fill="Study", y="Density") +theme(legend.position = c(0.9, 0.9))+
  guides(lty="none")

#------
#Fig 1c. incidence of temperatures over 30C

mod1<- lm(Tmean~factor(study), data=tdat.mean)

#make variable whether temperature hot
tdat.mean$o30<- 0
tdat.mean$o30[tdat.mean$Tmean>=30]<- 1

tdat.mean

mod1 <- glm(o30~factor(study), family = binomial, data=tdat.mean) 

summary(mod1)
anova(mod1)

#==================

#Plot temperature distribution with selection during study period
# OPUS 1999: 15-25 Aug 1999; doy 227-237, 
# KingsolverGenetica2001 says July 28-Aug 5; doy 209-217 (Fig 8)
# KingsolverGomulkiewicz2003 uses 195 to 217; check Fig 3 distribution
#Compare to Figure 8 in KingsolverGenetica2001

Tplot<- tdat.mean[which(tdat.mean$dt>227 & tdat.mean$dt<238 & tdat.mean$Year %in% c(1999,2024)),]
#Tplot<- tdat.mean[which(tdat.mean$dt>209 & tdat.mean$dt<217 & tdat.mean$Year %in% c(1999,2024)),]
  
  Tdist.plot <- ggplot(Tplot, aes(x=Tmean, color=factor(Year), fill=factor(Year), group=factor(Year))) +  geom_density(alpha=0.5)+
    scale_fill_manual(values=cols2) +scale_color_manual(values=cols2)+
    theme_classic(base_size = 18)+
    labs(x = "Temperature (°C)", color = "Year", fill="Year") 
  
  #----
  #Fig 1a. shift in temp distribution through time
  #equality of variances
  Tplot<- as.data.frame(Tplot)
  Tplot$Year<- factor(Tplot$Year)
  
  #Tplot %>% levene_test(Tmean~ Year)
  
  # #welch test
  # Tplot %>% 
  #   t_test(Tmean~ Year) %>%
  #   add_significance()
  # 
  # #equal variances t-test
  # Tplot %>% 
  #   t_test(Tmean~ Year, var.equal = TRUE) %>%
  #   add_significance()
  
  #----
  #load and plot short term growth rate
 
  tpc1<- read.csv("data/TPCconstant_past.csv")
  
  #estimates means across temperatures
  tpc.agg.f2 <- tpc1 %>%
    group_by(temp) %>% 
    dplyr::summarise(
      mean = mean(rgrlog, na.rm = TRUE),
      n= length(rgrlog),
      se = sd(rgrlog, na.rm = TRUE) / sqrt(n)
    )
  
  tpc.agg.f2 <- as.data.frame(tpc.agg.f2)
  tpc.agg.f2$Year= 1999
  
  #beta TPC parameters, estimated in archcive code/TPCfits.R
  beta.params= c(0.03134642, 34.65487908, 100.71536023, 11.55893992, 1.76555039)
  
  temps<- 1:50
  tpc.dat<- cbind(temps, beta_2012(temp = temps, a=beta.params[1], b=beta.params[2], c=beta.params[3], d=beta.params[4], e=beta.params[5]))
  colnames(tpc.dat)<- c("temp", "rate")
  tpc.dat <- as.data.frame(tpc.dat)
  tpc.dat$Year= 1999
  
  #Figure 2, mean short term mass-specific growth rate, from kingsolver 2000, 
  #selection gradient
  #Figure 7
  temps= c(11,17,23,29,35)
  pm.sg= c(0.338,0.182, 0.190, -0.233, -0.217) #selection gradient for pupal mass
  sg= as.data.frame(cbind(temps, pm.sg))
  sg$Year=1999
  
  #--------
  #add y values 
  #load data
  tpc<- read.csv("./data/PrapaeGardenExpt_WARP.csv")
  #drop unneeded columns
  tpc<- tpc[,-which(colnames(tpc) %in% c("X", "Species", "Population", "Study.Site"))]
  
  #update experiment labels
  expts<- c("june","july","aug")
  expts.lab<- c("June 2024","July 2024","Aug 1999")
  
  tpc$expt <- expts.lab[match(tpc$expt,expts)]
  tpc$expt <- factor(tpc$expt, levels= c("Aug 1999","June 2024","July 2024"), ordered=TRUE)
  
  #to long format
  tpc.l <- melt(tpc, id.vars = c("Mom", "ID", "f.ind", "expt","period","Mi","Pupa.wt","Fecundity", "Time.to.Pupation","Pupated","Eclosed","Time.to.Eclosion","Sex","Butt..Wt"), variable.name = "temp")
  tpc.l$temp= as.numeric(gsub("RGR","",tpc.l$temp))
  tpc.l$value= as.numeric(tpc.l$value)
  
  #estimate family mean values
  tpc.agg.f <- tpc.l %>% 
    group_by(Mom, temp, expt, period) %>% 
    dplyr::summarise(mean = mean(value, na.rm = TRUE),
                     se = sd(value, na.rm = TRUE)/length(value) )
  
  #estimate temperature mean values
  tpc.agg <- tpc.l %>% 
    group_by(temp, period) %>% 
    dplyr::summarise(mean = mean(value, na.rm = TRUE),
                     se = sd(value, na.rm = TRUE)/sqrt(length(value)) )
  
  tpc.agg.h<- as.data.frame(tpc.agg[tpc.agg$period=="past",])
  tpc.agg.h$Year<- 1999
  
  #--------
  #past tpc data
  inds<- match(sg$temps, tpc.agg.h$temp)
  sg$ys<- tpc.agg.h$mean[inds]
  
  #plot
  plot.sel<- Tdist.plot +
    #No longer: add TPC means
    #geom_point(data=tpc.agg.f2, aes(x=temp, y=mean))+
    #add beta fit
    #geom_line(data=tpc.dat, aes(x=temp, y=rate))+
    #add 1999 feeding data
    geom_point(data=tpc.agg.h, aes(x=temp, y=mean))+
    geom_line(data=tpc.agg.h, aes(x=temp, y=mean))+
    #add selection arrows
    geom_segment(data=sg, aes(x = temps, y = ys, xend = temps, yend = ys+pm.sg/20),
                 arrow = arrow(length = unit(0.5, "cm")), lwd=1.2)+
    xlim(0,42)+ theme(legend.position = c(0.8, 0.8))+
  #add additional axis
  scale_y_continuous(
    name = "Growth rate (g/g/h)", 
    sec.axis = sec_axis(~.x * 1, 
                        name = "Density of temperatures"))
  
  #==================
  #Estimate growth rate over temperatures
  
  Tplot$rgr<- beta_2012(temp = Tplot$Tmean, a=beta.params[1], b=beta.params[2], c=beta.params[3], d=beta.params[4], e=beta.params[5])
  
  #plot growth rate distributions
  RGRdist.plot <- ggplot(Tplot, aes(x=rgr, color=factor(Year), fill=factor(Year), group=factor(Year))) +  geom_density(alpha=0.5)+
    scale_fill_viridis_d() +scale_color_viridis_d()+
    theme_classic(base_size = 18)+
    labs(x = "Growth rate (g/g/h)", color = "Year", fill="Year") 
  
  #Plot like Fig 3 in KingsolverGomulkiewicz2003
  #fraction time spent at each temperature
  #1999
  thist= unlist(Tplot[which(Tplot$Year==1999),"Tmean"])
  thist= hist(thist, freq=TRUE, breaks=floor(min(thist)):ceiling(max(thist)) )
  
  #Then Z(T) is the proportion of total growth that results from growth at a particular temperature, and is simply the weighted average of growth rate at temperature T and the fraction of time spent at that temperature, f(T):
  thist1= thist$density*beta_2012(temp = thist$mids, a=beta.params[1], b=beta.params[2], c=beta.params[3], d=beta.params[4], e=beta.params[5])
  ZT= thist1 / sum(thist1)
  ZT= as.data.frame(cbind(temp=thist$mids, z= ZT, Year=1999))
  
  #2024 calculation
  thist= unlist(Tplot[which(Tplot$Year==2024),"Tmean"])
  thist= hist(thist, freq=TRUE, breaks=floor(min(thist)):ceiling(max(thist)) )
  
  #Then Z(T) is the proportion of total growth that results from growth at a particular temperature, and is simply the weighted average of growth rate at temperature T and the fraction of time spent at that temperature, f(T):
  thist1= thist$density*beta_2012(temp = thist$mids, a=beta.params[1], b=beta.params[2], c=beta.params[3], d=beta.params[4], e=beta.params[5])
  ZTp= thist1 / sum(thist1)
  ZTp= as.data.frame(cbind(temp=thist$mids, z= ZTp, Year=2024))
  
  #combine
  ZT= rbind(ZT, ZTp)
  
  #----
  TZdist.plot <- ggplot(Tplot, aes(x=Tmean, color=factor(Year), lty=factor(Year) )) +  
    geom_density(data=Tplot, alpha=0.5, aes(fill=factor(Year)))+
    scale_fill_manual(values=cols2) +scale_color_manual(values=cols2)+
    theme_classic(base_size = 18)+
    labs(x = "Temperature (°C)") +
      #add growth rate distribution
      geom_line(data=ZT, aes(x=temp, y=z), linewidth=0.75)+
    xlim(0,42)+
    ylab("Densities")+theme(legend.position = "none")+ 
    #add 1999 feeding data
    geom_point(data=tpc.agg.h, aes(x=temp, y=mean))+
    geom_line(data=tpc.agg.h, aes(x=temp, y=mean), lwd=1.4)+
    #add selection arrows
    geom_segment(data=sg, aes(x = temps, y = ys, xend = temps, yend = ys+pm.sg/20),
                 arrow = arrow(length = unit(0.5, "cm")), lwd=1.2)+
    xlim(0,42)+ theme(legend.position = c(0.8, 0.8))+
  #add additional axis
  scale_y_continuous(
    name = "Density", 
    sec.axis = sec_axis(~.x * 1, 
                        name = "Growth rate (g/g/h)"))+ 
    labs(lty="Year", color="Year", fill="Year")
  #solid is f(T); dashed is Z(T)
  
  #----
  #Fig 1b. shift in temp distribution through time
  #equality of variances
  ZT<- as.data.frame(ZT)
  ZT$Year<- factor(ZT$Year)
  
  mod1 <- lm(z~temp*factor(Year), data=ZT) 
  
  #make variable whether temperature hot
  ZT$o30<- 0
  ZT$o30[ZT$temp>=30]<- 1
  
  mod1 <- glm(z~o30*factor(Year), data=ZT) 
  
  summary(mod1)
  anova(mod1)
  
  #==================
  #Temp distributions over time  
  
  #GHCND date
  #find stations: https://ncics.org/portfolio/monitor/ghcn-d-station-data/ 
  # SEATTLE SAND POINT WEATHER FORECAST OFFICE, WA US (USW00094290)
  
  t.dat<- read.csv("data/GHCNdata/USW00094290_2025.csv")
  t.dat$site="Seattle"
  
  t.dat$tmin= t.dat$TMIN /10 #divide by ten due to GHCND format
  t.dat$tmax= t.dat$TMAX /10
  
  t.dat$month= round(month(as.POSIXlt(t.dat$DATE)))
  t.dat$year= year(as.POSIXlt(t.dat$DATE))
  #restrict to growing season
  t.dat= t.dat[which(t.dat$month %in% c(6,7,8)),] 
  
  #code season
  #month 3,4 vs 7,8
  t.dat$season<- NA
  t.dat$season[which(t.dat$month %in% c(4:5))] ="spring"
  t.dat$season[which(t.dat$month %in% c(7:8))] ="summer"
  
  #restrict years
  t.dat= t.dat[which(t.dat$year %in% c(1994:2024)),]
  t.dat1= t.dat[which(t.dat$year %in% c(1990:1999)),]
  #or 87-92, 1990:1994
  t.dat1$period="1990-1999"
  t.dat2= t.dat[which(t.dat$year %in% c(2015:2024)),]
  t.dat2$period="2015-2024"
  #combine
  t.dat= rbind(t.dat1,t.dat2)
  
  #dtr
  dtr=function(T_max, T_min, t=7:18){
    gamma= 0.44 - 0.46* sin(0.9 + pi/12 * t)+ 0.11 * sin(0.9 + 2 * pi/12 * t);   # (2.2) diurnal temperature function
    T = T_max*gamma + T_min - T_min*gamma
    return(T)
  }
  
  temps= sapply(t.dat$tmax, FUN="dtr", T_min=t.dat$tmin)
  temps= as.data.frame(t(temps))
  temps$period= t.dat$period
  temps$site= t.dat$site
  #temps$season= t.dat$season
  temps$month= t.dat$month
  
  #to long format
  temps1<- melt(temps, id.vars=c("period","site","month"))
 
  #order months colors to correspond to 1c
  cols.month<- colm[c(4,7,2)]
  
  #density plot
   month.plot<- ggplot(temps1, aes(x=value, y=month, color=factor(month), fill=factor(month), lty=period))+
    geom_density_ridges(lwd=1.2, alpha=0.5)+
    scale_color_manual(values=cols.month)+scale_fill_manual(values=cols.month)+
    xlim(0,42)+
    ylim(5.9, 9.5)+
    xlab("Temperature (°C)") +ylab("Month")+ 
    guides(fill="none", color="none")+ labs(lty="Years")+
    theme_classic(base_size = 18)+theme(legend.position = c(0.9, 0.9))
  
  #min, max distributions
  ggplot(t.dat, aes(x=tmin, color=period, fill=period))+
    facet_wrap(~month)+
    geom_density(alpha=0.5)+
    scale_color_manual(values=cols.month)+scale_fill_manual(values=cols.month)+
    xlim(-20,30)+
    xlab("Temperature (°C)")+
    theme_bw(base_size = 18)
  
  ggplot(t.dat, aes(x=tmax, color=period, fill=period))+
    facet_wrap(~month)+
    geom_density()+scale_color_viridis_d()+scale_fill_viridis_d(alpha=0.5)+
    xlim(0,45)+
    xlab("Temperature (°C)")+
    theme_bw(base_size = 18)
  
  #------------
  #Test increasing incidence of warm temperatures
  #make variable whether temperature hot
  mod1<- lm(value~month*period, data=temps1)
  
  temps1$o30<- 0
  temps1$o30[temps1$temp>=25]<- 1
  
  mod1 <- glm(o30~month+period, family=binomial, data=temps1) 
  
  summary(mod1)
  anova(mod1)
  
  #----------------
  #Save figures

  # design <- "AB
  #              CD"
  design<- "A
  B
  C"
  
  pdf("figures/Fig1_Tdist.pdf",height = 10, width = 6)
  month.plot + TZdist.plot + Tdist.exp.plot + plot_layout(design=design) +plot_annotation(tag_levels = 'A')
  dev.off()
  #=============================