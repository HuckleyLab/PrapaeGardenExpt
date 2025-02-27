library(ggplot2)
library(data.table)
library(dplyr)
library(patchwork)
library(reshape2)
library(viridis)
library(nlme)
library(lme4)

#toggle between desktop (y) and laptop (n)
desktop<- "y"

if(desktop=="y") setwd("/Users/laurenbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/data/")
if(desktop=="n") setwd("/Users/lbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/data/")

tpc1= read.csv("2024_06_20_tpc_field_seln_june.csv")
tpc1 <- tpc1[,-c(44)]
tpc1$expt<- "june"

tpc2= read.csv("2024_07_27_tpc_field_seln_july_batch_1.csv")
tpc2<- tpc2[,-which(colnames(tpc2)=="X29.notes.1")]
tpc3= read.csv("2024_07_28_tpc_field_seln_july_batch_2.csv")
tpc3<- tpc3[,-which(colnames(tpc3)=="X11.notes.1")]
tpc2= rbind(tpc2, tpc3)
tpc2$expt<- "july"
tpc2<- cbind(3, tpc2)
colnames(tpc2)[1]<- "Batch"
tpc2<- tpc2[,-which(colnames(tpc2)=="X23.notes.1")]

tpc= rbind(tpc1, tpc2)

#restrict to observations
tpc= tpc[!is.na(tpc$FEMALE),]
tpc= as.data.frame(tpc)

length(which(tpc$X23.mass.i<10)) # none less than 10mg
#take out
#tpc= tpc[-which(tpc$X23.mass.i<10),]

#estimate RGR = [log10(m f) -log10(m i)]/(t f − t i )
#forumulas in papers specify ln, but magnitude is too high

#tpc<- tpc[which(tpc$expt=="july"),]

#----
#fix formatting issues
# 23,11,29,35,17
#tpc[grep(":",tpc$X17.mass.f),]
tpc$X23.mass.f[133]<- 15.44
tpc$X11.mass.i[133]<- 15.44
tpc$X23.mass.f[139]<- 14.41

#fill in blank final times with weighing time
tpc$X11.time.f[which(is.na(tpc$X11.time.f))]<- tpc$weigh.time.2[which(is.na(tpc$X11.time.f))]
tpc$X29.time.f[which(is.na(tpc$X29.time.f))]<- tpc$weigh.time.3[which(is.na(tpc$X29.time.f))]
tpc$X35.time.f[which(is.na(tpc$X35.time.f))]<- tpc$weigh.time.4[which(is.na(tpc$X35.time.f))]
tpc$X17.time.f[which(is.na(tpc$X17.time.f))]<- tpc$weigh.time.5[which(is.na(tpc$X17.time.f))]
#fill in blank initial time with last weighing time
tpc$X29.time.i[which(is.na(tpc$X29.time.i))]<- tpc$X11.time.f[which(is.na(tpc$X29.time.i))]

#----
#estimate growth rate
tpc$rgr_23= (log10(as.numeric(tpc$X23.mass.f)*0.001)-log10(as.numeric(tpc$X23.mass.i)*0.001))/
  as.numeric(difftime(as.POSIXct(tpc$X23.time.f,format="%H:%M"), as.POSIXct(tpc$X23.time.i,format="%H:%M"), units='hours'))

tpc$rgr_29= (log10(as.numeric(tpc$X29.mass.f)*0.001)-log10(as.numeric(tpc$X29.mass.i)*0.001))/
  as.numeric(difftime(as.POSIXct(tpc$X29.time.f,format="%H:%M"), as.POSIXct(tpc$X29.time.i,format="%H:%M"), units='hours'))

#overnight
tpc$rgr_11= (log10(as.numeric(tpc$X11.mass.f)*0.001)-log10(as.numeric(tpc$X11.mass.i)*0.001))/
  as.numeric(difftime(as.POSIXct(paste("2023-06-21", tpc$X11.time.f, sep=" "),format="%Y-%m-%d %H:%M"), as.POSIXct(paste("2023-06-20", tpc$X11.time.i, sep=" "),format="%Y-%m-%d %H:%M"), units='hours'))

tpc$rgr_35= (log10(as.numeric(tpc$X35.mass.f)*0.001)-log10(as.numeric(tpc$X35.mass.i)*0.001))/
  as.numeric(difftime(as.POSIXct(tpc$X35.time.f,format="%H:%M"), as.POSIXct(tpc$X35.time.i,format="%H:%M"), units='hours'))

#overnight
tpc$rgr_17= (log10(as.numeric(tpc$X17.mass.f)*0.001)-log10(as.numeric(tpc$X17.mass.i)*0.001))/
  as.numeric(difftime(as.POSIXct(paste("2023-06-21", tpc$X11.time.f, sep=" "),format="%Y-%m-%d %H:%M"), as.POSIXct(paste("2023-06-20", tpc$X11.time.i, sep=" "),format="%Y-%m-%d %H:%M"), units='hours'))

# #account for caterpillars that didn't eat
# tpc$rgr_23[tpc$X23.notes %in% c("O", NA)]<- NA
# tpc$rgr_29[tpc$X29.notes %in% c("O", NA)]<- NA 
# tpc$rgr_11[tpc$X11.notes %in% c("O", NA)]<- NA 
# tpc$rgr_17[tpc$X17.notes %in% c("O", NA)]<- NA 
# tpc$rgr_35[tpc$X35.notes %in% c("O", NA)]<- NA 

tpc$f.ind= paste(tpc$FEMALE, tpc$INDV, tpc$expt, sep="_")
tpc= tpc[,c("FEMALE","f.ind","StartDate","rgr_23","rgr_29","rgr_11","rgr_17","rgr_35","expt")]

#to long format
tpc.l <- melt(setDT(tpc[,c("FEMALE","f.ind","rgr_23","rgr_29","rgr_11","rgr_17","rgr_35")]), id.vars = c("FEMALE","f.ind"), variable.name = "temp")
tpc.l$temp= as.numeric(gsub("rgr_","",tpc.l$temp))

#plot
tpc.plot= ggplot(tpc.l, aes(x=temp,y=value)) + #, group=f.ind
  geom_point()

#---
#plot family mean values
tpc.agg.f <- tpc.l %>% 
  group_by(FEMALE, temp) %>% 
  dplyr::summarise(mean = mean(value, na.rm = TRUE),
            se = sd(value, na.rm = TRUE)/length(value) )

#add means
tpc.plot= tpc.plot + 
  geom_line(data=tpc.agg.f, aes(x=temp, y = mean, group=FEMALE), linewidth=1, col="darkorange")

#---
#plot mean values
tpc.agg <- tpc.l %>% 
  group_by(temp) %>% 
  dplyr::summarise(mean = mean(value, na.rm = TRUE),
            se = sd(value, na.rm = TRUE)/sqrt(length(value)) )

#add means
tpc.plot= tpc.plot + 
  geom_errorbar(data=tpc.agg, aes(x=temp, y=mean, ymin=mean-se, ymax=mean+se), width=0, col="black")+
  geom_point(data=tpc.agg, aes(x=temp, y = mean), size=4, col="black", fill="darkgoldenrod1", pch=21)+
  theme_bw()+xlab("Temperature (C)")+ylab("RGR (g/g/h)")+
  ggtitle("2024") +ylim(-0.02,0.06)

#-------------------------
#Historic data

tpc.h= read.csv("PrapaeUW.Seln2.1999.Combineddata.OPUS2021.csv")

tpc.h$f.ind= paste(tpc.h$Mom, tpc.h$ID, sep="_")
tpc.h= tpc.h[,c("Mom","f.ind","RGR23","RGR29","RGR11","RGR17","RGR35")]

#to long format
tpc.lh <- melt(setDT(tpc.h), id.vars = c("Mom","f.ind"), variable.name = "temp")
tpc.lh$temp= as.numeric(gsub("RGR","",tpc.lh$temp))

#plot
tpc.plot.h= ggplot(tpc.lh, aes(x=temp,y=value)) + #, group=f.ind
  geom_point()

#---
#plot family mean values
tpc.agg.fh <- tpc.lh %>% 
  group_by(Mom, temp) %>% 
  dplyr::summarise(mean = mean(value, na.rm = TRUE),
            se = sd(value, na.rm = TRUE)/length(value) )

#add means
tpc.plot.h= tpc.plot.h + 
  geom_line(data=tpc.agg.fh, aes(x=temp, y = mean, group=Mom), size=1, col="blue1")

#---
#plot mean values
tpc.agg.h <- tpc.lh %>% 
  group_by(temp) %>% 
  dplyr::summarise(mean = mean(value, na.rm = TRUE),
            se = sd(value, na.rm = TRUE)/length(value) )

#add means
tpc.plot.h= tpc.plot.h + 
  geom_errorbar(data=tpc.agg.h, aes(x=temp, y=mean, ymin=mean-se, ymax=mean+se), width=0, col="black")+
  geom_point(data=tpc.agg.h, aes(x=temp, y = mean), size=4, col="black", fill="cornflowerblue", pch=21, linewidth=2)+
  theme_bw()+xlab("Temperature (C)")+ylab("RGR (g/g/h)")+
  ggtitle("1999") +ylim(-0.02,0.06)

#----------------
#plot TPCs over each other

#overall means
tpc.agg$year= 2024
tpc.agg.h$year=1999
tpc.agg.all= rbind(tpc.agg, tpc.agg.h)

#family means
tpc.agg.f$year=2024
tpc.agg.fh$year=1999
colnames(tpc.agg.fh)[1]="FEMALE"
tpc.agg.fh$FEMALE= as.character(tpc.agg.fh$FEMALE)
tpc.agg.f$FEMALE= as.character(tpc.agg.f$FEMALE)
tpc.agg.f.all= rbind(tpc.agg.f, tpc.agg.fh)
tpc.agg.f.all$yrfemale= paste(tpc.agg.f.all$year, tpc.agg.f.all$FEMALE)

tpc.all.plot= ggplot(tpc.agg.f.all, aes(x=temp,y=mean, col=factor(year)))+
  geom_line(aes(group=yrfemale)) +scale_color_manual(values=c("blue1", "darkorange"))
#+scale_color_viridis_d()

  #add means
  tpc.all.plot= tpc.all.plot + 
    geom_errorbar(data=tpc.agg.all, aes(x=temp, y=mean, ymin=mean-se, ymax=mean+se, col=factor(year)), width=0)+
    geom_point(data=tpc.agg.all, color="black", aes(x=temp, y = mean, fill=factor(year)), size=3, pch=21)+
    theme_bw()+xlab("Temperature (C)")+ylab("RGR (g/g/h)")+
    labs(color="Year")+ scale_fill_manual(values=c("cornflowerblue", "darkgoldenrod1"))+
    ggtitle("1999 & 2024") +ylim(-0.02,0.06) + guides(fill = FALSE)
  
  #----------------
  # Figure 2. TPC comparison
  
  #save figure
  if(desktop=="y") setwd("/Users/laurenbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/figures/")
  if(desktop=="n") setwd("/Users/lbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/figures/")
  pdf("Fig2_PrapaeTPC_2024.pdf",height = 6, width = 15)
  tpc.plot.h + tpc.plot +tpc.all.plot
  dev.off()
  
  #-------------------
  #Analysis
  
  names(tpc.lh)= names(tpc.l)
  tpc.l$time= "current"
  tpc.lh$time= "past"
  tpc.b= rbind(tpc.l, tpc.lh)
  tpc.b= na.omit(tpc.b)
  
  mod= lm(value ~ time, data= tpc.b[tpc.b$temp==35,]) # 11 17 23 29 35
  anova(mod)
  #trade-off, significant difference at 23, 29, 35
  
  mod.lmer <- lme(value~time,random=~1|FEMALE, data = tpc.b[tpc.b$temp==35,])
  anova(mod.lmer)
  #same significance
  
  #=================================
  # Match to field
  
  if(desktop=="y") setwd("/Users/laurenbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/data/")
  if(desktop=="n") setwd("/Users/lbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/data/")
  gdat= read.csv("FieldSelnExpt2024SurvivalChecks_June Experiments.csv", na.strings=c("N/A",""))
 gdat$FecCheckDate<- NA   
 gdat$FecEggCount<- NA
 gdat$FecMomAlive.<- NA
 gdat$pupaparasitized<- NA
  gdat$expt<- "june"
  
  gdat2= read.csv("FieldSelnExpt2024SurvivalChecks_July Experiments.csv", na.strings=c("N/A",""))
  gdat2$expt<- "july"
  
  gdat<- rbind(gdat, gdat2)
  
  #match TPC to field
  gdat$f.ind<- paste(gdat$Female, gdat$Individual, gdat$expt, sep="_")
  match1<- match(tpc$f.ind, gdat$f.ind)
  matched<- which(!is.na(match1))
  
  #metrics
  #pupal mass
  tpc$pupal_massmg<- NA
  tpc$pupal_massmg[matched]<- gdat$pupal_massmg[match1[matched]]
  tpc$pupal_massmg<- as.numeric(tpc$pupal_massmg)
  
  #fec egg count
  tpc$FecEggCount<- NA
  tpc$FecEggCount[matched]<- gdat$FecEggCount[match1[matched]]
  tpc$FecEggCount[which(tpc$FecEggCount=="13 (some aphids)")]<-"13"
  tpc$FecEggCount<- as.numeric(tpc$FecEggCount)
  
  #survival 
  gdat$surv<- 0
  gdat$surv[which(!is.na(gdat$pupal_massmg))]<- 1
  tpc$surv[matched]<- gdat$surv[match1[matched]]
  tpc$surv<- as.numeric(tpc$surv)
  
  #pupal time
  dates<- c("30-Jun", "1-Jul",  "29-Jun", "2-Jul",  "27-Jun", "3-Jul")
  dates2<- c("6/30/24", "7/1/24",  "6/29/24", "7/2/24",  "6/27/24", "7/3/24")
  match2<- match(gdat$pupacupdate, dates)
  gdat$pupacupdate[which(!is.na(match2))]<- dates2[na.omit(match2)]
  
  dates<- c("9-Jul",   "8-Jul",   "10-Jul",  "11-Jul",  "12-Jul",  "6-Jul",   "7-Jul")
  dates2<- c("7/9/24", "7/8/24",  "7/10/24", "7/11/24",  "7/12/24", "7/6/24", "7/7/24")
  match2<- match(gdat$eclosiondate, dates)
  gdat$eclosiondate[which(!is.na(match2))]<- dates2[na.omit(match2)]
  
  tpc$pupdate<- NA
  tpc$pupdate[matched]<- gdat$pupacupdate[match1[matched]]

  tpc$pupdate<- as.Date(as.character(tpc$pupdate), format="%m/%d/%y")
  tpc$StartDate<- gsub("/24", "/2024", tpc$StartDate)
  tpc$StartDate<- as.Date(as.character(tpc$StartDate), format="%m/%d/%Y")
  
  tpc$puptime<- as.double(difftime(tpc$pupdate, tpc$StartDate, units = c("days")))
  
  #drop one outlier big pupal mass
  tpc<- tpc[-which(tpc$pupal_massmg>275),]
  
  #to long format
  tpc2<- tpc[, !(names(tpc) %in% c("StartDate","pupdate"))]
  tpc.gl <- melt(tpc2, 
                 id.vars = c("FEMALE","f.ind","pupal_massmg","FecEggCount","surv","puptime","expt"), 
                 variable.name = "temp")
  tpc.gl$temp= as.numeric(gsub("rgr_","",tpc.gl$temp))
  
  #---------
  #find experiment
  tpc.gl$expt<- "july"
  tpc.gl$expt[grepl("june", tpc.gl$f.ind, ignore.case = FALSE, perl = FALSE,
                    fixed = FALSE, useBytes = FALSE)]<- "june"
  
  #plot
  plot.pm= ggplot(tpc.gl, aes(x=value,y=pupal_massmg)) + 
    facet_grid(expt~temp)+
    geom_point()+geom_smooth(method="lm")
  
#=======
  #fecundity
  plot.fec= ggplot(tpc.gl, aes(x=value,y=FecEggCount)) + 
    facet_grid(expt~temp)+
    geom_point()+geom_smooth(method="lm")
  
  #survival
  plot.surv= ggplot(tpc.gl, aes(x=value,y=surv)) + 
    facet_grid(expt~temp)+
    geom_point()+geom_smooth(method="lm")
  
  #time to pupation
  plot.ttp= ggplot(tpc.gl, aes(x=value,y=puptime)) + 
    facet_grid(expt~temp)+
    geom_point()+geom_smooth(method="lm")
  
  #----------------------
  #plot historic data
  
  tpc.h= read.csv("PrapaeUW.Seln2.1999.Combineddata.OPUS2021.csv")
  
  tpc.h$f.ind= paste(tpc.h$Mom, tpc.h$ID, sep="_")
  tpc.h$expt<- "aug"
  
  #align with recent data
  tpc.h2<- tpc.h[,c("Mom", "ID", "f.ind", "expt", "RGR11", "RGR17", "RGR23", "RGR29", "RGR35", "Pupated", "Time.to.Pupation", "Pupa.wt", "Fecundity")] 
  names(tpc.h2)[10:13] <- c("surv", "puptime", "pupal_massmg", "FecEggCount") 
  #also: Eclosed, Time.to.Eclosion, Butt..Wt
  
  #to long format
  tpc.gl.h <- melt(tpc.h2, id.vars = c("Mom", "ID", "f.ind", "expt","pupal_massmg","FecEggCount", "surv", "puptime"), variable.name = "temp")
  tpc.gl.h$temp= as.numeric(gsub("RGR","",tpc.gl.h$temp))
  
  #garden outcomes
  #pupal mass
  plot.pm.h= ggplot(tpc.gl.h, aes(x=value,y=pupal_massmg)) + 
    facet_grid(expt~temp)+
    geom_point()+geom_smooth(method="lm")
  
  #fecundity
  plot.fec.h= ggplot(tpc.gl.h, aes(x=value,y=FecEggCount)) + 
    facet_grid(expt~temp)+
    geom_point()+geom_smooth(method="lm")
  
  #survival
  plot.surv.h= ggplot(tpc.gl.h, aes(x=value,y=surv)) + 
    facet_grid(expt~temp)+
    geom_point()+geom_smooth(method="lm")
  
  #time to pupation
  plot.ttp.h= ggplot(tpc.gl.h, aes(x=value,y=puptime)) + 
    facet_grid(expt~temp)+
    geom_point()+geom_smooth(method="lm")
  
  #----------------------
  #historic and recent pupal mass
  tpc.gl.all<- as.data.frame(rbind(tpc.gl[,c("expt","pupal_massmg","temp","surv", "puptime", "FecEggCount","value")], tpc.gl.h[,c("expt","pupal_massmg","temp","surv", "puptime", "FecEggCount","value")]))
  
  expts<- c("june","july","aug")
  expts.lab<- c("June 2024","July 2024","Aug 1999")
  
  tpc.gl.all$expt <- expts.lab[match(tpc.gl.all$expt,expts)]
  tpc.gl.all$expt <- factor(tpc.gl.all$expt, levels= c("Aug 1999","June 2024","July 2024"), ordered=TRUE)
  
  #tpc.gl.all<- tpc.gl.all[-which(is.na(tpc.gl.all[,"pupal_massmg"])),]
  #tpc.gl.all<- tpc.gl.all[-which(is.na(tpc.gl.all[,"temp"])),]
  
  plot.pm.b<- ggplot(tpc.gl.all, aes(x=value,y=pupal_massmg)) + 
    facet_grid(expt~temp)+
    geom_point()+geom_smooth(method="lm")+
    ylab("Pupal mass (mg)") +xlab("RGR (g/g/h)")
  
  plot.surv.b<- ggplot(tpc.gl.all, aes(x=value,y=surv)) + 
    facet_grid(expt~temp)+
    geom_point()+geom_smooth(method="lm")+
    ylab("Survival") +xlab("RGR (g/g/h)")
  
  #survival distribution plots
  plot.surv.histb<- ggplot(tpc.gl.all[!is.na(tpc.gl.all$surv),], aes(x=value,color=factor(surv), group=surv)) + 
    facet_grid(expt~temp)+
    geom_density(aes(fill=factor(surv)), alpha=0.5)+
    ylab("Density") +xlab("RGR (g/g/h)")
  
  plot.pt.b<- ggplot(tpc.gl.all, aes(x=value,y=puptime)) + 
    facet_grid(expt~temp)+
    geom_point()+geom_smooth(method="lm")+
    ylab("Pupal time (day)") +xlab("RGR (g/g/h)")
  
  plot.ec.b<- ggplot(tpc.gl.all, aes(x=value,y=FecEggCount)) + 
    facet_grid(expt~temp)+
    geom_point()+geom_smooth(method="lm")+
    ylab("Egg count") +xlab("RGR (g/g/h)")
  
  #----------------------
  #save figure
  if(desktop=="y") setwd("/Users/laurenbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/figures/")
  if(desktop=="n") setwd("/Users/lbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/figures/")
  
  #Figure 3. selection
  pdf("Fig3_GardenSelection.pdf",height = 8, width = 11)
  plot.pm.b
  dev.off()
  
  #Figure S1. survival
  pdf("FigS1_GardenSurvival.pdf",height = 8, width = 11)
  plot.surv.b
  dev.off()
  
  pdf("FigS1_GardenSurvivalDist.pdf",height = 8, width = 11)
  plot.surv.histb
  dev.off()
  
  #Figure S2. pupal time
  pdf("FigS2_GardenPupalTime.pdf",height = 8, width = 11)
  plot.pt.b
  dev.off()
  
  #Figure S3. egg count
  pdf("FigS3_GardenEggCount.pdf",height = 8, width = 11)
  plot.ec.b
  dev.off()
  
  #-------
  # Plot trait changes
  
  #combine wide format
  tpc.wc<- tpc[,c("rgr_11", "rgr_17", "rgr_23", "rgr_29", "rgr_35", "surv", "puptime", "pupal_massmg", "FecEggCount","expt")]
  names(tpc.wc)<- c("RGR11", "RGR17", "RGR23", "RGR29", "RGR35", "Pupated", "Time.to.Pupation", "Pupa.wt", "Fecundity","expt")
  tpc.wc$tim_per<- "initial"
  
  tpc.wp<- tpc.h[,c("RGR11", "RGR17", "RGR23", "RGR29", "RGR35", "Pupated", "Time.to.Pupation", "Pupa.wt", "Fecundity","expt")]
  tpc.wp$tim_per<- "recent"
  
  tpc.w<- rbind(tpc.wc, tpc.wp)
  
  #mass
  plot.mass<- ggplot(tpc.w, aes(x=Pupa.wt,color=expt, group=expt)) + 
    geom_density(aes(fill=expt), alpha=0.5)+
    ylab("Density") +xlab("pupal mass (mg)")+
    scale_color_viridis_d()+scale_fill_viridis_d(alpha=0.5)
  
  #pupal time
  plot.pt<- ggplot(tpc.w, aes(x=Time.to.Pupation,color=expt, group=expt)) + 
    geom_density(aes(fill=expt), alpha=0.5)+
    ylab("Density") +xlab("pupal mass (mg)")+
    scale_color_viridis_d()+scale_fill_viridis_d(alpha=0.5)
  
  #-------
  # Plot correlations
  
  plot.cor1<- ggplot(tpc.gl.all, aes(x=pupal_massmg,y=puptime, color=expt)) + 
    geom_point()+geom_smooth(method="lm")
  
  plot.cor2<- ggplot(tpc.gl.all, aes(x=pupal_massmg,y=FecEggCount, color=expt)) + 
    geom_point()+geom_smooth(method="lm")
  
  pdf("Prapae_correlations.pdf",height = 6, width = 10)
  plot.cor1 +plot.cor2
  dev.off()
  
  #-------
  ## Analyze
  # Pupal mass: significant difference at 11, 23, 35C
  # Fecundity egg count: NS
  # Survival: significant difference at 17, 23C
  # Pupal time: NS
  
  #pick response variable: 
  #tpc$rvar<- tpc$pupal_massmg
  #tpcm<- tpc[,-"FecEggCount"]
  
  #tpc$rvar<- tpc$FecEggCount
  #tpcm<- tpc
  # 
  # tpc$rvar<- tpc$surv
  # tpcm<- tpc[,-c("FecEggCount","puptime")]
  # 
  tpc$rvar<- tpc$puptime
  tpcm<- tpc[,-which(colnames(tpc)=="FecEggCount")]
  
  #experiment currently fixed effect, make randow or otherwise change
  mod= lm(rvar ~ rgr_11 +rgr_17 +rgr_23 +rgr_29 +rgr_35 +expt, data= tpcm) 
  #divide experiments
  #mod= lm(rvar ~ rgr_11 +rgr_17 +rgr_23 +rgr_29 +rgr_35, data= tpcm[tpcm$expt=="june",])
  #mod= lm(rvar ~ rgr_11 +rgr_17 +rgr_23 +rgr_29 +rgr_35, data= tpcm[tpcm$expt=="july",])
  anova(mod)
  
  mod.lmer <- lme(rvar ~ rgr_11 +rgr_17 +rgr_23 +rgr_29 +rgr_35 +expt, random=~1|FEMALE, data = na.omit(tpcm))
  #divide experiments
  #mod.lmer <- lme(rvar ~ rgr_11 +rgr_17 +rgr_23 +rgr_29 +rgr_35, random=~1|FEMALE, data = na.omit(tpcm[tpcm$expt=="june",]))
  #mod.lmer <- lme(rvar ~ rgr_11 +rgr_17 +rgr_23 +rgr_29 +rgr_35, random=~1|FEMALE, data = na.omit(tpcm[tpcm$expt=="july",]))
  anova(mod.lmer)
  
  #ESTIMATE SELECTION GRADIENT
  
  #----
  #Combine across time and analyze 
  
  #code time
  tpc$time<- "recent"
  tpc.h2$time<- "past"
  
  #rename
  tpc.h2$FEMALE <- tpc.h2$Mom
  tpc.h2$rgr_11 <- tpc.h2$RGR11
  tpc.h2$rgr_17 <- tpc.h2$RGR17
  tpc.h2$rgr_23 <- tpc.h2$RGR23
  tpc.h2$rgr_29 <- tpc.h2$RGR29
  tpc.h2$rgr_35 <- tpc.h2$RGR35
  
  tpc<- tpc[,c("FEMALE","f.ind","expt","rgr_23","rgr_29","rgr_11","rgr_17","rgr_35","pupal_massmg","FecEggCount","surv","puptime","time")]
  tpc.h2<- tpc.h2[,c("FEMALE","f.ind","expt","rgr_23","rgr_29","rgr_11","rgr_17","rgr_35","pupal_massmg","FecEggCount","surv","puptime","time")]
  
  tpc.all<- rbind(tpc, tpc.h2)
  
  #pick response variable
  tpc.all$rvar<- tpc.all$pupal_massmg
  tpc.all$rvar<- tpc.all$FecEggCount
  tpc.all$rvar<- tpc.all$surv
  tpc.all$rvar<- tpc.all$puptime
  
  tpcm<- tpc.all[,-which(colnames(tpc.all)=="FecEggCount")]
  
  mod= lm(rvar ~ rgr_11 +rgr_17 +rgr_23 +rgr_29 +rgr_35 +time+ rgr_11*time +rgr_17*time +rgr_23*time +rgr_29*time +rgr_35*time, data= tpcm) 
  anova(mod)
  
  mod.lmer <- lme(rvar ~ rgr_11 +rgr_17 +rgr_23 +rgr_29 +rgr_35 +time+ rgr_11*time +rgr_17*time +rgr_23*time +rgr_29*time +rgr_35*time, random=~1|FEMALE, data = na.omit(tpcm))
  anova(mod.lmer)
  
  #--------------------
  ## Variance covariance analysis
# Estimate G matrix
  
  tpc.l2= na.omit(tpc.l)
  tpc.l2$temp= factor(tpc.l2$temp, levels=c(11,17,23,29,35))
  
  fm1= lmer(value~temp+(temp|FEMALE), 
            REML = TRUE, na.action = 'na.fail', 
            data= tpc.l2)
  
  vcov.m=as.matrix(vcov(fm1)*10^6)
  
  #plot
  #plot.cov= heatmap(vcov.m, Rowv=NA, Colv=NA)
  vcov.m <- melt(vcov.m)
  plot.cov= ggplot(data = vcov.m, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()+scale_fill_viridis()
  
  #library(evolqg)
  
  old <- options(contrasts=c("contr.sum","contr.poly"))
  
  #tpc.lm = lm(as.matrix(tpc[,c(5,6,3,4,7)])~as.factor(tpc[,"FEMALE"]))
  
  # cov.matrix <- CalculateMatrix(tpc.lm)
  # 
  # options(old)
  # #To obtain a correlation matrix, use:
  # cor.matrix <- cov2cor(cov.matrix)
  # 
  # cor.matrix <- cov2cor(cov.matrix)
  # 
  # cor.matrix.m <- melt(cor.matrix)
  # plot.cov2= ggplot(data = cor.matrix.m, aes(x=Var1, y=Var2, fill=value)) + 
  #   geom_tile()+scale_fill_viridis()
  # 
  # #tutorial
  # #  https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/j.1365-2656.2009.01639.x
  
  #But: a quick and dirty method is probably sufficient for now:  compute mean values for each family at each temperature, and then use the var function is compute the var-covariance matrix on the family means.  This is the least-squares estimate of the broad sense G matrix.  Using var on the individual values gives the comparable estimate of the P (phenotypic) matrix.  These should give a good approximation to the REML estimates (which are constrained so that you don’t get variances< 0 ).
  
  #CURRENT
  #family means
  tpc.f <- tpc %>% 
    group_by(FEMALE) %>% 
    dplyr::summarise(rgr_11 = mean(rgr_11, na.rm=T),
                     rgr_17 = mean(rgr_17, na.rm=T),
                     rgr_23 = mean(rgr_23, na.rm=T),
                     rgr_29 = mean(rgr_29, na.rm=T),
                     rgr_35 = mean(rgr_35, na.rm=T) )
  
  #G matrix
  g.mat<- var(tpc.f[,-1], na.rm=TRUE)*10^6
  g.mat.m <- melt(g.mat)
  
  #P matrix
  p.mat<- var(tpc[,c("rgr_11","rgr_17","rgr_23","rgr_29","rgr_35")], na.rm=TRUE)*10^6
  p.mat.m <- melt(p.mat)
  
  g.mat.c<-g.mat
  p.mat.c<-p.mat
  
  #HISTORIC
  #family means
  tpc.fh <- tpc.h %>% 
    group_by(Mom) %>% 
    dplyr::summarise(RGR11 = mean(RGR11, na.rm=T),
                     RGR17 = mean(RGR17, na.rm=T),
                     RGR23 = mean(RGR23, na.rm=T),
                     RGR29 = mean(RGR29, na.rm=T),
                     RGR35 = mean(RGR35, na.rm=T) )
  
  #G matrix
  g.mat<- var(tpc.fh[,-1], na.rm=TRUE)*10^6
  g.mat.m.h <- melt(g.mat)
  
  #P matrix
  p.mat<- var(tpc.h[,c("RGR11","RGR17","RGR23","RGR29","RGR35")], na.rm=TRUE)*10^6
  p.mat.m.h <- melt(p.mat)
  
  #-------------------
  #Combine Var matrices
  g.mat.m$type<- "G"; g.mat.m$time<- "current"
  p.mat.m$type<- "P"; p.mat.m$time<- "current"
  g.mat.m.h$type<- "G"; g.mat.m.h$time<- "historic"
  p.mat.m.h$type<- "P"; p.mat.m.h$time<- "historic"
  var.all<- rbind(g.mat.m, p.mat.m, g.mat.m.h, p.mat.m.h)
  var.all$Var1 <- sub("rgr_", "RGR", var.all$Var1)
  var.all$Var2 <- sub("rgr_", "RGR", var.all$Var2)

  #write out matrices
  write.csv(rbind(g.mat.c,p.mat.c,g.mat,p.mat), "matrices.csv" )
  write.csv(var.all, "matriceslong.csv" )
  
  #write out data 
  if(desktop=="y") setwd("/Users/laurenbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/out/")
  if(desktop=="n") setwd("/Users/lbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/out/")
  
  write.csv(tpc.all, "garden_tpcs.csv")
  
    #----------------
  ### Variance Covariance plot
  # Current variances are larger now than in the past for both G and P, particularly at 35C
  # Stronger covariances now between performance at 29 and 35: indicative of tradeoff
  
  plot.var= ggplot(data = var.all, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()+
    facet_grid(type~time)+
    scale_fill_gradient2(low ="orange", high = "blue", space = "Lab")
  
  #save figure 
  if(desktop=="y") setwd("/Users/laurenbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/figures/")
  if(desktop=="n") setwd("/Users/lbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/figures/")
  
  pdf("PrapaeTPC_cov.pdf",height = 10, width = 10)
  plot.var
  dev.off()
  
  #-------------------- 
  ## Old correlations
  # Kingsolver et al. 2001. Variation, selection and evolution of function-valued traits, [https://link.springer.com/chapter/10.1007/978-94-010-0585-2_7]
  
  cov.old= matrix(c(1.255, -.229, .043, -1.027, -.214,
                    -.229, 3.156, -1.099, -1.026, .735,
                    .043, -1.099, 4.505, 3.725, 1.613,
                    -1.027, -1.026, 3.725, 14.393, 3.947,
                    -.214, .735, 1.613, 3.947, 23.094),
                  nrow=5, ncol=5, byrow=T )
  colnames(cov.old)<- c("rgr_11", "rgr_17", "rgr_23", "rgr_29", "rgr_35")
  rownames(cov.old)<- c("rgr_11", "rgr_17", "rgr_23", "rgr_29", "rgr_35")
  
  plot.cov.old= heatmap(cov.old, Rowv=NA, Colv=NA)
  
  cov.old <- melt(cov.old)
  plot.cov.old= ggplot(data = cov.old, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()+scale_fill_viridis()
  
  #-------------------- 
  
  