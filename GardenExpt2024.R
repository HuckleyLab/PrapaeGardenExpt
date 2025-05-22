library(ggplot2)
library(data.table)
library(dplyr)
library(patchwork)
library(reshape2)
library(viridis)
library(nlme)
library(lme4)
library(car)

#toggle between desktop (y) and laptop (n)
desktop<- "n"
#if(desktop=="y") setwd("/Users/laurenbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/data/")
#if(desktop=="n") setwd("/Users/lbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/data/")

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

#-----------
#FIGURE 2. TPCs comparison

#Recent TPC
tpc.r<- tpc[which(tpc$period=="recent"),]
tpc.rl<- tpc.l[which(tpc.l$period=="recent"),]
tpc.ragg<- tpc.agg[which(tpc.agg$period=="recent"),]
tpc.ragg.f<- tpc.agg.f[which(tpc.agg.f$period=="recent"),]

tpc.plot= ggplot(tpc.rl, aes(x=temp,y=value)) + 
  geom_point()+
#add family lines
  geom_line(data=tpc.ragg.f, aes(x=temp, y = mean, group=Mom), linewidth=1, col="darkorange")+
#add points for temperature means  
  geom_errorbar(data=tpc.ragg, aes(x=temp, y=mean, ymin=mean-se, ymax=mean+se), width=0, col="black")+
  geom_point(data=tpc.ragg, aes(x=temp, y = mean), size=4, col="black", fill="darkgoldenrod1", pch=21)+
  theme_bw()+xlab("Temperature (C)")+ylab("RGR (g/g/h)")+
  ggtitle("2024")+ylim(-0.02,0.06)

#------------
#Past TPC
tpc.p<- tpc[which(tpc$period=="past"),]
tpc.pl<- tpc.l[which(tpc.l$period=="past"),]
tpc.pagg<- tpc.agg[which(tpc.agg$period=="past"),]
tpc.pagg.f<- tpc.agg.f[which(tpc.agg.f$period=="past"),]

tpc.plot.p= ggplot(tpc.pl, aes(x=temp,y=value)) + 
  geom_point()+
  #add family lines
  geom_line(data=tpc.pagg.f, aes(x=temp, y = mean, group=Mom), linewidth=1, col="blue1")+
  #add points for temperature means  
  geom_errorbar(data=tpc.pagg, aes(x=temp, y=mean, ymin=mean-se, ymax=mean+se), width=0, col="black")+
  geom_point(data=tpc.pagg, aes(x=temp, y = mean), size=4, col="black", fill="cornflowerblue", pch=21)+
  theme_bw()+xlab("Temperature (C)")+ylab("RGR (g/g/h)")+
  ggtitle("2024")+ylim(-0.02,0.06)

#------------
#plot TPCs over each other
tpc.agg.f$MomPer<- paste(tpc.agg.f$Mom, tpc.agg.f$period, sep="_")

tpc.plot.all= ggplot(tpc.agg.f, aes(x=temp,y=mean, col=factor(period)))+
  geom_line(aes(group=MomPer)) +scale_color_manual(values=c("blue1", "darkorange"))+
  #add means
 geom_errorbar(data=tpc.agg, aes(x=temp, y=mean, ymin=mean-se, ymax=mean+se, col=factor(period)), width=0)+
    geom_point(data=tpc.agg, color="black", aes(x=temp, y = mean, fill=factor(period)), size=3, pch=21)+
    theme_bw()+xlab("Temperature (C)")+ylab("RGR (g/g/h)")+
    labs(color="Year")+ scale_fill_manual(values=c("cornflowerblue", "darkgoldenrod1"))+
    ggtitle("1999 & 2024") +ylim(-0.02,0.06) + guides(fill = FALSE)
  
#------------
#save figure
  if(desktop=="y") setwd("/Users/laurenbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/figures/")
  if(desktop=="n") setwd("/Users/lbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/figures/")

  pdf("Fig2_PrapaeTPC_2024.pdf",height = 6, width = 15)
  tpc.plot.p + tpc.plot +tpc.plot.all
  dev.off()
  
  #-------------------
  #Analysis
  
 #drop garden columns
  tpc.b<- tpc[,c("Mom","ID","f.ind","RGR23","RGR29","RGR11","RGR17","RGR35","period")]
  tpc.b= na.omit(tpc.b)
  
  #single temp
  mod= lm(RGR35 ~ period, data= tpc.b) # 11 17 23 29 35
  anova(mod)
  #trade-off, significant difference at 23, 29, 35
  
  mod.lmer <- lme(RGR35 ~ period,random=~1|Mom, data = tpc.b)
  anova(mod.lmer)
  #same significance
  
  #across temps
  tpc.b<- tpc.l[,c("Mom","ID","f.ind","temp","value","period")]
  tpc.b= na.omit(tpc.b)
  
  mod= lm(value ~ temp*period, data= tpc.b)
  anova(mod)
  
  mod.lmer <- lme(value ~ temp*period,random=~1|Mom, data = tpc.b)
  anova(mod.lmer)
  
  #=================================
  # Selection plot
  
  #drop one outlier big pupal mass
  tpc.l<- tpc.l[-which(tpc.l$Pupa.wt>275),]
  
  #selection plots
  
  plot.pm.b<- ggplot(tpc.l, aes(x=value,y=Pupa.wt)) + 
    facet_grid(expt~temp)+
    geom_point()+geom_smooth(method="lm")+
    ylab("Pupal mass (mg)") +xlab("RGR (g/g/h)")+
    theme_bw()
  
  plot.surv.b<- ggplot(tpc.l, aes(x=value,y=Pupated)) + 
    facet_grid(expt~temp)+
    geom_point()+geom_smooth(method="lm")+
    ylab("Survival") +xlab("RGR (g/g/h)")
  
  #survival distribution plots
  plot.surv.histb<- ggplot(tpc.l[!is.na(tpc.l$Pupated),], aes(x=value,color=factor(Pupated), group=Pupated)) + 
    facet_grid(expt~temp)+
    geom_density(aes(fill=factor(Pupated)), alpha=0.5)+
    ylab("Density") +xlab("RGR (g/g/h)")
  
  plot.pt.b<- ggplot(tpc.l, aes(x=value,y=Time.to.Pupation)) + 
    facet_grid(expt~temp)+
    geom_point()+geom_smooth(method="lm")+
    ylab("Pupal time (day)") +xlab("RGR (g/g/h)")
  
  plot.ec.b<- ggplot(tpc.l, aes(x=value,y=Fecundity)) + 
    facet_grid(expt~temp)+
    geom_point()+geom_smooth(method="lm")+
    ylab("Egg count") +xlab("RGR (g/g/h)")
  
  #-------
  #save figures
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
  
  #-------------------
  #Estimate selection
  #Kingsolver and Gomulkiewicz 2003
  # linear selection gradient (b), which relates variation in the trait (in units of standard deviation of the trait) to variation in relative fitness w (where mean fitness in a population or sample is defined to equal 1)  
  
  #Kingsolver, Gomulkiewicz, and Carter 2001
  #  The components of a selection gradient represent the direct strength of selection on each trait, adjusting for the phenotypic correlations among the traits; they can be readily estimated with partial regression analyses,
  
  #From JGK: For the figure in the Genetica paper, the selection gradients were standardized, with RGR at each temperature standardized to mean = 0 and SD = 1;  similarly each fitness metric (pupal mass, development rate, and survival) was standardized to mean = 1. For survival this means that the (relative) fitness of survivors depends strongly the proportion surviving to pupation: e.g. if overall survival to pupation is 50%, then the relative fitness of survivors and non-survivors is 2 and 0, respectively.
  #One thing I’m not sure about: for pupal mass and development rate, RGR may have been standardized to mean and SD for ALL individuals (including those that didn’t survive to pupate) or only for pupate.  I don’t have the actual analyses, only some of the intermediate datasets using in analyses.
  #For our current paper I still think we should use values NOT standardized by SD, since they are all the same trait  (RGR) and I think this is what the model in the 2003 ICB paper used (I will re-read to check this). 
  
  #standardization: https://doi.org/10.1525/bio.2012.62.12.6
  
  #ESTIMATE SELECTION GRADIENT
  tpc.sel<- tpc
  tpc.sel$surv<- tpc.sel$Pupated
  tpc.sel$puptime<- tpc.sel$Time.to.Pupation
  
  #remove RGR for individuals that don't survive
  tpc.sel$RGR11s <- tpc.sel$RGR11
  tpc.sel$RGR11s[which(is.na(tpc.sel$pupal_massmg))] <- NA
  
  tpc.sel$RGR17s <- tpc.sel$RGR17
  tpc.sel$RGR17s[which(is.na(tpc.sel$pupal_massmg))] <- NA
  
  tpc.sel$RGR23s <- tpc.sel$RGR23
  tpc.sel$RGR23s[which(is.na(tpc.sel$pupal_massmg))] <- NA
  
  tpc.sel$RGR29s <- tpc.sel$RGR29
  tpc.sel$RGR29s[which(is.na(tpc.sel$pupal_massmg))] <- NA
  
  tpc.sel$RGR35s <- tpc.sel$RGR35
  tpc.sel$RGR35s[which(is.na(tpc.sel$pupal_massmg))] <- NA
  
  #normalize RGR response to +- 1SD
  #normalize fitness metrics to mean of 1 by dividing my mean, CHECK NORMALIZATION
  tpc.sel1 <- tpc.sel %>% 
    group_by(period, expt) %>% 
    dplyr::mutate(
      #standardize traits by subtracting mean
      RGR11_ms = (RGR11-mean(RGR11, na.rm = TRUE)),
      RGR17_ms = (RGR17-mean(RGR17, na.rm = TRUE)),
      RGR23_ms = (RGR23-mean(RGR23, na.rm = TRUE)),
      RGR29_ms = (RGR29-mean(RGR29, na.rm = TRUE)),
      RGR35_ms = (RGR35-mean(RGR35, na.rm = TRUE)),
      #standardize traits by subtracting mean and dividing by sd
      RGR11_scale = (RGR11-mean(RGR11, na.rm = TRUE))/sd(RGR11, na.rm = TRUE),
      RGR17_scale = (RGR17-mean(RGR17, na.rm = TRUE))/sd(RGR17, na.rm = TRUE),
      RGR23_scale = (RGR23-mean(RGR23, na.rm = TRUE))/sd(RGR23, na.rm = TRUE),
      RGR29_scale = (RGR29-mean(RGR29, na.rm = TRUE))/sd(RGR29, na.rm = TRUE),
      RGR35_scale = (RGR35-mean(RGR35, na.rm = TRUE))/sd(RGR35, na.rm = TRUE),
      #same for only survivors
      RGR11s_scale = (RGR11-mean(RGR11s, na.rm = TRUE))/sd(RGR11s, na.rm = TRUE),
      RGR17s_scale = (RGR17-mean(RGR17s, na.rm = TRUE))/sd(RGR17s, na.rm = TRUE),
      RGR23s_scale = (RGR23-mean(RGR23s, na.rm = TRUE))/sd(RGR23s, na.rm = TRUE),
      RGR29s_scale = (RGR29-mean(RGR29s, na.rm = TRUE))/sd(RGR29s, na.rm = TRUE),
      RGR35s_scale = (RGR35-mean(RGR35s, na.rm = TRUE))/sd(RGR35s, na.rm = TRUE),
      #scale response by dividing by mean
      surv_norm = surv /mean(surv, na.rm = TRUE),
      puptime_norm = puptime /mean(puptime, na.rm = TRUE),
      Pupa.wt_norm = Pupa.wt /mean(Pupa.wt, na.rm = TRUE),
      Fecundity_norm = Fecundity /mean(Fecundity, na.rm = TRUE)
                  ) %>%
    ungroup()
  #Or are some means or sds used to normalize after estimating gradient?
  
  tpc.sel1<- as.data.frame(tpc.sel1)
  tpc.sel2 <- na.omit(tpc.sel1[,!(names(tpc.sel1) %in% c("ID","Fecundity","Fecundity_norm"))])
  tpc.sel2f <- na.omit(tpc.sel1[,!(names(tpc.sel1) %in% c("ID"))])
  
  #set up matrix for coefficients
  expts<- expts.lab
  periods<- c("recent","recent","past")
  rgrs<- c("RGR11","RGR17","RGR23","RGR29","RGR35")
  fitcomp<- c("mass","surv","puptime", "fec", "mass.s","surv.s","puptime.s")
  
  sg= expand.grid(rgrs, expts.lab, fitcomp)
  sg= as.data.frame(cbind( sg,matrix(NA, nrow=nrow(sg), ncol=4)))
  colnames(sg)= c("rgr","expt", "fitcomp", "value", "se", "t-value", "p-value")
  sg$period <- periods[match(sg$expt, expts.lab)]
  
  #estimate selection
  for(k in 1:length(expts.lab)){
    #subset to experiment
    tpc.sub<- tpc.sel2[which(tpc.sel2$expt==expts.lab[k] & tpc.sel2$period==periods[k]),]
    tpc.sub.f<- tpc.sel2f[which(tpc.sel2f$expt==expts.lab[k] & tpc.sel2f$period==periods[k]),]
    
    #mod.pmass <- lme(pupal_massmg_norm ~ RGR11_scale +RGR17_scale +RGR23_scale +RGR29_scale +RGR35_scale, random=~1|Mom, data = tpc.sub)
    #coef.pmass<- summary(mod.pmass)$tTable
    mod.pmass <- lm(Pupa.wt_norm ~ RGR11_scale +RGR17_scale +RGR23_scale +RGR29_scale +RGR35_scale, data = tpc.sub)
    coef.pmass<- summary(mod.pmass)$coefficients
    
    #only surviving individuals
    mod.pmass.s <- lm(Pupa.wt_norm ~ RGR11s_scale +RGR17s_scale +RGR23s_scale +RGR29s_scale +RGR35s_scale, data = tpc.sub)
    coef.pmass.s<- summary(mod.pmass.s)$coefficients
    
    coef.surv.s<- NA
    try({
    #mod.surv <- lme(surv_norm ~ RGR11_scale +RGR17_scale +RGR23_scale +RGR29_scale +RGR35_scale, random=~1|Mom, data = tpc.sub)
    #coef.surv<- summary(mod.surv)$tTable
    mod.surv <- lm(surv_norm ~ RGR11_scale +RGR17_scale +RGR23_scale +RGR29_scale +RGR35_scale, data = tpc.sub)
    coef.surv<- summary(mod.surv)$coefficients
    
    #only surviving individuals
    mod.surv.s <- lm(surv_norm ~ RGR11s_scale +RGR17s_scale +RGR23s_scale +RGR29s_scale +RGR35s_scale, data = tpc.sub)
    coef.surv.s<- summary(mod.surv.s)$coefficients
    })
    
    #mod.puptime <- lme(puptime_norm ~ RGR11_scale +RGR17_scale +RGR23_scale +RGR29_scale +RGR35_scale, random=~1|Mom, data = tpc.sub)
    #coef.puptime<- summary(mod.puptime)$tTable
    mod.puptime <- lm(puptime_norm ~ RGR11_scale +RGR17_scale +RGR23_scale +RGR29_scale +RGR35_scale, data = tpc.sub)
    coef.puptime<- summary(mod.puptime)$coefficients
    
    #only surviving individuals
    mod.puptime.s <- lm(puptime_norm ~ RGR11s_scale +RGR17s_scale +RGR23s_scale +RGR29s_scale +RGR35s_scale, data = tpc.sub)
    coef.puptime.s<- summary(mod.puptime.s)$coefficients
    
    #save coefficients
    #for lme
    #sg[which(sg$expt==expts[k] & sg$fitcomp=="mass"),c(4:7)]= coef.pmass[2:nrow(coef.pmass), c(1:2,4:5)]
    #sg[which(sg$expt==expts[k] & sg$fitcomp=="surv"),c(4:7)]= coef.surv[2:nrow(coef.surv), c(1:2,4:5)]
    #sg[which(sg$expt==expts[k] & sg$fitcomp=="puptime"),c(4:7)]= coef.puptime[2:nrow(coef.puptime), c(1:2,4:5)]
    
    #for lm
    sg[which(sg$expt==expts[k] & sg$fitcomp=="mass"),c(4:7)]= coef.pmass[2:nrow(coef.pmass),]
    sg[which(sg$expt==expts[k] & sg$fitcomp=="surv"),c(4:7)]= coef.surv[2:nrow(coef.surv), ]
    sg[which(sg$expt==expts[k] & sg$fitcomp=="puptime"),c(4:7)]= coef.puptime[2:nrow(coef.puptime), ]
    
    #only survivors
    sg[which(sg$expt==expts[k] & sg$fitcomp=="mass.s"),c(4:7)]= coef.pmass.s[2:nrow(coef.pmass.s),]
    sg[which(sg$expt==expts[k] & sg$fitcomp=="surv.s"),c(4:7)]= coef.surv.s[2:nrow(coef.surv.s), ]
    sg[which(sg$expt==expts[k] & sg$fitcomp=="puptime.s"),c(4:7)]= coef.puptime.s[2:nrow(coef.puptime.s), ]
    
    #fecundity
    if(nrow(tpc.sub.f)>0){
    #mod.fec <- lme(Fecundity_norm ~ RGR11_scale +RGR17_scale +RGR23_scale +RGR29_scale +RGR35_scale, random=~1|Mom, data = tpc.sub.f)
    #coef.fec<- summary(mod.fec)$tTable
    #sg[which(sg$expt==expts[k] & sg$fitcomp=="fec"),c(4:7)]= coef.fec[2:nrow(coef.fec), c(1:2,4:5)]
    
    mod.fec <- lm(Fecundity_norm ~ RGR11_scale +RGR17_scale +RGR23_scale +RGR29_scale +RGR35_scale, data = tpc.sub.f)
    coef.fec<- summary(mod.fec)$coefficients
    sg[which(sg$expt==expts[k] & sg$fitcomp=="fec"),c(4:7)]= coef.fec[2:nrow(coef.fec), ]
    }
    
  }
  
  #plot coefficients
  sg$temp<- gsub("RGR","",sg$rgr)

  #code significance
  colnames(sg)[6:7]<- c("tvalue","pvalue")
  sg$sig<- ifelse(sg$pvalue<=0.05, "sig", "ns")
  
  #change timeperiod names
  sg$expt <- expts.lab[match(sg$expt,expts)]
  sg$expt <- factor(sg$expt, levels= c("Aug 1999","June 2024","July 2024"), ordered=TRUE)
  
  #Genetica plot, include only pupating individuals
  plot.sg<- ggplot(sg[which(sg$fitcomp %in% c("mass.s", "surv.s", "puptime.s")),], aes(x=temp, y=value, color=fitcomp, fill=sig, group=fitcomp)) + 
    geom_point(size=4, pch=21)+ geom_smooth(se=FALSE)+
    facet_wrap(.~expt)+
    ylab("Selection gradient") +xlab("Temperature (C)")+
    scale_color_viridis_d()+
    #ylim(-0.12, 0.12)+
    theme_classic()+
    scale_fill_manual(values = c("sig" = "gray", "ns" = "white"))+
  #add standard errors
  geom_errorbar(aes(x=temp, y=value, ymin=value-se, ymax=value+se), width=0)
  
  pdf("Prapae_selectiongradients.pdf",height = 6, width = 10)
  plot.sg
  dev.off()
  
  #plot models
  car::avPlots(mod.pmass.s)
  car::avPlots(mod.puptime.s)
  
#------------------------
  #plot coefficients
  
  keep= c("Mom","ID","f.ind","expt","RGR11s_scale","RGR17s_scale","RGR23s_scale","RGR29s_scale","RGR35s_scale", "surv_norm","puptime_norm","Pupa.wt_norm","Fecundity_norm")
  
  #to long format
  tpc.gl.h2 <- melt(tpc.sel1[,colnames(tpc.sel1) %in% keep], id.vars = c("Mom", "ID", "f.ind", "expt","surv_norm","puptime_norm","Pupa.wt_norm","Fecundity_norm"), variable.name = "temp")
  tpc.gl.h2$temp= gsub("RGR","",tpc.gl.h2$temp)
  tpc.gl.h2$temp= as.numeric(gsub("s_scale","",tpc.gl.h2$temp))
  
  plot.pm.b<- ggplot(tpc.gl.h2, aes(x=value,y=Pupa.wt_norm)) + 
    facet_grid(expt~temp)+
    geom_point()+geom_smooth(method="lm")+
    ylab("Pupal mass (mg)") +xlab("RGR (g/g/h)")+
    theme_bw()
  #FIX
#==============================================
  # Plot trait changes
  
  tpc.tc<- tpc[,c("f.ind", "expt","Mi","Pupa.wt","Butt..Wt","Time.to.Pupation","Time.to.Eclosion","Fecundity")]
  
  #to long format
  tpc.tcl<- melt(tpc.tc, id.vars = c("f.ind", "expt"), variable.name = "trait")
  
  #plot
  plot.tc<- ggplot(tpc.tcl, aes(x=value,color=expt, group=expt)) + 
    geom_density(aes(fill=expt), alpha=0.5)+
    facet_wrap(.~trait, scales="free")+
    ylab("Density") +theme_bw()+
    scale_color_viridis_d()+scale_fill_viridis_d(alpha=0.5)
  
  pdf("Prapae_TraitChange.pdf",height = 6, width = 10)
  plot.tc
  dev.off()
  #-------
  # Plot correlations
  
  plot.cor1<- ggplot(tpc.l, aes(x=Pupa.wt,y=Time.to.Pupation, color=expt)) + 
    geom_point()+geom_smooth(method="lm")
  
  plot.cor2<- ggplot(tpc.l, aes(x=Pupa.wt,y=Fecundity, color=expt)) + 
    geom_point()+geom_smooth(method="lm")
  
  pdf("Prapae_correlations.pdf",height = 6, width = 10)
  plot.cor1 +plot.cor2
  dev.off()
  
  #-----------------------------
  #Variance covariance analysis
  
  # Load libraries
  library(MCMCglmm)
 
  #loop through past and recent
  for(k in 1:2){
    
  if(k==1) df<- tpc[which(tpc$period=="past"),c("Mom","RGR23","RGR29","RGR11","RGR17","RGR35")]
  if(k==2) df<- tpc[which(tpc$period=="recent"),c("Mom","RGR23","RGR29","RGR11","RGR17","RGR35")]
    
  df<- na.omit(df)
  #make new id
  df$animal<- (max(df$Mom)+1):((max(df$Mom)+1)+nrow(df)-1)
  
  #https://devillemereuil.legtux.org/wp-content/uploads/2021/09/tuto_en.pdf
  #make pedigree
  ped<- rbind(cbind(unique(df$Mom),NA,NA),cbind(df$animal,df$Mom,NA))
  colnames(ped)<- c("animal","mother","father")
  
  # Set priors for multivariate analysis
  #prior= list(R = list(V = diag(5) * 0.002 / 1.002, nu = 1.002),
  #     G = list(G1 = list(V = diag(5) * 0.002 / 1.002, nu = 1.002)))
  
  #gentler prior
  prior <- list(R = list(V = diag(5), nu = 5),
                 G = list(G1 = list(V = diag(5), nu = 5)))
  
  #variances
  var(df$RGR11)
  var(df$RGR35)
  
  # Fit the model
  model <- MCMCglmm(
    cbind(RGR11,RGR17,RGR23,RGR29,RGR35) ~ trait - 1,  
    # Can add period in as fixed effect to evaluate change over time? 
    # structure of the variance-covariance matrix for the random effects (random) or the residual variances (rcov)
    random = ~ us(trait):animal, #models unstructured genetic covariance (G-matrix)
    rcov = ~ us(trait):units, #models unstructured residual covariance (R-matrix)
    family = rep("gaussian", 5), 
    prior = prior,
    pedigree = ped,
    data = df,
    nitt = 100000,
    burnin = 10000,
    thin = 10)
    
  if(k==1) model.p= model
  if(k==2) model.r= model
    
  } #end loop past recent loop
  
  # Print the model summary
  summary(model)
  
  #diagnostics
  plot(model[["Sol"]])
  plot(model[["VCV"]])
  
  #model$VCV: Posterior samples of variance components (columns 1-25 = G, 26-50 = R)
  # Extract genetic covariance matrix (G)
  g.mat.hb <- matrix(apply(model.p$VCV[,1:25], 2, median), 5, 5)
  colnames(g.mat.hb) <- rownames(g.mat.hb) <- c("RGR11","RGR17","RGR23","RGR29","RGR35")
  
  # Extract phenotypic covariance matrix (P)
  #Calculated as P= G + R 
  p.mat.hb <- g.mat.hb + matrix(apply(model.p$VCV[,26:50], 2, median), 5, 5)
  colnames(p.mat.hb) <- rownames(p.mat.hb) <- c("RGR11","RGR17","RGR23","RGR29","RGR35")
  
  #recent
  g.mat.b <- matrix(apply(model.r$VCV[,1:25], 2, median), 5, 5)
  colnames(g.mat.b) <- rownames(g.mat.b) <- c("RGR11","RGR17","RGR23","RGR29","RGR35")
  p.mat.b <- g.mat.b + matrix(apply(model.r$VCV[,26:50], 2, median), 5, 5)
  colnames(p.mat.b) <- rownames(p.mat.b) <- c("RGR11","RGR17","RGR23","RGR29","RGR35")
  
  #But: a quick and dirty method is probably sufficient for now:  compute mean values for each family at each temperature, and then use the var function is compute the var-covariance matrix on the family means.  This is the least-squares estimate of the broad sense G matrix.  Using var on the individual values gives the comparable estimate of the P (phenotypic) matrix.  These should give a good approximation to the REML estimates (which are constrained so that you don’t get variances< 0 ).
  
  #RECENT
  #family means
  tpc.f <- tpc[tpc$period=="recent",] %>% 
    group_by(Mom) %>% 
    dplyr::summarise(RGR11 = mean(RGR11, na.rm=T),
                     RGR17 = mean(RGR17, na.rm=T),
                     RGR23 = mean(RGR23, na.rm=T),
                     RGR29 = mean(RGR29, na.rm=T),
                     RGR35 = mean(RGR35, na.rm=T) )
  
  #G matrix
  g.mat<- var(tpc.f[,-1], na.rm=TRUE)*10^6
  g.mat.m <- melt(g.mat)
  
  #P matrix
  p.mat<- var(tpc[,c("RGR11","RGR17","RGR23","RGR29","RGR35")], na.rm=TRUE)*10^6
  p.mat.m <- melt(p.mat)
  
  #correlation matrix
  c.mat<-  cor(na.omit(tpc[,c("RGR11","RGR17","RGR23","RGR29","RGR35")]))
  c.mat.m <- melt(c.mat)
  
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
  g.mat.h<- var(tpc.fh[,-1], na.rm=TRUE)*10^6
  g.mat.m.h <- melt(g.mat.h)
  
  #P matrix
  p.mat.h<- var(tpc.h[,c("RGR11","RGR17","RGR23","RGR29","RGR35")], na.rm=TRUE)*10^6
  p.mat.m.h <- melt(p.mat.h)
  
  #correlation matrix
  c.mat.h<-  cor(na.omit(tpc.h[,c("RGR11","RGR17","RGR23","RGR29","RGR35")]))
  c.mat.m.h <- melt(c.mat.h)
  
  #-------------------
  #Combine Var matrices
  g.mat.m$type<- "G"; g.mat.m$time<- "recent"
  p.mat.m$type<- "P"; p.mat.m$time<- "recent"
  g.mat.m.h$type<- "G"; g.mat.m.h$time<- "past"
  p.mat.m.h$type<- "P"; p.mat.m.h$time<- "past"
  
  #add Bayesian matrices
  g.mat.m.b <- melt(g.mat.b)
  p.mat.m.b <- melt(p.mat.b)
  g.mat.m.hb <- melt(g.mat.hb)
  p.mat.m.hb <- melt(p.mat.hb)
  
  g.mat.m.b$type<- "G bayes"; g.mat.m.b$time<- "recent"
  p.mat.m.b$type<- "P bayes"; p.mat.m.b$time<- "recent"
  g.mat.m.hb$type<- "G bayes"; g.mat.m.hb$time<- "past"
  p.mat.m.hb$type<- "P bayes"; p.mat.m.hb$time<- "past"
  
  var.all<- rbind(g.mat.m, p.mat.m, g.mat.m.h, p.mat.m.h,
                  g.mat.m.b, p.mat.m.b, g.mat.m.hb, p.mat.m.hb)
  var.all$Var1 <- sub("rgr_", "RGR", var.all$Var1)
  var.all$Var2 <- sub("rgr_", "RGR", var.all$Var2)

  #write out matrices
  write.csv(rbind(g.mat,p.mat,g.mat.h,p.mat.h), "matrices.csv" )
  write.csv(var.all, "matriceslong.csv" )
  
    #----------------
  ### Variance Covariance plot
  # Current variances are larger now than in the past for both G and P, particularly at 35C
  # Stronger covariances now between performance at 29 and 35: indicative of tradeoff
  
  plot.var= ggplot(data = var.all[which(var.all$type %in% c("G","P") ),], aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()+
    facet_grid(type~time)+
    scale_fill_gradient2(low ="orange", high = "blue", space = "Lab")
  
  #Bayesian
  plot.var.b= ggplot(data = var.all[which(var.all$type %in% c("G bayes","P bayes") ),], aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()+
    facet_grid(type~time)+
    scale_fill_gradient2(low ="orange", high = "blue", space = "Lab")
  
  #save figure 
  if(desktop=="y") setwd("/Users/laurenbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/figures/")
  if(desktop=="n") setwd("/Users/lbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/figures/")
  
  pdf("PrapaeTPC_cov.pdf",height = 10, width = 10)
  plot.var
  dev.off()
  
  #----------
  #Correlation plot
  
  c.mat.m$type<- "cor"; c.mat.m$time<- "recent"
  c.mat.m.h$type<- "cor"; c.mat.m.h$time<- "past"
  cor.all<- rbind(c.mat.m, c.mat.m.h)
  cor.all$Var1 <- sub("rgr_", "RGR", cor.all$Var1)
  cor.all$Var2 <- sub("rgr_", "RGR", cor.all$Var2)
  
  cor.var= ggplot(data = cor.all, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()+
    facet_grid(type~time)+
    scale_fill_gradient2(low ="orange", high = "blue", space = "Lab")
  
  #-----------------
  #Estimate eigenfunctions
  
  # Eigen decomposition
  eigen_decomp <- eigen(p.mat)
  eigen_decomp.h <- eigen(p.mat.h)
  # Corresponding eigenvalues
  eigenvalues <- eigen_decomp$values
  
  # Extract eigenvectors (principal components)
  evs <- as.data.frame(eigen_decomp$vectors)
  evs.h <- as.data.frame(eigen_decomp.h$vectors)
  
  evs$period<- "current"
  evs.h$period<- "past"
  
  evs$ev<- c(1:5)
  evs.h$ev<- c(1:5)
  
  #eigenvectors from paper
 ev.old= as.data.frame(matrix(c(-0.029, 0.084, -0.179,
                  -0.001, 0.175, 0.614,
                  0.039, -0.276, -0.705,
                  0.166, -0.926, 0.305,
                  0.985, 0.170, -0.029),
                  nrow=5, ncol=3, byrow=T ))
  colnames(ev.old)<- c("PC1", "PC2", "PC3")
  ev.old$temp<- c(11,17,23,29,35)
  #reformat
  ev.old= as.data.frame(rbind(c(ev.old[,1]), c(ev.old[,2])))
  ev.old$period<- c("2001 paper", "2001 paper")
  ev.old$ev<- c(1,2)
  
  #plot eigen vectors
  evp<- as.data.frame(rbind(evs[1:2,], evs.h[1:2,], ev.old))
  colnames(evp)[1:5]<- c(11, 17, 23, 29, 35)
  
  #to long format
  evp.l <- melt(evp, 
                id.vars= c("period","ev"),
                 variable.name = "temp")
  evp.l$per.ev<- paste(evp.l$period, evp.l$ev, sep="_")
  
  plot.ev= ggplot(data = evp.l, aes(x=temp, y=value, color=factor(ev), lty=period, group=per.ev)) + 
    geom_point()+geom_smooth()+ylab("eigenvector")

  pdf("Fig_eigenvector.pdf",height = 10, width = 10)
  plot.ev
  dev.off()
  
  #------
  #eigen plot for correlation matrix
  
  # Eigen decomposition
  eigen_decomp <- eigen(c.mat)
  eigen_decomp.h <- eigen(c.mat.h)
  # Corresponding eigenvalues
  eigenvalues <- eigen_decomp$values
  
  # Extract eigenvectors (principal components)
  evs <- as.data.frame(eigen_decomp$vectors)
  evs.h <- as.data.frame(eigen_decomp.h$vectors)
  
  evs$period<- "current"
  evs.h$period<- "past"
  
  evs$ev<- c(1:5)
  evs.h$ev<- c(1:5)
  
  #plot eigen vectors
  evp<- rbind(evs[1:2,], evs.h[1:2,])
  colnames(evp)[1:5]<- c(11, 17, 23, 29, 35)
  
  #to long format
  evp.l <- melt(evp, 
                id.vars= c("period","ev"),
                variable.name = "temp")
  evp.l$per.ev<- paste(evp.l$period, evp.l$ev, sep="_")
  
  plot.corev= ggplot(data = evp.l, aes(x=temp, y=value, color=factor(ev), lty=period, group=per.ev)) + 
    geom_point()+geom_smooth()+ylab("eigenvector")
  
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
  
  