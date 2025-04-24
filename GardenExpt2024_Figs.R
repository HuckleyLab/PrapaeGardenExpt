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
desktop<- "y"
if(desktop=="y") setwd("/Users/laurenbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/")
if(desktop=="n") setwd("/Users/lbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/")

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
  
  pdf("./figures/Fig2_PrapaeTPC_2024.pdf",height = 6, width = 15)
  tpc.plot.p + tpc.plot +tpc.plot.all
  dev.off()
  
  #-------------------
  #Analysis
  
 #drop garden columns
  tpc.b<- tpc[,c("Mom","ID","f.ind","Mi","RGR23","RGR29","RGR11","RGR17","RGR35","period")]
  tpc.b= na.omit(tpc.b)
  
  #single temp
  mod= lm(RGR35 ~ Mi + period, data= tpc.b) # 11 17 23 29 35
  anova(mod)
  #trade-off, significant difference at 23, 29, 35
  
  mod.lmer <- lme(RGR35 ~ period,random=~1|Mom, data = tpc.b)
  anova(mod.lmer)
  #same significance
  
  #across temps
  tpc.b<- tpc.l[,c("Mom","ID", "Mi", "f.ind","temp","value","period")]
  tpc.b= na.omit(tpc.b)
  
  mod= lm(value ~ poly(temp)*period*Mi, data= tpc.b)
  anova(mod)
  
  mod.lmer <- lme(value ~ poly(temp)*period*Mi,random=~1|Mom, data = tpc.b)
  anova(mod.lmer)
  
  #========================================================
  ## Modeling fitness responses. All three experiments combined: main effects and interactions
  ##expt factor should account for differences in responses among expts#
  
  # center RGR values (mean = 0) to account for historic changes in RGR (probably doesn't matter...)
  tpc.all <- tpc
  tpc.all.c <- tpc.all %>% 
    group_by(period, expt) %>% 
    dplyr::mutate(
      #center trait values by subtracting mean
      RGR11_c = (RGR11-mean(RGR11, na.rm = TRUE)),
      RGR17_c = (RGR17-mean(RGR17, na.rm = TRUE)),
      RGR23_c = (RGR23-mean(RGR23, na.rm = TRUE)),
      RGR29_c = (RGR29-mean(RGR29, na.rm = TRUE)),
      RGR35_c = (RGR35-mean(RGR35, na.rm = TRUE))
    ) %>%
    ungroup()
  names(tpc.all.c) #check
  
  ##model each response for all traits, with and without interaction (with experiment); compare models
  # survival
  mod.all.s <- glm(Pupated~Mi+RGR11+RGR17+RGR23+RGR29+RGR35+ expt, family = binomial, data=tpc.all.c) 
  anova(mod.all.s)
  mod.int.s<- glm(Pupated~Mi+RGR11+RGR17+RGR23+RGR29+RGR35+expt+expt:(RGR11+RGR17+RGR23+RGR29+RGR35), family = binomial, data=tpc.all.c) 
  anova(mod.int.s)
  anova(mod.all.s,mod.int.s, test = "Chi")
  ## best model:  no interactions
  summary(mod.all.s)
  
  #pupal weight
  mod.all.m <- lm(Pupa.wt~Mi+RGR11+RGR17+RGR23+RGR29+RGR35+ expt,  data=tpc.all.c) 
  anova(mod.all.m)
  mod.int.m<- lm(Pupa.wt~Mi+RGR11+RGR17+RGR23+RGR29+RGR35+expt+expt:(RGR11+RGR17+RGR23+RGR29+RGR35), data=tpc.all.c) 
  anova(mod.int.m)
  anova(mod.all.m,mod.int.m, test = "Chi")
  ## best model: with interactions
  summary(mod.int.m)
  
  #dev time
  mod.all.d <- lm(Time.to.Pupation~Mi+RGR11+RGR17+RGR23+RGR29+RGR35+ expt,  data=tpc.all.c) 
  anova(mod.all.d)
  mod.int.d<- lm(Time.to.Pupation~Mi+RGR11+RGR17+RGR23+RGR29+RGR35+expt+expt:(RGR11+RGR17+RGR23+RGR29+RGR35), data=tpc.all.c) 
  anova(mod.int.d)
  anova(mod.all.d,mod.int.d, test = "Chi")
  ## best model: no interactions
  summary(mod.all.d)
  
  #=======================================
  #ESTIMATE SELECTION GRADIENT
  
  #set up fitness metrics
  tpc.sel<- tpc.all.c
  tpc.sel$surv<- tpc.sel$Pupated
  tpc.sel$devrate<- tpc.sel$Time.to.Pupation #1/tpc.sel$Time.to.Pupation
  
  tpc.seln <- tpc.sel %>% 
    group_by(period, expt) %>% 
    dplyr::mutate(
      #standardize trait values by dividing by mean
      Mi_ms = (Mi/mean(Mi, na.rm = TRUE)),
      RGR11_ms = (RGR11/mean(RGR11, na.rm = TRUE)),
      RGR17_ms = (RGR17/mean(RGR17, na.rm = TRUE)),
      RGR23_ms = (RGR23/mean(RGR23, na.rm = TRUE)),
      RGR29_ms = (RGR29/mean(RGR29, na.rm = TRUE)),
      RGR35_ms = (RGR35/mean(RGR35, na.rm = TRUE)),
      #scale response by dividing by mean
      surv_norm = surv/mean(surv, na.rm = TRUE),
      devrate_norm = devrate /mean(devrate, na.rm = TRUE),
      Pupa.wt_norm = Pupa.wt /mean(Pupa.wt, na.rm = TRUE),
      Fecundity_norm = Fecundity /mean(Fecundity, na.rm = TRUE)
    ) %>%
    ungroup()
  
  #--------------------------------------
  #set up matrix for coefficients
  expts<- expts.lab
  periods<- c("recent","recent","past")
  rgrs<- c("RGR11","RGR17","RGR23","RGR29","RGR35")
  fitcomp<- c("mass","surv","devrate", "fec")
  
  sg= expand.grid(rgrs, expts.lab, fitcomp)
  sg= as.data.frame(cbind( sg,matrix(NA, nrow=nrow(sg), ncol=4)))
  colnames(sg)= c("rgr","expt", "fitcomp", "value", "se", "t-value", "p-value")
  sg$period <- periods[match(sg$expt, expts.lab)]
  
  #estimate selection
  for(k in 1:length(expts.lab)){
    
    #subset to experiment
    seln<- tpc.seln[which(tpc.seln$expt==expts.lab[k]),]
    
    #survival
    selmod.s <- lm(surv_norm~RGR11_ms+RGR17_ms+RGR23_ms+RGR29_ms+RGR35_ms, data=seln) 
    anova(selmod.s)
    coef.surv<- summary(selmod.s)$coefficients
    sg[which(sg$expt==expts[k] & sg$fitcomp=="surv"),c(4:7)]= coef.surv[2:nrow(coef.surv), ]
    
    #mass
    selmod.m <- lm(Pupa.wt_norm~RGR11_ms+RGR17_ms+RGR23_ms+RGR29_ms+RGR35_ms, data=seln) 
    anova(selmod.m)
    coef.pmass<- summary(selmod.m)$coefficients
    sg[which(sg$expt==expts[k] & sg$fitcomp=="mass"),c(4:7)]= coef.pmass[2:nrow(coef.pmass),]
    
    #dev time
    selmod.d <- lm(devrate_norm~RGR11_ms+RGR17_ms+RGR23_ms+RGR29_ms+RGR35_ms, data=seln) 
    anova(selmod.d)
    coef.devrate<- summary(selmod.d)$coefficients
    sg[which(sg$expt==expts[k] & sg$fitcomp=="devrate"),c(4:7)]= coef.devrate[2:nrow(coef.devrate), ]
    
    #fecundity
    seln.f<- seln[which(!is.na(seln$Fecundity_norm)),]
    
    if(nrow(seln.f)>0){
    selmod.f <- lm(Fecundity_norm~RGR11_ms+RGR17_ms+RGR23_ms+RGR29_ms+RGR35_ms, data=seln.f) 
    anova(selmod.f)
    coef.fec<- summary(selmod.f)$coefficients
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
  
  #Genetica plot #SWITCH FACETS
  plot.sg<- ggplot(sg[which(sg$fitcomp %in% c("mass", "surv", "devrate")),], aes(x=temp, y=value, color=expt, fill=sig, group=expt)) + 
    geom_point(size=4, pch=21)+ geom_smooth(se=FALSE)+
    facet_wrap(.~fitcomp)+
    ylab("Selection gradient") +xlab("Temperature (C)")+
    scale_color_viridis_d()+
    #ylim(-0.12, 0.12)+
    theme_classic()+
    scale_fill_manual(values = c("sig" = "gray", "ns" = "white"))+
  #add standard errors
  geom_errorbar(aes(x=temp, y=value, ymin=value-se, ymax=value+se), width=0)+
  #add horizontal line
  geom_hline(yintercept=0, color="gray")
  
  #--------------------------------
  #plot curves and selection arrows
  
  
  #see temp plots
  plot.sel<- Tdist.plot +
    #add TPC means
    geom_point(data=tpc.agg.f2, aes(x=temp, y=mean))+
    #add beta fit
    geom_line(data=tpc.dat, aes(x=temp, y=rate))+
    #add 1999 feeding data
    geom_point(data=tpc.agg.h, aes(x=temp, y=mean))+
    geom_line(data=tpc.agg.h, aes(x=temp, y=mean))+
    #add selection arrows
    geom_segment(data=sg, aes(x = temps, y = ys, xend = temps, yend = ys+pm.sg/20),
                 arrow = arrow(length = unit(0.5, "cm")), lwd=1.2)+
    #add additional axis
    scale_y_continuous(
      name = "Growth rate (g/g/h)", 
      sec.axis = sec_axis(~.x * 1, 
                          name = "Density of environmental data"))
  
  #save plots
  pdf("./figures/Prapae_selectiongradients.pdf",height = 6, width = 10)
  plot.sg
  dev.off()
  
  #plot models
  car::avPlots(mod.pmass.s)
  car::avPlots(mod.puptime.s)
  
  #==========================================
  ## P matrices, past and present
  
  #RECENT
  #P matrix
  p.mat<- var(tpc[tpc$period=="recent",c("RGR11","RGR17","RGR23","RGR29","RGR35")], na.rm=TRUE)*10^6
  p.mat.m <- melt(p.mat)
  
  #correlation matrix
  c.mat<-  cor(na.omit(tpc[tpc$period=="recent",c("RGR11","RGR17","RGR23","RGR29","RGR35")]))
  c.mat.m <- melt(c.mat)
  
  #HISTORIC
  #P matrix
  p.mat.h<- var(tpc[tpc$period=="past",c("RGR11","RGR17","RGR23","RGR29","RGR35")], na.rm=TRUE)*10^6
  p.mat.m.h <- melt(p.mat.h)
  
  #correlation matrix
  c.mat.h<-  cor(na.omit(tpc[tpc$period=="past",c("RGR11","RGR17","RGR23","RGR29","RGR35")]))
  c.mat.m.h <- melt(c.mat.h)
  
  #-------------------
  #Combine Var matrices
  p.mat.m$time<- "recent"; p.mat.m$type<- "var"
  p.mat.m.h$time<- "past"; p.mat.m.h$type<- "var"
  c.mat.m$time<- "recent"; c.mat.m$type<- "cor"
  c.mat.m.h$time<- "past"; c.mat.m.h$type<- "cor"
  
  var.all<- rbind(p.mat.m, p.mat.m.h, c.mat.m, c.mat.m.h)
  var.all$Var1 <- sub("RGR", "", var.all$Var1)
  var.all$Var2 <- sub("RGR", "", var.all$Var2)

  ### Variance Covariance plot
  # Current variances are larger now than in the past for both G and P, particularly at 35C
  # Stronger covariances now between performance at 29 and 35: indicative of tradeoff
  
  plot.var= ggplot(data = var.all[var.all$type=="var",], aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()+
    facet_grid(.~time)+
    scale_fill_gradient2(low ="orange", high = "blue", space = "Lab")+
    theme_bw(base_size=16) +xlab("Temperature (°C)") +ylab("Temperature (°C)") +ggtitle('A. correlation')
  
  cor.var= ggplot(data = var.all[var.all$type=="cor",], aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()+
    facet_grid(.~time)+
    scale_fill_gradient2(low ="orange", high = "blue", space = "Lab")+
    theme_bw(base_size=16) +xlab("Temperature (°C)") +ylab("Temperature (°C)")+ggtitle('B. variance covariance')
  
  #----------
  #Estimate eigenvectors (principal components)
  ep<- as.data.frame(eigen(p.mat)$vectors)
  eph<- as.data.frame(eigen(p.mat.h)$vectors)
  ec<- as.data.frame(eigen(c.mat)$vectors)
  ech<- as.data.frame(eigen(c.mat.h)$vectors)
  
  ep$period= "recent"; ep$type= "var"; ep$ev= c(1:5)
  eph$period= "past"; eph$type= "var"; eph$ev= c(1:5)
  ec$period= "recent"; ec$type= "cor"; ec$ev= c(1:5)
  ech$period= "past";  ech$type= "cor"; ech$ev= c(1:5)
  
  e.all<- rbind(ep, eph, ec, ech)
  colnames(e.all)[1:5]<- c(11, 17, 23, 29, 35)
  #restrict to first 2 eigenvectors
  e.all<- e.all[which(e.all$ev %in% c(1:2)),]
  
  #flip past ev 2s
  e.all[which(e.all$ev==2 & e.all$period=="past"),1:5]=  -1*e.all[which(e.all$ev==2 & e.all$period=="past"),1:5]
  
  #plot eigen vectors
  #to long format
  evp.l <- melt(e.all, 
                id.vars= c("period","ev","type"),
                 variable.name = "temp")
  evp.l$per.ev<- paste(evp.l$period, evp.l$ev, sep="_")
  
  plot.ev= ggplot(data = evp.l, aes(x=temp, y=value, color=factor(ev), lty=period, group=per.ev)) + 
    geom_point()+geom_smooth(se=FALSE)+ylab("eigenvector")+
    facet_wrap(type~., ncol=1) +theme_bw(base_size=16) +xlab("Temperature (°C)")

  design <- "AAC
             BBC"
  
  #save figure 
  pdf("./figures/PrapaeTPC_cov.pdf",height = 10, width = 10)
 plot.var + cor.var +plot.ev +plot_layout(design = design)
  dev.off()
  
  #=================================
  # Selection plots supplement
  
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
  #------------------
  
  # Plot correlations
  
  plot.cor1<- ggplot(tpc.l, aes(x=Pupa.wt,y=Time.to.Pupation, color=expt)) + 
    geom_point()+geom_smooth(method="lm")
  
  plot.cor2<- ggplot(tpc.l, aes(x=Pupa.wt,y=Fecundity, color=expt)) + 
    geom_point()+geom_smooth(method="lm")
  
  pdf("Prapae_correlations.pdf",height = 6, width = 10)
  plot.cor1 +plot.cor2
  dev.off()
  
  