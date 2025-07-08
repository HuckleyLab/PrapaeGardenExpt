library(ggplot2)
library(data.table)
library(dplyr)
library(patchwork)
library(reshape2)
library(viridis)
library(nlme)
library(lme4)
library(car)
#library(rstatix)

colm<- viridis_pal(option = "mako")(8)
cols<- colm[c(2,4,7)]
cols2<- colm[c(2,6)]

#toggle between desktop (y) and laptop (n)
desktop<- "n"
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
  geom_line(data=tpc.ragg.f, aes(x=temp, y = mean, group=Mom), linewidth=1, col=colm[6])+
#add points for temperature means  
  geom_errorbar(data=tpc.ragg, aes(x=temp, y=mean, ymin=mean-se, ymax=mean+se), width=0, col="black")+
  geom_point(data=tpc.ragg, aes(x=temp, y = mean), size=4, col="black", fill=colm[7], pch=21)+
  theme_classic(base_size=16)+xlab("Temperature (°C)")+ylab("")+
  ggtitle("B. 2024")+ylim(-0.02,0.06)

#------------
#Past TPC
tpc.p<- tpc[which(tpc$period=="past"),]
tpc.pl<- tpc.l[which(tpc.l$period=="past"),]
tpc.pagg<- tpc.agg[which(tpc.agg$period=="past"),]
tpc.pagg.f<- tpc.agg.f[which(tpc.agg.f$period=="past"),]

tpc.plot.p= ggplot(tpc.pl, aes(x=temp,y=value)) + 
  geom_point()+
  #add family lines
  geom_line(data=tpc.pagg.f, aes(x=temp, y = mean, group=Mom), linewidth=1, col=colm[2])+
  #add points for temperature means  
  geom_errorbar(data=tpc.pagg, aes(x=temp, y=mean, ymin=mean-se, ymax=mean+se), width=0, col="black")+
  geom_point(data=tpc.pagg, aes(x=temp, y = mean), size=4, col="black", fill="cornflowerblue", pch=21)+
  theme_classic(base_size=16)+xlab("")+ylab("Growth rate (g/g/h)")+
  ggtitle("A. 1999")+ylim(-0.02,0.06)

#------------
#plot TPCs over each other
tpc.agg.f$MomPer<- paste(tpc.agg.f$Mom, tpc.agg.f$period, sep="_")

tpc.plot.all= ggplot(tpc.agg.f, aes(x=temp,y=mean, col=factor(period)))+
  geom_line(aes(group=MomPer),linewidth=1, alpha=0.6) +scale_color_manual(values=cols2)+
  #add means
 geom_errorbar(data=tpc.agg, aes(x=temp, y=mean, ymin=mean-se, ymax=mean+se, col=factor(period)), width=0)+
    geom_point(data=tpc.agg, color="black", aes(x=temp, y = mean, fill=factor(period)), size=3, pch=21)+
    theme_classic(base_size=16)+xlab("")+ylab("")+
    labs(color="Year")+ scale_fill_manual(values=c("cornflowerblue",colm[7]))+
    ggtitle("C. 1999 & 2024") +ylim(-0.02,0.06) + guides(fill = FALSE, color=FALSE)
  
#------------
#save figure
  
  pdf("./figures/Fig2_PrapaeTPC_2024.pdf",height = 6, width = 12)
  tpc.plot.p + tpc.plot +tpc.plot.all
  dev.off()
  
  #-------------------
  #Analysis
  
 #drop garden columns
  tpc.b<- tpc[,c("Mom","ID","f.ind","Mi","RGR23","RGR29","RGR11","RGR17","RGR35","period")]
  tpc.b= na.omit(tpc.b)
  
  #single temp
  mod= lm(RGR35 ~ Mi + period, data= tpc.b) # 11 17 23 35
  anova(mod)
  #trade-off, significant difference at 17, 23, 35
  
  mod.lmer <- lme(RGR35 ~ Mi + period,random=~1|Mom, data = tpc.b)
  anova(mod.lmer)
  #same significance
  
  #across temps
  tpc.b<- tpc.l[,c("Mom","ID", "Mi", "f.ind","temp","value","period")]
  tpc.b= na.omit(tpc.b)
  
  mod= lm(value ~ Mi*poly(temp)*period, data= tpc.b)
  anova(mod)
  
  mod.lmer <- lme(value ~ poly(temp)*period*Mi,random=~1|Mom, data = tpc.b)
  table1<- as.data.frame(anova(mod.lmer))
  colnames(table1)[3:4]<- c("F","p")
  table1$sig<-""
  table1$sig[table1$p<0.05]<-"*"
  table1$sig[table1$p<0.01]<-"**"
  table1$sig[table1$p<0.001]<-"***"
  table1$F= round(table1$F,1)
  table1$p= round(table1$p,4)
  
  #Table 1
  write.csv(table1, "./figures/Table1.csv")
  
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
  
  #--------------
  #table 2.
  table2 <- anova(mod.int.m)
  table2 <- anova(mod.all.s, test="Chisq")
  table2 <- anova(mod.all.d)
 
  colnames(table2)[4:5]<- c("F","p")
  table2$sig<-""
  table2$sig[table2$p<0.05]<-"*"
  table2$sig[table2$p<0.01]<-"**"
  table2$sig[table2$p<0.001]<-"***"
  table2$F= round(table2$F,1)
  table2$p= round(table2$p,4)
  
  write.csv(table2, "./figures/tab2b.csv")
  
  #=======================================
  #FIG 4. ESTIMATE SELECTION GRADIENT
  
  #set up fitness metrics
  tpc.sel<- tpc.all.c
  tpc.sel$surv<- tpc.sel$Pupated
  tpc.sel$devrate<- 1/tpc.sel$Time.to.Pupation
  
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
      Fecundity_norm = Fecundity /mean(Fecundity, na.rm = TRUE),
      RGR11m = mean(RGR11, na.rm = TRUE),
      RGR17m = mean(RGR17, na.rm = TRUE),
      RGR23m = mean(RGR23, na.rm = TRUE),
      RGR29m = mean(RGR29, na.rm = TRUE),
      RGR35m = mean(RGR35, na.rm = TRUE) 
    ) %>%
    ungroup()
  
  #mean growth rate
  tpc.mean <- tpc.sel %>% 
    group_by(period, expt) %>% 
    dplyr::summarise(
      RGR11m = mean(RGR11, na.rm = TRUE),
      RGR17m = mean(RGR17, na.rm = TRUE),
      RGR23m = mean(RGR23, na.rm = TRUE),
      RGR29m = mean(RGR29, na.rm = TRUE),
      RGR35m = mean(RGR35, na.rm = TRUE) 
    )
  
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
    
    #dev rate
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
  
  sg$temp<- as.numeric(sg$temp)
  
  #labels
  fitcomp.lab<- c("Pupal mass","Survival","Development rate", "Fecundity")
  sg$fitcomp.lab<- fitcomp.lab[match(sg$fitcomp, fitcomp)]
  #order
  sg$fitcomp.lab<- factor(sg$fitcomp.lab, levels=c("Survival","Development rate", "Pupal mass","Fecundity"), ordered=TRUE)
  
  #Genetica plot
  plot.sg<- ggplot(sg[which(sg$fitcomp %in% c("mass", "surv", "devrate")),], aes(x=temp, y=value, color=expt, fill=sig, group=expt)) + 
    geom_point(size=4, pch=21)+ geom_smooth(se=FALSE)+
    facet_wrap(.~fitcomp.lab, scales="free_y")+
    ylab("Selection gradient") +xlab("Temperature (°C)")+
    scale_color_manual(values=cols)+
    #ylim(-0.3, 0.3)+
    theme_bw()+xlim(10,36)+
    scale_fill_manual(values = c("sig" = "gray", "ns" = "white"))+
  #add standard errors
  geom_errorbar(aes(x=temp, y=value, ymin=value-se, ymax=value+se, color=expt), width=0)+
  #add horizontal line
  geom_hline(yintercept=0, color="gray")+
    theme(legend.position = "none")+ #+guides(fill = "none")+
  #center scales
  geom_blank(aes(y = -value, ymin = -(value-se), ymax = -(value+se)))
  
  #fecundity
  plot.sg.fec<- ggplot(sg[which(sg$fitcomp %in% c("fec")),], aes(x=temp, y=value, color=expt, fill=sig, group=expt)) + 
    geom_point(size=4, pch=21)+ geom_smooth(se=FALSE)+
    facet_wrap(.~fitcomp.lab, scales="free_y")+
    ylab("Selection gradient") +xlab("Temperature (°C)")+
    scale_color_manual(values=cols)+
    #ylim(-0.3, 0.3)+
    theme_bw()+xlim(10,36)+
    scale_fill_manual(values = c("sig" = "gray", "ns" = "white"))+
    #add standard errors
    geom_errorbar(aes(x=temp, y=value, ymin=value-se, ymax=value+se, color=expt), width=0)+
    #add horizontal line
    geom_hline(yintercept=0, color="gray")+
    theme(legend.position = "none")+ #+guides(fill = "none")+
    #center scales
    geom_blank(aes(y = -value, ymin = -(value-se), ymax = -(value+se)))
  
  #-------------
  #Table S2. write selection gradients
  sg.p <- sg[,c("expt", "fitcomp.lab", "temp",  "value", "se", "tvalue", "pvalue")]
  #order
  sg.p<- sg.p[order(sg.p$expt, sg.p$fitcomp.lab, sg.p$temp),]
  #round
  sg.p[,4:7]<- round( sg.p[,4:7], 3)
  
  #write out
  write.csv(sg.p, "./figures/TableS2_selgradient.csv")
  #--------------------------------
  #plot curves and selection arrows
  
  #account for normalization by mean
  tpc.meanl<- melt(tpc.mean, id.vars = c("expt","period"), variable.name = "temp")
  tpc.meanl$temp<- gsub("m", "", as.character(tpc.meanl$temp))
  tpc.meanl$et<- paste(tpc.meanl$expt, tpc.meanl$temp, sep="_")
  sg$et<- paste(sg$expt, sg$rgr, sep="_")
    
  sg$value_ms <- sg$value * tpc.meanl$value[match(sg$et, tpc.meanl$et)]
  
  tpc.s<- tpc.agg
  tpc.s$tp<- paste(tpc.s$temp, tpc.s$period, sep="_") 
  #add selection data
  sg$tp<- paste(sg$temp, sg$period, sep="_")
  match1<- match(sg$tp, tpc.s$tp)
  sg$gr<- tpc.s$mean[match1]
  sg$gr.se<- tpc.s$se[match1]
  
  #plot
  plot.arr<- ggplot(sg[which(sg$fitcomp %in% c("mass", "surv", "devrate")),], aes(x= temp, y=gr, col=expt, group=expt)) +
    geom_line(linewidth=1)+
    facet_wrap(.~fitcomp.lab)+
    ylab("Growth rate (g/g/h)") + xlab("Temperature (°C)")+
    theme_bw()+scale_color_manual(values=cols)+
    ylim(0.0045, 0.0315)+xlim(10,36)+
    #add selection arrows
    geom_segment( aes(x = temp, y = gr, xend = temp, yend = gr+value_ms),
                 arrow = arrow(length = unit(0.2, "cm")), linewidth=1.0, 
                 position = position_jitter(w = 1, h = 0))+
    theme(legend.position = "bottom")+labs(col="Study")
  #scale arrow length
  
  #fec
  plot.arr.fec<- ggplot(sg[which(sg$fitcomp %in% c("fec")),], aes(x= temp, y=gr, col=expt, group=expt)) +
    geom_line(linewidth=1)+
    facet_wrap(.~fitcomp.lab)+
    ylab("Growth rate (g/g/h)") + xlab("Temperature (°C)")+
    theme_bw()+scale_color_manual(values=cols)+
    ylim(0.005, 0.035)+xlim(10,36)+
    #add selection arrows
    geom_segment( aes(x = temp, y = gr, xend = temp, yend = gr+value_ms),
                  arrow = arrow(length = unit(0.2, "cm")), linewidth=1.0, 
                  position = position_jitter(w = 1, h = 0))+
    theme(legend.position = "bottom")+labs(col="Study")
  
  #save plots
  pdf("./figures/Fig4_Prapae_selectiongradients.pdf",height = 8, width = 8)
  plot.sg +plot.arr +plot_layout(ncol=1)+plot_annotation(tag_levels = 'A')
  dev.off()
  
  #fecundity plots
  pdf("./figures/Fig4_Prapae_selectiongradients_fec.pdf",height = 8, width = 4)
  plot.sg.fec +plot.arr.fec +plot_layout(ncol=1)+plot_annotation(tag_levels = 'A')
  dev.off()
  
  #---------------------------
  #mean and sd fitness metrics
  
  tpc.mfit <- tpc.sel %>% 
    group_by(expt) %>% 
    dplyr::summarize(
      surv.m = mean(surv, na.rm = TRUE),
      surv.sd = sd(surv, na.rm = TRUE),
      devrate.m = mean(devrate, na.rm = TRUE),
      devrate.sd = sd(devrate, na.rm = TRUE),
      Pupa.wt.m = mean(Pupa.wt, na.rm = TRUE),
      Pupa.wt.sd = sd(Pupa.wt, na.rm = TRUE),
      Fecundity.m = mean(Fecundity, na.rm = TRUE),
      Fecundity.sd = sd(Fecundity, na.rm = TRUE)
    )

  #round
  tpc.mfit[,c(2:3)] <- round(tpc.mfit[,c(2:3)], 3) 
  tpc.mfit[,c(4:5)] <- round(tpc.mfit[,c(4:5)], 4)
  tpc.mfit[,c(6:9)] <- round(tpc.mfit[,c(6:9)], digits=2)
  
  #save table  
  write.csv( tpc.mfit, "./figures/FitTable.csv")
  
  #==========================================
  #FIGURE 3. P, cor matrices, past and present
  
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
  p.mat.m.h$time<- "initial"; p.mat.m.h$type<- "var"
  c.mat.m$time<- "recent"; c.mat.m$type<- "cor"
  c.mat.m.h$time<- "initial"; c.mat.m.h$type<- "cor"
  
  var.all<- rbind(p.mat.m, p.mat.m.h, c.mat.m, c.mat.m.h)
  var.all$Var1 <- sub("RGR", "", var.all$Var1)
  var.all$Var2 <- sub("RGR", "", var.all$Var2)

  ### Variance Covariance plot
  # Current variances are larger now than in the past for both G and P, particularly at 35C
  # Stronger covariances now between performance at 29 and 35: indicative of tradeoff
  
  plot.var= ggplot(data = var.all[var.all$type=="var",], aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()+
    facet_grid(.~time)+
    scale_fill_gradient2(low ="darkgreen", high = "darkblue", space = "Lab",
                         breaks = c(-30,0,130),labels=c(-30,0,130))+
    theme_bw(base_size=16) +xlab("Temperature (°C)") +ylab("Temperature (°C)") +ggtitle('A. variance covariance')
  
  cor.var= ggplot(data = var.all[var.all$type=="cor",], aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()+
    facet_grid(.~time)+
    scale_fill_gradient2(low ="darkgreen", high = "darkblue", space = "Lab",
                         breaks = c(-0.3,0,1),labels=c(-0.3,0,1))+
    theme_bw(base_size=16) +xlab("Temperature (°C)") +ylab("Temperature (°C)")+ggtitle('B. correlation')
  
  #----------
  #Estimate eigenvectors (principal components)
  ep<- eigen(p.mat)
  eph<- eigen(p.mat.h)
  ec<- eigen(c.mat)
  ech<- eigen(c.mat.h)
  
  #proportion variance accounted
  eig<- ech
  eig$values / sum(eig$values)
  
  ep<- as.data.frame(eigen(p.mat)$vectors)
  eph<- as.data.frame(eigen(p.mat.h)$vectors)
  ec<- as.data.frame(eigen(c.mat)$vectors)
  ech<- as.data.frame(eigen(c.mat.h)$vectors)
  
  ep$period= "recent"; ep$type= "var"; ep$ev= c(1:5)
  eph$period= "initial"; eph$type= "var"; eph$ev= c(1:5)
  ec$period= "recent"; ec$type= "cor"; ec$ev= c(1:5)
  ech$period= "initial";  ech$type= "cor"; ech$ev= c(1:5)
  
  e.all<- rbind(ep, eph, ec, ech)
  colnames(e.all)[1:5]<- c(11, 17, 23, 29, 35)
  #restrict to first 2 eigenvectors
  e.all<- e.all[which(e.all$ev %in% c(1:2)),]
  
  #flip past ev 2s
  e.all[which(e.all$ev==2 & e.all$period=="initial"),1:5]=  -1*e.all[which(e.all$ev==2 & e.all$period=="initial"),1:5]
  
  #plot eigen vectors
  #to long format
  evp.l <- melt(e.all, 
                id.vars= c("period","ev","type"),
                 variable.name = "temp")
  evp.l$per.ev<- paste(evp.l$period, evp.l$ev, sep="_")
  
  types<- c("var","cor")
  types.lab<- c("variance","correlation")
  evp.l$type.lab<- types.lab[match(evp.l$type, types)]
  evp.l$type.lab<- factor(evp.l$type.lab, levels=c("variance","correlation"), ordered=TRUE)
  
  plot.ev= ggplot(data = evp.l, aes(x=temp, y=value, lty=factor(ev), color=period, group=per.ev)) + 
    geom_point()+geom_smooth(se=FALSE)+ylab("eigenvector")+
    facet_wrap(type.lab~., ncol=1) +
    theme_bw(base_size=16) +xlab("Temperature (°C)")+ylab("Eigenvector")+
    scale_color_manual(values=cols2)+labs(lty="vector")
    #scale_color_viridis_d()#+guides(lty = "none")

  design <- "AA
             BB"
  
  #save figure 
  pdf("./figures/Fig3_PrapaeTPC_cov.pdf",height = 8, width = 8)
 plot.var + cor.var +plot_layout(design = design)
  dev.off()
  
  #eigenvectors
  pdf("./figures/FigSx_eigenvectors.pdf",height = 8, width = 8)
  plot.ev
  dev.off()
  
  #Table S. P and correlation matrices
  p.mat2<- as.data.frame(p.mat)
  p.mat.h2<- as.data.frame(p.mat.h)
  c.mat2<- as.data.frame(c.mat)
  c.mat.h2<- as.data.frame(c.mat.h)
  
  p.mat2$type= "P"; p.mat2$period="recent"
  p.mat.h2$type= "P"; p.mat.h2$period="initial"
  c.mat2$type= "correlation"; c.mat2$period="recent"
  c.mat.h2$type= "correlation"; c.mat.h2$period="initial"
  
  var.all2<- rbind(p.mat2, p.mat.h2, c.mat2, c.mat.h2)
  #round
  var.all2[,1:5]=round( var.all2[,1:5], 3)
  
  #write out
  write.csv(var.all2, "./figures/TableS1_pcor.csv")
  
  #---------------------------
  #Compare matrices through time
  
  library(heplots)
  library(evolqg)
  library(CPC)
  
 # Matrix Similarity Metrics
  MatrixCompare(p.mat, p.mat.h)
  
  # Krzanowski's distance
  #Calculates covariance matrix correlation via Krzanowski Correlation
  krz_distance <- KrzCor(p.mat, p.mat.h)
  
  # Random Skewers analysis
  # Cheverud, J. M., and Marroig, G. (2007). Comparing covariance matrices: Random skewers method compared to the common principal components model. Genetics and Molecular Biology, 30, 461-469.
  RandomSkewers(p.mat, p.mat.h, num_vectors = 1000)
  #returns average value of response vectors correlation ('correlation'), significance ('probability') and standard deviation of response vectors correlation ('correlation_sd')
  #other: http://www.phytools.org/static.help/skewers.html
  
  #Compare matrices via the correlation between response vectors
  n_skewers = 5
  skewers = matrix(rnorm(5*n_skewers), 5, n_skewers)
  DeltaZCorr(p.mat, p.mat.h, skewers)
  
  #Eigen analysis
  #Bookstein, F. L., and P. Mitteroecker, P. "Comparing Covariance Matrices by Relative Eigenanalysis, with Applications to Organismal Biology." Evolutionary Biology 41, no. 2 (June 1, 2014): 336-350. doi:10.1007/s11692-013-9260-5.
  RelativeEigenanalysis(p.mat, p.mat.h, symmetric = FALSE)
  
  PCAsimilarity(p.mat, p.mat.h)
  
  #Mantel Test of correlation matrices
  MantelCor(c.mat.h, c.mat)
  #returns matrix Pearson correlation coefficient and significance via Mantel permutations
  
  #Other packages
  #https://cloud.r-project.org/web/packages/covTestR/covTestR.pdf
  #https://rdrr.io/github/bbolker/cpcbp/man/phillips.cpc.html
  
  #----------------------
  #correlations
  ggplot(data=tpc, aes(x=RGR35, y=RGR29, color=period))+geom_point()+geom_smooth(method=lm)
  anova(lm(RGR35~RGR29*period, data=tpc))
  
  ggplot(data=tpc, aes(x=RGR11, y=RGR23, color=period))+geom_point()+geom_smooth(method=lm)
  ggplot(data=tpc, aes(x=RGR11, y=RGR29, color=period))+geom_point()+geom_smooth(method=lm)
  anova(lm(RGR11~RGR23*period, data=tpc))
  anova(lm(RGR11~RGR29*period, data=tpc))
  
  #=================================
  # Selection plots supplement
  
  #drop one outlier big pupal mass for plotting
  tpc.l<- tpc.l[-which(tpc.l$Pupa.wt>275),]
  
  #update temperature label
  temp5<- c(11, 17, 23, 29, 35)
  temps.lab<- c("11°C","17°C","23°C","29°C","35°C")
  tpc.l$temps.lab<- temps.lab[match(tpc.l$temp, temp5)]
  tpc.l$temps.lab<- factor(tpc.l$temps.lab, levels=c("11°C","17°C","23°C","29°C","35°C"), ordered=TRUE)
  
  #selection plots
  plot.pm.b<- ggplot(tpc.l, aes(x=value,y=Pupa.wt)) + 
    facet_grid(expt~temps.lab)+
    geom_point()+geom_smooth(method="lm")+
    ylab("Pupal mass (mg)") +xlab("Growth rate (g/g/h)")+
    theme_bw(base_size=18)
  
  plot.surv.b<- ggplot(tpc.l, aes(x=value,y=Pupated)) + 
    facet_grid(expt~temps.lab)+
    geom_point()+geom_smooth(method="lm")+
    ylab("Survival") +xlab("Growth rate (g/g/h)")+
    theme_bw(base_size=18)
  
  #survival distribution plots
  plot.surv.histb<- ggplot(tpc.l[!is.na(tpc.l$Pupated),], aes(x=value,color=factor(Pupated), group=Pupated)) + 
    facet_grid(expt~temps.lab)+
    geom_density(aes(fill=factor(Pupated)), alpha=0.5)+
    ylab("Density") +xlab("Growth rate (g/g/h)")+
    theme_bw(base_size=18)
  
  #change to development rate
  tpc.l$devrate= 1/tpc.l$Time.to.Pupation
  
  plot.pt.b<- ggplot(tpc.l, aes(x=value,y=devrate)) + 
    facet_grid(expt~temps.lab)+
    geom_point()+geom_smooth(method="lm")+
    ylab("Development rate (1/day)") +xlab("Growth rate (g/g/h)")+
    theme_bw(base_size=18)
  
  plot.ec.b<- ggplot(tpc.l, aes(x=value,y=Fecundity)) + 
    facet_grid(expt~temp)+
    geom_point()+geom_smooth(method="lm")+
    ylab("Egg count") +xlab("Growth rate (g/g/h)")+
    theme_bw(base_size=18)
  
  #-------
  #save figures
  #Figure S1. Pupal mass
  pdf("./figures/FigS1_GardenMass.pdf",height = 8, width = 11)
  plot.pm.b
  dev.off()
  
  #Figure S2. survival
  pdf("./figures/FigS2_GardenSurvival.pdf",height = 8, width = 11)
  plot.surv.b
  dev.off()
  
  pdf("./figures/FigS3x_GardenSurvivalDist.pdf",height = 8, width = 11)
  plot.surv.histb
  dev.off()
  
  #Figure S2. pupal time
  pdf("./figures/FigS3_GardenPupalTime.pdf",height = 8, width = 11)
  plot.pt.b
  dev.off()
  
  #Figure S4. egg count
  pdf("./figures/FigS4_GardenEggCount.pdf",height = 8, width = 11)
  plot.ec.b
  dev.off()
  
  #==============================================
  # Plot trait changes
  tpc.tc<- tpc[,c("f.ind", "expt","Mi","Pupa.wt","Butt..Wt","Time.to.Pupation","Time.to.Eclosion","Fecundity","Pupated")]
  
  #means over time
  tpc.tc.mean <- tpc.tc %>% 
    group_by(expt) %>% 
    dplyr::summarise(
      Mi = mean(Mi, na.rm = TRUE),
      Pupa.wt = mean(Pupa.wt, na.rm = TRUE),
      Butt..Wt = mean(Butt..Wt, na.rm = TRUE),
      Time.to.Pupation = mean(Time.to.Pupation, na.rm = TRUE),
      Time.to.Eclosion = mean(Time.to.Eclosion, na.rm = TRUE),
      Fecundity = mean(Fecundity, na.rm = TRUE),
      Survival = mean(Pupated, na.rm = TRUE)
    )
  
  #to long format
  tpc.tcl<- melt(tpc.tc, id.vars = c("f.ind", "expt"), variable.name = "trait")
  tpc.tc.meanl<- melt(tpc.tc.mean, id.vars = c("expt"), variable.name = "trait")
  tpc.tc.meanl$value[is.nan(tpc.tc.meanl$value)]<-NA
  #drop survival
  tpc.tcl<- tpc.tcl[-which(tpc.tcl$trait=="Pupated"),]
  tpc.tc.meanl<- tpc.tc.meanl[-which(tpc.tc.meanl$trait=="Survival"),]
  
  #labels
  traits= c("Mi","Pupa.wt","Butt..Wt","Time.to.Pupation","Time.to.Eclosion","Fecundity")
  traits.lab= c("Initial mass (mg)","Pupal mass (mg)","Butterfly mass (mg)","Time to pupation (days)","Time to eclosion (days)","Fecundity")
  tpc.tcl$trait.lab<- traits.lab[match(tpc.tcl$trait, traits)]
  tpc.tc.meanl$trait.lab<- traits.lab[match(tpc.tc.meanl$trait, traits)]
  #order
  tpc.tcl$trait.lab<- factor(tpc.tcl$trait.lab, levels=c("Initial mass (mg)","Pupal mass (mg)","Butterfly mass (mg)","Time to pupation (days)","Time to eclosion (days)","Fecundity"), ordered=TRUE)
  tpc.tc.meanl$trait.lab<- factor(tpc.tc.meanl$trait.lab, levels=c("Initial mass (mg)","Pupal mass (mg)","Butterfly mass (mg)","Time to pupation (days)","Time to eclosion (days)","Fecundity"), ordered=TRUE)
  
  #plot
  plot.tc<- ggplot(tpc.tcl, aes(x=value,color=expt, group=expt)) + 
    geom_density(aes(fill=expt), alpha=0.5)+
    ylab("Density") +theme_bw()+
    scale_color_manual(values=cols)+scale_fill_manual(values=cols)+labs(color="Study", fill="Study")+
    #add mean lines
    geom_vline(data=tpc.tc.meanl, aes(xintercept=value, color=expt, group=expt))+
    facet_wrap(.~trait.lab, scales="free")+
    theme(legend.position="bottom")
    
  pdf("./figures/FigS5_Prapae_TraitChange.pdf",height = 6, width = 10)
  plot.tc
  dev.off()
  
  #----
  #Traits over time
  
  tpc.tcl %>% 
    group_by(trait) %>%
    rstatix::levene_test(value~ expt)
  #time to putation and time to eclosion change in variance
  
  tpc.tcl[which(tpc.tcl$trait==traits[2]),] %>% 
    #group_by(trait) %>%
    #welch test
    #t_test(value~ expt) %>%
    #equal variances t-test
    rstatix::t_test(value~expt, var.equal = TRUE) %>%
    rstatix::add_significance()
  
  #compare means
  mod.m<- lm(value~expt, data=tpc.tcl[which(tpc.tcl$trait==traits[5]),])
  anova(mod.m)
  
  #------------------
  
  # Plot correlations
  plot.cor1<- ggplot(tpc.l, aes(x=Pupa.wt,y=Time.to.Pupation, color=expt)) + 
    geom_point()+geom_smooth(method="lm")
  
  plot.cor2<- ggplot(tpc.l, aes(x=Pupa.wt,y=Fecundity, color=expt)) + 
    geom_point()+geom_smooth(method="lm")
  
  pdf("Prapae_correlations.pdf",height = 6, width = 10)
  plot.cor1 +plot.cor2
  dev.off()
  
  