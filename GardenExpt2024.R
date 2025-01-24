library(ggplot2)
library(data.table)
library(dplyr)
library(patchwork)
library(reshape2)
library(viridis)

#toggle between desktop (y) and laptop (n)
desktop<- "y"

if(desktop=="y") setwd("/Users/laurenbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Metadata/2024PierisExpts/")
if(desktop=="n") setwd("/Users/lbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Metadata/2024PierisExpts/")
#setwd("/Users/laurenbuckley/Google Drive/My Drive/Buckley/Work/WARP/data/PrapaeGarden2024/")

tpc1= read.csv("2024TPCFieldSelnJune.csv")
tpc1$expt<- "june"
tpc1 <- tpc1[,-c(44:47)]

tpc2= read.csv("2024TPCFieldSelnJuly(Batch 1, 7-27).csv")
tpc2<- tpc2[,-which(colnames(tpc2)=="X29.notes.1")]
tpc3= read.csv("2024TPCFieldSelnJuly(Batch 2, 7-28).csv")
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
tpc= tpc[,c("FEMALE","f.ind","rgr_23","rgr_29","rgr_11","rgr_17","rgr_35")]

#to long format
tpc.l <- melt(setDT(tpc), id.vars = c("FEMALE","f.ind"), variable.name = "temp")
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
  geom_line(data=tpc.agg.f, aes(x=temp, y = mean, group=FEMALE), size=1, col="orange")

#---
#plot mean values
tpc.agg <- tpc.l %>% 
  group_by(temp) %>% 
  dplyr::summarise(mean = mean(value, na.rm = TRUE),
            se = sd(value, na.rm = TRUE)/length(value) )

#add means
tpc.plot= tpc.plot + 
  geom_errorbar(data=tpc.agg, aes(x=temp, y=mean, ymin=mean-se, ymax=mean+se), width=0, col="blue")+
  geom_point(data=tpc.agg, aes(x=temp, y = mean), size=3, col="blue")+
  theme_bw()+xlab("Temperature (C)")+ylab("RGR (g/g/h)")+
  ggtitle("2024") +ylim(-0.02,0.06)

#-------------------------
#Historic data

if(desktop=="y") setwd("/Users/laurenbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Analyses/data/Initial/")
if(desktop=="n") setwd("/Users/lbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Analyses/data/Initial/")
#setwd('/Volumes/GoogleDrive/Shared drives/TrEnCh/Projects/WARP/Analyses/data/Initial/')
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
  geom_line(data=tpc.agg.fh, aes(x=temp, y = mean, group=Mom), size=1, col="darkgreen")

#---
#plot mean values
tpc.agg.h <- tpc.lh %>% 
  group_by(temp) %>% 
  dplyr::summarise(mean = mean(value, na.rm = TRUE),
            se = sd(value, na.rm = TRUE)/length(value) )

#add means
tpc.plot.h= tpc.plot.h + 
  geom_errorbar(data=tpc.agg.h, aes(x=temp, y=mean, ymin=mean-se, ymax=mean+se), width=0, col="blue")+
  geom_point(data=tpc.agg.h, aes(x=temp, y = mean), size=3, col="blue")+
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
  geom_line(aes(group=yrfemale)) +scale_color_manual(values=c("darkgreen", "orange"))
#+scale_color_viridis_d()

  #add means
  tpc.all.plot= tpc.all.plot + 
    geom_errorbar(data=tpc.agg.all, aes(x=temp, y=mean, ymin=mean-se, ymax=mean+se, col=factor(year)), width=0)+
    geom_point(data=tpc.agg.all, aes(x=temp, y = mean, col=factor(year)), size=3)+
    theme_bw()+xlab("Temperature (C)")+ylab("RGR (g/g/h)")+
    labs(color="Year")+
    ggtitle("1999 & 2024") +ylim(-0.02,0.06) 
  
  #----------------
  #save figure
  if(desktop=="y") setwd("/Users/laurenbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Analyses/figures/")
  if(desktop=="n") setwd("/Users/lbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Analyses/figures/")
  #setwd('/Volumes/GoogleDrive/Shared drives/TrEnCh/Projects/WARP/Analyses/figures/')
  pdf("PrapaeTPC_2024.pdf",height = 6, width = 15)
  tpc.plot.h + tpc.plot +tpc.all.plot
  dev.off()
  
  #-------------------
  #Analysis
  
  library(nlme)
  
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
  
  if(desktop=="y") setwd("/Users/laurenbuckley/Google Drive/My Drive/Buckley/Work/WARP/data/PrapaeGarden2024/")
  if(desktop=="n") setwd("/Users/lbuckley/Google Drive/My Drive/Buckley/Work/WARP/data/PrapaeGarden2024/")
  gdat= read.csv("Expt2ChecksSurvival2024.csv")
  gdat$expt<- "june"
  gdat<- gdat[,-c(11:14)]
  gdat2= read.csv("Expt2ChecksSurvival2024 - July Experiments.csv")
  gdat2$expt<- "july"
  
  gdat<- rbind(gdat, gdat2)
  
  #match TPC to field
  gdat$f.ind<- paste(gdat$Female, gdat$Individual, gdat$expt, sep="_")
  
  match1<- match(tpc$f.ind, gdat$f.ind)
  
  matched<- which(!is.na(match1))
  
  tpc$pupal_massmg<- NA
  tpc$pupal_massmg[matched]<- gdat$pupal_massmg[match1[matched]]
  tpc$pupal_massmg<- as.numeric(tpc$pupal_massmg)
  
  #to long format
  tpc.gl <- melt(tpc, id.vars = c("FEMALE","f.ind","pupal_massmg"), variable.name = "temp")
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
  
  if(desktop=="y") setwd("/Users/laurenbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Analyses/data/Initial/")
  if(desktop=="n") setwd("/Users/lbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Analyses/data/Initial/")
  #setwd('/Volumes/GoogleDrive/Shared drives/TrEnCh/Projects/WARP/Analyses/data/Initial/')
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
  tpc.gl.all<- as.data.frame(rbind(tpc.gl[,c("expt","pupal_massmg","temp","value")], tpc.gl.h[,c("expt","pupal_massmg","temp","value")]))

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
  
  #----------------------
  #save figure
  if(desktop=="y") setwd("/Users/laurenbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Analyses/figures/")
  if(desktop=="n") setwd("/Users/lbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Analyses/figures/")
  #setwd('/Volumes/GoogleDrive/Shared drives/TrEnCh/Projects/WARP/Analyses/figures/')
  pdf("PrapaeGarden_2024.pdf",height = 8, width = 8)
  plot.pm
  dev.off()
  
  pdf("PrapaeGardenHist_2024.pdf",height = 8, width = 11)
  plot.pm.h +plot.fec.h +plot.surv.h +plot.ttp.h
  dev.off()
  
  pdf("PrapaeGarden_both.pdf",height = 8, width = 11)
  plot.pm.b
  dev.off()
  
  #-------
  #analyze
  mod= lm(pupal_massmg ~ rgr_11 +rgr_17 +rgr_23 +rgr_29 +rgr_35, data= tpc) 
  anova(mod)
  
  mod.lmer <- lme(pupal_massmg ~ rgr_11 +rgr_17 +rgr_23 +rgr_29 +rgr_35,random=~1|FEMALE, data = na.omit(tpc))
  anova(mod.lmer)
  
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
  
  tpc.h2<- tpc.h2[,c("FEMALE","f.ind","expt","rgr_23","rgr_29","rgr_11","rgr_17","rgr_35","pupal_massmg","FecEggCount","surv","puptime","time")]
  
  tpc.all<- rbind(tpc, tpc.h2)
  
  #pick response variable
  tpc.all$rvar<- tpc.all$pupal_massmg
  tpc.all$rvar<- tpc.all$FecEggCount
  tpc.all$rvar<- tpc.all$surv
  tpc.all$rvar<- tpc.all$puptime
  
  tpcm<- tpc.all[,-"FecEggCount"]
  
  mod= lm(rvar ~ rgr_11 +rgr_17 +rgr_23 +rgr_29 +rgr_35 +time+ rgr_11*time +rgr_17*time +rgr_23*time +rgr_29*time +rgr_35*time, data= tpcm) 
  anova(mod)
  
  mod.lmer <- lme(rvar ~ rgr_11 +rgr_17 +rgr_23 +rgr_29 +rgr_35 +time+ rgr_11*time +rgr_17*time +rgr_23*time +rgr_29*time +rgr_35*time, random=~1|FEMALE, data = na.omit(tpcm))
  anova(mod.lmer)
  
  #--------------------
  #G matrix
  
  library(lme4)
  
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
  
  library(evolqg)
  
  old <- options(contrasts=c("contr.sum","contr.poly"))
  
  tpc<- as.matrix(tpc)
  tpc.lm = lm(tpc[,c(5,6,3,4,7)]~as.factor(tpc[,"FEMALE"]))
  cov.matrix <- CalculateMatrix(tpc.lm)
  
  options(old)
  #To obtain a correlation matrix, use:
  cor.matrix <- cov2cor(cov.matrix)
  
  cor.matrix <- cov2cor(cov.matrix)
  
  cor.matrix.m <- melt(cor.matrix)
  plot.cov2= ggplot(data = cor.matrix.m, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()+scale_fill_viridis()
  
  #tutorial
  #  https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/j.1365-2656.2009.01639.x
  
  #But: a quick and dirty method is probably sufficient for now:  compute mean values for each family at each temperature, and then use the var function is compute the var-covariance matrix on the family means.  This is the least-squares estimate of the broad sense G matrix.  Using var on the individual values gives the comparable estimate of the P (phenotypic) matrix.  These should give a good approximation to the REML estimates (which are constrained so that you don’t get variances< 0 ).
  
  #family means
  tpc.f <- tpc %>% 
    group_by(FEMALE) %>% 
    dplyr::summarise(rgr_11 = mean(rgr_11, na.rm=T),
                     rgr_17 = mean(rgr_17, na.rm=T),
                     rgr_23 = mean(rgr_23, na.rm=T),
                     rgr_29 = mean(rgr_29, na.rm=T),
                     rgr_35 = mean(rgr_35, na.rm=T) )
  
  tpc.fh <- tpc.h %>% 
    group_by(Mom) %>% 
    dplyr::summarise(rgr_11 = mean(RGR11, na.rm=T),
                     rgr_17 = mean(RGR17, na.rm=T),
                     rgr_23 = mean(RGR23, na.rm=T),
                     rgr_29 = mean(RGR29, na.rm=T),
                     rgr_35 = mean(RGR35, na.rm=T) )
  
  #G matrix
  g.mat<- var(tpc.f[,-1], na.rm=TRUE)*10^6
  
  g.mat.m <- melt(g.mat)
  plot.g.mat= ggplot(data = g.mat.m, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()+scale_fill_viridis()
  
  g.mat.h<- var(tpc.fh[,-1], na.rm=TRUE)*10^6
  
  g.mat.mh <- melt(g.mat.h)
  plot.g.math= ggplot(data = g.mat.mh, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()+scale_fill_viridis()
  
  #P matrix
  p.mat<- var(tpc[,c(5,6,3,4,7)], na.rm=TRUE)*10^6
  
  p.mat.m <- melt(p.mat)
  plot.p.mat= ggplot(data = p.mat.m, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()+scale_fill_viridis()
  
  p.mat.h<- var(tpc.h[,c(5,6,3,4,7)], na.rm=TRUE)*10^6
  
  p.mat.mh <- melt(p.mat.h)
  plot.p.math= ggplot(data = p.mat.mh, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()+scale_fill_viridis()
  
  #old correlations
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
  
  #----------------
  #save figure
  setwd('/Volumes/GoogleDrive/Shared drives/TrEnCh/Projects/WARP/Analyses/figures/')
  pdf("PrapaeTPC_cov.pdf",height = 6, width = 15)
  plot.cov + plot.cov2 |
    plot.p.mat + plot.cov.old
  dev.off()
  
  plot.p.mat + plot.g.mat +plot.cov.old
  
  #-------------------- 
  plot.cov
  plot.cov.old
  
  plot.g.mat + plot.p.mat | plot.g.math + plot.p.math
  
  