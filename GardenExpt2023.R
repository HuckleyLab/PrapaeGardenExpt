library(ggplot2)
library(data.table)
library(dplyr)
library(patchwork)
library(reshape2)
library(viridis)

desktop<- "y"
if(desktop=="y") setwd("/Users/laurenbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Analyses/data/PrapaeGarden2023/")
if(desktop=="n") setwd("/Users/lbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Analyses/data/PrapaeGarden2023/")

tpc1= read.csv("CaterpillarTPC_8Jul2023.csv")
tpc1$date= "8Jul2023"
tpc1$expt="July 8 2023"
tpc2= read.csv("CaterpillarTPC_10Jul2023.csv")
tpc2$date= "10Jul2023"
tpc2$expt="July 8 2023"

#add second batch
tpc3= read.csv("CaterpillarExpt2DataSheet_30July2023.csv")
tpc3$X= NA
tpc3$date= "30Jul2023"
#make unique female
tpc3$FEMALE= paste("30Jul", tpc3$FEMALE, sep="_")
tpc3$expt="July 30 2023"

#restrict to observations
tpc1= tpc1[!is.na(tpc1$FEMALE),]

tpc= rbind(tpc1,tpc2, tpc3)
tpc[tpc=="NV"]<- "NA"
tpc= as.data.frame(tpc)

length(which(tpc$X23.mass.i<10)) # 64 of 214 less than 10mg
#take out
#tpc= tpc[-which(tpc$X23.mass.i<10),]

#estimate RGR = [log10(m f) -log10(m i)]/(t f − t i )
#forumulas in papers specify ln, but magnitude is too high

tpc$rgr_23= (log10(as.numeric(tpc$X23.mass.f)*0.001)-log10(as.numeric(tpc$X23.mass.i)*0.001))/
  as.numeric(difftime(as.POSIXct(tpc$X23.time.f,format="%H:%M"), as.POSIXct(tpc$X23.time.i,format="%H:%M"), units='hours'))

tpc$rgr_29= (log10(as.numeric(tpc$X29.mass.f)*0.001)-log10(as.numeric(tpc$X29.mass.i)*0.001))/
  as.numeric(difftime(as.POSIXct(tpc$X29.time.f,format="%H:%M"), as.POSIXct(tpc$X29.time.i,format="%H:%M"), units='hours'))

tpc$rgr_11= (log10(as.numeric(tpc$X11.mass.f)*0.001)-log10(as.numeric(tpc$X11.mass.i)*0.001))/
  as.numeric(difftime(as.POSIXct(paste("2023-07-02", tpc$X11.time.f, sep=" "),format="%Y-%m-%d %H:%M"), as.POSIXct(paste("2023-07-01", tpc$X11.time.i, sep=" "),format="%Y-%m-%d %H:%M"), units='hours'))

tpc$rgr_17= (log10(as.numeric(tpc$X17.mass.f)*0.001)-log10(as.numeric(tpc$X17.mass.i)*0.001))/
  as.numeric(difftime(as.POSIXct(tpc$X17.time.f,format="%H:%M"), as.POSIXct(tpc$X17.time.i,format="%H:%M"), units='hours'))

tpc$rgr_35= (log10(as.numeric(tpc$X35.mass.f)*0.001)-log10(as.numeric(tpc$X35.mass.i)*0.001))/
  as.numeric(difftime(as.POSIXct(tpc$X35.time.f,format="%H:%M"), as.POSIXct(tpc$X35.time.i,format="%H:%M"), units='hours'))

#account for caterpillars that didn't eat
tpc$rgr_23[tpc$X23.notes %in% c("O", NA)]<- NA
tpc$rgr_29[tpc$X29.notes %in% c("O", NA)]<- NA 
tpc$rgr_11[tpc$X11.notes %in% c("O", NA)]<- NA 
tpc$rgr_17[tpc$X17.notes %in% c("O", NA)]<- NA 
tpc$rgr_35[tpc$X35.notes %in% c("O", NA)]<- NA 

tpc$f.ind= paste(tpc$FEMALE, tpc$INDV, sep="_")

#write out
tpc.write<- tpc[,c("FEMALE","INDV","f.ind","X23.mass.i","rgr_23","rgr_29","rgr_11","rgr_17","rgr_35","expt")]
names(tpc.write) <- c("Mom","ID","f.ind","Mi","RGR23","RGR29","RGR11","RGR17","RGR35","expt")
write.csv(tpc.write, "PrapaeGardenExpt_WARP_TPC2023.csv")

#----------
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
  geom_line(data=tpc.agg.f, aes(x=temp, y = mean, group=FEMALE), size=1, col="darkgreen")

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
  ggtitle("2023") +ylim(-0.02,0.06)

#-------------------------
#Historic data

setwd('/Volumes/GoogleDrive/Shared drives/TrEnCh/Projects/WARP/Analyses/data/Initial/')
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
tpc.agg$year= 2023
tpc.agg.h$year=1999
tpc.agg.all= rbind(tpc.agg, tpc.agg.h)

#family means
tpc.agg.f$year=2023
tpc.agg.fh$year=1999
colnames(tpc.agg.fh)[1]="FEMALE"
tpc.agg.fh$FEMALE= as.character(tpc.agg.fh$FEMALE)
tpc.agg.f.all= rbind(tpc.agg.f, tpc.agg.fh)
tpc.agg.f.all$yrfemale= paste(tpc.agg.f.all$year, tpc.agg.f.all$FEMALE)

tpc.all.plot= ggplot(tpc.agg.f.all, aes(x=temp,y=mean, col=factor(year)))+
  geom_line(aes(group=yrfemale)) +scale_color_viridis_d()

  #add means
  tpc.all.plot= tpc.all.plot + 
    geom_errorbar(data=tpc.agg.all, aes(x=temp, y=mean, ymin=mean-se, ymax=mean+se, col=factor(year)), width=0)+
    geom_point(data=tpc.agg.all, aes(x=temp, y = mean, col=factor(year)), size=3)+
    theme_bw()+xlab("Temperature (C)")+ylab("RGR (g/g/h)")+
    ggtitle("1999 & 2023") +ylim(-0.02,0.06) 
  
  #----------------
  #save figure
  setwd('/Volumes/GoogleDrive/Shared drives/TrEnCh/Projects/WARP/Analyses/figures/')
  pdf("PrapaeTPC.pdf",height = 6, width = 15)
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
  
  tpc.lm = lm(as.matrix(tpc[,c(5,6,3,4,7)])~as.factor(tpc$FEMALE))
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
    dplyr::summarise(rgr_11 = mean(rgr_11),
              rgr_17 = mean(rgr_17, na.rm=T),
              rgr_23 = mean(rgr_23, na.rm=T),
              rgr_29 = mean(rgr_29, na.rm=T),
              rgr_35 = mean(rgr_35, na.rm=T) )
  
  #G matrix
  g.mat<- var(tpc.f[,-1])*10^6
  
  g.mat.m <- melt(g.mat)
  plot.g.mat= ggplot(data = g.mat.m, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()+scale_fill_viridis()
  
  #P matrix
  p.mat<- var(tpc[,c(5,6,3,4,7)], na.rm=TRUE)*10^6
  
  p.mat.m <- melt(p.mat)
  plot.p.mat= ggplot(data = p.mat.m, aes(x=Var1, y=Var2, fill=value)) + 
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
  
  #-------------------- 
  plot.cov
  plot.cov.old
  