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
tpc<- tpc[,-which(colnames(tpc) %in% c("X", "Species", "Population", "Study.Site","uid"))]

#update experiment labels
expts<- c("june","july","aug")
expts.lab<- c("June 2024","July 2024","Aug 1999")

tpc$expt <- expts.lab[match(tpc$expt,expts)]
tpc$expt <- factor(tpc$expt, levels= c("Aug 1999","June 2024","July 2024"), ordered=TRUE)

tpc2024<- tpc[,1:10]

#==============================
#ADD 2023 data to assess acclimation

tpc2023<- read.csv("./data/PrapaeGardenExpt_WARP_TPC2023.csv")

tpc<- rbind(tpc2024, tpc2023)

#drop past
tpc<- tpc[-which(tpc$expt %in% c("Aug 1999")),]

#==============================

#to long format
tpc.l <- melt(tpc, id.vars = c("Mom", "ID", "f.ind", "expt","Mi"), variable.name = "temp")
tpc.l$temp= as.numeric(gsub("RGR","",tpc.l$temp))
tpc.l$value= as.numeric(tpc.l$value)

#estimate family mean values
tpc.agg.f <- tpc.l %>% 
  group_by(Mom, temp, expt) %>% 
  dplyr::summarise(mean = mean(value, na.rm = TRUE),
                   se = sd(value, na.rm = TRUE)/length(value) )

#estimate temperature mean values
tpc.agg <- tpc.l %>% 
  group_by(temp, expt) %>% 
  dplyr::summarise(mean = mean(value, na.rm = TRUE),
                   se = sd(value, na.rm = TRUE)/sqrt(length(value)) )

#-----------
#TPC comparison between experiments

tpc.plot= ggplot(tpc.l, aes(x=temp,y=value)) + 
  geom_point(color="black")+
 #geom_point( aes(col=expt), position = "jitter")+
#add family lines
  geom_line(data=tpc.agg.f, aes(x=temp, y = mean, group=Mom), col=colm[6], linewidth=1, alpha=0.5)+  
#add points for temperature means  
  geom_errorbar(data=tpc.agg, aes(x=temp, y=mean, ymin=mean-se, ymax=mean+se), width=0, col="black")+
  geom_point(data=tpc.agg, aes(x=temp, y = mean), size=2, fill=colm[7], col="black", pch=21)+
  theme_classic(base_size=16)+xlab("Temperature (°C)")+ylab("Growth rate (g/g/h)") +
  ylim(-0.02,0.06)+
  facet_wrap(.~expt)

#save figure
pdf("./figures/FigS1_TPCacclim.pdf",height = 8, width = 8)
tpc.plot
dev.off()

#plot over each other
tpc.plot= ggplot(tpc.l, aes(x=temp,y=value, color=expt)) + 
  #geom_point(color="black")+
  #geom_point( aes(col=expt), position = "jitter")+
  #add family lines
  geom_line(data=tpc.agg.f, aes(x=temp, y = mean, group=Mom), linewidth=1, alpha=0.5)+  
  #add points for temperature means  
  geom_errorbar(data=tpc.agg, aes(x=temp, y=mean, ymin=mean-se, ymax=mean+se), width=0, col="black")+
  geom_point(data=tpc.agg, aes(x=temp, y = mean, fill=expt), size=2, col="black", pch=21)+
  theme_classic(base_size=16)+xlab("Temperature (°C)")+ylab("Growth rate (g/g/h)") +
  ylim(-0.02,0.06) #+facet_wrap(.~expt)

#------------

  #assess potential for acclimation
  tpc.b<- tpc.l[,c("Mom","ID", "Mi", "f.ind","temp","value","expt")]
  tpc.b$Mi <- as.numeric(tpc.b$Mi)
  tpc.b= na.omit(tpc.b)
  
  #just 2024
  mod= lm(value ~ poly(temp,3)*expt*Mi, data= tpc.b[which(tpc.b$expt %in% c("June 2024","July 2024")),])
  
  #all recent
  mod= lm(value ~ poly(temp,3)*expt, data= tpc.b)
  
  mod= lm(value ~ Mi*poly(temp,3)*expt, data= tpc.b)
  anova(mod)
  
  mod.noexpt= lm(value ~ Mi*poly(temp,3), data= tpc.b) 
  anova(mod.noexpt, mod, test = "Chi")
  
  sjPlot::plot_model(mod, type = "pred", terms = c("temp [all]", "expt"), show.data=FALSE, title="")
  
  mod.lmer <- lme(value ~ poly(temp,3)*expt*Mi,random=~1|Mom, data = tpc.b) #random=~1|expt/Mom
  
  #put experiment as random effect
  mod.lmer <- lme(value ~ poly(temp,3)*Mi,random=~1|expt/Mom, data = tpc.b) 
  
  sigma(mod.lmer)
  #standard deviations of the random effects
  VarCorr(mod.lmer)
  
  #Save anova
  tables1<- as.data.frame(anova(mod.lmer))
  colnames(tables1)[3:4]<- c("F","p")
  tables1$sig<-""
  tables1$sig[tables1$p<0.05]<-"*"
  tables1$sig[tables1$p<0.01]<-"**"
  tables1$sig[tables1$p<0.001]<-"***"
  tables1$F= round(tables1$F,1)
  tables1$p= round(tables1$p,4)
  
  #Table 1
  write.csv(tables1, "./figures/Tables1_acclim.csv")
  
  sjPlot::plot_model(mod.lmer, type = "pred", terms = c("temp [all]", "expt"), show.data=FALSE, title="")
  
  library(glmm.hp)
  
  #tpc.c<- tpc.b[which(tpc.b$expt %in% c("June 2024", "July 2024")),]
  
  mod.lmer <- lme(value ~ Mi+poly(temp,3)+expt+
                    poly(temp,3):expt +poly(temp,3):Mi +Mi:expt+
                    Mi:poly(temp,3):expt,
                  random=~1|expt/Mom, data = tpc.b)
  glmm.hp(mod.lmer, iv = NULL, type = "adjR2", commonality = FALSE)
  
  
  