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
desktop<- "y"
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
tpc2024<- tpc2024[-which(tpc2024$expt=="Aug 1999"),]
#==============================
#ADD 2023 data to assess acclimation

tpc2023<- read.csv("./data/PrapaeGardenExpt_WARP_TPC2023.csv")

tpc<- rbind(tpc2024, tpc2023)

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
  group_by(temp) %>% 
  dplyr::summarise(mean = mean(value, na.rm = TRUE),
                   se = sd(value, na.rm = TRUE)/sqrt(length(value)) )

#-----------
#TPC comparison between experiments

tpc.plot= ggplot(tpc.l, aes(x=temp,y=value)) + 
 # geom_point( aes(col=expt), position = "jitter")+
#add family lines
  geom_line(data=tpc.agg.f, aes(x=temp, y = mean, group=Mom, col=expt), linewidth=1, alpha=0.5)+
#add points for temperature means  
  geom_errorbar(data=tpc.agg, aes(x=temp, y=mean, ymin=mean-se, ymax=mean+se), width=0, col="black")+
  geom_point(data=tpc.agg, aes(x=temp, y = mean), size=4, fill=colm[7], col="black", pch=21)+
  theme_classic(base_size=16)+xlab("Temperature (°C)")+ylab("")+
  facet_wrap(.~expt)

#------------

  #assess potential for acclimation
  tpc.b<- tpc.l[,c("Mom","ID", "Mi", "f.ind","temp","value","expt")]
  tpc.b= na.omit(tpc.b)
  
  #just 2024
  mod= lm(value ~ Mi*poly(temp,3)*expt, data= tpc.b[which(tpc.b$expt %in% c("June 2024","July 2024")),])
  
  #all
  mod= lm(value ~ Mi*poly(temp,3)*expt, data= tpc.b)
  anova(mod)
  
  mod.lmer <- lme(value ~ poly(temp)*expt*Mi,random=~1|Mom, data = tpc.b)
  anova(mod.lmer)
  
  sjPlot::plot_model(mod, type = "pred", terms = c("temp", "expt"), show.data=FALSE, title="")
  
  #---------
  mod <- lm(value ~ Mi +temp +I(temp^2) +I(temp^3) + expt+
              temp:expt +I(temp^2):expt +I(temp^3):expt, data= tpc.b)
    
  # plot marginal effects of polynomial term
  sjPlot::plot_model(mod, type = "pred", terms = c("temp","expt"))
  
  
  