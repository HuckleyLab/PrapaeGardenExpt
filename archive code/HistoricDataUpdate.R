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
#if(desktop=="n") setwd("/Users/lbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/")

#load data
tpc.h<- read.csv("./data/Prapae Fieldseln.1999.Exp2.finallabdata.jul2025_cleanedsheet.csv")
#check whether there are more matches in uncleaned data
tpc.h<- read.csv("./data/Prapae Fieldseln.1999.Exp2.finallabdata.jul2025_combinedsheet.csv")

#unique ID
tpc.h$uid <- paste(tpc.h$mom, tpc.h$ID, sep="_")

#check duplicated across batches
duplicated(tpc.h$uid[which(tpc.h$temp==23)])
#unique
length(tpc.h$uid[which(tpc.h$temp==23)])
#190 individuals, 196 in combined sheet, 206 in field

#recalculate growth rate
tpc.h$RGRn= (log10(as.numeric(tpc.h$fw.lrv)*0.001)-log10(as.numeric(tpc.h$iw.lrv)*0.001))/tpc.h$duration

#wide format
tpc.hw<- spread(tpc.h[,c("uid","temp","RGRn")], temp, RGRn) #Test with Mo then use TRGR
colnames(tpc.hw)[2:6]<- paste("RGR", colnames(tpc.hw)[2:6], sep="")

#------
#load data
tpc<- read.csv("./data/PrapaeGardenExpt_WARP.csv")
#drop unneeded columns
tpc<- tpc[,-which(colnames(tpc) %in% c("X", "Species", "Population", "Study.Site"))]

#update experiment labels
expts<- c("june","july","aug")
expts.lab<- c("June 2024","July 2024","Aug 1999")

tpc$expt <- expts.lab[match(tpc$expt,expts)]
tpc$expt <- factor(tpc$expt, levels= c("Aug 1999","June 2024","July 2024"), ordered=TRUE)

#restrict to past study
tpc<- tpc[which(tpc$expt=="Aug 1999"),]

#unique ID
tpc$uid <- paste(tpc$Mom, tpc$ID, sep="_")

#------
match1<- match(tpc$uid, tpc.hw$uid) 
#55 of 206 without matches in both combined and cleaned

#check match
#plot(tpc$Mi, tpc.hw$Mo11[match1])
plot(tpc$RGR35, tpc.hw$RGR35[match1])

#MATCHES: add into PrepareData.R

