library(ggplot2)
library(data.table)
library(dplyr)
library(patchwork)
library(reshape2)
library(viridis)
library(nlme)
library(lme4)
library(stringr)
library(tidyr)

home<- getwd()

#GARDEN DATA
#toggle between desktop (y) and laptop (n)
desktop<- "n"

if(desktop=="y") setwd("/Users/laurenbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/data/")
if(desktop=="n") setwd("/Users/lbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/data/")

#RGR Data
#Recent data
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
tpc$RGR23= (log10(as.numeric(tpc$X23.mass.f)*0.001)-log10(as.numeric(tpc$X23.mass.i)*0.001))/
  as.numeric(difftime(as.POSIXct(tpc$X23.time.f,format="%H:%M"), as.POSIXct(tpc$X23.time.i,format="%H:%M"), units='hours'))

tpc$RGR29= (log10(as.numeric(tpc$X29.mass.f)*0.001)-log10(as.numeric(tpc$X29.mass.i)*0.001))/
  as.numeric(difftime(as.POSIXct(tpc$X29.time.f,format="%H:%M"), as.POSIXct(tpc$X29.time.i,format="%H:%M"), units='hours'))

#overnight
tpc$RGR11= (log10(as.numeric(tpc$X11.mass.f)*0.001)-log10(as.numeric(tpc$X11.mass.i)*0.001))/
  as.numeric(difftime(as.POSIXct(paste("2023-06-21", tpc$X11.time.f, sep=" "),format="%Y-%m-%d %H:%M"), as.POSIXct(paste("2023-06-20", tpc$X11.time.i, sep=" "),format="%Y-%m-%d %H:%M"), units='hours'))

tpc$RGR35= (log10(as.numeric(tpc$X35.mass.f)*0.001)-log10(as.numeric(tpc$X35.mass.i)*0.001))/
  as.numeric(difftime(as.POSIXct(tpc$X35.time.f,format="%H:%M"), as.POSIXct(tpc$X35.time.i,format="%H:%M"), units='hours'))

#overnight
tpc$RGR17= (log10(as.numeric(tpc$X17.mass.f)*0.001)-log10(as.numeric(tpc$X17.mass.i)*0.001))/
  as.numeric(difftime(as.POSIXct(paste("2023-06-21", tpc$X11.time.f, sep=" "),format="%Y-%m-%d %H:%M"), as.POSIXct(paste("2023-06-20", tpc$X11.time.i, sep=" "),format="%Y-%m-%d %H:%M"), units='hours'))

# #account for caterpillars that didn't eat
# tpc$rgr_23[tpc$X23.notes %in% c("O", NA)]<- NA
# tpc$rgr_29[tpc$X29.notes %in% c("O", NA)]<- NA 
# tpc$rgr_11[tpc$X11.notes %in% c("O", NA)]<- NA 
# tpc$rgr_17[tpc$X17.notes %in% c("O", NA)]<- NA 
# tpc$rgr_35[tpc$X35.notes %in% c("O", NA)]<- NA 

tpc$f.ind= paste(tpc$FEMALE, tpc$INDV, tpc$expt, sep="_")
#initial mass at start of field study
tpc$Mi<- tpc$X17.mass.f

#-------------------------------
#Garden data

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

#butterfly weight 
tpc$Butt..Wt<- NA
tpc$Butt..Wt[matched]<- gdat$massateclosion.mg.[match1[matched]]

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

tpc$eclosiondate<- NA
tpc$eclosiondate[matched]<- gdat$eclosiondate[match1[matched]]

tpc$Sex<- NA
tpc$Sex[matched]<- gdat$gender[match1[matched]]

tpc$pupdate<- as.Date(as.character(tpc$pupdate), format="%m/%d/%y")
tpc$eclosiondate<- as.Date(as.character(tpc$eclosiondate), format="%m/%d/%y")
tpc$StartDate<- gsub("/24", "/2024", tpc$StartDate)
tpc$StartDate<- as.Date(as.character(tpc$StartDate), format="%m/%d/%Y")

tpc$Time.to.Pupation<- as.double(difftime(tpc$pupdate, tpc$StartDate, units = c("days")))
tpc$Time.to.Eclosion<- as.double(difftime(tpc$eclosiondate, tpc$StartDate, units = c("days")))

#Pupated and Eclosed
tpc$Pupated<- 0
tpc$Pupated[which(!is.na(tpc$pupdate))] <- 1

tpc$Eclosed<- 0
tpc$Eclosed[which(!is.na(tpc$eclosiondate))] <- 1

#---
#Combine
tpc$Fecundity<- tpc$FecEggCount           
tpc$Pupa.wt<- tpc$pupal_massmg
tpc$Species<- "Pieris_rapae"
tpc$Population<- "Seattle_WA" 
tpc$Study.Site<- "Study.Site"

tpc= tpc[,c("Species","Population","Study.Site","FEMALE","INDV","f.ind","Mi","RGR23","RGR29","RGR11","RGR17","RGR35","expt","Pupated","Time.to.Pupation","Pupa.wt","Eclosed","Time.to.Eclosion","Sex","Butt..Wt","Fecundity")]
tpc$period<- "recent"
colnames(tpc)[4:5]<- c("Mom","ID")
#-----------------------------------------------
#Historic data

tpc.h= read.csv("PrapaeUW.Seln2.1999.Combineddata.OPUS2021.csv")
tpc.h$f.ind= paste(tpc.h$Mom, tpc.h$ID, sep="_")
tpc.h$expt<- "aug"

tpc.h= tpc.h[,c("Species","Population","Study.Site","Mom","ID","f.ind","Mi","RGR23","RGR29","RGR11","RGR17","RGR35","expt","Pupated","Time.to.Pupation","Pupa.wt","Eclosed","Time.to.Eclosion","Sex","Butt..Wt","Fecundity")]
tpc.h$period<- "past"

#combine past and recent
tpc<- rbind(tpc, tpc.h)

#-----------------------
#FIX NAs in past data

#load data
tpc.h<- read.csv("./Prapae Fieldseln.1999.Exp2.finallabdata.jul2025_cleanedsheet.csv")

#unique ID
tpc.h$uid <- paste(tpc.h$mom, tpc.h$ID, "aug", sep="_")

#recalculate growth rate
tpc.h$RGRn= (log10(as.numeric(tpc.h$fw.lrv)*0.001)-log10(as.numeric(tpc.h$iw.lrv)*0.001))/tpc.h$duration
#fix infinite due to 0 final weight
tpc.h$RGRn[which(!is.finite(tpc.h$RGRn))]<- NA

#wide format
tpc.hw<- spread(tpc.h[,c("uid","temp","RGRn")], temp, RGRn) #Test with Mo then use TRGR
colnames(tpc.hw)[2:6]<- paste("RGR", colnames(tpc.hw)[2:6], sep="")

#match to data
tpc$uid <- paste(tpc$Mom, tpc$ID, tpc$expt, sep="_")

#find what needs matches by temp 
#"RGR23"
inds<- which(is.na(tpc$RGR23) & tpc$expt=="aug")
match1<- match(tpc$uid[inds], tpc.hw$uid) 
tpc$RGR23[inds]<- tpc.hw$RGR23[match1]
#"RGR29"
inds<- which(is.na(tpc$RGR29) & tpc$expt=="aug")
match1<- match(tpc$uid[inds], tpc.hw$uid) 
tpc$RGR29[inds]<- tpc.hw$RGR29[match1]
#"RGR11"
inds<- which(is.na(tpc$RGR11) & tpc$expt=="aug")
match1<- match(tpc$uid[inds], tpc.hw$uid) 
tpc$RGR11[inds]<- tpc.hw$RGR11[match1]
#"RGR17": drops from 109 to 49 NAs
inds<- which(is.na(tpc$RGR17) & tpc$expt=="aug")
match1<- match(tpc$uid[inds], tpc.hw$uid) 
tpc$RGR17[inds]<- tpc.hw$RGR17[match1]
#"RGR35"
inds<- which(is.na(tpc$RGR35) & tpc$expt=="aug")
match1<- match(tpc$uid[inds], tpc.hw$uid) 
tpc$RGR35[inds]<- tpc.hw$RGR35[match1]

#write out
setwd(home)
write.csv(tpc, "./PrapaeGardenExpt_WARP.csv")

#================================================
#TEMPERATURE DATA
#Taylor version: https://github.com/taylorhatcher/WARP2024/tree/main

if(desktop=="y") setwd("/Users/laurenbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/data/WeatherData/")
if(desktop=="n") setwd("/Users/lbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/data/WeatherData/")

#1997
temp.1997= read.csv("CUH.metdata.1997_viii12_19.csv") #8/12 - 8/26
temp.1997$Period<- 1
temp2.1997= read.csv("CUH.metdata.1997_viii20_26.csv")
temp2.1997$Period<- 2
temp.1997= rbind(temp.1997,temp2.1997)
#format time
min <- str_sub(temp.1997$Time,-2,-1)
min[min %in% c("0","5")]<- paste("0", str_sub(min[min %in% c("0","5")],-1,-1), sep="")
hr<- str_sub(temp.1997$Time,-4,-3)
hr[hr==""]<- "0"
temp.1997$hm<- paste(hr, min, sep=":")
temp.1997$hr<- as.numeric(hr); temp.1997$min<- as.numeric(min)
temp.1997$dt<- temp.1997$Date + temp.1997$hr/24 + temp.1997$min/(24*60)

#1999
temp.1999= read.csv("FormattedHistoricMetDataFieldSln.csv") #8/18-8/25
temp.1999$hr<- as.numeric(str_sub(temp.1999$LONGTIME,-5,-4))
temp.1999$dt<- temp.1999$JDATE + as.numeric(str_sub(temp.1999$LONGTIME,-5,-4))/24 +as.numeric(str_sub(temp.1999$LONGTIME,-2,-1))/(24*60)

temp.1999e= read.csv("UWCUH.MetData.jul_aug1999.csv") #DOY 209-217; 7/29 - 8/5
temp.1999e$hr<- as.numeric(str_sub(temp.1999e$TIME,-4,-3))
temp.1999e$dt<- temp.1999e$JDATE + as.numeric(str_sub(temp.1999e$TIME,-4,-3))/24 +as.numeric(str_sub(temp.1999e$TIME,-2,-1))/(24*60)

#2024
temp.2024= read.csv("combined_loggers_2024.csv") #6/21 - 8/18
#format time  
jday<- as.numeric(format(as.Date(temp.2024$datetime, format = "%Y-%m-%d %H:%M:%S"), "%j"))
temp.2024$hr<- as.numeric(format(strptime(temp.2024$datetime, format = "%Y-%m-%d %H:%M:%S"), "%H"))
min<- as.numeric(format(strptime(temp.2024$datetime, format = "%Y-%m-%d %H:%M:%S"), "%M"))
temp.2024$dt= jday +temp.2024$hr/24 + min/(24*60)
temp.2024$Date<- format(strptime(temp.2024$datetime, format = "%Y-%m-%d %H:%M:%S"), "%Y-%m-%d")
temp.2024$Time<- format(strptime(temp.2024$datetime, format = "%Y-%m-%d %H:%M:%S"), "%H:%M")

#2023
#Data processed here: https://github.com/taylorhatcher/WARP2023Analysis
temp.2023= read.csv("ToptExpt1_2023.csv") 
temp.2023$expt<- 1
temp.2023.2= read.csv("ToptExpt2_2023.csv") 
temp.2023.2$expt<- 2
#combine
temp.2023<- rbind(temp.2023, temp.2023.2)

jday<- as.numeric(format(as.Date(temp.2023$Date, format = "%Y-%m-%d %H:%M:%S"), "%j"))
temp.2023$hr<- as.numeric(format(strptime(temp.2023$Date, format = "%Y-%m-%d %H:%M:%S"), "%H"))
min<- as.numeric(format(strptime(temp.2023$Date, format = "%Y-%m-%d %H:%M:%S"), "%M"))
temp.2023$dt= jday +temp.2023$hr/24 + min/(24*60)

#------------------------------
#change to long format and combine
t1997.l <- melt(temp.1997[,c("dt","Date","Time","hr","Tm1","Tm2","Tm3","Tm4","Tm5","Tm6","Tm7","Tm8","Tm9","Tm10")], id.vars = c("dt","Date","Time","hr"), variable.name = "T")
t1997.l$Year<- 1997

t1999.l <-  temp.1999[,c("dt","DATE","TIME","hr","Variable", "Value","YEAR")]
names(t1999.l) <- c("dt","Date","Time","hr","T", "value","Year")

#Plot earlier 1999 data?
t1999e.l <- melt(temp.1999e[,c("dt","JDATE","TIME","hr","TM1","TM2","TM3","TM4","TM5","TM6","TM7","TM8","TM9","TM10",
                               "TM11","TM12","TM13","TM14","TM15","TM16","TM17","TM18","TM19","TM20")], id.vars = c("dt","JDATE","TIME","hr"), variable.name = "T")
t1999e.l$Year<- 1999
names(t1999e.l) <- c("dt","Date","Time","hr","T", "value","Year")

t2024.l <- melt(temp.2024[,c("dt","Date","Time", "hr", "Logger1.T1","Logger1.T2","Logger1.T3","Logger1.T4","Logger2.T1","Logger2.T2","Logger2.T3","Logger2.T4","Logger3.T1","Logger3.T2","Logger3.T3","Logger3.T4.shadedT")], id.vars = c("dt","Date","Time","hr"), variable.name = "T")
#drop NAs
t2024.l<- t2024.l[which(!is.na(t2024.l$value)),]
t2024.l$Year<- 2024

t2023.l <- melt(temp.2023[,c("dt","Date","Time", "hr", "shade1", "sun2", "shade3", "sun4", "sun5", "shade6", "sun7", "shade8")], id.vars = c("dt","Date","Time","hr"), variable.name = "T")
#drop NAs
t2023.l<- t2023.l[which(!is.na(t2023.l$value)),]
t2023.l$Year<- 2023

#combine
tdat<- rbind(t1997.l, t1999.l, t1999e.l, t2024.l, t2023.l)

#write out
#if(desktop=="y") setwd("/Users/laurenbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/data/")
#if(desktop=="n") setwd("/Users/lbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/data/")

setwd(home)
write.csv(tdat, "data/PrapaeGardenTemps_WARP.csv")


