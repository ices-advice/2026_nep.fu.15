# Prepare data, write CSV data tables

# Before:
# After:


library(icesTAF)

taf.bootstrap()
library(ggplot2)
library(reshape2)

#set working data year
survey.year<- 2026
catch.year<- 2025

#working directories
mkdir("data")
datadir<- "data/"

#Clear files from datadir directories
unlink(list.files(datadir, full.names = T, recursive = T))

#copy input files from initial datadir to datadir
file.copy(list.files("boot/initial/data/", full.names = TRUE), datadir, overwrite=T)

Catch<-read.csv("data/Table1_Nep_FU15_2026.csv")
Catch$Tonnes<-Catch$CATON/1000
aggregate(Tonnes~CatchCategory, sum, data = Catch)
aggregate(Tonnes~CatchCategory+Country, sum, data = Catch)


#Table2
Lens<-read.csv("data/Table2_Nep_FU15_2026.csv")
CatNum<-aggregate(CANUM~CatchCategory, data = Lens, sum)
CatNum[CatNum$CatchCategory == "Landings"]


##LF Plot
LFPlot<-aggregate(CANUM~AgeOrLength+CatchCategory+Sex, sum, data = Lens)
LFPlot<-dcast(LFPlot, AgeOrLength+Sex~CatchCategory, value.var = "CANUM")
LFPlot[is.na(LFPlot)]<-0
LFPlot$Catch<-LFPlot$Discards+LFPlot$Landings
write.csv(LFPlot, file = "data/LFPlot.csv")


Lens$MCRS<-"Below"
Lens$MCRS[Lens$AgeOrLength > 20] <-"Above"
Lens$WatL<-(Lens$WECA*Lens$CANUM)/1000000

aggregate(WatL~CatchCategory+MCRS, sum, data = Lens)

DR<-read.csv("data/DiscardRate_25.csv") # need to copy this from the previous years intercatch data file and it gets then updated to the next year.

summary(DR)

Y1<-c(2025, 839.33051+101.72928, "Discard")        #add to the discard rates table, those are the values that we get from the aggregate a few lines above...
Y2<-c(2025, 6196.96034+23.30693, "Landing") # landings above and below MCRS
Y3<-c(2025, 839.33051, "AboveMCRS") #Discards Only#
Y4<-c(2025, 101.72928, "BelowMCRS") #Discards Only



DR<-rbind(DR,Y1,Y2,Y3,Y4)
DR$Category<-as.character(DR$Category)
DR$Tonnes<-as.numeric(DR$Tonnes)

DR_C<-subset(DR, Category %in% c("BelowMCRS", "AboveMCRS"))
ggplot(data=DR_C, aes(x=Year, y=Tonnes, fill=Category)) +
  geom_bar(stat="identity")+
  ggtitle("Discards")+
  theme(axis.text.x = element_text(angle = 85))


DR_L<-subset(DR, Category %in% c("Discard","Landing"))
ggplot(data=DR_L, aes(x=Year, y=Tonnes, fill=Category)) +
  geom_bar(stat="identity")+
  ggtitle("Catch")+
  theme(axis.text.x = element_text(angle = 85))

write.csv(DR, file = "data/DiscardRate_26.csv", row.names = F)


#######Mean Weight in Catch
MeanW<-read.csv("data/InputTable_25.csv")

Lens$TW<-(Lens$WECA*Lens$CANUM)
Lens$TL<-Lens$AgeOrLength*Lens$CANUM

LensL<-aggregate(TL~CatchCategory, sum, data = Lens)
LensW<-aggregate(TW~CatchCategory, sum, data = Lens)
LensN<-aggregate(CANUM~CatchCategory, sum, data = Lens)

LensN$Len<-LensL$TL[match(paste(LensN$CatchCategory),paste(LensL$CatchCategory))]
LensN$Wt<-LensW$TW[match(paste(LensN$CatchCategory),paste(LensW$CatchCategory))]

LensN$AW<-LensN$Wt/LensN$CANUM
LensN$AL<-LensN$Len/LensN$CANUM

head(MeanW)
Wlan<-as.data.frame(cbind(MeanW$Year, MeanW$Mean.weight.in.landings, "Landings"))
Wdis<-as.data.frame(cbind(MeanW$Year, MeanW$Mean.weight.in.discards, "Discards"))
Wts<-rbind(Wlan,Wdis)

LensN$AW[LensN$CatchCategory == "Discards"]
#LensN
Wts<-rbind(Wts,c(2025,LensN$AW[LensN$CatchCategory == "Discards"], "Discards"))
Wts<-rbind(Wts,c(2025,LensN$AW[LensN$CatchCategory == "Landings"], "Landings"))

Wts$V1<-as.numeric(as.character(Wts$V1))
Wts$V2<-as.numeric(as.character(Wts$V2))
Wts$V3<-as.character(Wts$V3)

Wts<-dcast(Wts, V1~V3, value.var = "V2")

xlim <- c(2003, 2025)
ylim <- c(6,19)
plot(NA,xlim=xlim,ylim=ylim,xlab='Year',ylab='Weight (g)',main='Mean weight in catch')
lines(Wts$V1,Wts$Discards, col = "blue", type = "b")
lines(Wts$V1,Wts$Landings, col = "red", type = "b")

legend('left',c('Discards','Landings'),lty=c(2,2),pch=c(1,1),col = c("blue", "red"),bty='n',y.intersp=0.8)

#######Mean Length in catch
MeanL<-read.csv("data/MeanLength_25.csv")

Lens$TW<-(Lens$WECA*Lens$CANUM)
Lens$TL<-Lens$AgeOrLength*Lens$CANUM

LensL<-aggregate(TL~Country+Sex, sum, data = Lens)
LensW<-aggregate(TW~Country+Sex, sum, data = Lens)
LensN<-aggregate(CANUM~Country+Sex, sum, data = Lens)

LensN$Len<-LensL$TL[match(paste(LensN$Country,LensN$Sex),paste(LensL$Country,LensL$Sex))]
LensN$Wt<-LensW$TW[match(paste(LensN$Country,LensN$Sex),paste(LensW$Country,LensW$Sex))]

LensN$AW<-LensN$Wt/LensN$CANUM
LensN$AL<-LensN$Len/LensN$CANUM

Y1<-c(2025, LensN$AL[LensN$Sex == "Male" & LensN$Country  == "UK(Northern Ireland)"],"NI-M")
Y2<-c(2025,LensN$AL[LensN$Sex == "Female" & LensN$Country  == "UK(Northern Ireland)"],"NI-F" )
Y3<-c(2025, LensN$AL[LensN$Sex == "Male" & LensN$Country  == "Ireland"],"IRE-M")
Y4<-c(2025,LensN$AL[LensN$Sex == "Female" & LensN$Country  == "Ireland"],"IRE-F")

MeanL<-rbind(MeanL, Y1,Y2,Y3,Y4)
tail(MeanL)
MeanL$Length<-as.numeric(MeanL$Length)

write.csv(MeanL, file = "data/MeanLength_26.csv")

ggplot(data=MeanL, aes(x=Year, y=Length, group = Group)) +
  geom_line(aes(color=Group))+
  geom_point(aes(color=Group))+
  ggtitle("Mean length in catch") +
  ylab("Length (cm)") + 
  xlab("Year")+
  ylim(20,30)+
  theme(axis.text.x = element_text(angle = 85))

####################
####################
####################

LensL<-aggregate(TL~CatchCategory, sum, data = Lens)
LensW<-aggregate(TW~CatchCategory, sum, data = Lens)
LensN<-aggregate(CANUM~CatchCategory, sum, data = Lens)

LensN$Len<-LensL$TL[match(paste(LensN$CatchCategory),paste(LensL$CatchCategory))]
LensN$Wt<-LensW$TW[match(paste(LensN$CatchCategory),paste(LensW$CatchCategory))]

LensN$AW<-LensN$Wt/LensN$CANUM
LensN$AL<-LensN$Len/LensN$CANUM
LensN

###################
###################
###################

LensN<-aggregate(CANUM~CatchCategory, sum, data = Lens)
SexN<-aggregate(CANUM~CatchCategory+Sex, sum, data = Lens)

SexN$Total<-LensN$CANUM[match(SexN$CatchCategory,LensN$CatchCategory)]
SexN$PropCat<-(SexN$CANUM/SexN$Total)*100

SexN$Catch<-sum(SexN$CANUM)
SexN$PropAll<-(SexN$CANUM/SexN$Catch)*100.

SRM<-read.csv("data/SexRatioMales_25.csv")
Y1<-c(2025, SexN$PropCat[SexN$Sex == "Male" & SexN$CatchCategory =="Landings"],"Landing") 
Y2<-c(2025, SexN$PropAll[SexN$Sex == "Male" & SexN$CatchCategory =="Landings"]+
        SexN$PropAll[SexN$Sex == "Male" & SexN$CatchCategory =="Discards"], "Catch")
SRM<-rbind(SRM, Y1,Y2)

SRM$Proportion<-as.numeric(SRM$Proportion)
ggplot(data=SRM, aes(x=Year, y=Proportion, group = Category)) +
  geom_line(aes(color=Category))+
  ylim(35,70)+
  geom_point()+
  ggtitle("Sex Ratio (% males)") +
  xlab("Year") + 
  ylab("Sex ratio")+
  theme(axis.text.x = element_text(angle = 85))

write.csv(SRM, file = "data/SexRatioMales_26.csv")

########

CatchLF<-aggregate(CANUM~AgeOrLength, sum, data = Lens)
CatchLF$Prop<-CatchLF$CANUM/sum(CatchLF$CANUM)
plot(CatchLF$Prop~CatchLF$AgeOrLength, type = "b" ,ylab = "Proportion (%)", xlab = "Length (cm)", main = "Commercial Catch 2025")

ggplot(data=CatchLF, aes(x=AgeOrLength, y=Prop)) +
  geom_line()+
  geom_point()+
  ggtitle("Commercial Fishery 2025") +
  xlab("Length (cm)") + 
  ylab("Proportion")

#################
#################
#################

CatchLF<-aggregate(CANUM~AgeOrLength+Country, sum, data = Lens)
CatchLF<-subset(CatchLF, Country %in% c("Ireland", "UK(Northern Ireland)"))
CatchLF$CANUM<-CatchLF$CANUM/1000000

ggplot(data=CatchLF, aes(x=AgeOrLength, y=CANUM, group = Country)) +
  geom_line(aes(color=Country))+
  geom_point(aes(color=Country))+
  ggtitle("Commercial Fishery 2025 - By country") +
  xlab("Length (cm)") + 
  ylab("Abundance (M)")

################


CatchLF<-aggregate(CANUM~AgeOrLength+Country+CatchCategory, sum, data = Lens)
CatchLF<-subset(CatchLF, Country %in% c("Ireland", "UK(Northern Ireland)"))
CatchLF$CANUM<-CatchLF$CANUM/1000000
CatchLF$CountryCatchType<-paste(CatchLF$Country,CatchLF$CatchCategory)


ggplot(data=CatchLF, aes(x=AgeOrLength, y=CANUM, group = CountryCatchType)) +
  geom_line(aes(color=CountryCatchType))+
  geom_point(aes(color=CountryCatchType))+
  ggtitle("Commercial Fishery 2025 - By country") +
  xlab("Length (cm)") + 
  ylab("Abundance (M)")


#######
Landings<-read.csv("data/Processed_landings_2025.csv")
Landings<-subset(Landings, SPECIES_CODE == "NEP")
LandCal<-aggregate(wgtliveT ~ MONTH+FU, data = Landings, sum)
LandCal<-subset(LandCal,FU==15)
ggplot(data=LandCal, aes(x=MONTH, y=wgtliveT)) +
  geom_smooth()+
  geom_point()+
  ggtitle("Annual Landing Pattern (UK-NI)") +
  xlab("Month") + 
  ylab("Landings (T)")


########



#setwd("P:/FAEB/AWP/46718_Data collection - EU Data Collection (EMFF) Data base coordination/science/Datacalls/ReferenceTables")
Areas <- read.csv('data/Area_FU_Rect.csv')
FU <- read.csv('data/StatRectFU.csv')
Gear <- read.csv('data/Gear_codes.csv')
Presentation <- read.csv('data/Presentation Codes.csv')
Stocks <- read.csv("data/WorkingGroupStocks.csv",stringsAsFactors = FALSE)

FU$Rect <- paste(FU$STATRECT_VERT, FU$STATRECT_HORZ, sep = "")
FU <- subset(FU, FU %in% c(11,12,13,14,15,16,17,18,19,20,21,22))


Landings$FU<-FU$FU[match(Landings$Rectangle, FU$Rect)]
Landings$VesFU<-paste(Landings$FU, Landings$Vessel)


Ves<-subset(Landings, !duplicated (Landings$VesFU))
aggregate(VesFU~FU, data = Ves,length)


