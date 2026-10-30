# This runs the Forecast

# Before:
# After:

library(icesTAF)

mkdir("output")

gc()
rm(list=ls())

library(tidyverse)
library(lattice)
library(lubridate)

library(reshape2)
library(readxl)
library(DataCombine)
library(lattice)
library(magrittr)
library(viridis)
library(sessioninfo)
library(stringr)

curr.year <- 2026
dat.year <- curr.year -1
fu.n <- "FU15"

save.plots <- T # set it up as T or F whether you want to save the plots when running all the chunks or kniting the document
save.tables <- T # set it up as T or F whether you want to save the tables "propmature"" and the "L50" into .txt files when running all the chunks or knitting the document

fu15.exp <- read.csv("model/fu15.assess.csv")


fu15.exp$prop.removals.ret <- fu15.exp$int.lan.num/fu15.exp$removals.n

fu15.assess <- fu15.exp[,c("year", "abund", "upper", "lower", "ci", "int.lan.num", "int.dis.num", "removals.n", "hr","int.lan.wgt", "int.dis.wgt", "dis.rn", "dead.disc.r", "mw.lan", "mw.dis")]


## The basis for the catch advice and scenarios. 
# select here the current year for adg
wgneph.yr <- curr.year

# use for wgneph 
# wgneph.yr <-"2021"
# wgneph.yr <- as.numeric(wgneph.yr)

# full range
#land.wt.yrs <- seq(2003,wgneph.yr-1,1)
#disc.wt.yrs <- seq(2003,wgneph.yr-1,1)

land.wt.yrs <<- seq(wgneph.yr-3,wgneph.yr-1,1)
disc.wt.yrs <<- seq(wgneph.yr-3,wgneph.yr-1,1)

discard.rate.yrs <- seq(wgneph.yr-3,wgneph.yr-1,1)
dead.discard.rate.yrs <- seq(wgneph.yr-3,wgneph.yr-1,1)
prop.removal.ret.yrs <- seq(wgneph.yr-3,wgneph.yr-1,1)


stock.abundance <- fu15.exp$abund[length(fu15.exp$abund)]


land.mean.wt <- mean(fu15.exp$mw.lan[fu15.exp$year %in% land.wt.yrs],na.rm=T)
disc.mean.wt <- mean(fu15.exp$mw.dis[fu15.exp$year %in% land.wt.yrs],na.rm=T)

discard.rate <- mean(fu15.exp$dis.rn[fu15.exp$year%in% discard.rate.yrs],na.rm=T)

dead.discard.rate <- mean(fu15.exp$dead.disc.r[fu15.exp$year %in% dead.discard.rate.yrs],na.rm=T)
prop.removal.ret.n <- mean(fu15.exp$prop.removals.ret[fu15.exp$year %in% prop.removal.ret.yrs], na.rm=T)
disc.survival <- 10


fu15.catch.inputs<- data.frame(wgneph.yr, stock.abundance, land.mean.wt, disc.mean.wt,discard.rate,dead.discard.rate,disc.survival)

knitr::kable(fu15.catch.inputs , digits=5)


## MSY ranges inputs for the forecast tables.

ref <- read.csv("data/MSY_nep_stocks.csv")

ref15 <- ref[ref$Stock.code=="nep-15", c("F_MSY", "MSY_F_lower", "MSY_F_upper", "MSY_Btrigger")]

MSY_Btrigger <- as.numeric(as.character(ref15$MSY_Btrigger))


HR15 <- cbind(ref15[,1:3],
              "MSY approach"= ref15$F_MSY,
              "Flower_Trig"= round(ref15$MSY_F_lower, 3),
              "F_recent"= mean (fu15.exp[fu15.exp$year %in% (wgneph.yr-3):(wgneph.yr-1),] $ hr /100),
              "F_current"= fu15.exp[fu15.exp$year==(wgneph.yr-1),] $ hr /100)
row.names(HR15) <- NULL

HR15 <- HR15[, c("MSY approach","F_MSY", "MSY_F_lower", "MSY_F_upper", "MSY approach", "Flower_Trig",  "F_current")]

colnames(HR15)[5] <- "Fupper_Trig"

#reorder for advice sheet
HR15 <- HR15[,c("MSY approach", "Flower_Trig","Fupper_Trig", "F_MSY","MSY_F_lower", "MSY_F_upper", "F_current")]

knitr::kable(HR15)

## Table 3: Catch scenarios assuming zero discards

forecast.year <- as.character(curr.year + 1)
wanted.catch <-  ((100-discard.rate)*(land.mean.wt/100)*stock.abundance*(HR15))
unwanted.catch <- ((discard.rate)*(disc.mean.wt/100)*stock.abundance*(HR15))
total.catch <- wanted.catch + unwanted.catch


#adv25 <- 2820
#advice.change <- ((round(total.catch)/adv19-1)*100)

LO <- rbind(total.catch, wanted.catch, unwanted.catch, HR15*100, forecast.year)
LO <- as.data.frame(t(LO))
names(LO) <- c("total.catch", "projected landings", "projected discards", "% harvest rate",  "forecast.year")



for (i in 1:(ncol(LO))){
  LO[,i] <- as.character(LO[,i])
  LO[,i] <- as.numeric(LO[,i])
  if (names(LO)[i] == "% harvest rate"){
    LO[,i] <- round(LO[,i], 3)
  } else {
    LO[,i] <- round(LO[,i])
  }
}

knitr::kable(LO)


## Table 3: Catch scenarios assuming recent discards.

landings <-  (land.mean.wt)*stock.abundance*prop.removal.ret.n*(HR15)
dead.discards <- (disc.mean.wt)*stock.abundance*(1-prop.removal.ret.n)*(HR15)
surviving.discards <- dead.discards/3
dead.removals <- landings + dead.discards
total.catch <- landings+dead.discards+surviving.discards

#advice.change <- ((round(total.catch)/adv19-1)*100)

DA <- rbind(total.catch, dead.removals, landings, dead.discards, 
            surviving.discards, HR15*100, forecast.year)
DA <- as.data.frame(t(DA))

names(DA) <- c("total.catch", "dead.removals", "projected landings", "projected dead discards", 
               " projected surviving discards", "% harvest rate","forecast.year") 

for (i in 1:(ncol(DA))){
  DA[,i] <- as.character(DA[,i])
  DA[,i] <- as.numeric(DA[,i])
  if (names(DA)[i] == "% harvest rate"){
    DA[,i] <- round(DA[,i], 3)
  } else {
    DA[,i] <- round(DA[,i])
  }
}


knitr::kable(DA)



