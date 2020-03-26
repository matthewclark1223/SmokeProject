library(tidyverse)
library(lme4)
library(rstanarm)
library(MASS)

dat<-read_csv("~/Smoke_Proj/Data/MergedDataComplete.csv")
dat$Year<-as.factor(dat$Year)
dat$catY<-paste0(dat$UnitCode,dat$Year)
MattDat<-dat[seq(1,nrow(dat),10),]


fitM<-stan_glmer.nb(RecreationVisits~(stdsmoke|UnitCode)+(Season|catY), chains=1,data=MattDat)
coef(fitM)

fitM<-stan_glmer(RecreationVisits~(stdsmoke|UnitCode)+(Season|catY),
                 family="poisson",chains=1,data=MattDat)
coef(fitM)
fitz<-stan_glmer(RecreationVisits~(stdsmoke|UnitCode)+
                      (UnitCode:Season|Year), chains=1,family="poisson",data=MattDat)

MattDat<-dat[seq(1,nrow(dat),10),]












