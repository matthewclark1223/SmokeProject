library(tidyverse)
library(lme4)
library(rstanarm)
library(MASS)
options(mc.cores = parallel::detectCores())
dat<-read_csv("~/Smoke_Proj/Data/MergedDataComplete.csv")
dat$Year<-as.factor(dat$Year)
#models below increase in complexity..Currently, non of  them are converging using 
#the lme4 package or with rstanarm
#this may be a computational problem on my end though, rather than a model specification problem

#glm with independent intercept for each park for each season
fit<-glmer.nb(RecreationVisits~stdsmoke+CatColS+(1|CatColS),data=dat)

#glm with independent intercept for each park for each Month
fit2<-glmer.nb(RecreationVisits~stdsmoke+CatColM+(1|CatColM),data=dat)


#glm with independent intercept for each park for each Season and
# random slope for smoke for each park
fit3<-glmer.nb(RecreationVisits~stdsmoke+CatColS+(stdsmoke|CatColS),data=dat)

#glm with independent intercept for each park for each Month and
# random slope for smoke for each park
#this is the preferred model
fit4<-glmer.nb(RecreationVisits~stdsmoke+CatColM+(stdsmoke|CatColM),data=dat)


#any of these models can be run using stan by specifying 
#stan_glmer.nb instead of the glmer.nb function from lme4


fit5<-glmer.nb(RecreationVisits~(1|CatColS)+(stdsmoke|UnitCode), data=dat)
save(fit5,file="fit5.rda")
summary(fit5)

#best
fit6<-glmer.nb(RecreationVisits~(1|CatColM)+(stdsmoke|UnitCode), data=dat)
save(fit6,file="fit6.rda")

fit7<-glmer.nb(RecreationVisits~(stdsmoke|CatColM), data=dat)
save(fit7,file="fit7.rda")

fit8<-glm.nb(RecreationVisits~stdsmoke+Month+UnitCode,data=dat)
save(fit8,file="fit8.rda")

fit9<-glm.nb(RecreationVisits~stdsmoke+Season+UnitCode,data=dat)
save(fit9,file="fit9.rda")


coef(fit6)

fit<-glmer.nb(RecreationVisits~
                     (1|CatColM)+
                     (stdsmoke|UnitCode)+
                     Year,
                   data=dat)




fit<-glmer.nb(RecreationVisits~
                stdsmoke+
                
                (Year+Season|UnitCode),
              data=dat)




fit<-glmer.nb(RecreationVisits~
                     stdsmoke+
                     (UnitCode*Season|Year),
                   data=dat[1:1404,])


