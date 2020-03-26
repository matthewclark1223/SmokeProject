library(tidyverse)
library(rstan)
library(rethinking)
raw.dat <- read_csv("~/Smoke_Proj/Data/ParkDataSubset.csv") 

raw.dat<-raw.dat[,-c(1:2)]
raw.dat%>% 
  
  mutate(season = rethinking::coerce_index(.$Season),
         unit = rethinking::coerce_index(.$UnitCode)) 




stan.dat <- list(N = nrow(raw.dat),
                units = length(unique(raw.dat$unit)),
                seasons = length(unique(raw.dat$season)),
                unitid = raw.dat$unit,
                season = raw.dat$season,
                visits = raw.dat$RecreationVisits,
                smoke = raw.dat$stdsmoke)




mod <- stan_model("./Downloads/clarkmodel.stan")

fit <- sampling(mod, data=stan.dat, chains = 3)
