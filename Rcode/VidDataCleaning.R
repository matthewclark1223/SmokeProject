library(tidyverse)
VisDat<-read_csv("~/Smoke_Proj/Data/VisDat_NoHeader.csv")
View(VisDat)

#this data is massive.
#Let's chop it down a bit
names(VisDat)
VisDat<-VisDat %>% 
  select(ParkName,UnitCode,Year,Month,RecreationVisits)

write.csv(VisDat, file="~/Smoke_Proj/Data/VisitationDataClean.csv")
nrow(VisDat)
