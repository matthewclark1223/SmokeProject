library(ncdf4)
library(chron)
library(lattice)
library(RColorBrewer)

library(raster)
library(rgdal)
library(exactextractr)
library(readr)
library(plyr)
library(purrr)

#Data From - https://public-nps.opendata.arcgis.com/datasets/national-park-service-park-unit-boundaries

nps<-readOGR("~/SmokeProject/NPS_poly/NPS_poly.shp")

#select only polygons designated as National Parks
nps<-nps[nps@data$UNIT_TYPE=="National Park",]

#remove National Parks outside our Regions
nps<-nps[nps@data$REGION=="PW"|nps@data$REGION=="IM",]
nps<-nps[nps@data$UNIT_CODE!="NPSA",] #Samoa
nps<-nps[nps@data$STATE!="HI",]

#reformat to use park codes later
nps_df<-as.data.frame(nps)
nps_df<-as.matrix(nps_df$UNIT_CODE)

park_codes<-as.data.frame(nps_df)
names(park_codes)<-"park"

install.packages("remotes")
remotes::install_github("ejanalysis/analyze.stuff", force=TRUE)
library(analyze.stuff)
library(exactextractr)
poly<-sf::st_as_sfc(nps) #converts to new type of poly


fileNames<-list.files("/Users/Matthewclark989/Downloads/MERRA_all",full.names = TRUE)

years<-c(1980:2019)  #all the years
months<-c("01","02","03","04","05","06","07","08","09","10","11","12") #all the months
comb<-expand.grid(years,months) #just to get the number of unique combinations
list_all<-list() #create an empty list to fill with the file names

for (i in 1:nrow(comb)){ #creates the number of list objects
  year<-comb[i,]$Var1
  month<-comb[i,]$Var2
  x<-(paste0(year,month))
  list_all[[i]]<-fileNames[grep(x,fileNames)]  #fill the list with the file names
  names(list_all)[i]<-paste0("x",year,month) #name the list objects
}


empty_list<-list()

for (i in 1:length (list_all)) {
  for (j in 1:length (list_all[[i]])){
    bcphilic<-mean(brick(list_all[[i]][[j]], varname="BCPHILIC", level=72)) #one day - first bracket is month, 2nd is day, within a day is 8 layers by hour - the "level" dimension refers to veritcal layers where 72=surface and 1=top of atmosphere
    bcphobic<-mean(brick(list_all[[i]][[j]], varname="BCPHOBIC", level=72))
    ocphilic<-mean(brick(list_all[[i]][[j]], varname="OCPHILIC", level=72))
    ocphobic<-mean(brick(list_all[[i]][[j]], varname="OCPHOBIC", level=72))
    
    all<-bcphilic+bcphobic+ocphilic+ocphobic
    
    empty_list[[j]]<-exact_extract(all, poly, 'mean') #intersects boundary
    
  }
  
  out<-as.data.frame(empty_list)
  out_max<-as.data.frame(rowMaxs(out)) #take mean of the assign list above
  
  
  #write csv
  n<-substr(list_all[[i]][[j]],71,78) # !!!!!!!! you will have to change 94 and 100 based on the length of your filepaths - this selects just the year and month i.e. 20041008
  colnames(out_max)[1] <- n #give values column name based on month
  name<-paste0("/Users/Matthewclark989/Documents/output_max/",n) #create empty folder to save to
  newname<-paste0(name, ".csv")
  write.csv(out_max, newname, row.names = FALSE) #save as csv
  
}

#loads those .csv and makes into single df
list_dates = list.files(path="/Users/Matthewclark989/Documents/output_max/", pattern="*.csv", full.names=TRUE) #same folder you saved above output

library(tidyverse)
final_max<-list_dates %>%
  map(read_csv) %>%
  reduce(cbind)

#append park code
final_max<-data.frame(park=park_codes, final_max)

#save
write.csv(final_max, "/Users/Matthewclark989/Documents/final_max.csv", row.names = FALSE)
