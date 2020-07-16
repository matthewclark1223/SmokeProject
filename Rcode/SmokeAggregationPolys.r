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


#Mean in Parks w/ New Data
#L71 will need to be hashed out when working with full data - it selects only the months which have data in our sample folder
#L93 values need to change based on the length of your new filepath

poly<-sf::st_as_sfc(nps) #converts to new type of poly


fileNames<-list.files("~/SmokeProject/MERRA_DATA_ORG_PRACT",full.names = TRUE)

years<-c(1980,2004)  #all the years
months<-c("01","10") #all the months
comb<-expand.grid(years,months) #just to get the number of unique combinations
list_all<-list() #create an empty list to fill with the file names

for (i in 1:nrow(comb)){ #creates the number of list objects
  year<-comb[i,]$Var1
  month<-comb[i,]$Var2
  x<-(paste0(year,month))
  list_all[[i]]<-fileNames[grep(x,fileNames)]  #fill the list with the file names
  names(list_all)[i]<-paste0("x",year,month) #name the list objects
}

list_all<- list_all[c(1,4)] #You don't need this for final run with all data

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
  out_mean<-as.data.frame(rowMeans(out)) #take mean of the assign list above
  
  
  #write csv
  n<-substr(list_all[[i]][[j]],97,103) # !!!!!!!! you will have to change 94 and 100 based on the length of your filepaths - this selects just the year and month i.e. 20041008
  colnames(out_mean)[1] <- n #give values column name based on month
  name<-paste0("/Users/Matthewclark989/Documents/output_means/",n) #create empty folder to save to
  newname<-paste0(name, ".csv")
  write.csv(out_mean, newname, row.names = FALSE) #save as csv
  
}

#loads those .csv and makes into single df
list_dates = list.files(path="/Users/Matthewclark989/Documents/output_means/", pattern="*.csv", full.names=TRUE) #same folder you saved above output

final_means<-list_dates %>%
  map(read_csv) %>%
  reduce(cbind)

#append park code
final_means<-data.frame(park=park_codes, final_means)

#save
write.csv(final_means, "/Users/Matthewclark989/Documents/final_means2.csv", row.names = FALSE)




















