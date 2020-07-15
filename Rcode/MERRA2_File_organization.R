#make fake file names

MERRA2_300.inst3_3d_aer_Nv.20101218.nc4.nc4


MER300<-vector()
for (i in c("01","02","03","04","05","06","07","08","09","11","12")){
  for (j in c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010)){
    for (l in c("01","05","10","25")){
      
    y<-paste0("MERRA2_300.inst3_3d_aer_Nv.",j,i,l,".nc4.nc4")
    MER300<-c(MER300,y)
}}}

MER200<-vector()
for (i in c("01","02","03","04","05","06","07","08","09","11","12")){
  for (j in c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000)){
    for (l in c("01","05","10","25")){
      
      y<-paste0("MERRA2_200.inst3_3d_aer_Nv.",j,i,l,".nc4.nc4")
      MER200<-c(MER200,y)
    }}}

MER<-c(MER200,MER300)



####Now let's just make individual batches for each month/year combination

for (i in c(1991,1992,1993,1994,1995,1996,1997,1998,1999,2000)){
for (j in c("01","02","03","04","05","06","07","08","09","10","11","12")){
    x<-(paste0(i,j))
    assign(paste0("x",i,j),MER[grep(x,MER)])
    
}}

x199101


#Now do it with the files stored in a folder
fileNames<-list.files("~/SmokeProject/MERRA_DATA_ORG_PRACT") #this is a practice example of what the actual data foler will look like
for (i in c(1980,2004)){ #this will have all the years whe we actually do it. The example folder only has 1980 and 2004
  for (j in c("01","10")){ #same as above but for months
    x<-(paste0(i,j))
    assign(paste0("x",i,j),fileNames[grep(x,fileNames)])
    
  }}


x200410 #this will give you all the file names from the folder that are from oct 2004 regardless of the DAY





