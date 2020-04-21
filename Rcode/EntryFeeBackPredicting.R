library(tidyverse)
#install.packages("rstanarm")
library(rstanarm)
library(parallel)
#FEdat<-read.csv("C:/Users/laura/OneDrive/Documents/VisitorFee/LauraStoddardEntryFeeCsv.csv")
FEdat<-read.csv("~/SmokeProject/Data/LauraStoddardEntryFeeCsv.csv")
head(FEdat)

FEdat<-FEdat %>% dplyr::select(Park,Year,SingleVisitorEntryFee) #keep only these columns
d<-na.omit(FEdat) #get rid of rows with any NA
head(d) #check it out

#options(mc.cores=3)


#feefit<-stan_glm(SingleVisitorEntryFee~Year+Park,family="poisson",data=d,chains=3,iter=6000)

fillData<-FEdat[,1:2]


FEdat$PredictedFee<-apply(posterior_predict(feefit,fillData),2,mean) #assuming this is for a new column

xx<-na.omit(FEdat) #remove na's to compare accuracy of predictions and observed data
cor(xx$PredictedFee,xx$SingleVisitorEntryFee) #correlation of predicted to observed
plot(xx$PredictedFee,xx$SingleVisitorEntryFee) #plot it

#fill the na's leave it if it's not an na
FEdat$SingleVisitorFee<-ifelse(is.na(FEdat$SingleVisitorEntryFee),FEdat$PredictedFee,FEdat$SingleVisitorEntryFee)

#indicate if it's a predicted fee or observed
FEdat$Source<-ifelse(is.na(FEdat$SingleVisitorEntryFee),"Predicted","Observed")

#remove extra columns
FEdat<-FEdat[,-c(3,4)]


#the FEdat data doesn't have a unit code column. Let's add that
NamesData<-read_csv("Data/MergedDataComplete.csv")[,-c(1,3,4,6:13)]
names(NamesData)[2]<-"Park" #match the column names so we can merge
NamesData<-data.frame(UnitCode=unique(NamesData$UnitCode),Park=unique(NamesData$Park))#get just each park once
FEdat<-merge(FEdat, NamesData, by = "Park") #merge

#now add the predicted visitation data
VEstDat<-read.csv("~/SmokeProject/Data/VisitationEstimatesMinSmokeSignificantParks.csv")[,-1]

#get the visitation observed and predicted yearly totals (same scale as fee data)
VEstDat<-VEstDat%>%group_by(UnitCode,Year)%>%
  summarise(RecVisits=sum(RecreationVisits),PredVisits = sum(No_Smoke_Mean_est))

#only include significant parks and years we predicted for
FEdat<-FEdat%>%filter(UnitCode %in% VEstDat$UnitCode)%>%
  filter(Year %in% VEstDat$Year)

#merge the fee and visitation data
dat<-merge(FEdat, VEstDat, by = c("UnitCode","Year"))

#multiply the fees and visitation (predicted and observed)
dat<-dat%>%mutate(incomeObserved= round(SingleVisitorFee*RecVisits,digits = 2),incomePred= round(SingleVisitorFee*PredVisits,digits=2))

#get the dollar estimates for smoke loss for each park)
summaryFinancialLoss<-dat %>% group_by(UnitCode)%>%summarise(totrec=sum(incomeObserved),totpred=sum(incomePred))%>%
  mutate(IncomeDif=totrec-totpred,IncomePrecDiff=round((totrec/totpred)*100,digits=2))


#some example plots
library(ggplot2) 
FEdat%>%filter(Park=="Zion NP")%>%
  ggplot(., aes(x=Year,y=SingleVisitorFee))+
  geom_line(aes(color=Source))
 
FEdat%>%filter(Source=="Observed")%>%
  ggplot(., aes(x=Year,y=SingleVisitorFee,by=Park))+
  geom_smooth(method="lm",se=F)
