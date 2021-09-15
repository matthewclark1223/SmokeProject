library(tidyverse)
BS_Column<-read_csv("~/SmokeProject/Data/BS_Data/BS_column_mean.csv")
BS_Surface<-read_csv("~/SmokeProject/Data/BS_Data/BS_surface_mean.csv")
BS_Col_long<-BS_Column%>%gather(key="time",value="smokeCol",m_198001:m_202009)
BS_Surf_long<-BS_Surface%>%gather(key="time",value="smokeSurf",m_198001:m_202009)

smokecor<-cor(BS_Col_long$smokeCol,BS_Surf_long$smokeSurf)

BSD<-cbind(BS_Surf_long,BS_Col_long[,3])

mytheme<- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                plot.title = element_text( size=18, color="black",face="bold"),
                axis.title.x = element_text( size=18),
                axis.title.y = element_text( size=18),
                axis.text=(element_text(color="black", size=14)),
                legend.title = element_text(colour="black", size=18),
                legend.text = element_text( size = 14))

ggplot(BSD,aes(x=smokeCol*1e5,y=smokeSurf*1e8))+
  geom_abline(intercept = 0, slope = 1,linetype="dashed",color="#525252",size=1.5 )+
  geom_point(alpha=0.5)+theme_classic()+mytheme+
  ylab("Surface Monthly Mean e-8")+xlab("Column Monthly Mean e-5")+
  geom_text(x=0.5,y=1.5,color="black",label=paste0("cor = ", round(smokecor,digits = 2)),size=8)


