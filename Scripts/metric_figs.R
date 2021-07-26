#FLOAT-MAST metric figures

library(tidyr)
library(dplyr)
library(ggplot2)

Zoop_FLOAT_LT<-readRDS("Outputs/zoop_float_lt.rds")
Zoop_FLOAT_ST<-readRDS("Outputs/zoop_float_st.rds")
Zoop_Drought_LT<-readRDS("Outputs/zoop_drought_lt.rds")
Zoop_Drought_LT_CPUE<-readRDS("Outputs/zoop_drought_lt_cpue.rds")
Effort<-readRDS("Outputs/effort.rds")

Zoop_FLOAT_LT$Analysis<-"FLOAT_LT_BPUE"
Zoop_FLOAT_ST$Analysis<-"FLOAT_ST_BPUE"
Zoop_Drought_LT$Analysis<-"Drought_LT_BPUE"
Zoop_Drought_LT_CPUE$Analysis<-"Drought_LT_CPUE"

Zoop_FLOAT_LT$metric<-Zoop_FLOAT_LT$BPUE_ug
Zoop_FLOAT_ST$metric<-Zoop_FLOAT_ST$BPUE_ug
Zoop_Drought_LT$metric<-Zoop_Drought_LT$BPUE_ug
Zoop_Drought_LT_CPUE$metric<-Zoop_Drought_LT_CPUE$szn_CPUE


all_metrics<-Zoop_FLOAT_LT%>%
  rbind(Zoop_FLOAT_ST)%>%
  rbind(Zoop_Drought_LT)%>%
  rbind(Zoop_Drought_LT_CPUE)
all_metrics$Season<-factor(all_metrics$Season,levels = c("Winter","Spring","Summer","Fall"))
all_metrics$Year_Season<-paste(all_metrics$water_year,all_metrics$Season,sep=" ")

p<-ggplot(all_metrics,aes(water_year,metric))+
  geom_bar(stat="identity")+
  theme_classic()+theme(axis.text.x=element_text(angle=90,size=8))+
  facet_wrap(Analysis~.,nrow=4,scales ="free_y")
p
ggsave("Figures/all_metrics.png")

#Effort figures
size_types<-c("Macro","Meso")
szn<-c("Winter","Spring","Summer","Fall")
plot_list<-list()
for(i in 1:2){
  target_size<-size_types[i]
  d=Effort%>%
    filter(SizeClass==target_size)
    d2<-d
    p<-ggplot(d2,aes(year,Region))+
      geom_tile(aes(fill=tows))+
      theme_classic()+
      theme(axis.text.x=element_text(angle=90,size=10),axis.text.y = element_text(size=10),legend.position = "none")+
      scale_fill_gradient(low="white",high="red")+
      geom_text(aes(label=round(tows,1)),size=4)+
      guides(colour=F)+
      ggtitle(paste(target_size,"Sampling Coverage",sep=" "))+
      facet_grid(Source~.)
    p
    ggsave(paste("Figures/",target_size,".png",sep=""),width=18,height=8)
}

#distribution figures
dist_matrix<-readRDS("Outputs/distribution_matrix.rds")
taxa_list<-unique(dist_matrix$Taxlifestage)
#szn<-unique(dist_matrix$Season)

for(i in 1:length(taxa_list)){
  d<-dist_matrix%>%
    filter(Taxlifestage==taxa_list[i])
    d2<-d
    p<-ggplot(d2,aes(water_year,km_distr))+
      annotate("rect",ymin=30,ymax=42,xmin=1970,xmax=max(d2$water_year+1),alpha=.3,fill="blue")+
      annotate("rect",ymin=42,ymax=56,xmin=1970,xmax=max(d2$water_year+1),alpha=.3,fill="green")+
      annotate("rect",ymin=56,ymax=75,xmin=1970,xmax=max(d2$water_year+1),alpha=.3,fill="yellow")+
      annotate("rect",ymin=75,ymax=90,xmin=1970,xmax=max(d2$water_year+1),alpha=.3,fill="orange")+
      annotate("rect",ymin=90,ymax=105,xmin=1970,xmax=max(d2$water_year+1),alpha=.3,fill="red")+
      annotate("rect",ymin=105,ymax=140,xmin=1970,xmax=max(d2$water_year+1),alpha=.3,fill="purple")+
      annotate("text", y = 36, x = 1972, label = "San Pablo \n Bay",size=3)+
      annotate("text", y = 49, x = 1972, label = "Carquinez \n Strait",size=3)+
      annotate("text", y = 66, x = 1972, label = "Suisun",size=3)+
      annotate("text", y = 82, x = 1972, label = "West \n Delta",size=3)+
      annotate("text", y = 98, x = 1972, label = "Central Delta",size=3)+
      annotate("text", y = 122, x = 1972, label = "East Delta",size=3)+
      geom_point()+
      geom_vline(xintercept=d2$water_year, linetype="dashed", 
                 color = "red", size=.2)+
      scale_y_continuous(expand = c(0, 0),limits=c(30,140),breaks = round(seq(from=30,
                                                                              to=140,
                                                                              by = 10),1))+
      scale_x_continuous(expand = c(0, 0),limits=c(1970,2021),breaks = round(seq(from=1970,
                                                                              to=2021,
                                                                              by = 5),1))+
      ggtitle(paste(taxa_list[i],"Annual center of distribution",sep=" "))+
      xlab("Year") + ylab("Center of distribution (km)")+
      coord_flip()
    p
    ggsave(paste("Figures/distributions/",taxa_list[i],".png",sep=""))
}
