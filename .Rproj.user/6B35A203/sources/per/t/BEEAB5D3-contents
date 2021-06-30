#FLOAT-MAST metric figures

library(tidyr)
library(dplyr)
library(ggplot2)

Zoop_FLOAT_LT<-readRDS("Outputs/zoop_float_lt.rds")
Zoop_FLOAT_ST<-readRDS("Outputs/zoop_float_st.rds")
Zoop_Drought_LT<-readRDS("Outputs/zoop_drought_lt.rds")
Zoop_Drought_LT_CPUE<-readRDS("Outputs/zoop_drought_lt_cpue.rds")

Zoop_FLOAT_LT$Analysis<-"FLOAT_LT_BPUE"
Zoop_FLOAT_ST$Analysis<-"FLOAT_ST_BPUE"
Zoop_Drought_LT$Analysis<-"Drought_LT_BPUE"
Zoop_Drought_LT_CPUE$Analysis<-"Drought_LT_CPUE"

Zoop_FLOAT_LT$metric<-Zoop_FLOAT_LT$szn_BPUE
Zoop_FLOAT_ST$metric<-Zoop_FLOAT_ST$szn_BPUE
Zoop_Drought_LT$metric<-Zoop_Drought_LT$szn_BPUE
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
  facet_wrap(Analysis~Season,nrow=4)
p
ggsave("Figures/all_metrics.png")
