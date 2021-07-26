rm( list = ls()) #clear env
#load packages
library(tidyverse)
library(tibble)
library(dplyr)
library(ggplot2)
library(plotrix)
library(purrr)
library(cowplot)
library(plyr)
library(readxl)
library(deltamapr)
library(spacetools)
library(sf)

#################################
#DROUGHT Center of distribution metrics
#################################


#dist function
dist_center<-function(data,taxa){
  d=data%>%
    filter(Taxlifestage==taxa)
  d$km_freq<-d$CPUE*d$distance_km
  d1<-d%>%
    group_by(Survey,seasons,Year,Taxlifestage,Regime)%>%
    dplyr::summarize(km_center=sum(km_freq)/sum(CPUE),Mean_X2=mean(X2))
}

ez_stations<-readRDS("Outputs/ez_stations.rds")
zoop_stations<-readRDS("Outputs/zoopstations.rds")

#calculate station distances using spacetools
zoop_stations$Station_ID<-paste(zoop_stations$Source,zoop_stations$Station,sep="_")
ez_stations$Station_ID<-paste(ez_stations$Source,ez_stations$Station,ez_stations$Date,sep="_")

#add in goldengate lat/long
gg_c<-c("Golden_Gate","Golden_Gate","-122.4783","37.8199")
gg_ez<-c("Golden_Gate","Golden_Gate","-122.4783","37.8199","1/1/1999")
zoop_stations<-zoop_stations%>%rbind(gg_c)
ez_stations<-ez_stations%>%rbind(gg_ez)

#station_distances<-Waterdist(Water_map = spacetools::Delta, Points = zoop_stations,
                             #Latitude_column = Latitude, Longitude_column = Longitude, 
                             #PointID_column = Station_ID)
#ez_distances<-Waterdist(Water_map = spacetools::Delta, Points = ez_stations,
                        #Latitude_column = Latitude, Longitude_column = Longitude, 
                        #PointID_column = Station_ID)

#saveRDS(station_distances,"Outputs/station_distances.rds")
#saveRDS(ez_distances,"Outputs/ez_distances.rds")

station_distances<-readRDS("Outputs/station_distances.rds")
ez_distances<-readRDS("Outputs/ez_distances.rds")

station_distances_df<-as.data.frame(station_distances)
station_distances_df<-rownames_to_column(station_distances_df,var="Station_ID")
station_distances_df<-dplyr::select(station_distances_df,Station_ID,Golden_Gate)

ez_distances_df<-as.data.frame(ez_distances)
ez_distances_df<-rownames_to_column(ez_distances_df,var="Station_ID")
ez_distances_df<-dplyr::select(ez_distances_df,Station_ID,Golden_Gate)

#seperate our Station ID info
station_distances_df$Station_ID<-as.character(station_distances_df$Station_ID)
station_distances_df<-separate(station_distances_df, col=Station_ID,c("Source","Station"),sep="_")

ez_distances_df$Station_ID<-as.character(ez_distances_df$Station_ID)
ez_distances_df<-separate(ez_distances_df, col=Station_ID,c("Source","Station","Date"),sep="_")

#change meters to km
station_distances_df$distance_km<-station_distances_df$Golden_Gate/1000
ez_distances_df$ez_km<-ez_distances_df$Golden_Gate/1000

station_dist<-dplyr::select(station_distances_df,Source,Station,distance_km)
ez_dist<-dplyr::select(ez_distances_df,Source,Station,Date,ez_km)
ez_dist$Date<-as.Date(ez_dist$Date,"%Y-%m-%d")

Drought_LT_dist<-readRDS("Outputs/Drought_LT_CPUE_dist.rds")
#join distances back into Drought dataset
Drought_LT_dist<-Drought_LT_dist%>%
  left_join(station_dist)

Drought_LT_dist<-Drought_LT_dist%>%
  left_join(ez_dist)

Drought_LT_dist$distance_km<-ifelse(is.na(Drought_LT_dist$distance_km),Drought_LT_dist$ez_km,Drought_LT_dist$distance_km)

Drought_LT_dist<-Drought_LT_dist[complete.cases(Drought_LT_dist$distance_km), ]

test<-Drought_LT_dist%>%filter(Taxlifestage=="Neomysis mercedis Adult")

#lets do total CPUE distribution first
Drought_dist_tot<-Drought_LT_dist%>%group_by(Station,Date,water_year,month,Season,distance_km)%>%
  filter(water_year>1974)%>%
  dplyr::summarise(totalCPUE=sum(CPUE))

#calculate center of distributions for total CPUE
ddt_grouped<-Drought_dist_tot%>%group_by(Station,distance_km,Date,water_year,month,Season)%>%
  filter(water_year>1974)%>%
  dplyr::summarise(sumCPUE=sum(totalCPUE))
ddt_grouped<-ddt_grouped%>%
  group_by(water_year,month,Season,Station,distance_km)%>%
  dplyr::summarise(month_CPUE=mean(sumCPUE))
#ddt_grouped<-ddt_grouped%>%
  #inner_join(rosies_table)%>%
  #filter(Long_term==TRUE)
#calculate seasonal averages
ddt_grouped<-ddt_grouped%>%
  group_by(water_year,Season,Station,distance_km)%>%
  dplyr::summarise(s_CPUE=mean(month_CPUE))
ddt_grouped<-ddt_grouped%>%
  group_by(Season,water_year,Station,distance_km)%>%
  dplyr::summarise(szn_CPUE=mean(s_CPUE))
ddt_grouped<-ddt_grouped%>%
  group_by(water_year,Station,distance_km)%>%
  dplyr::summarise(yr_CPUE=mean(szn_CPUE))
ddt_grouped$freq<-as.integer(ddt_grouped$yr_CPUE)
ddt_grouped$km_freq<-ddt_grouped$distance_km*ddt_grouped$freq
ddt_total<-ddt_grouped%>%
  group_by(water_year)%>%
  dplyr::summarise(km_distr=sum(km_freq)/sum(freq))

saveRDS(ddt_total,"Outputs/ddt_total.rds")

#calculate center of distributions for each taxa
ddtaxa<-Drought_LT_dist%>%filter(water_year>1974)
ddtaxa_grouped<-ddtaxa%>%group_by(Station,distance_km,Date,water_year,month,Season,Taxlifestage)%>%
  dplyr::summarise(sumCPUE=sum(CPUE))
ddtaxa_grouped<-ddtaxa_grouped%>%
  group_by(water_year,month,Season,Station,distance_km,Taxlifestage)%>%
  dplyr::summarise(month_CPUE=mean(sumCPUE))
#ddtaxa_grouped<-ddtaxa_grouped%>%
 #inner_join(rosies_table)%>%
  #filter(Long_term==TRUE)
#calculate seasonal averages
ddtaxa_grouped<-ddtaxa_grouped%>%
  group_by(water_year,Season,Station,distance_km,Taxlifestage)%>%
  dplyr::summarise(s_CPUE=mean(month_CPUE))
ddtaxa_grouped<-ddtaxa_grouped%>%
  group_by(Season,water_year,Station,distance_km,Taxlifestage)%>%
  dplyr::summarise(szn_CPUE=mean(s_CPUE))

ddtaxa_grouped<-ddtaxa_grouped%>%
  group_by(Season,water_year,Taxlifestage,Station,distance_km)%>%
  dplyr::summarise(yr_CPUE=mean(szn_CPUE))
ddtaxa_grouped$freq<-as.integer(ddtaxa_grouped$yr_CPUE)
ddtaxa_grouped$freq<-ifelse(ddtaxa_grouped$yr_CPUE>0 & ddtaxa_grouped$yr_CPUE<1,1,ddtaxa_grouped$freq)
ddtaxa_grouped$km_freq<-ddtaxa_grouped$distance_km*ddtaxa_grouped$freq

ddtaxa_total<-ddtaxa_grouped%>%
  group_by(water_year,Taxlifestage)%>%
  dplyr::summarise(km_distr=sum(km_freq)/sum(freq))

saveRDS(ddtaxa_total,"Outputs/ddtaxa_total.rds")

#make one matrix with totals and taxa specific distribution values
distribution_matrix<-ddt_total
distribution_matrix$Taxlifestage<-"All Taxa"
distribution_matrix<-distribution_matrix%>%rbind(ddtaxa_total)
saveRDS(distribution_matrix,"Outputs/distribution_matrix.rds")

distribution_matrix_flat<-distribution_matrix%>%pivot_wider(names_from=Taxlifestage,values_from=km_distr)
write.csv(distribution_matrix_flat,"Outputs/distribution_matrix.csv",row.names = F)
