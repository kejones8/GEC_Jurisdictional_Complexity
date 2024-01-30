
#connects each incident_id to the mtbs_ids and map attributes
library(tidyverse)
library(sf)
library(dplyr)
`%notin%` <- Negate(`%in%`)


# all attributes tied to a single incident/burned boundary
mtbs_insamp<-read_sf("data/select_mtbs.shp")
mtbs_incid_proj<-st_transform(mtbs_insamp,5070)

#read in table that joins mtbs & incids
link_mtbs_incids<-read.csv("data/incid_fires_year.csv")

#join incids to mtbs
mtbs_incid<-merge(link_mtbs_incids,mtbs_incid_proj,by.x="mtbs_ids",by.y="Event_ID",all.x=TRUE)
dim(mtbs_incid)


write_sf(mtbs_incid,"data/mtbs_with_incids.shp")

#make geometries representative of whole incident id/all mtbs footprints
incid_polys<-mtbs_incid %>%
  group_by(incident_id) %>% 
  dplyr::summarize(geometry = st_union(geometry))


#dissolve all mtbs footprints to incident id to join to incident level data
write_sf(incid_polys,"data/incids_multipoly.shp")

#read in the jurisdictional count data 
jur_counts<-read.csv(final_out)

#read in the jurisdictional area estimates 
jur_area<-read.csv("data/areas_burn_eng_byjur_landtenure.csv")

#join tabular data, make sure i preserve NAs where needed 
tab_merged<-merge(jur_counts,jur_area,by="incident_id",all=TRUE)

#merge it all together
polys_with_att<-merge(incid_polys,tab_merged,by="incident_id")

#after making changes from burn - threat to burn - engage need to restructure and name many columns 
polys_with_att$X.x<-NULL
polys_with_att$X.y<-NULL
polys_with_att$fed_engag_cnt<-polys_with_att$fed_burn_cnt+polys_with_att$fed_threat_cnt
polys_with_att$fed_threat_cnt<-NULL
polys_with_att$trib_engag_cnt<-polys_with_att$trib_burn_cnt+polys_with_att$trib_threat_cnt
polys_with_att$trib_threat_cnt<-NULL
polys_with_att$st_engag_cnt<-polys_with_att$st_burn_count+polys_with_att$st_threat_count
polys_with_att$st_threat_count<-NULL
names(polys_with_att)[names(polys_with_att)=="st_burn_count"]<-"st_burn_cnt"
polys_with_att$cnty_engag_cnt<-polys_with_att$cnty_burn_count+polys_with_att$cnty_threat_count
polys_with_att$cnty_threat_count<-NULL
names(polys_with_att)[names(polys_with_att)=="cnty_burn_count"]<-"cnty_burn_cnt"
polys_with_att$cenpl_engag_cnt<-polys_with_att$cenpl_burn_count+polys_with_att$cenpl_threat_count
polys_with_att$cenpl_threat_count<-NULL
names(polys_with_att)[names(polys_with_att)=="cenpl_burn_count"]<-"cenpl_burn_cnt"
polys_with_att$gacc_engag_cnt<-polys_with_att$gacc_burn_count+polys_with_att$gacc_threat_count
polys_with_att$gacc_threat_count<-NULL



polys_with_att<-polys_with_att %>% dplyr::mutate(alljur_engag_cnt=  fed_engag_cnt + trib_engag_cnt + st_engag_cnt + cnty_engag_cnt + cenpl_engag_cnt,na.rm=TRUE)
polys_with_att$jur_threatened<-NULL
names(polys_with_att)[names(polys_with_att)=="jur_burned"]<-"alljur_burn_cnt"
names(polys_with_att)[names(polys_with_att)=="total_ac_burn"]<-"total_acre_burn"
names(polys_with_att)[names(polys_with_att)=="total_ac_engag"]<-"total_acre_engag"
names(polys_with_att)[names(polys_with_att)=="gacc_burn_count"]<-"gacc_burn_cnt"


order_colnames<-c("incident_id","fed_burn_cnt","trib_burn_cnt","st_burn_cnt","cnty_burn_cnt",
                  "cenpl_burn_cnt","alljur_burn_cnt","fed_engag_cnt","trib_engag_cnt","st_engag_cnt",
                  "cnty_engag_cnt","cenpl_engag_cnt","alljur_engag_cnt","fed_acre_burn",
                  "trib_acre_burn","state_acre_burn","loc_acre_burn","priv_acre_burn","total_acre_burn",
                  "fed_acre_engag","trib_acre_engag","state_acre_engag","loc_acre_engag",
                  "priv_acre_engag","total_acre_engag","fed_percburn","trib_percburn","state_percburn",
                  "loc_percburn","priv_percburn","fed_percengag","trib_percengag","state_percengag",
                  "loc_percengag","priv_percengag","gacc_burn_cnt","gacc_engag_cnt","geometry")

need_ordered<-polys_with_att[,order_colnames]


df_fed<-need_ordered[,c("fed_burn_cnt","fed_engag_cnt"),]
df_fed$level<-rep("federal",nrow(df_fed))
df_trib<-need_ordered[,c("trib_burn_cnt","trib_engag_cnt"),]
df_trib$level<-rep("tribal",nrow(df_trib))
df_st<-need_ordered[,c("st_burn_cnt","st_engag_cnt"),]
df_st$level<-rep("state",nrow(df_st))
df_cnty<-need_ordered[,c("cnty_burn_cnt","cnty_engag_cnt"),]
df_cnty$level<-rep("county",nrow(df_cnty))
df_cenpl<-need_ordered[,c("cenpl_burn_cnt","cenpl_engag_cnt"),]
df_cenpl$level<-rep("cenpl",nrow(df_cenpl))

df_fed %>% 
  pivot_longer(
    c(fed_burn_cnt, fed_engag_cnt)
  ) %>% 
  ggplot(aes(value, fill=name))+
  geom_histogram(position = "dodge")+ggtitle("Federal")

df_trib %>% 
  pivot_longer(
    c(trib_burn_cnt, trib_engag_cnt)
  ) %>% 
  ggplot(aes(value, fill=name))+
  geom_histogram(position = "dodge")+ggtitle("Tribal")

df_st %>% 
  pivot_longer(
    c(st_burn_cnt, st_engag_cnt)
  ) %>% 
  ggplot(aes(value, fill=name))+
  geom_histogram(position = "dodge")+ggtitle("State")

df_cnty %>% 
  pivot_longer(
    c(cnty_burn_cnt, cnty_engag_cnt)
  ) %>% 
  ggplot(aes(value, fill=name))+
  geom_histogram(position = "dodge")+ggtitle("County")

df_cenpl %>% 
  pivot_longer(
    c(cenpl_burn_cnt, cenpl_engag_cnt)
  ) %>% 
  ggplot(aes(value, fill=name))+
  geom_histogram(position = "dodge")+ggtitle("Census Place")



poly_att_2<-need_ordered %>% dplyr::mutate(fed_lev_engag=rowSums(dplyr::select(.,"fed_acre_engag")!=0|dplyr::select(.,"fed_acre_burn")!=0),
                                             trib_lev_engag=rowSums(dplyr::select(.,"trib_acre_engag")!=0|dplyr::select(.,"trib_acre_burn")!=0),
                                             st_lev_engag=rowSums(dplyr::select(.,"state_acre_engag")!=0|dplyr::select(.,"state_acre_burn")!=0),
                                             loc_lev_engag=rowSums(dplyr::select(.,"loc_acre_engag")!=0|dplyr::select(.,"loc_acre_burn")!=0),
                                             priv_lev_engag=rowSums(dplyr::select(.,"priv_acre_engag")!=0|dplyr::select(.,"priv_acre_burn")!=0),
                                             total_lev_engag = (fed_lev_engag+trib_lev_engag+st_lev_engag+loc_lev_engag+priv_lev_engag),
                                             fed_lev_burn=rowSums(dplyr::select(.,"fed_acre_burn")!=0),
                                             trib_lev_burn=rowSums(dplyr::select(.,"trib_acre_burn")!=0),
                                             st_lev_burn=rowSums(dplyr::select(.,"state_acre_burn")!=0),
                                             loc_lev_burn=rowSums(dplyr::select(.,"loc_acre_burn")!=0),
                                             priv_lev_burn=rowSums(dplyr::select(.,"priv_acre_burn")!=0),
                                             total_lev_burn = (fed_lev_burn+trib_lev_burn+st_lev_burn+loc_lev_burn+priv_lev_burn))

#add start year back in
getstartyr<-link_mtbs_incids[,c("incident_id","start_year")]

#incid_info<-merge(what_i_need,what_i_need_arealev,by="incident_id",all=TRUE)
incid_info<-merge(poly_att_2,getstartyr,all.x=TRUE,by="incident_id")


incid_info_nodup_geom<-incid_info[!duplicated(incid_info$incident_id),]

#tabular output
incid_info_nodup<-incid_info[!duplicated(incid_info$incident_id),]
incid_info_nodup$geometry<-NULL



st_write(incid_info_nodup_geom,"data\incids_mtbs_final.gpkg",delete_layer=TRUE)

