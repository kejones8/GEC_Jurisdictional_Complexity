#this script merges all of the tables created for burned & threatened jurisdictions to #cleanly add these data to a final table linked to each incident
library(dplyr)

#read in mtbs_ids & incident_ids to get baseline JCA sample 
mtbs_incid<-read.csv("data/incid_fires_year.csv")
mtbs_incid$X<-NULL #column clean up
mtbs_incid$mtbs_ids<-NULL #column clean up 

###1. all federal counts to merge 

#1.1 usfs
fed_usfs_burn<-read.csv("data/usfs_burn_count.csv")
fed_usfs_burn$X<-NULL
colnames(fed_usfs_burn)[2]<-"usfs_burn_count"
fed_usfs_threat<-read.csv("data/usfs_eng_count.csv")
fed_usfs_threat$X<-NULL
colnames(fed_usfs_threat)[2]<-"usfs_threat_count"
fed_usfs<-merge(fed_usfs_burn,fed_usfs_threat,by="incident_id",all=TRUE)

#1.2 nps
fed_nps_burn<-read.csv("data/nps_burn_count.csv")
fed_nps_burn$X<-NULL
colnames(fed_nps_burn)[2]<-"nps_burn_count"
fed_nps_threat<-read.csv("data/nps_eng_count.csv")
fed_nps_threat$X<-NULL
colnames(fed_nps_threat)[2]<-"nps_threat_count"
fed_nps<-merge(fed_nps_burn,fed_nps_threat,by="incident_id",all=TRUE)

#1.3 usfws
fed_usfws_burn<-read.csv("data/usfws_burn_count.csv")
fed_usfws_burn$X<-NULL
colnames(fed_usfws_burn)[2]<-"usfws_burn_count"
fed_usfws_threat<-read.csv("data/usfws_eng_count.csv")
fed_usfws_threat$X<-NULL
colnames(fed_usfws_threat)[2]<-"usfws_threat_count"
fed_usfws<-merge(fed_usfws_burn,fed_usfws_threat,by="incident_id",all=TRUE)

#1.4 tva  
fed_tva_burn<-read.csv("data/tva_burn_count.csv")
fed_tva_burn$X<-NULL
colnames(fed_tva_burn)[2]<-"tva_burn_count"
fed_tva_threat<-read.csv("data/tva_eng_count.csv")
fed_tva_threat$X<-NULL
colnames(fed_tva_threat)[2]<-"tva_threat_count"
fed_tva<-merge(fed_tva_burn,fed_tva_threat,by="incident_id",all=TRUE)

#1.5 bor
fed_bor_burn<-read.csv("data/bor_burn_count.csv")
fed_bor_burn$X<-NULL
colnames(fed_bor_burn)[2]<-"bor_burn_count"
fed_bor_threat<-read.csv("data/bor_eng_count.csv")
fed_bor_threat$X<-NULL
colnames(fed_bor_threat)[2]<-"bor_threat_count"
fed_bor<-merge(fed_bor_burn,fed_bor_threat,by="incident_id",all=TRUE)

#1.6 doe
fed_doe_burn<-read.csv("data/doe_burn_count.csv")
fed_doe_burn$X<-NULL
colnames(fed_doe_burn)[2]<-"doe_burn_count"
fed_doe_threat<-read.csv("data/doe_eng_count.csv")
fed_doe_threat$X<-NULL
colnames(fed_doe_threat)[2]<-"doe_threat_count"
fed_doe<-merge(fed_doe_burn,fed_doe_threat,by="incident_id",all=TRUE)

#1.7 dod
fed_dod_burn<-read.csv("data/dod_burn_count.csv")
fed_dod_burn$X<-NULL
colnames(fed_dod_burn)[2]<-"dod_burn_count"
fed_dod_threat<-read.csv("data/dod_eng_count.csv")
fed_dod_threat$X<-NULL
colnames(fed_dod_threat)[2]<-"dod_threat_count"
fed_dod<-merge(fed_dod_burn,fed_dod_threat,by="incident_id",all=TRUE)

#1.8 blm
fed_blm_burn<-read.csv("data/blm_burn_count.csv")
fed_blm_burn$X<-NULL
colnames(fed_blm_burn)[2]<-"blm_burn_count"
fed_blm_threat<-read.csv("data/blm_eng_count.csv")
fed_blm_threat$X<-NULL
colnames(fed_blm_threat)[2]<-"blm_threat_count"
fed_blm<-merge(fed_blm_burn,fed_blm_threat,by="incident_id",all=TRUE)

#merge all federal tables
fed_merged <- Reduce(function(...) merge(...,by="incident_id", all=T), list(fed_usfs, fed_nps, fed_usfws, fed_tva, fed_bor, fed_doe, fed_dod, fed_blm))

#sum federal burned counts
fed_merged<-fed_merged %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(fed_burn_cnt = sum(usfs_burn_count, nps_burn_count,usfws_burn_count,tva_burn_count,bor_burn_count,
                                   doe_burn_count,dod_burn_count,blm_burn_count, na.rm = TRUE))

#sum federal threatened/engaged counts
fed_merged<-fed_merged %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(fed_threat_cnt = sum(usfs_threat_count, nps_threat_count,usfws_threat_count,tva_threat_count,bor_threat_count,
                                     doe_threat_count,dod_threat_count,blm_threat_count, na.rm = TRUE))

#table with incident_ids & federal burned & threatened/enaged counts
fed_counts<-fed_merged[,c("incident_id","fed_burn_cnt","fed_threat_cnt")]


###2. all tribal counts to merge

#2.1 ancsa
trib_ancsa_burn<-read.csv("data/ancsa_burn_count.csv")
trib_ancsa_burn$X<-NULL
colnames(trib_ancsa_burn)[2]<-"ancsa_burn_count"
trib_ancsa_threat<-read.csv("data/ancsa_eng_count.csv")
trib_ancsa_threat$X<-NULL
colnames(trib_ancsa_threat)[2]<-"ancsa_threat_count"
trib_ancsa<-merge(trib_ancsa_burn,trib_ancsa_threat,by="incident_id",all=TRUE)

#2.2 oth tribal 
trib_othtrib_burn<-read.csv("data/othtrib_burn_count.csv")
trib_othtrib_burn$X<-NULL
colnames(trib_othtrib_burn)[2]<-"othtrib_burn_count"
trib_othtrib_threat<-read.csv("data/othtrib_eng_count.csv")
trib_othtrib_threat$X<-NULL
colnames(trib_othtrib_threat)[2]<-"othtrib_threat_count"
trib_othtrib<-merge(trib_othtrib_burn,trib_othtrib_threat,by="incident_id",all=TRUE)

#2.3 bia 
trib_bia_burn<-read.csv("data/bia_burn_count.csv")
trib_bia_burn$X<-NULL
colnames(trib_bia_burn)[2]<-"bia_burn_count"
trib_bia_threat<-read.csv("data/bia_eng_count.csv")
trib_bia_threat$X<-NULL
colnames(trib_bia_threat)[2]<-"bia_threat_count"
trib_bia<-merge(trib_bia_burn,trib_bia_threat,by="incident_id",all=TRUE)

#merge all federal tables
trib_merged <- Reduce(function(...) merge(...,by="incident_id", all=T), list(trib_ancsa,trib_othtrib,trib_bia))

#sum federal burned counts
trib_merged<-trib_merged %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(trib_burn_cnt = sum(ancsa_burn_count,othtrib_burn_count,bia_burn_count, na.rm = TRUE))

#sum federal threatened counts
trib_merged<-trib_merged %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(trib_threat_cnt = sum(ancsa_threat_count, othtrib_threat_count,bia_threat_count, na.rm = TRUE))

#table with incident_ids & federal burned & threatened counts
trib_counts<-trib_merged[,c("incident_id","trib_burn_cnt","trib_threat_cnt")]


###3. all states 

st_cnt_burn<-read.csv("data/state_burn_count.csv")
st_cnt_burn$X<-NULL
colnames(st_cnt_burn)[2]<-"st_burn_count"
st_cnt_threat<-read.csv("data/state_eng_count.csv")
st_cnt_threat$X<-NULL
colnames(st_cnt_threat)[2]<-"st_threat_count"
st_cnt<-merge(st_cnt_burn,st_cnt_threat,by="incident_id",all=TRUE)



#4. county 

county_cnt_burn<-read.csv("data/county_burn_count.csv")
county_cnt_burn$X<-NULL
colnames(county_cnt_burn)[2]<-"cnty_burn_count"
county_cnt_threat<-read.csv("data/county_eng_count.csv")
county_cnt_threat$X<-NULL
colnames(county_cnt_threat)[2]<-"cnty_threat_count"
county_cnt<-merge(county_cnt_burn,county_cnt_threat,by="incident_id",all=TRUE)

#5. census place 

cenpl_cnt_burn<-read.csv("data\cenpl_burn_count_.csv")
cenpl_cnt_burn$X<-NULL
colnames(cenpl_cnt_burn)[2]<-"cenpl_burn_count"
cenpl_cnt_threat<-read.csv("data\cenpl_eng_count_.csv")
cenpl_cnt_threat$X<-NULL
colnames(cenpl_cnt_threat)[2]<-"cenpl_threat_count"
cenpl_cnt<-merge(cenpl_cnt_burn,cenpl_cnt_threat,by="incident_id",all=TRUE)



#now, combine all count data
jur_counts <- Reduce(function(...) merge(...,by="incident_id", all=T), list(fed_counts,trib_counts,st_cnt,county_cnt,cenpl_cnt,gacc_cnt))
inc_jur_counts[is.na(inc_jur_counts)] <- 0 #remove any incids with no jurisidction information
inc_jur_counts$jur_burned<-inc_jur_counts$fed_burn_cnt+inc_jur_counts$trib_burn_cnt+inc_jur_counts$st_burn_count+inc_jur_counts$cnty_burn_count+inc_jur_counts$cenpl_burn_count

inc_jur_counts$jur_threatened<-inc_jur_counts$fed_threat_cnt+inc_jur_counts$trib_threat_cnt+inc_jur_counts$st_threat_count+inc_jur_counts$cnty_threat_count+inc_jur_counts$cenpl_threat_count

#write out table with jurisdictional counts
write.csv(inc_jur_counts,"data/incid_withjur_counts.csv")
