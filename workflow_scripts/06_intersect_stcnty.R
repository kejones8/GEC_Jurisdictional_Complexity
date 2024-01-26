#this script intersects states and counties ONLY for incident_ids that burned OR threatened # that left federal land 
#it then counts the states and counties involved in each incident 

library(sf)
library(dplyr)
library(foreach) #for parallelizing intersection
library(doParallel)

#read in burn & threat intersections from wfdss
surfman_int_burnarea<-read_sf("data/surfman_intersect_burn.shp")

surfman_int_threatarea<-read_sf("data/surfman_intersect_eng.shp")



#find jurisdictional records that are not federal for both burned and engaged

#nonfederal burn
nonfed_surfman_burn<-surfman_int_burnarea[surfman_int_burnarea$JrsdcUK!="Federal",]
nonfed_surfman_burn_buf<-st_buffer(nonfed_surfman_burn,0)
colnames(nonfed_surfman_burn_buf)[1]<-"Event_ID" #column name clean up
nonfed_surfman_burn_buf<-nonfed_surfman_burn_buf[!is.na(nonfed_surfman_burn_buf$Event_ID),]

#dissolve to make simpler spatial objects
nonfed_burn_diss <- nonfed_surfman_burn_buf %>% group_by(Event_ID) %>% dplyr::summarize()
#write out dissolved non federal burned jurisdictions
write_sf(nonfed_burn_diss,"data/nonfed_burn_jurs.shp")

#nonfederal engaged
#do same steps for engaged boundaries
nonfed_surfman_threat<-surfman_int_threatarea[surfman_int_threatarea$JrsdcUK!="Federal",]
nonfed_surfman_threat_buf<-st_buffer(nonfed_surfman_threat,0)
colnames(nonfed_surfman_threat_buf)[1]<-"Event_ID"
nonfed_threat_diss <- nonfed_surfman_threat_buf %>% group_by(Event_ID) %>% dplyr::summarize()
write_sf(nonfed_threat_diss,"data/nonfed_eng_jurs.shp")


#eventually intersect each of these with state and county files 
#but for now, going to use original fire footprints to subset states and counties to only those that burn/eng jurs

#read in national county data
cnty<-read_sf("data\\county\\tl_2017_us_county\\tl_2017_us_county.shp")
cnty_proj<-st_transform(cnty,5070)
cnty_buf<-st_buffer(cnty_proj,0)

#read in national state date
state<-read_sf("data\\states\\tl_2017_us_state\\tl_2017_us_state.shp")
state_proj<-st_transform(state,5070)
state_buf<-st_buffer(state_proj,0)


#reading this in to link list of incidents with mtbs_ids
mtbs_withincid<-read.csv("data/incid_fires_year.csv")

#read in mtbs burned area our sample footprints
mtbs_burn<-read_sf("data/select_mtbs.shp")
burn_proj<-st_transform(mtbs_burn,5070)
burn_buf<-st_buffer(burn_proj,0)
#read in mtbs threatened area our sample
mtbs_threat<-read_sf("data/eng_buf_no_donut.shp")
threat_proj<-st_transform(mtbs_threat,5070)
threat_buf<-st_buffer(threat_proj,0)


##now do the actual intersecting so we can get to counting
#combine to test for intersection
test<-rbind(burn_proj,threat_proj)

#pair down counties & states to those that have fires - 
cnty_proj$indicator <- st_intersects(cnty_proj,test)%>% lengths > 0
state_proj$indicator <- st_intersects(state_proj,test)%>% lengths > 0

#subset to states and counties that do intersect the burn/engaged boundary
cnty_tointer<-cnty_proj[cnty_proj$indicator==TRUE,]
state_tointer<-state_proj[state_proj$indicator==TRUE,]



#parallelize these too
registerDoParallel(makeCluster(12))
ptm <- proc.time()
print(Sys.time())

#get unique list of non federal land / jurisdictions by EVENTID
non_fed_threat_mtbsids<-unique(nonfed_threat_diss$Event_ID)

#for every threatened area mtbs footprint, intersect with surface management 
#write out combined sf object with all intersections


#Steps below repeat for burned / engaged and state/ cnty


#ENGAGED COUNTY
threat_nonfed_cnty_inter<-foreach(i=non_fed_threat_mtbsids, .combine = rbind, .packages=c('sf')) %dopar%  {
  
  fp<-nonfed_threat_diss[nonfed_threat_diss$Event_ID==i,]
  cnty_forthreat<-st_intersection(fp,cnty_tointer)#5 miles = 8047 meters
  
}
print(Sys.time())
stopImplicitCluster()
proc.time() - ptm



#ENGAGED STATE
registerDoParallel(makeCluster(12))
ptm <- proc.time()
print(Sys.time())


non_fed_threat_mtbsids<-unique(nonfed_threat_diss$Event_ID)

#for every burned area mtbs footprint, intersect with surface management 
#write out combined sf object with all intersections
threat_nonfed_st_inter<-foreach(i=non_fed_threat_mtbsids, .combine = rbind, .packages=c('sf')) %dopar%  {
  
  fp<-nonfed_threat_diss[nonfed_threat_diss$Event_ID==i,]
  st_forthreat<-st_intersection(fp,state_tointer)#5 miles = 8047 meters
  
}
print(Sys.time())
stopImplicitCluster()
proc.time() - ptm



#BURNED COUNTY
registerDoParallel(makeCluster(12))
ptm <- proc.time()
print(Sys.time())


non_fed_burn_mtbsids<-unique(nonfed_burn_diss$Event_ID)

#for every burned area mtbs footprint, intersect with surface management 
#write out combined sf object with all intersections
burn_nonfed_cnty_inter<-foreach(i=non_fed_burn_mtbsids, .combine = rbind, .packages=c('sf')) %dopar%  {
  
  fp<-nonfed_burn_diss[nonfed_burn_diss$Event_ID==i,]
  cnty_forburn<-st_intersection(fp,cnty_tointer)#5 miles = 8047 meters
  
}
print(Sys.time())
stopImplicitCluster()
proc.time() - ptm


#BURNED STATE
registerDoParallel(makeCluster(12))
ptm <- proc.time()
print(Sys.time())


non_fed_burn_mtbsids<-unique(nonfed_burn_diss$Event_ID)

#for every burned area mtbs footprint, intersect with surface management 
#write out combined sf object with all intersections
burn_nonfed_st_inter<-foreach(i=non_fed_burn_mtbsids, .combine = rbind, .packages=c('sf')) %dopar%  {
  
  fp<-nonfed_burn_diss[nonfed_burn_diss$Event_ID==i,]
  st_forburn<-st_intersection(fp,state_tointer)#5 miles = 8047 meters
  
}
print(Sys.time())
stopImplicitCluster()
proc.time() - ptm



#now, join back with incident ids
burn_st_withincid<-merge(burn_nonfed_st_inter,mtbs_withincid,by.x="Event_ID",by.y="mtbs_ids")
burn_st_trim<-burn_st_withincid[,c("incident_id","Event_ID","GEOID")]
burn_st_trim$geometry<-NULL

burn_st_uni<-unique(burn_st_trim)


#now, join back with incident ids
burn_cnty_withincid<-merge(burn_nonfed_cnty_inter,mtbs_withincid,by.x="Event_ID",by.y="mtbs_ids")
burn_cnty_trim<-burn_cnty_withincid[,c("incident_id","Event_ID","GEOID")]
burn_cnty_trim$geometry<-NULL

burn_cnty_uni<-unique(burn_cnty_trim)


#now, join back with incident ids
threat_st_withincid<-merge(threat_nonfed_st_inter,mtbs_withincid,by.x="Event_ID",by.y="mtbs_ids")
threat_st_trim<-threat_st_withincid[,c("incident_id","Event_ID","GEOID")]
threat_st_trim$geometry<-NULL

threat_st_uni<-unique(threat_st_trim)


#now, join back with incident ids
threat_cnty_withincid<-merge(threat_nonfed_cnty_inter,mtbs_withincid,by.x="Event_ID",by.y="mtbs_ids")
threat_cnty_trim<-threat_cnty_withincid[,c("incident_id","Event_ID","GEOID")]
threat_cnty_trim$geometry<-NULL

threat_cnty_uni<-unique(threat_cnty_trim)



#remove state/counties from threatened if they occur in burned
#if geoid exists for that incident id for burned, mark it to remove
threat_cnty_uni$torm_inburn <- ifelse(is.na(match(paste0(threat_cnty_uni$incident_id, threat_cnty_uni$GEOID), 
                                                  paste0(burn_cnty_uni$incident_id, burn_cnty_uni$GEOID))),FALSE, TRUE)

threat_st_uni$torm_inburn <- ifelse(is.na(match(paste0(threat_st_uni$incident_id, threat_st_uni$GEOID), 
                                                paste0(burn_st_uni$incident_id, burn_st_uni$GEOID))),FALSE, TRUE)


threat_cnt_jurs_tokeep<-threat_cnty_uni[threat_cnty_uni$torm_inburn==FALSE,]
threat_st_jurs_tokeep<-threat_st_uni[threat_st_uni$torm_inburn==FALSE,]

##now write out tables for counting
threat_cnty_cnt<-threat_cnt_jurs_tokeep %>% group_by(incident_id) %>% dplyr::summarize(count=n_distinct(GEOID))
threat_st_cnt<-threat_st_jurs_tokeep %>% group_by(incident_id) %>% dplyr::summarize(count=n_distinct(GEOID))

write.csv(threat_cnty_cnt,"data/county_eng_count.csv")
write.csv(threat_st_cnt,"data/state_eng_count.csv")

burn_cnty_cnt<-burn_cnty_uni %>% group_by(incident_id) %>% dplyr::summarize(count=n_distinct(GEOID))
burn_st_cnt<-burn_st_uni %>% group_by(incident_id) %>% dplyr::summarize(count=n_distinct(GEOID))

write.csv(burn_cnty_cnt,"data/county_burn_count.csv")
write.csv(burn_st_cnt,"data/state_burn_count.csv")
