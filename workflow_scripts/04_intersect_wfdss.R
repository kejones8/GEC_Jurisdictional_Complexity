#this script that intersects the surface management dataset (i.e, WFDSS)
#with both the burned & threatened footprints 

#it writes out: 

#1. shapefile of surfman polygons that intersect any burned areas
#2  shapefile of surfman polygons that intersect any threatened areas
#3. shapefile of surfman intersected with individual burned areas
#4. shapefile of surfman intersected with individual threatened areas
#5. a csv list of burned jurisdictions by incident id (effectively same as 3, just cleaned)
#6. a csv list of threatened jurisdictions by incident id (excluding those already burned)


#load libraries
library(sf)
library(dplyr)
library(foreach) #for parallelizing intersection
library(doParallel)


#read in cleaned surf man dataset
#has erroneous frances marion and sumter national forest corrected
surfman_buf<-st_read("data\\JCA\\surfman_fin.shp")
surfman_buf<-st_transform(surfman_buf,5070)

#reading in df of incident_ids and mtbs_ids
mtbs_withincid<-read.csv("data/incid_fires_year.csv")

#reading in the mtbs w/ 5 mile buffer (no cut outs for this first st_intersects)
buf_nodonut<-st_read("data/eng_buf_no_donut.shp")
buf_nodonut<-st_transform(buf_nodonut,5070)

#identifies which polygons in surface management intersect any one of the mtbs w/ buf
#adds T/F to indicator column
surfman_buf$indicator <- st_intersects(surfman_buf, buf_nodonut) %>% lengths > 0


#make surfman smaller by just grabbing those we know we'll want to work with 
surf_dointersect<-surfman_buf[surfman_buf$indicator==TRUE,]


#for every year we have burned and threatened footprints
#read in burned
burned<-st_read("data/select_mtbs.shp")
burned_proj<-st_make_valid(st_transform(burned,5070))

#do the st_intersects again and write out and indicator column
surf_dointersect$burn_inter<-st_intersects(surf_dointersect, burned_proj) %>% lengths > 0
surf_inters_burn<-surf_dointersect[surf_dointersect$burn_inter==TRUE,]

#write out just the surfman polygons that will get intersected with burned areas
write_sf(surf_inters_burn,"data\\surf_thatintersect_burn.shp")


#can write out just the surfman polygons that will get intersected with threatened areas
write_sf(surf_inters_threat,"data\\surf_thatintersect_eng.shp")

#then for each of those surf_man TRUE polygons, subset and do the actual intersection with those
burned_tointer<-unique(burned_proj$Event_ID)


cl<-makeCluster(12)
registerDoParallel(cl)
ptm <- proc.time()
print(Sys.time())


#for every burned area mtbs footprint, intersect with surface management 
#write out combined sf object with all intersections
burn_intersected<-foreach(i=burned_tointer, .combine = rbind, .packages=c('sf')) %dopar% {
  
  #fore very burn footprints, get the surface management data that intersect
  fp<-burned_proj[burned_proj$Event_ID==i,]
  mang_forburns<-st_intersection(fp,surf_inters_burn)
  
}

print(Sys.time())
stopImplicitCluster()
proc.time() - ptm

print("finished parallel intersect of burned area with surfman")

#merge the incident id with mtbsid with surface management jurisdictions
incid_fireid_jur<-merge(burn_intersected,mtbs_withincid,by.x="Event_ID","mtbs_ids")

write_sf(incid_fireid_jur,"data/surfman_intersect_burn.shp",overwrite=TRUE)

gc()


#subset down to relevant columns
burn_inter_trimmed<-incid_fireid_jur[,c("incident_id","Event_ID","start_year","JrsdcUK","JrsdcUA","JrsdcUN","JrsdcUI","LndwnrK","LndwnrC")]

#rename
colnames(burn_inter_trimmed)<-c("incident_id","Event_ID","START_YEAR","JrsdcUK","JrsdcUA","JrsdcUN","JrsdcUI","LndwnrK","LndwnrC")

#get rid of geometeries
burn_df<-st_drop_geometry(burn_inter_trimmed)

burn_df2<-burn_df[,1:length(colnames(burn_df))-1]
burned_jurs_unique<-unique(burn_df2)

#write out a list of burned, unique jurisdictions
write.csv(burned_jurs_unique,"data/list_burned_jurs.csv")


#read in threatened
threat<-st_read("data/eng_buf_no_donut.shp")
threat_proj<-st_make_valid(st_transform(threat,5070))

#do the st_intersects again and write out and indicator column
surf_dointersect$threat_inter<-st_intersects(surf_dointersect, threat_proj) %>% lengths > 0
surf_inters_threat<-surf_dointersect[surf_dointersect$threat_inter==TRUE,]

surf_inters_threat_buf<-st_buffer(surf_inters_threat,0)

#then do threatened intersection

#buffer to ensure valid geoms
threat_buf<-st_buffer(threat_proj,0)


registerDoParallel(makeCluster(12))
ptm <- proc.time()
print(Sys.time())

#same process as above for burned boundaries
threat_tointer<-unique(threat_buf$Event_ID)

threat_intersected<-foreach(i=threat_tointer, .combine = rbind, .packages=c('sf')) %dopar%  {
  
  fp_threat<-threat_buf[threat_buf$Event_ID==i,]
  mang_forthreat<-st_intersection(fp_threat,surf_inters_threat_buf)#5 miles = 8047 meters
  
}
print(Sys.time())
stopImplicitCluster()
proc.time() - ptm

####did the parallel process work? 12/14/2022

incid_fireid_jur_eng<-merge(threat_intersected,mtbs_withincid,by.x="Event_ID",by.y="mtbs_ids")

print("finished parallel intersect of threatened (engaged) area with surfman")


#write out all surfman polygons intersected wtih individual threatened areas
write_sf(incid_fireid_jur_eng,"data/surfman_intersect_eng.shp",overwrite=TRUE)


#same steps as above with burned data
threat_inter_trimmed<-incid_fireid_jur_eng[,c("incident_id","Event_ID","start_year","JrsdcUK","JrsdcUA","JrsdcUN","JrsdcUI","LndwnrK","LndwnrC")]
colnames(threat_inter_trimmed)<-c("incident_id","Event_ID","START_YEAR","JrsdcUK","JrsdcUA","JrsdcUN","JrsdcUI","LndwnrK","LndwnrC")


threat_df<-st_drop_geometry(threat_inter_trimmed)
threat_df2<-threat_df[,1:length(colnames(threat_df))-1]

threatened_jurs_unique<-unique(threat_df2)

##EXTRA step required for threatened data becauase we want mutually exclusive lists
#only include in threatened list jurisdictions that were not already burned


###remove any jursidctions that were burned
only_threat<-dplyr::anti_join(threatened_jurs_unique, burned_jurs_unique, by=c('incident_id', 'JrsdcUK','JrsdcUA','JrsdcUN'))



write.csv(only_threat,"data/list_engaged_jurs.csv")
