#this script pulls in the downloaded mtbs data (needs to be downloaded manually) & matched to the ids in our sample (as defined in 01_definesample.R)

#necessary pacckages and functions
library(sf)
library(rmapshaper)
library(foreach) #for parallelizing intersection
library(doParallel)
`%notin%` <- Negate(`%in%`)

#read in output from 01_definesample.R 
#a csv of incident_ids &  associated mtbs footprints
jca_samp<-read.csv("data/incid_fires_year.csv") 

#read in mtbs data
all_mtbs<-read_sf("data/mtbs_perimeter_data/mtbs_perims_DD.shp")

#column as defined August 2022
#if different names, need the event id (unique MTBS identifier) and date/year at a minimum
colstocheck<-(all_mtbs[,c("Event_ID","Incid_Name","Ig_Date")])
colstocheck$geometry<-NULL


#get a list of my mtbs ids
mtbs_toget<-jca_samp$mtbs_ids

#extract mtbs footprints that match our sample
select_mtbs<-all_mtbs[all_mtbs$Event_ID %in% mtbs_toget,]
st_crs(select_mtbs) <- 4326 #assign crs

#write out the selected mtbs footprints
write_sf(select_mtbs,"data/select_mtbs.shp")



#in order to buffer for threatened, need to project
sel_mtbs_proj<-st_transform(select_mtbs, 5070)

#function to buffer by 5 miles, then erase inside
outerBuffer<-function(x, dist){
  buff<-st_buffer(x, dist - 1, dissolve = T) #5 miles = 8047 meters
  e<-rmapshaper::ms_erase(buff,x)
  return(e)
}

#parallel the buffer to speed things up
registerDoParallel(makeCluster(12))
ptm <- proc.time()
print(Sys.time())

#get unique footprints
mtbs_tobuf<-unique(sel_mtbs_proj$Event_ID)

#for each footprint
threat_work<-foreach(i=mtbs_tobuf, .combine = rbind, .packages=c('sf','rmapshaper')) %dopar%  {
  #get single footprint
  to_buf<-sel_mtbs_proj[sel_mtbs_proj$Event_ID==i,]
  #add buffer
  threat<-outerBuffer(to_buf,8048)#5 miles ~ 8048 meters
  
}
print(Sys.time())
stopImplicitCluster()
proc.time() - ptm

#write out the engaged boundaries with hole for burned area
write_sf(threat_work,"data/eng_buf_donut.shp",overwrite=TRUE)

#now make a filled in buffer for using in future intersections
buffer_nodonuts_forinter<-st_buffer(sel_mtbs_proj,8048) #5 miles ~ 8048 meters

#write out the engaged boundaries with NO hole for burned area
write_sf(buffer_nodonuts_forinter,"data/eng_buf_no_donut.shp")

