#this script is calculating percent and total area threatened & burned for 
#federal, state, tribal, other, private
#because I have already intersected surface management w/ burned & threatened footprints
#going to use surface management designation of Jrsdc UA to designate federal, state, tribal, local, private


library(sf)
library(rgeos)
library(sp)
library(raster)
require(UScensus2010)
library(stringr)
library(tidyverse)

#read in data with mtbs_ids & incident_ids
mysamp<-read.csv("\\data\\incid_fires_year.csv")


burn_surfman<-st_read("data/surfman_intersect_burn.shp")
threat_surfman<-st_read("data/surfman_intersect_eng.shp")
burn_surfman$JrsdcUA[is.na(burn_surfman$JrsdcUA)]<-"CenPriv"
threat_surfman$JrsdcUA[is.na(threat_surfman$JrsdcUA)]<-"CenPriv"

#split out designations for tabulating area
fed<-c("BLM","DOD","USFS","USFWS","NPS","BOR","DOE","OthFed")
state<-c("State")
trib<-c("BIA","ANCSA","Tribal")
local<-c("City","County","OthLoc")
priv<-c("CenPriv")

colnames(burn_surfman)[1]<-"Evnt_ID"

#merge mtbs_ids & incident_ids
burn_surfman_incid<-merge(burn_surfman,mysamp,by.x="Evnt_ID",by.y="mtbs_ids",all=TRUE)
threat_surfman_incid<-merge(threat_surfman,mysamp,by.x="Evnt_ID",by.y="mtbs_ids",all=TRUE)#note column name change

#remove any areas where incident id is NA
bsman_incid_nona <- burn_surfman_incid[!is.na(row.names(burn_surfman_incid)),]
tsman_incid_nona <- threat_surfman_incid[!is.na(row.names(threat_surfman_incid)),]


#now, make new column in burned & threatened that designates area category assignment
bsman_incid_nona$area_cat<-NA
tsman_incid_nona$area_cat<-NA

#assigning areas to categories we want to tally
bsman_incid_nona$area_cat[bsman_incid_nona$JrsdcUA %in% fed]<-"fed"
bsman_incid_nona$area_cat[bsman_incid_nona$JrsdcUA %in% state]<-"state"
bsman_incid_nona$area_cat[bsman_incid_nona$JrsdcUA %in% trib]<-"trib"
bsman_incid_nona$area_cat[bsman_incid_nona$JrsdcUA %in% local]<-"loc"
bsman_incid_nona$area_cat[bsman_incid_nona$JrsdcUA %in% priv]<-"priv"

#assigning areas to categories we want to tally
tsman_incid_nona$area_cat[tsman_incid_nona$JrsdcUA %in% fed]<-"fed"
tsman_incid_nona$area_cat[tsman_incid_nona$JrsdcUA %in% state]<-"state"
tsman_incid_nona$area_cat[tsman_incid_nona$JrsdcUA %in% trib]<-"trib"
tsman_incid_nona$area_cat[tsman_incid_nona$JrsdcUA %in% local]<-"loc"
tsman_incid_nona$area_cat[tsman_incid_nona$JrsdcUA %in% priv]<-"priv"

#get the nonempty geometries, clean up anything that won't return area
nonemptygeoms_burn <- bsman_incid_nona[!st_is_empty(bsman_incid_nona),,drop=FALSE]
nonemptygeoms_burn$bort<-"burn"
nonemptygeoms_burn$brn_ntr<-NULL
nonemptygeoms_burn$indicator<-NULL 
nonemptygeoms_burn$indictr<-NULL
nonemptygeoms_burn$burn_inter<-NULL 
nonemptygeoms_threat <- tsman_incid_nona[!st_is_empty(tsman_incid_nona),,drop=FALSE]
nonemptygeoms_threat$bort<-"threat"
nonemptygeoms_threat$brn_ntr<-NULL
nonemptygeoms_threat$thrt_nt<-NULL
nonemptygeoms_threat$indictr<-NULL 



colnames(nonemptygeoms_threat)<-colnames(nonemptygeoms_burn)

#polygons that are not empty geoms 
nonemptygeoms<-rbind(nonemptygeoms_burn,nonemptygeoms_threat)


#remove any incident_ids that don't have nonempty geoms in both burned & threatened
commonids<-intersect(unique(nonemptygeoms_burn$incident_id),unique(nonemptygeoms_threat$incident_id))


#unique incident_ids burned
uni_incids<-commonids


#to get unique incident_ids for threatened, want to remove any incident_ids where zero jurisdictions were threatened

#read in final jur table & sort on 0 jur_threatened
#then, remove those incidents from the threat_surfman_incid layer
#saves computations time 
jur_counts<-read.csv("data/incid_withjur_counts.csv")


#calculate areas burned for unique incident_ids and valid geoms
burn_engag_tab<-calc_areas(uni_incids,nonemptygeoms)

#write out table that contains burned and threatened areas by land tenure
write.csv(burn_engag_tab,burn_threat_perc_area_tab_out)

