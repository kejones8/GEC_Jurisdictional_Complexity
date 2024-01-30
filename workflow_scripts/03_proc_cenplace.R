#identify how many census place and burned and/or engaged on an incident
#count the number of times any single census place is engaged over the study period


library(sf) #working w/ shapefiles
library(foreach) #for parallelizing intersection
library(doParallel)
library(dplyr)
`%notin%` <- Negate(`%in%`)


#read in funcstat a census places
#WHATEVER FOLDER CP YEARLY DATA ARE IN
file_list_years<-list.files("data/census_place/yearly",full.names=TRUE,pattern="*.shp")

#separate out 2000 and 2007 because they will be used in multiple years
#do the absence of yearly data between 2000 and 2007
cp_2000<-read_sf(file_list_years[1]) #year2000
cp_2007<-read_sf(file_list_years[2]) #year2007


#get geoids for census places that were func state a in 2007
geoid2007<-cp_2007$GEOID

#only select 2020 cp if they were func state a in 2007
cp_2000_funca<-cp_2000[cp_2000$GEOID %in% geoid2007,]

#project and buffer for geom fixing
cenpl_2000_proj<-st_transform(cp_2000_funca,5070)
cenp_2000_buf<-st_buffer(cenpl_2000_proj,0)

###now, want to intersect cenpl data with mtbs footprints 
#read in mtbs footprints 
jca_mtbs<-read_sf("data/select_mtbs.shp")

jca_alb<-st_transform(jca_mtbs, 5070)
jca_buf_orig<-st_buffer(jca_alb,0)

#for burned
jca_buf_orig$date<-as.Date(jca_buf_orig$Ig_Date)
jca_buf_orig$year<-format(jca_buf_orig$date, format="%Y")#jca_buf$(date

mtbs_threat<-read_sf("data/eng_buf_donut.shp")

threat_alb<-st_transform(mtbs_threat, 5070)
threat_buf_orig<-st_buffer(threat_alb,0)

#for threatened
threat_buf_orig$date<-as.Date(threat_buf_orig$Ig_Date)
threat_buf_orig$year<-format(threat_buf_orig$date, format="%Y")#jca_buf$(date


#reading this in to link list of incidents with mtbs_ids
mtbs_withincid<-read.csv("data/incid_fires_year.csv")



year<-seq(1999,2020,1)

#don't want to include year 2000 in larger yearly list because it is a special case
file_list_years_no2000<-file_list_years[-1]#for teh else statement


###lines 66-180 are repeated for years 2007 and on (180-303)

#for every year
for (i in 1:length(year)){
  
  
  if (i<9){ #for years 2000-2006, need to run with 2000 census data
    
    #for original burned footprints, not engaged areas
    jca_buf<-st_make_valid(jca_buf_orig[jca_buf_orig$year==year[i],])
    
    #does it intersect a census place in this year
    jca_buf$indicator <- st_intersects(jca_buf, cenp_2000_buf) %>% lengths > 0
    
    
    #if so, put a true flag that it does intersect & subset by those 
    jca_buf_dointersect<-jca_buf[jca_buf$indicator==TRUE,]
    
    #get unique list of all mtbs footprints that do intersect a cp in this year
    mtbs_list<-unique(jca_buf_dointersect$Event_ID)
    
    #make list of cenplaces that intersect a wildfire boundary
    cenp_2000_buf$indicator <- st_intersects(cenp_2000_buf,jca_buf_dointersect) %>% lengths > 0
    
    #if so, put a true flag that it does intersect & subset by those 
    cenpl_buf2000_dointersect<-cenp_2000_buf[cenp_2000_buf$indicator==TRUE,]
    
    #now to deal with engaged areas
    threat_buf<-st_make_valid(threat_buf_orig[threat_buf_orig$year==year[i],])

    #does the engaged area for a wildfire intersect a cp
    threat_buf$indicator <- st_intersects(threat_buf, cenp_2000_buf) %>% lengths > 0
    
    
    #flag the engaged areas that do intersect and subset by those
    threat_buf_dointersect<-threat_buf[threat_buf$indicator==TRUE,]
    
    #make a list of mtbs events that the buffered engaged area intersects a cp
    threat_list<-unique(threat_buf_dointersect$Event_ID)
    
    #make list of cenplaces
    cenp_2000_buf$indicator <- st_intersects(cenp_2000_buf,threat_buf_dointersect) %>% lengths > 0
    
    
    #flag cp that intersect engaged area & subset by them
    cenpl_buf_threat_dointersect<-cenp_2000_buf[cenp_2000_buf$indicator==TRUE,]
    
    registerDoParallel(makeCluster(12))
    ptm <- proc.time()
    print(Sys.time())
    
    
    #cenpl_burn becomes the census places burned by each fire boundary
    cenpl_burn<-foreach(i=mtbs_list, .combine = rbind, .packages='sf')  %dopar%  {

      #for each fire boundary it
      fp <- jca_buf[jca_buf$Event_ID==i,]

      #intersect any census places that we already determined intersected at least one fire       #boundary
      inter<-sf::st_intersection(fp,cenpl_buf2000_dointersect)
      
    }
    
    
    print(Sys.time())
    stopImplicitCluster()
    proc.time() - ptm

    
  
    #now, need to intersect threatened & also remove cenpl that already intersected the         #burned footprints because we don't want to count twice
    registerDoParallel(makeCluster(12))
    ptm <- proc.time()
    print(Sys.time())
    
    
    #same as parallel operation directly above
    cenpl_threat<-foreach(i=threat_list, .combine = rbind, .packages='sf')  %dopar%  {

      fp_threat <- threat_buf[threat_buf$Event_ID==i,]

      inter_threat<-sf::st_intersection(fp_threat,cenpl_buf_threat_dointersect)

    }
    print(Sys.time())
    stopImplicitCluster()
    proc.time() - ptm
    

    
  #now have list of cp that burned and were engaged/threatened
    #designate wether or not counted as burned or engaged
    cenpl_burn<-cenpl_burn[,c("Event_ID","GEOID",'year')]
    cenpl_burn$b_t<-"burn"
    cenpl_threat<-cenpl_threat[,c("Event_ID","GEOID","year")]
    cenpl_threat$b_t<-"threat"
    
    #combine these datasets
    cenpl_counts<-rbind(cenpl_burn,cenpl_threat)
    
    #remove geoms for quicker working
    st_geometry(cenpl_burn)<-NULL
    st_geometry(cenpl_threat)<-NULL
    
    #combine the burned and engaged census place data and assign it back to the census place     #spatial boundary
    
    #write out each yearly file that countines which cp were burned and or engaged
    merge_cpburn<-merge(cenp_2000_buf,cenpl_burn,by.x="GEOID",by.y="GEOID")
    write_sf(merge_cpburn,paste0("data\\census_place\\cenpl_burn_byyr",year[i],".shp"))
    
    merge_cpthreat<-merge(cenp_2000_buf,cenpl_threat,by.x="GEOID",by.y="GEOID")
    write_sf(merge_cpthreat,paste0("data\\census_place\\cenpl_engag_byyr",year[i],".shp"))
    
    print(paste0("finished ",year[i]))
    
    
    
  } else{
    
    cenpl_yr<-read_sf(file_list_years_no2000[i-8])
    cenpl_proj<-st_transform(cenpl_yr,5070)
    cenpl_buf<-st_buffer(cenpl_proj,0)
    print("buffered cenplaces for this year")
    
    
    
    
    jca_buf<-st_make_valid(jca_buf_orig[jca_buf_orig$year==year[i],])
    
    jca_buf$indicator <- st_intersects(jca_buf, cenpl_buf) %>% lengths > 0
    
    
    #make surfman smaller by just grabbing those we know we'll want to work with 
    jca_buf_dointersect<-jca_buf[jca_buf$indicator==TRUE,]
    
    mtbs_list<-unique(jca_buf_dointersect$Event_ID)
    
    #make list of cenplaces
    cenpl_buf$indicator <- st_intersects(cenpl_buf,jca_buf_dointersect) %>% lengths > 0
    
    
    #make surfman smaller by just grabbing those we know we'll want to work with 
    cenpl_buf_dointersect<-cenpl_buf[cenpl_buf$indicator==TRUE,]
    
    
    #mtbs_list_2010<-unique(jca_buf_2010$Event_ID)
    
    
    
    threat_buf<-st_make_valid(threat_buf_orig[threat_buf_orig$year==year[i],])
    print('made valid threat polys')
    #threat_buf_2010<-st_make_valid(threat_buf[threat_buf$year>=2010,])
    
    threat_buf$indicator <- st_intersects(threat_buf, cenpl_buf) %>% lengths > 0
    
    
    #make surfman smaller by just grabbing those we know we'll want to work with 
    threat_buf_dointersect<-threat_buf[threat_buf$indicator==TRUE,]
    
    #make a list of mtbs events
    threat_list<-unique(threat_buf_dointersect$Event_ID)
    
    #make list of cenplaces
    cenpl_buf$indicator <- st_intersects(cenpl_buf,threat_buf_dointersect) %>% lengths > 0
    
    
    #make surfman smaller by just grabbing those we know we'll want to work with 
    cenpl_buf_threat_dointersect<-cenpl_buf[cenpl_buf$indicator==TRUE,]
    
    #threat_list_2010<-unique(threat_buf_2010$Event_ID)
    
    #now intersect jca_mtbs footprints with census places of func stat a
    registerDoParallel(makeCluster(12))
    ptm <- proc.time()
    print(Sys.time())
    
    
    cenpl_burn<-foreach(i=mtbs_list, .combine = rbind, .packages='sf')  %dopar%  {

      fp <- jca_buf[jca_buf$Event_ID==i,]

      inter<-sf::st_intersection(fp,cenpl_buf_dointersect)
      
    }
    print(Sys.time())
    stopImplicitCluster()
    proc.time() - ptm
    
    head(cenpl_burn)
    

    
    #now, need to intersect threatened & also remove cenpl that occur in burned 
    #now intersect jca_mtbs footprints with census places of func stat a
    registerDoParallel(makeCluster(12))
    ptm <- proc.time()
    print(Sys.time())
    
    
    cenpl_threat<-foreach(i=threat_list, .combine = rbind, .packages='sf')  %dopar%  {

      fp_threat <- threat_buf[threat_buf$Event_ID==i,]

      inter_threat<-sf::st_intersection(fp_threat,cenpl_buf_threat_dointersect)
      
    }
    prin
    
    t(Sys.time())
    stopImplicitCluster()
    proc.time() - ptm
    
    head(cenpl_threat)
    

    
    cenpl_burn<-cenpl_burn[,c("Event_ID","GEOID",'year')]
    cenpl_burn$b_t<-"burn"
    cenpl_threat<-cenpl_threat[,c("Event_ID","GEOID","year")]
    cenpl_threat$b_t<-"threat"
    
    cenpl_counts<-rbind(cenpl_burn,cenpl_threat)
    
    print("rbinded burn & threat for this year")
    
    st_geometry(cenpl_burn)<-NULL
    st_geometry(cenpl_threat)<-NULL
    
    merge_cpburn<-merge(cenpl_buf,cenpl_burn,by.x="GEOID",by.y="GEOID")
    write_sf(merge_cpburn,paste0("data\\census_place\\cenpl_burn_byyr",year[i],".shp"))
    

    merge_cpthreat<-merge(cenpl_buf,cenpl_threat,by.x="GEOID",by.y="GEOID")
    write_sf(merge_cpthreat,paste0("data\\census_place\\cenpl_engag_byyr",year[i],".shp"))
    


    print(paste0("finished ",year[i]))
    
  }
}


#reading in list of incident ids and footprints created in 01_definesample.R
mtbs_withincid<-read.csv(jca_samp_in)

#read in year 2020
cp_2020<-read_sf("raw_data\\census_place\\yearly\\funca_2020.shp")

#counting numbers of times wildfires engage census places
all_year_geoid_tabs<-data.frame()

#create df to count number of census places burned/ engaged in a given year (but first by incident)
count_cenpl_burn_all<-data.frame()
count_cenpl_threat_all<-data.frame()

#for each year, count the number of burned and engaged census places
for (i in 1:length(year)){
  
  #iterate through years and get file name
  file_to_burn<-paste0("data\\census_place\\cenpl_burn_byyr",year[i],".shp")
  file_to_threat<-paste0("data\\census_place\\cenpl_engag_byyr",year[i],".shp")
  
  #read in burned and engaged for each year
  cenpl_counts_burn<-read_sf(file_to_burn)
  cenpl_counts_threat<-read_sf(file_to_threat)
  
  #count burned cenpl
  cenpl_burn_incid<-merge(cenpl_counts_burn,mtbs_withincid,by.x="Event_ID",by.y="mtbs_ids")
  
  count_cenpl_burn<-cenpl_burn_incid %>% dplyr::group_by(incident_id)%>% dplyr::summarize(cnt_cenpl_burn=n_distinct(GEOID))
  st_geometry(count_cenpl_burn)<-NULL
  
  #count engaged cenpl
  cenpl_threat_incid<-merge(cenpl_counts_threat,mtbs_withincid,by.x="Event_ID",by.y="mtbs_ids")
  
  #remove from threatened censusplaces that intersect burns
  cenpl_threat_incid$torm_inburn <- ifelse(is.na(match(paste0(cenpl_threat_incid$incident_id,cenpl_threat_incid$GEOID),paste0(cenpl_burn_incid$incident_id,cenpl_burn_incid$GEOID))),FALSE, TRUE)
  
  #now, remove juris from threatened cenpl that already appear in burned
  #keep records where torm_inburn==FALSE (i.e., we dont want to remove them)
  threat_cenpl_rmburn<-cenpl_threat_incid[cenpl_threat_incid$torm_inburn==FALSE,]
  
  #get unique list of threatened cenpl
  threat_cenpl_unique<-unique(threat_cenpl_rmburn[,c("incident_id","GEOID")])
  
  #count the number of threatened census places for each incident
  count_cenpl_threat<-threat_cenpl_unique %>% group_by(incident_id) %>% dplyr::summarize(cnt_cenpl_threat=n_distinct(GEOID))
  
  #remove geoms
  st_geometry(count_cenpl_threat)<-NULL
  
  #create count list for each geoid
  cenpl_threat_incid$torm_inburn<-NULL
  
  #rbind these BECAUSE we want both burned and "threat" to reflect engaged
  countthis<-rbind(cenpl_burn_incid,cenpl_threat_incid)
  geoid_tab<-countthis %>% group_by(GEOID,incident_id) %>% dplyr::summarize(Count = n())
  
  #want to count the number of times each census place is engaged over the study period
  countsofgeoids<-as.data.frame(table(geoid_tab$GEOID))
  colnames(countsofgeoids)<-c("GEOID","num_times_eng") 
  countsofgeoids$year<-rep(year[i],nrow(countsofgeoids))
  all_year_geoid_tabs<-rbind(all_year_geoid_tabs,countsofgeoids)

  #identify all burned and threatened census places
  count_cenpl_burn_all<-rbind(count_cenpl_burn_all,count_cenpl_burn)
  count_cenpl_threat_all<-rbind(count_cenpl_threat_all,count_cenpl_threat)
  
}


####summarize all years geoids counts by geoid
range_of_this<-all_year_geoid_tabs %>% group_by(GEOID) %>% summarize(count_engaged = sum(num_times_eng))

#creates probability of engagement census place map
num_times_eng<-merge(cp_2020,range_of_this,by="GEOID",all.y=TRUE) 
write_sf(num_times_eng,"data\\census_place\\cp_bynumtimes_eng.shp")


#write out records of burned and engaged cnensus places 
write.csv(count_cenpl_burn_all,"data/count_cpburn_foreach_incident.csv")
write.csv(count_cenpl_threat_all,"data/count_cpeng_foreach_incident.csv")

