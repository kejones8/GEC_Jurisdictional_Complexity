library(sf)
library(rgeos)
library(sp)
library(raster)
require(UScensus2010)
library(stringr)
library(tidyverse)




calc_areas<-function (uni_incids,nonemptygeoms){
  
  colnames(nonemptygeoms)[1]<-"Event_ID"
  
  nonemptygeoms_sel<-nonemptygeoms[,c("Event_ID","incident_id","area_cat","bort")]
  put_burncalcs_here_rbind<-data.frame()
  put_engagcalcs_here_rbind<-data.frame()
  yes<-data.frame()
  
  
  count=0
  
  
  for (i in uni_incids){

    count=count+1 
    print(count)
    print(i)

    put_burncalcs_here<-data.frame()
    put_engagcalcs_here<-data.frame()
    

    #now incident id for the correct calculations
    test_grouped_areas<-nonemptygeoms_sel[nonemptygeoms_sel$incident_id==i,]
    print("got non-empty geoms for incident_id")

    
    test_byjuris_burn= test_grouped_areas %>% 
      st_set_precision(10000) %>% 
      filter(bort=="burn")%>%
      group_by(incident_id,Event_ID, area_cat) %>% 
      mutate(juris_group=paste0(Event_ID,"_",area_cat))
    print("created mtbs & juris groups")
    
    test_byjuris_threat= test_grouped_areas %>% 
      st_set_precision(10000) %>% 
      filter(bort=="threat")%>%
      group_by(incident_id,Event_ID, area_cat) %>% 
      mutate(juris_group=paste0(Event_ID,"_",area_cat))
    print("created mtbs & juris groups")
    

    

    test_bygoodjuris_burn<-test_byjuris_burn[!is.na(test_byjuris_burn$geometry),] 
    testing_burn<-as_Spatial(test_bygoodjuris_burn, cast = TRUE, IDs = juris_group)
    print("making burn spatial object")
    
    test_bygoodjuris_threat<-test_byjuris_threat[!is.na(test_byjuris_threat$geometry),] 
    testing_threat<-as_Spatial(test_bygoodjuris_threat, cast = TRUE, IDs = juris_group)
    print("making threat spatial object")
    
    
    hmm_burn<-gUnaryUnion(testing_burn, id = testing_burn@data$juris_group)
    print("unioning same juris_group")
    
    hmm_threat<-gUnaryUnion(testing_threat, id = testing_threat@data$juris_group)
    print("unioning same juris_group")
    
    
    # Extract polygon ID's
    pid_burn <- sapply(slot(hmm_burn, "polygons"), function(x) slot(x, "ID")) 
    print("Extracted burn polygon ids")
    
    pid_threat <- sapply(slot(hmm_threat, "polygons"), function(x) slot(x, "ID")) 
    print("Extracted threat polygon ids")

    # Create dataframe with correct rownames
    p.df_burn <- as.data.frame(cbind(1:length(hmm_burn), pid_burn),row.names=pid_burn) 
    colnames(p.df_burn)<-c("id","mtbs_jur_group")
    # Try coersion again and check class
    p_burn <- SpatialPolygonsDataFrame(hmm_burn, p.df_burn)
    print("created spatial df_burn")
    
    # Create dataframe with correct rownames
    p.df_threat <- as.data.frame(cbind(1:length(hmm_threat), pid_threat),row.names=pid_threat) 
    colnames(p.df_threat)<-c("id","mtbs_jur_group")
    # Try coersion again and check class
    p_threat <- SpatialPolygonsDataFrame(hmm_threat, p.df_threat)
    
    print("created spatial df_threat")
    
    hopeful_burn<-areaPoly(p_burn)
    print("got area of poly_burn")
    
    hopeful_threat<-areaPoly(p_threat)
    print("got area of poly_threat")

    
    p_burn@data$area<-hopeful_burn
    p_burn@data$incident_id<-rep(i,nrow(p_burn@data))
    print("vector of burn incident ids to match data rows")
    
    p_threat@data$area<-hopeful_threat
    p_threat@data$incident_id<-rep(i,nrow(p_threat@data))
    print("vector of threat incident ids to match data rows")
    
    
    
    p_burn@data$jur_group<-sapply(p_burn@data$mtbs_jur_group,function(x){str_split(x,"_")[[1]][2]})

    p_burn@data$Event_ID<-sapply(p_burn@data$mtbs_jur_group,function(x){str_split(x,"_")[[1]][1]})
    
    print("assigning burn juris group + mtbs_id")
    
    
    p_threat@data$jur_group<-sapply(p_threat@data$mtbs_jur_group,function(x){str_split(x,"_")[[1]][2]})
    p_threat@data$Event_ID<-sapply(p_threat@data$mtbs_jur_group,function(x){str_split(x,"_")[[1]][1]})
    
    print("assigning threat juris group + mtbs_id")
    

    
    p_burn@data$percent_burned_area<-sapply(p_burn@data$area,function(x){round((x/sum(p_burn@data$area))*100,1)})
    
    p_threat@data$percent_threat_area<-sapply(p_threat@data$area,function(x){round((x/sum(p_threat@data$area))*100,1)})
    
    print("calc & assign percent")
    
    #convert from sq. m. to acres 
    p_burn@data$areaburned_acres<-round(p_burn@data$area * 0.000247105) 
    p_threat@data$areathreatened_acres<-round(p_threat@data$area * 0.000247105) 

    p_burn@data$jur_group<-as.factor(p_burn@data$jur_group)
    p_threat@data$jur_group<-as.factor(p_threat@data$jur_group)
    
    put_burncalcs_here<-p_burn@data
    put_engagcalcs_here<-p_threat@data
    
    #switch from threat to engage
    put_burncalcs_here_rbind<-p_burn@data
    put_engagcalcs_here_rbind<-p_threat@data

    
    put_burncalcs_here_rbind<-rbind(put_burncalcs_here_rbind,p_burn@data)
    put_engagcalcs_here_rbind<-rbind(put_engagcalcs_here_rbind,p_threat@data)
    print("rbind new data, on to the next incident")
    
    

    
    
    wut_regburn_perc<-put_burncalcs_here %>% group_by(incident_id,jur_group,.drop=FALSE) %>% dplyr::summarize(jur_group_burnperc=sum(percent_burned_area))
    wut_regburn_acre<-put_burncalcs_here %>% group_by(incident_id,jur_group,.drop=FALSE) %>% dplyr::summarize(jur_group_burnacres=sum(areaburned_acres))
    
    wut_regengag_perc<-put_engagcalcs_here %>% group_by(incident_id,jur_group,.drop=FALSE) %>% dplyr::summarize(jur_group_threatperc=sum(percent_threat_area))
    wut_regengag_acre<-put_engagcalcs_here %>% group_by(incident_id,jur_group,.drop=FALSE) %>% dplyr::summarize(jur_group_threatacres=sum(areathreatened_acres))
    
    
    
    wide_regburn_perc<-as.data.frame(tidyr::pivot_wider(wut_regburn_perc, names_from = jur_group, values_from = jur_group_burnperc))
    wide_regburn_acre<-as.data.frame(tidyr::pivot_wider(wut_regburn_acre, names_from = jur_group, values_from = jur_group_burnacres))
    

    
    wide_regengag_perc<-as.data.frame(tidyr::pivot_wider(wut_regengag_perc, names_from = jur_group, values_from = jur_group_threatperc))
    wide_regengag_acre<-as.data.frame(tidyr::pivot_wider(wut_regengag_acre, names_from = jur_group, values_from = jur_group_threatacres))
    

    
    print ("made wide format tables")
    
    if (is.null(wide_regburn_perc$fed)) {
      wide_regburn_perc$fed<-0
      wide_regburn_acre$fed<-0
    } else {
      print("burn fed col there")
    }
    
    if (is.null(wide_regengag_perc$fed)) {
      wide_regengag_perc$fed<-0
      wide_regengag_acre$fed<-0
    } else {
      print("threat fed col there")
    }
    
    
    if (is.null(wide_regburn_perc$trib)) {
      wide_regburn_perc$trib<-0
      wide_regburn_acre$trib<-0
      
    } else {
      print("burn trib col there")
    }
    
    
    if (is.null(wide_regengag_perc$trib)) {
      wide_regengag_perc$trib<-0
      wide_regengag_acre$trib<-0
      
    } else {
      print("threat trib col there")
    }
    
    
    if (is.null(wide_regburn_perc$state)) {
      wide_regburn_perc$state<-0
      wide_regburn_acre$state<-0
      
    } else {
      print("burn state col there")
    }
    
    if (is.null(wide_regengag_perc$state)) {
      wide_regengag_perc$state<-0
      wide_regengag_acre$state<-0
      
    } else {
      print("threat state col there")
    }
    
    if (is.null(wide_regburn_perc$loc)) {
      wide_regburn_perc$loc<-0
      wide_regburn_acre$loc<-0
      
    } else {
      print("burn loc col there")
    }
    
    if (is.null(wide_regengag_perc$loc)) {
      wide_regengag_perc$loc<-0
      wide_regengag_acre$loc<-0
      
    } else {
      print("threat loc col there")
    }
    
    if (is.null(wide_regburn_perc$priv)) {
      wide_regburn_perc$priv<-0
      wide_regburn_acre$priv<-0
      
    } else {
      print("priv col there")
    }
    
    if (is.null(wide_regengag_perc$priv)) {
      wide_regengag_perc$priv<-0
      wide_regengag_acre$priv<-0
      
    } else {
      print("priv col there")
    }
    
    print("added cols as needed")
    
    wide_regburn_perc<-wide_regburn_perc[,c("incident_id","fed","loc","priv","state","trib")]
    colnames(wide_regburn_perc)<-c("incident_id","fed_percburn","loc_percburn","priv_percburn","state_percburn","trib_percburn")
    
    wide_regburn_acre<-wide_regburn_acre[,c("incident_id","fed","loc","priv","state","trib")]
    colnames(wide_regburn_acre)<-c("incident_id","fed_acre_burn","loc_acre_burn","priv_acre_burn","state_acre_burn","trib_acre_burn")
    wide_regburn_acre$total_ac_burn<-sum(wide_regburn_acre$fed,wide_regburn_acre$loc,wide_regburn_acre$priv,wide_regburn_acre$state,wide_regburn_acre$trib)
    
    

    wide_regengag_acre<-wide_regengag_acre[,c("incident_id","fed","loc","priv","state","trib")]
    colnames(wide_regengag_acre)<-c("incident_id","fed_threat","loc_threat","priv_threat","state_threat","trib_threat")
    wide_regengag_acre$total_ac_threat<-sum(wide_regengag_acre$fed,wide_regengag_acre$loc,wide_regengag_acre$priv,wide_regengag_acre$state,wide_regengag_acre$trib)
    
    
    #round all acreage appropriately
    wide_acre<-merge(wide_regburn_acre,wide_regengag_acre,by="incident_id")
    wide_acre$total_ac_engag<-sum(wide_acre$total_ac_burn,wide_acre$total_ac_threat)
    wide_acre$fed_acre_engag<-sum(wide_acre$fed_acre_burn,wide_acre$fed_threat)
    wide_acre$fed_percengag<-round(wide_acre$fed_acre_engag/wide_acre$total_ac_engag,2)
    wide_acre$trib_acre_engag<-sum(wide_acre$trib_acre_burn,wide_acre$trib_threat)
    wide_acre$trib_percengag<-round(wide_acre$trib_acre_engag/wide_acre$total_ac_engag,2)
    wide_acre$state_acre_engag<-sum(wide_acre$state_acre_burn,wide_acre$state_threat)
    wide_acre$state_percengag<-round(wide_acre$state_acre_engag/wide_acre$total_ac_engag,2)
    wide_acre$loc_acre_engag<-sum(wide_acre$loc_acre_burn,wide_acre$loc_threat)
    wide_acre$loc_percengag<-round(wide_acre$loc_acre_engag/wide_acre$total_ac_engag,2)
    wide_acre$priv_acre_engag<-sum(wide_acre$priv_acre_burn,wide_acre$priv_threat)
    wide_acre$priv_percengag<-round(wide_acre$priv_acre_engag/wide_acre$total_ac_engag,2)
    
    
    #remove unneeded columns
    wide_acre$fed_threat<-NULL
    wide_acre$loc_threat<-NULL
    wide_acre$priv_threat<-NULL
    wide_acre$state_threat<-NULL
    wide_acre$trib_threat<-NULL
    wide_acre$total_ac_threat<-NULL
    
    wide<-merge(wide_acre,wide_regburn_perc,by="incident_id")
    
    
    print ("renamed cols")
    
    yes<-rbind(yes,wide)

    
    print("successfully added new records, now onto next incident")

  }
  
  return(yes)

}
