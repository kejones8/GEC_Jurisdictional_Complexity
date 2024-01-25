#this script reads in burned and threatened jurisdictional data from 04_intersect_wfdss.R
#it writes out a burned and engaged count .csv for each unique jurisdictional agency/actor type

library(tidyverse)
`%notin%` <- Negate(`%in%`)

#reads in both burned juridictions and threatened jurisdictions for cleaning
burn<-read.csv("data/list_burned_jurs.csv")
burn$burn_threat<-"burn"
threat<-read.csv("data/list_engaged_jurs.csv")
end<-ncol(threat)

# do some column clean up as needed 
threat_up<-threat[,-c(end-1,end)] #remove previous columns written out into csv
threat_up$burn_threat<-"threat"
colnames(threat_up)[3]<-c("Event_ID") #make sure spelling is correct
burn$LndwnrC<-NULL
burn$LndwnrK<-NULL

# once burn & threat are in same format, rbind rows
inc_juris<-rbind(burn,threat_up)


#following sections are broken up by jurisdictional agency and level of govt.

###FED
fed<-inc_juris[inc_juris$JrsdcUK=="Federal",]
unique(fed$JrsdcUA)
fed_nona<-fed[!is.na(fed$JrsdcUA),]

#othfed to be handled separately
othfed<-fed_nona[fed_nona$JrsdcUA=="OthFed",]
unique(othfed$JrsdcUN)
othfed_keep<-othfed[othfed$JrsdcUN=="Tennessee Valley Authority",]

#handle BLM & BIA separately, just handled othfed above
federal_tosort<-fed_nona[fed_nona$JrsdcUA %notin% c("BLM","BIA","OthFed"),] 

fed_keep<-rbind(federal_tosort,othfed_keep)

#do the clean up of federal burn/threatened
#so don't count threatened jursidictions that are captured in burned
fed_burn<-fed_keep[fed_keep$burn_threat=="burn",]
fed_threat<-fed_keep[fed_keep$burn_threat=="threat",]

#flag to remove if already in burn
fed_threat$torm_inburn<-ifelse(is.na(match(paste0(fed_threat$incident_id,fed_threat$JrsdcUI), paste0(fed_burn$incident_id, fed_burn$JrsdcUI))),FALSE, TRUE)
fed_threat_burnrm<-fed_threat[fed_threat$torm_inburn==FALSE,]
fed_threat_burnrm$torm_inburn<-NULL



###next 3 sections handle DOD, DOE, BOR, TVA the same
dod_count_burn<-fed_burn %>% filter(JrsdcUA=="DOD") %>% group_by(incident_id) %>% dplyr::summarize(dod_count=n_distinct(JrsdcUN))
dod_count_burn$dod_cnt_cln<-1
dod_count_burn$dod_count<-NULL

threat_dod<-fed_threat_burnrm[fed_threat_burnrm$JrsdcUA=="DOD",]

fed_threat_dod<-threat_dod[threat_dod$incident_id %notin% dod_count_burn$incident_id,]

dod_count_threat<-fed_threat_dod %>% group_by(incident_id) %>% dplyr::summarize(dod_count=n_distinct(JrsdcUN))
dod_count_threat$dod_cnt_thrt_cln<-1
dod_count_threat$dod_count<-NULL

write.csv(dod_count_burn,"data/dod_burn_count.csv")
write.csv(dod_count_threat,"data/dod_eng_count_out.csv")


#treat doe the same way as DOD
doe_count_burn<-fed_burn %>% filter(JrsdcUA=="DOE") %>% group_by(incident_id) %>% dplyr::summarize(doe_count=n_distinct(JrsdcUN))
doe_count_burn$doe_cnt_cln<-1
doe_count_burn$doe_count<-NULL

threat_doe<-fed_threat_burnrm[fed_threat_burnrm$JrsdcUA=="DOE",]

fed_threat_doe<-threat_doe[threat_doe$incident_id %notin% doe_count_burn$incident_id,]

doe_count_threat<-fed_threat_doe %>% group_by(incident_id) %>% dplyr::summarize(doe_count=n_distinct(JrsdcUN))
doe_count_threat$doe_cnt_thrt_cln<-1
doe_count_threat$doe_count<-NULL

write.csv(doe_count_burn,"data/doe_burn_count.csv")
write.csv(doe_count_threat,"data/doe_eng_count.csv")


#handle BOR the same as dod & doe
bor_count_burn<-fed_burn %>% filter(JrsdcUA=="BOR") %>% group_by(incident_id) %>% dplyr::summarize(bor_count=n_distinct(JrsdcUN))
bor_count_burn$bor_cnt_cln<-1
bor_count_burn$bor_count<-NULL

threat_bor<-fed_threat_burnrm[fed_threat_burnrm$JrsdcUA=="BOR",]

fed_threat_bor<-threat_bor[threat_bor$incident_id %notin% bor_count_burn$incident_id,]

bor_count_threat<-fed_threat_bor %>% group_by(incident_id) %>% dplyr::summarize(bor_count=n_distinct(JrsdcUN))
bor_count_threat$bor_cnt_thrt_cln<-1
bor_count_threat$bor_count<-NULL

write.csv(bor_count_burn,"data/bor_burn_count.csv")
write.csv(bor_count_threat,"data/bor_eng_count.csv")



#TVA - same as BOR, DOE, DOD
tva_count_burn<-fed_burn %>% filter(JrsdcUI=="TNTVA") %>% group_by(incident_id) %>% dplyr::summarize(tva_count=n_distinct(JrsdcUN))
tva_count_burn$tva_cnt_cln<-1
tva_count_burn$tva_count<-NULL

threat_tva<-fed_threat_burnrm[fed_threat_burnrm$JrsdcUI=="TNTVA",]

fed_threat_tva<-threat_tva[threat_tva$incident_id %notin% tva_count_burn$incident_id,]

tva_count_threat<-fed_threat_tva %>% group_by(incident_id) %>% dplyr::summarize(tva_count=n_distinct(JrsdcUN))
tva_count_threat$tva_cnt_thrt_cln<-1
tva_count_threat$tva_count<-NULL


write.csv(tva_count_burn,"data/tva_burn_count.csv")
write.csv(tva_count_threat,"data/tva_eng_count.csv")



#NPS
#want to count unique JrsdcUN codes
nps_count_burn<-fed_burn %>% filter(JrsdcUA=="NPS") %>% group_by(incident_id) %>% dplyr::summarize(nps_count=n_distinct(JrsdcUN))
nps_count_burn$nps_cnt_cln<-1

nps_count_threat<-fed_threat_burnrm %>% filter(JrsdcUA=="NPS") %>% group_by(incident_id) %>% dplyr::summarize(nps_count=n_distinct(JrsdcUN))
nps_count_threat$nps_cnt_thrt_cln<-1

write.csv(nps_count_burn,"data/nps_burn_count.csv")
write.csv(nps_count_threat,"data/nps_eng_count.csv")

#USFS
usfs<-inc_juris %>% filter(JrsdcUA=="USFS")
usfs_burn<-fed_burn %>% filter(JrsdcUA=="USFS")
usfs_threat<-fed_threat_burnrm%>% filter(JrsdcUA=="USFS")

usfs_count_burn<-usfs_burn %>% filter(JrsdcUA=="USFS") %>% group_by(incident_id) %>% dplyr::summarize(usfs_count=n_distinct(JrsdcUN))

usfs_count_threat<-usfs_threat %>% filter(JrsdcUA=="USFS") %>% group_by(incident_id) %>% dplyr::summarize(usfs_count=n_distinct(JrsdcUN))

write.csv(usfs_count_burn,"data/usfs_burn_count.csv")
write.csv(usfs_count_threat,"data/usfs_eng_count.csv")

#USFWS
usfws_count_burn<-fed_burn %>% filter(JrsdcUA=="USFWS") %>% group_by(incident_id) %>% dplyr::summarize(usfws_count=n_distinct(JrsdcUN))
usfws_count_burn$usfws_count<-NULL

threat_usfws<-fed_threat_burnrm[fed_threat_burnrm$JrsdcUA=="USFWS",]

fed_threat_usfws<-threat_usfws[threat_usfws$incident_id %notin% usfws_count_burn$incident_id,]

usfws_count_threat<-fed_threat_usfws %>% group_by(incident_id) %>% dplyr::summarize(usfws_count=n_distinct(JrsdcUN))
usfws_count_threat$usfws_count<-NULL

write.csv(usfws_count_burn,"data/usfws_burn_count.csv")
write.csv(usfws_count_threat,"data/usfws_eng_count.csv")




###OTHER tribal designations
other<-inc_juris[inc_juris$JrsdcUK=="Other",]
unique(other$JrsdcUA) 
othloc<-other[other$JrsdcUA=="OthLoc",] #local reserves, cant count them evenly - will be represented by county and census place
#keep tribal 
other_tribal_keep<-other[other$JrsdcUA=="Tribal" & !is.na(other$JrsdcUI) ,] #don't want othlocal, state, conty, city, NA

other_tribal_burn<-other_tribal_keep[other_tribal_keep$burn_threat=="burn",]

other_tribal_threat<-other_tribal_keep[other_tribal_keep$burn_threat=="threat",]

#make flag to remove if in burned
other_tribal_threat$torm_inburn<-ifelse(is.na(match(paste0(other_tribal_threat$incident_id,other_tribal_threat$JrsdcUI), paste0(other_tribal_burn$incident_id, other_tribal_burn$JrsdcUI))),FALSE, TRUE)

#
othtrib_threat_burnrm<-other_tribal_threat[other_tribal_threat$torm_inburn==FALSE,]
othtrib_threat_burnrm$torm_inburn<-NULL

othtrib_count_burn<-other_tribal_burn  %>% group_by(incident_id) %>% dplyr::summarize(trib_count=n_distinct(JrsdcUN))

othtrib_count_threat<-othtrib_threat_burnrm %>% group_by(incident_id) %>% dplyr::summarize(trib_count=n_distinct(JrsdcUN))

write.csv(othtrib_count_burn,"data/othtrib_burn_count.csv")
write.csv(othtrib_count_threat,"data/othtrib_eng_count.csv")



###ANCSA are classified as private, but included as tribal jurisdictions in the same counting method as DOD, DOE, BOR etc.
priv<-inc_juris[inc_juris$JrsdcUK=="Private",]
unique(priv$JrsdcUA)
ancsa<-priv[priv$JrsdcUA=="ANCSA",] #keep these, make them tribal categorization


ancsa_burn<-ancsa[ancsa$burn_threat=="burn",]

ancsa_threat<-ancsa[ancsa$burn_threat=="threat",]

#make flag for included or not already included in burned area
ancsa_threat$torm_inburn<-ifelse(is.na(match(paste0(ancsa_threat$incident_id,ancsa_threat$JrsdcUI),paste0(ancsa_burn$incident_id, ancsa_burn$JrsdcUI))),FALSE, TRUE)

ancsa_threat_burnrm<-ancsa_threat[ancsa_threat$torm_inburn==FALSE,]
ancsa_threat_burnrm$torm_inburn<-NULL

#ANCSA will get handled like dod/doe/majority of fed jurisdictions
#treat doe the same way as DOD
ancsa_count_burn<-ancsa_burn  %>% group_by(incident_id) %>% dplyr::summarize(ancsa_count=n_distinct(JrsdcUN))
ancsa_count_burn$ancsa_cnt_cln<-1
ancsa_count_burn$ancsa_count<-NULL

threat_ancsa<-fed_threat_burnrm[fed_threat_burnrm$JrsdcUA=="ANCSA",]

fed_threat_ancsa<-ancsa_threat_burnrm[ancsa_threat_burnrm$incident_id %notin% ancsa_count_burn$incident_id,]

ancsa_count_threat<-fed_threat_ancsa %>% group_by(incident_id) %>% dplyr::summarize(ancsa_count=n_distinct(JrsdcUN))
ancsa_count_threat$ancsa_cnt_thrt_cln<-1
ancsa_count_threat$ancsa_count<-NULL

write.csv(ancsa_count_burn,"data/ancsa_burn_count.csv")
write.csv(ancsa_count_threat,"data/ancsa_eng_count.csv")




###NA - these are census blocks, remove from jurisdictional count
na_cen<-inc_juris[is.na(inc_juris$JrsdcUK),]
unique(na_cen$JrsdcUA)
#no keepers here



