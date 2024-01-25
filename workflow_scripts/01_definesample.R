library(stringr)
library(tidyverse)
library(plyr)
library(reshape2)

#From St. Denis' data, determining our sample of incidents 
#that had associated MTBS footprints
incidents<-read.csv("data\\ics209-plus-wf_incidents_1999to2020.csv")
nrow(incidents)


#choose all rows where there were MTBS footprints
inc_withmtbs <- incidents[incidents$MTBS_FIRE_NUM>0,]
nrow(inc_withmtbs)

#only want wildfires
wfinc_mtbs<-inc_withmtbs[inc_withmtbs$INCTYP_ABBREVIATION %in% c("WF","WFU","CX"),]
length(unique(wfinc_mtbs$INCIDENT_ID))

#check yearly range of data
range(wfinc_mtbs$START_YEAR) 

#check total acres burned from wildfires that have MTBS footprints
acres_peryear<- wfinc_mtbs %>% dplyr::group_by(START_YEAR) %>% dplyr::summarize(final_acres_total=sum(FINAL_ACRES,na.rm=TRUE))


#barplot of total annual ancres burned based on acreages reported in incident data
ggplot(acres_peryear, aes(x = START_YEAR , y= final_acres_total)) +
  geom_bar( stat = "identity")+ ggtitle("Total Acres Burned per Year")+
  xlab("year")+ ylab("Acres Burned")+ scale_y_continuous(labels = scales::comma)


#splitting up the different MTBS fire names and assigning them to incident ids
#makes a list of fire ids named by the incident they correspond to
tester<-(str_split(wfinc_mtbs$MTBS_FIRE_LIST, c("'|-")))
names(tester)<-wfinc_mtbs$INCIDENT_ID

#cleaning function for grabbing id names
get_just_id<-function(x){
  str_extract(x,"[A-Z]{2}[0-9]{10,}")
}

#pull the ids
grab_ids<-lapply(tester,get_just_id)
#then ditch NAs
clean_ids<-lapply(grab_ids,function(x){x[!is.na(x)]})

#onlyl get fire ids that exist
nomtbs_rm <- clean_ids[lengths(clean_ids)!=0]

#make a dataframe that contains only incident ids and fire ids
mtbs<-as.vector(nomtbs_rm)
df <- as.data.frame(matrix(0, ncol = 2, nrow = length(nomtbs_rm)))
colnames(df)[1] <- "incident_id"
colnames(df)[2] <- "mtbs_ids"
df$incident_id<-names(nomtbs_rm)
df$mtbs_ids<-mtbs

#get the year each incident started from earlier table
incid_years<-wfinc_mtbs[wfinc_mtbs$INCIDENT_ID %in% df$incident_id,c("INCIDENT_ID","START_YEAR")]

#make sure each mtbs id is reported in a separate column for workability
hmm<-df %>% 
  unnest(mtbs_ids) %>% 
  group_by(incident_id) %>% 
  dplyr::mutate(key = row_number()) %>% 
  spread(key, mtbs_ids)

#melt/reshape df to get list of mtbs ids
workable<-data.frame(hmm[1], F=unlist(hmm[-1]))
incids_mtbs_nonas<-workable[!is.na(workable$F),]
colnames(incids_mtbs_nonas)[2]<-"mtbs_ids"

#now every unique incident id / fire id pair is it's own row, add the incident years
addyears<-merge(incids_mtbs_nonas,incid_years,by.x="incident_id",by.y="INCIDENT_ID")

write.csv(addyears,"data/incid_fires_year.csv")


