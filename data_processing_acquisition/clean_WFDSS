#this script does preliminary spatial processig for the WFDSS data to be used in subsequent scripts

library(sf)


#would normally need to specify layer reading directly from gdb, but only one layer in here
surf_man<-read_sf("raw_data/JurisdictionalAgencies/WFDSS_Jurisdictional_Agency.gdb")

#project
surf_man_alb<-st_transform(surf_man, 5070)

#want everything to be multipolygon, not multisurface, so cast
surf_man_multi <- st_cast(surf_man_alb, "MULTIPOLYGON")

#zero buffer takes awhile
surfman_buf<-st_buffer(surf_man_multi,0)
write_sf(surfman_buf,"data/surfman_zerobuf.shp")

#used the output of this script to manually create a new surfman layer that has francis marion & sumter NF assigned correctly


