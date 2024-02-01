# Jurisdictional_Complexity

This repository contains scripts used to process wildfire and jurisdictional data for the publication "Mapping Wildfire Jurisdictional Complexity Reveals Opportunities for Regional Co-Management".
&nbsp;   
&nbsp;   
&nbsp;  

### Citation: 

**Jones, K., Vukomanovic, J.V., Nowell, B., McGovern, S.M. 2024. Mapping Wildfire Jurisdictional Complexity Reveals Opportunities for Regional Co-Management. _Global Environmental Change_
84, 102084. https://doi.org/10.1016/j.gloenvcha.2024.102804.**
&nbsp;  
&nbsp;   
&nbsp;   

This respository provides guidance for using the demonstrated jurisdictional complexity methods on new applications (using the same decision rules outlined in the manuscript). Data are not provided but data organization and data use / processing are detailed to the extent possible. &nbsp;  
&nbsp;  
Generally, the required data are:&nbsp;  
&nbsp;  
&nbsp;  
**1)** A single or multiple incidents or events for which jurisdictional complexity metrics are desired (i.e., wildfire incidents & wildfire boundaries)&nbsp;  
**2)** Jurisdictional data (spatial data delineating jurisdictions to identify)&nbsp;  
**3)** Decision rules to count and identify jurisdictions (outlined in the manuscript and implemented in the scripts)&nbsp;  
&nbsp;  
&nbsp;  
&nbsp;  

## Scripts contained in _workflow_scripts_ are ordered by number:

* _01_definesample.R_ : Reads in all-hazard data, finds wildfire incidents that have MTBS boundaries. 
* _02_getMTBSbounds.R_ : Using the MTBS boundaries identified in 01_definesample.R, MTBS boundaries are subset and engaged (5mi) buffers are generated.
* _03_proc_cenplace.R_ : Yearly census place data (functional statistical type A) are read in and interseced with the burned and engaged fire boundaries.
* _04_intersect_wfdss.R_ : Intersects the WFDSS/surface management dataset with burned and engaged fire boundaries.
* _05_count_wfdss_jurs.R_ : Uses outputs from _04_intersect_wfdss.R_ to count the number of jurisdictions for each unique level of government & agency.
* _06_intersect_stcnty.R_ : Uses outputs from _04_intersect_wfdss.R_ to find non-federal lands burned/engaged. Intersects those lands with states and counties.
* _07_intersect_bia_blm.R_ : Uses outputs from _04_intersect_wfdss.R_ to identify BLM and BIA lands. Intersects these lands with corrected BIA & BLM jurisdictional shapefiles.
* _08_merge_jur_data.R_ : Creates table with final jurisdictional counts for each incident. Uses outputs from _03_proc_cenplace.R_, _05_count_wfdss_jurs.R_, _06_intersect_stcnty.R_, _07_intersect_bia_blm.R_. 
* _09_calc_landarea.R_ : Defines function called in _10_finalize_area_calcs.R_ to compute land area burned and engaged by land tenure (federal, tribal, state, local, private).
* _10_finalize_area_calcs.R_ : Calculates land area burned and engaged and formats data for merging to create final geospatial layer.
* _11_write_outputs.R_ : Writes out final geopackage with unique fire incident IDs matched with their boundaries and jurisdictional attributes.
&nbsp;
&nbsp;  
&nbsp;  

## Important Notes on Data Availability:
&nbsp;  
## Wildfire Incidents and Boundaries
### All-Hazards (incident tabular data)
The wildfire incident data were downloaded, [here](https://figshare.com/articles/dataset/All-hazards_dataset_mined_from_the_US_National_Incident_Management_System_1999-2020/19858927/3) (St. Denis et al., 2023). Select the ics209plus-wildfire.zip. The ics209-plug-wf_incidents_1999to2020.csv is used in [*scripts/01_definesample.R*](https://github.com/kejones8/Jurisdictional_Complexity/blob/main/workflow_scripts/01_definesample.R).

### MTBS Fire Boundaries
Downloaded, [here](https://www.mtbs.gov/direct-download) in July 2022. The mtbs_perims_DD.shp is used in [*scripts/02_getMTBSbounds.R*](https://github.com/kejones8/Jurisdictional_Complexity/blob/main/workflow_scripts/02_getMTBSbounds.R).
&nbsp;  
&nbsp;  
&nbsp;  
## Jurisdictional Spatial Data
### WFDSS Data
New version of data downloadable [here](https://data-nifc.opendata.arcgis.com/datasets/nifc::jurisdictional-unit-public/about). Data were projected and geometries cleaned using [data_processing/clean_WFDSS](https://github.com/kejones8/Jurisdictional_Complexity/blob/main/data_acquisition_processing/clean_WFDSS.R). Downloaded version may differ from manuscript.

### BLM Data
File GDB downloaded [here](https://gbp-blm-egis.hub.arcgis.com/datasets/4ec898f8fb104ce4910932d02791563a/about). Unit districts were used. Downloaded version may differ from manuscript.

### BIA Data
Original data downloaded [here](https://biamaps.doi.gov/index.html). This website is now closed; the redirect is to [this page](https://biamaps.geoplatform.gov/BIA-Opendata/), but the same data are not available for download. See [this static map](https://www.bia.gov/bia/ojs/districts) for the districts used.

### State & County
States downloaded [here](https://www2.census.gov/geo/tiger/TIGER2020/STATE/). Counties downloaded [here](https://www2.census.gov/geo/tiger/TIGER2020/COUNTY/).

### Census Places
Downloaded using wget with the commands in [data_acquisition_processing/get_cp_yearly](https://github.com/kejones8/Jurisdictional_Complexity/blob/main/data_acquisition_processing/get_cp_yearly.txt). For every year and every state, the census places designated as Functional Statistical Type A were selected and yearly files were generated (yearly files called in [_scripts/proc_cenplace.R_](https://github.com/kejones8/Jurisdictional_Complexity/blob/main/workflow_scripts/03_proc_cenplace.R). As referenced in the script, these yearly data were placed in the /data/census_place/yearly directory.
