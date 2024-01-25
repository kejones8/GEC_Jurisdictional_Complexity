# Jurisdictional_Complexity

This repository contains scripts used to process wildfire and jurisdictional data for the publication "Mapping Wildfire Jurisdictional Complexity Reveals Opportunities for Regional Co-Management".

Citation: 

Jones, K., Vukomanovic, J.V., Nowell, B., McGovern, S.M. 2024. Mapping Wildfire Jurisdictional Complexity Reveals Opportunities for Regional Co-Management. Global Environmental Change
84, 102084. https://doi.org/10.1016/j.gloenvcha.2024.102804.

This respository is provided as guidance to use this framework on new jurisdictional complexity applications with the same decision rules as outline in the manuscript. Data are not provided but data organization and data use / processing are detailed to the extent possible. The data needs are: 

1) A single or multiple incidents or events for which jurisdictional complexity metrics are desired (i.e., wildfire incidents & wildfire boundaries)
2) Jurisdictional data (spatial data delineating jurisdictions to identify)
3) Counting decision rules (outlined in the manuscript and implemented in the scripts)

## Important Notes on Data Availability:

## Wildfire Incidents and Spatial Data
### All-Hazards (incident tabular data)
The wildfire incident data were downloaded, [here](https://figshare.com/articles/dataset/All-hazards_dataset_mined_from_the_US_National_Incident_Management_System_1999-2020/19858927/3) (St. Denis et al., 2023). Select the ics209plus-wildfire.zip. The ics209-plug-wf_incidents_1999to2020.csv is used in [*scripts/01_definesample.R*] (INSERTLINK).

#### MTBS Fire Boundaries
Downloaded, [here](https://www.mtbs.gov/direct-download) in July 2022. The mtbs_perims_DD.shp is used in [*scripts/02_getMTBSbounds.R*] (INSERTLINK).

## Jurisdictional Data
### WFDSS Data
New version of data downloadable [here](https://data-nifc.opendata.arcgis.com/datasets/nifc::jurisdictional-unit-public/about). Data were projected and geometries cleaned using [data_processing/clean_WFDSS] (INSERT LINK)

### Census Places
Downloaded using wget with the commands in [get_census_place_data/get_cp_yearly](https://github.com/kejones8/Jurisdictional_Complexity/tree/main/get_census_place_data). For every year and every state, the census places designated as Functional Statistical Type A were selected and yearly files were generated (yearly files called in [*scripts/proc_cenplace.R] (INSERT LINK)*. As referenced in the script, these yearly data were placed in the /data/census_place/yearly directory.
