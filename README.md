## Jurisdictional_Complexity

This repository contains scripts used to process wildfire and jurisdictional data for the publication "Mapping Wildfire Jurisdictional Complexity Reveals Opportunities for Regional Co-Management".

Citation: 

Jones, K., Vukomanovic, J.V., Nowell, B., McGovern, S.M. 2024. Mapping Wildfire Jurisdictional Complexity Reveals Opportunities for Regional Co-Management. Global Environmental Change
84, 102084. https://doi.org/10.1016/j.gloenvcha.2024.102804.

## Important Notes on Data Availability:

# All-Hazards (wildfire incident tabular data)
The wildfire incident data were downloaded, [here](https://figshare.com/articles/dataset/All-hazards_dataset_mined_from_the_US_National_Incident_Management_System_1999-2020/19858927/3) (St. Denis et al., 2023). Select the ics209plus-wildfire.zip. The ics209-plug-wf_incidents_1999to2020.csv is used in [*scripts/01_definesample.R*] (INSERTLINK).

# MTBS Fire Boundaries
Downloaded, [here](https://www.mtbs.gov/direct-download) in July 2022. The mtbs_perims_DD.shp is used in [*scripts/02_getMTBSbounds.R*] (INSERTLINK).

# Census Places
Downloaded using wget with the commands in [get_census_place_data/get_cp_yearly](https://github.com/kejones8/Jurisdictional_Complexity/tree/main/get_census_place_data). For every year and every state, the census places designated as Functional Statistical Type A were selected and yearly files were generated (yearly files called in [*scripts/proc_cenplace.R] (INSERT LINK)*. As referenced in the script, these yearly data were placed in the /data/census_place/yearly directory.
