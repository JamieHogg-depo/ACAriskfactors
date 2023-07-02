## Load occupation data

ocu <- suppressMessages(list(read_csv("~/OneDrive - Queensland University of Technology/DataLab/Requested file load/2016/2016_SA2_Census_GeneralCommunityProfile/2016 Census GCP Statistical Area 2 for AUST/2016Census_G57A_AUS_SA2.csv"),
                      read_csv("~/OneDrive - Queensland University of Technology/DataLab/Requested file load/2016/2016_SA2_Census_GeneralCommunityProfile/2016 Census GCP Statistical Area 2 for AUST/2016Census_G57B_AUS_SA2.csv"))) %>% 
  reduce(inner_join, by = "SA2_MAINCODE_2016") %>% 
  dplyr::select(SA2_MAINCODE_2016,
                contains("P_Tot")) %>% 
  rename(SA2 = SA2_MAINCODE_2016) %>% 
  mutate_at(vars(-SA2, -P_Tot_Tot), ~ returnProps(.,P_Tot_Tot)) %>% 
  dplyr::select(-P_Tot_Tot) %>% 
  rename_with(~gsub("P_Tot_", "", .x)) %>% 
  mutate_at(vars(-SA2), ~ checkBounds(.))
