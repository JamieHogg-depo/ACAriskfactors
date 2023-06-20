## -----------------------------------------------------------------------------
## TABLES ## -------------------------------------------------------------------
## -----------------------------------------------------------------------------

## MAIN Table 1: National and state direct estimates ## ------------------------

# Write function to derive columns for direct estimate table
column_of_direct <- function(i, generic = T){
  temp_list <- raw_est[[i]]
  rf <- names(raw_est)[i]
  
  temp_data <- bind_rows(data.frame(state_name = "Australia", 
             point = 100 * temp_list$national[1],
             lower = 100 * (temp_list$national[1] - 1.96 * sqrt(temp_list$national[2])),
             upper = 100 * (temp_list$national[1] + 1.96 * sqrt(temp_list$national[2]))),
  left_join(temp_list$state, state_name_concor, by = "ps_state") %>% 
    mutate(point = 100 * HT, 
           lower = 100 * (HT - 1.96 * HT_SE),
           upper = 100 * (HT + 1.96 * HT_SE)) %>% 
    dplyr::select(state_name, point, lower, upper)) %>% 
    make_numeric_decimal(digits = 1) %>% 
    mutate(out = paste0(point, " (", lower, ", ", upper, ")"))
  
  if(generic){return(temp_data$out)}else{return(temp_data$state_name)}
}

# Return all colunns and make large table
ll <- lapply(1:8, column_of_direct)
temp_df <- bind_cols(ll) %>% 
  setNames(names(raw_est)) %>% 
  mutate(State = column_of_direct(1, generic = FALSE)) %>% 
  relocate(State, smoking, alcohol, diet, obesity, overweight, waist_circum, activityleis, activityleiswkpl) %>% 
  rename(`Leisure physical activity` = activityleis,
         `All physical activity` = activityleiswkpl,
         `Risky waist circumference` = waist_circum,
         `Overweight` = overweight,
         `Obesity` = obesity,
         `Diet` = diet,
         `Alcohol` = alcohol,
         `Current smoking` = smoking)

# split into two datasets of 4 and save
temp_df %>% dplyr::select(1:5) %>% write.csv("out/tables/national_state_direct1.csv")
temp_df %>% dplyr::select(1, 6:9) %>% write.csv("out/tables/national_state_direct2.csv")
rm(temp_df, column_of_direct)

## SUPP Table 6-7: Model Building ## -------------------------------------------

S1_side <- c("Intercept only", "Fixed effects (FE)",
             "NORF random effects (RE)",
             "DH RE", "SA2 RE", "Residual error (sd = 1)",
             "Residual error (sd = 2)")

# Function
rows_of_mbs1 <- function(i){
  model_building[[i]]$s1 %>% 
    make_numeric_decimal() %>% 
    mutate(specs = S1_side,
           rf = names(model_building)[i]) %>% 
    relocate(rf, specs)
}

# Full table
temp_df <- bind_rows(lapply(1:4, rows_of_mbs1))

# split into two datasets of 4 and save
temp_df %>% filter(rf %in% c("smoking", "alcohol", "diet", "obesity")) %>% write.csv("out/tables/mbs1_1.csv")
temp_df %>% filter(rf %in% c("overweight", "waist_circum", "activityleis", "activityleiswkpl")) %>% write.csv("out/tables/mbs1_2.csv")
rm(temp_df, rows_of_mbs1, S1_side)

## SUPP Table 8-9: Model Building ## -------------------------------------------

S2_side <- c("Intercept only", "FE (non-varying)",
             "FE (varying)",
             "External latent field", "SA2 RE", "SA3 RE",
             "Benchmarking")

# Function
rows_of_mbs2 <- function(i){
  model_building[[i]]$s2 %>% 
    dplyr::select(contains(c("sa4_", "msb_"))) %>% 
    mutate(sa4_MARB = sa4_MARB * 100,
           sa4_MRRMSE = sa4_MRRMSE * 100,
           msb_MARB = msb_MARB * 100,
           msb_MRRMSE = msb_MRRMSE * 100) %>% 
    make_numeric_decimal() %>% 
    mutate(specs = S2_side,
           rf = names(model_building)[i]) %>% 
    relocate(rf, specs)
}

tempSetNames <- function(.data){
  .data %>% 
    setNames(c("", "", "MARB", "MRRMSE", "MIOP", "MARB", "MRRMSE", "MIOP"))
}

# Full table
temp_df <- bind_rows(lapply(1:4, rows_of_mbs2))

# split into two datasets of 4 and save
temp_df %>% filter(rf %in% c("smoking", "alcohol", "diet", "obesity")) %>% tempSetNames() %>%  write.csv("out/tables/mbs2_1.csv")
temp_df %>% filter(rf %in% c("overweight", "waist_circum", "activityleis", "activityleiswkpl")) %>% tempSetNames() %>%  write.csv("out/tables/mbs2_2.csv")
rm(temp_df, rows_of_mbs2, S2_side, tempSetNames)

## END SCRIPT ## ---------------------------------------------------------------
