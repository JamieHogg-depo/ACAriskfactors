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

## MAIN Table 6-9: Model Building ## -------------------------------------------

## END SCRIPT ## ---------------------------------------------------------------
