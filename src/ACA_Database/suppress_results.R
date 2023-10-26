
# get suppressed SA2s
s9_to_s5 <- function(x){paste0(str_sub(x, 1, 1), str_sub(x, start = -4))}
flt <- filter(summsa2all, rr_CV_b > 50 | N_persons < 100)
SA2_to_suppress <- split(flt$SA2, flt$model)
SA2_to_suppress <- lapply(SA2_to_suppress, s9_to_s5)

# Load original data
RiskFactor_Aus_for_ViseR <- read_csv("src/ACA_Database/RiskFactor estimates for ViseR.csv")
vise_sp <- split(RiskFactor_Aus_for_ViseR, RiskFactor_Aus_for_ViseR$sub_indicator_string)
names(vise_sp) <- c("smoking","diet", "activityleiswkpl","activityleis", "obese", "overweight","alcohol", "waist_circum")

# Loop through
ll <- list()
for(i in 1:8){
  ll[[i]] <- vise_sp[[names(vise_sp)[i]]] %>% 
    mutate(across(18:34, ~ ifelse(SA2_code %in% SA2_to_suppress[[names(vise_sp)[i]]], NA, .)))
}
write.csv(bind_rows(ll), file = "src/ACA_Database/RiskFactor estimates for ViseR_withsuppression.csv", row.names = FALSE) 
