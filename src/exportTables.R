## -----------------------------------------------------------------------------
## TABLES ## -------------------------------------------------------------------
## -----------------------------------------------------------------------------

source("src/ms.R")

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

## MAIN: Table 3: Evidence classification ## -----------------------------------

summsa2all %>% 
  mutate(ec = ifelse(mu_EP > 0.8, "H", 
                     ifelse(mu_EP < 0.2, "L", NA)),
         out = factor(ifelse(is.na(LISA_mu) & !is.na(ec), 
                             ec, as.character(LISA_mu)),
                      levels = c("HH", "H", "L", "LL")),
         model = getRFFullNames(model)) %>% 
  mutate(ww =  2221*8 * N_persons/sum(N_persons)) %>% 
  group_by(model) %>% 
  summarise(HH = sum(ww*(out == "HH"), na.rm= T),
            H = sum(ww*(out == "H"), na.rm= T),
            L = sum(ww*(out == "L"), na.rm= T),
            LL = sum(ww*(out == "LL"), na.rm= T),
            .groups = "drop") %>% 
  mutate(tot = HH + H + L + LL) %>% 
  relocate(model, tot) %>% 
  make_numeric_decimal(digits = 0) %>% 
  setNames(c("", "", names(.)[-c(1:2)])) %>% 
  write.csv("out/tables/ec.csv")

## MAIN: Table 3: LISA_ra ## ---------------------------------------------------

# setup list
ll <- list()

# for loop
for(k in 1:8){

  rf <- names(raw_est)[k]
  message("Started ", k, ": ", rf)
  
  # load data
  modelled_est <- readRDS(file = paste0("data/summary_files/", rf, "_b1.rds"))
  
  # get totals
  temp <- modelled_est$summ$sa2 %>% 
    group_by(LISA) %>% 
    tally() %>% ungroup() %>% 
    filter(!is.na(LISA)) %>% 
    filter(LISA %in% c("HH", "LL")) %>% 
    mutate(out = as.character(n)) %>% 
    mutate(ra_sa2 = "Total") %>% 
    dplyr::select(ra_sa2, LISA, out)
  
  # rest of table
  ll[[k]] <- modelled_est$summ$sa2 %>% 
    group_by(ra_sa2, LISA) %>% 
    tally() %>% ungroup() %>% 
    filter(!is.na(LISA)) %>% 
    filter(LISA %in% c("HH", "LL")) %>% 
    group_by(LISA) %>% mutate(p = n/sum(n)) %>% ungroup() %>% 
    mutate(n = as.character(n)) %>% 
    make_numeric_decimal(digits = 2) %>% 
    mutate(out = paste0(n, " \textcolor{gray}{(", p, ")}")) %>% 
    dplyr::select(-c(n,p)) %>% 
    bind_rows(.,temp) %>% 
    pivot_wider(names_from = ra_sa2, values_from = out,
                values_fill = "") %>% 
    relocate(c(1,3,2)) %>% 
    mutate(rf = rf) %>% 
    relocate(rf, LISA, Total)

}

# combine
bind_rows(ll) %>% 
  setNames(c("", "", "", names(.)[-c(1:3)])) %>% 
  write.csv("out/tables/LISA_ra.csv")

# cleanup
rm(ll, rf, modelled_est, k, temp)

## MAIN: Table 3: LISA_irsd ## -------------------------------------------------

# setup list
ll <- list()

# for loop
for(k in 1:8){
  
  rf <- names(raw_est)[k]
  message("Started ", k, ": ", rf)
  
  # load data
  modelled_est <- readRDS(file = paste0("data/summary_files/", rf, "_b1.rds"))
  
  # get totals
  temp <- modelled_est$summ$sa2 %>% 
    group_by(LISA) %>% 
    tally() %>% ungroup() %>% 
    filter(!is.na(LISA)) %>% 
    filter(LISA %in% c("HH", "LL")) %>% 
    mutate(out = as.character(n)) %>% 
    mutate(irsd_5c = "Total") %>% 
    dplyr::select(irsd_5c, LISA, out)
  
  # rest of table
  ll[[k]] <- modelled_est$summ$sa2 %>% 
    mutate(irsd_5c = irsd_5c) %>% 
    group_by(irsd_5c, LISA) %>% 
    tally() %>% ungroup() %>% 
    filter(!is.na(LISA)) %>% 
    filter(LISA %in% c("HH", "LL")) %>% 
    group_by(LISA) %>% mutate(p = n/sum(n)) %>% ungroup() %>% 
    mutate(n = as.character(n)) %>% 
    make_numeric_decimal(digits = 2) %>% 
    mutate(out = paste0(n, " \textcolor{gray}{(", p, ")}")) %>% 
    dplyr::select(-c(n,p)) %>% 
    bind_rows(.,temp) %>% 
    pivot_wider(names_from = irsd_5c, values_from = out,
                values_fill = "") %>% 
    mutate(rf = rf) %>% 
    relocate(rf, LISA, Total)
  
}

# combine
bind_rows(ll) %>% 
  setNames(c("", "", "", names(.)[-c(1:3)])) %>% 
  write.csv("out/tables/LISA_irsd.csv")

# cleanup
rm(ll, rf, modelled_est, k, temp)

## SUPP Table: Benchmarking comparison ## --------------------------------------

# setup list
ll <- list()

# for loop
for(k in 1:8){
  
  rf <- names(raw_est)[k]
  message("Started ", k, ": ", rf)
  
  # load data
  modelled_est <- readRDS(file = paste0("data/summary_files/", rf, "_b1.rds"))
  modelled_est_nb <- readRDS(file = paste0("data/summary_files/", rf, "_b0.rds"))
  
  # mu
  mu <- data.frame(nb = modelled_est_nb$summ$sa2$mu_median, 
                   b = modelled_est$summ$sa2$mu_median) %>% 
    bind_cols(.,modelled_est$summ$sa2) %>%
    filter(ra_sa2 != "Very Remote",
           ps_state != 7)
  
  # cisize
  cisize <- data.frame(nb = modelled_est_nb$summ$sa2$mu_cisize, 
                       b = modelled_est$summ$sa2$mu_cisize) %>% 
    bind_cols(.,modelled_est$summ$sa2) %>%
    filter(ra_sa2 != "Very Remote",
           ps_state != 7) %>% 
    mutate(r = nb/b) %>% # over 1 means nb is less certain
    dplyr::select(b, nb, r)
  
  ll[[k]] <- data.frame(rf = rf,
                        MARD = with(mu, mean(abs(nb - b)/b)),
                        m = median(cisize$r),
                        p25 = unname(quantile(cisize$r, probs = 0.25)),
                        p75 = unname(quantile(cisize$r, probs = 0.75)))
  
  this <- data.frame(nb = modelled_est_nb$summ$sa2$mu_cisize, 
                     b = modelled_est$summ$sa2$mu_cisize) %>% 
    bind_cols(.,modelled_est$summ$sa2) %>%
    filter(ra_sa2 != "Very Remote",
           ps_state != 7) %>% 
    mutate(r = b/nb,
           r_c = cut_number(r, 20, labels = FALSE)) %>% # over 1 means nb is less certain
    filter(r_c == 20) %>% 
    dplyr::select(b, nb, r, N_persons, r_c)
  
  message("Top 5%: N_persons ", round(median(this$N_persons),2), "(",
          round(quantile(this$N_persons, probs = 0.25),2), ", ",
          round(quantile(this$N_persons, probs = 0.75),2), ")")
  
}

bind_rows(ll) %>% 
  mutate(MARD = 10*MARD) %>% 
  make_numeric_decimal(digits = 2) %>% 
  mutate(out = paste0(m, " (", p25, ", ", p75, ")"),
         rf = getRFFullNames(rf)) %>% 
  dplyr::select(rf, MARD, out) %>% 
  setNames(c("", "MARD", "Median (IQR) of relative reduction in width of HDIs")) %>% 
  write.csv("out/tables/benchmark_comp.csv")

## SUPP Table 6-7: Model Building ## -------------------------------------------

S1_side <- c("Intercept only", "Fixed effects (FE)",
             "NORF random effects (RE)",
             "DH RE", "SA2 RE", "Residual error (sd = 1)",
             "Residual error (sd = 2)")

# Functions
rows_of_mbs1 <- function(i){
  model_building[[i]]$s1 %>% 
    make_numeric_decimal() %>% 
    mutate(specs = S1_side,
           rf = names(model_building)[i]) %>% 
    relocate(rf, specs) %>% 
    mutate(id = 1:7)
}
temp_wrangle <- function(.data, rfs){
  .data %>% 
    filter(rf %in% rfs) %>% 
    mutate(rf = ifelse(id == 1, rf, "")) %>% 
    dplyr::select(-id) %>% 
    setNames(c("", "", "ALC", "SR", "LOOCV"))
}

# Full table
temp_df <- bind_rows(lapply(1:8, rows_of_mbs1))

# split into two datasets of 4 and save
temp_df %>% 
  temp_wrangle(c("smoking", "alcohol", "diet", "obesity")) %>% 
  write.csv("out/tables/mbs1_1.csv")
temp_df %>% 
  temp_wrangle(c("overweight", "waist_circum", "activityleis", "activityleiswkpl")) %>% 
  write.csv("out/tables/mbs1_2.csv")
rm(temp_df, rows_of_mbs1, S1_side, temp_wrangle)

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
    relocate(rf, specs)%>% 
    mutate(id = 1:7)
}
temp_wrangle <- function(.data, rfs){
  .data %>% 
    filter(rf %in% rfs) %>% 
    mutate(rf = ifelse(id == 1, rf, "")) %>% 
    dplyr::select(-id) %>% 
    setNames(c("", "", "MARB", "MRRMSE", "MIOP", "MARB", "MRRMSE", "MIOP"))
}

# Full table
temp_df <- bind_rows(lapply(1:8, rows_of_mbs2))

# split into two datasets of 4 and save
temp_df %>% 
  temp_wrangle(c("smoking", "alcohol", "diet", "obesity")) %>%
  write.csv("out/tables/mbs2_1.csv")
temp_df %>% 
  temp_wrangle(c("overweight", "waist_circum", "activityleis", "activityleiswkpl")) %>% 
  write.csv("out/tables/mbs2_2.csv")
rm(temp_df, rows_of_mbs2, S2_side, temp_wrangle)

## END SCRIPT ## ---------------------------------------------------------------
