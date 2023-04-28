## -----------------------------------------------------------------------------
## TABLES ## -------------------------------------------------------------------
## -----------------------------------------------------------------------------

bind_rows(
  (b_est$summ_mu_sa4 %>% 
    mutate(overlap = overlap_v(lower, upper, HT_lower, HT_upper),
           cover = between_vec(HT, lower, upper)) %>% 
    group_by(model) %>% 
    summarise(MRRMSE = mean(RRMSE),
              MARB = mean(ARB),
              cisize = median(cisize),
              coverage = mean(cover),
              overlap1 = mean(overlap),
              overlap2 = weighted.mean(overlap, w = 1/HT_SE)) %>% 
    mutate(type = "Benchmarked") %>% relocate(type)),
  (nb_est$summ_mu_sa4 %>% 
    mutate(overlap = overlap_v(lower, upper, HT_lower, HT_upper),
           cover = between_vec(HT, lower, upper)) %>% 
    group_by(model) %>% 
    summarise(MRRMSE = mean(RRMSE),
              MARB = mean(ARB),
              cisize = median(cisize),
              coverage = mean(cover),
              overlap1 = mean(overlap),
              overlap2 = weighted.mean(overlap, w = 1/HT_SE)) %>% 
    mutate(type = "Not benchmarked") %>% relocate(type))
) %>% 
  make_numeric_decimal() %>% 
  write_excel_csv(., file = paste0(base_folder, "/comparative_performance.csv"))

## END SCRIPT ## --------------------------------------------------------------
