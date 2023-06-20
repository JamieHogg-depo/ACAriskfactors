## -----------------------------------------------------------------------------
## summary_fit ## --------------------------------------------------------------
## -----------------------------------------------------------------------------

# Add its back in
s2LN_list$its <- rstan::extract(s2LN_list$fit)

# small function
apa <- function(.data, .m_n = FALSE, model_name = NULL){
  if(.m_n){
    temp <- .data %>% 
      mutate(ps_area = 1:nrow(.),
             model = model_name)
  }else{
    temp <- .data %>% 
      mutate(ps_area = 1:nrow(.))
  }
}

# create empty list
sf_list <- list()
#sf_list$rt_hours <- (s1LN_list$rt_mins + s2LN_list$rt_mins)/60

# Benchmark status
#bench_status <- s2LN_list$data$benchmark #ifelse(QwS == QwS_length, "", "_nb")
bench_status <- ifelse(QwS == QwS_length, "", "_nb")

## Select census vars ## ----

sf_list$census <- nrfs$aux %>% 
  dplyr::select(SA2, ps_area, ps_majorstatebench, ps_state, 
                PC1, PC2, PC2, PC3, PC4, PC5, PC6, N_persons, ABS_irsd_decile_nation_complete, ra_sa2, ra_sa2_3c, 
                PHN, pha, starts_with("SHA_estimate"), starts_with("SHAPHA_estimate"))

## Posterior draws ## ----

sf_list$draws$mu <- s2LN_list$its$mu
sf_list$draws$or <- getORs(sf_list$draws$mu, baseline_odds = rfs$HT_odds)
sf_list$draws$mu_sa4 <- s2LN_list$sa4_draws
sf_list$draws$mu_msb <- s2LN_list$its$msb_C_tilde

# spatially lagged values
sf_list$draws$mu_spo1 <- (sf_list$draws$mu) %*% t(nrfs$W_sa2/rowSums(nrfs$W_sa2))
W_sq <- nrfs$W_sa2 %*% nrfs$W_sa2
sf_list$draws$mu_spo2 <- (sf_list$draws$mu) %*% t(W_sq/rowSums(W_sq))
rm(W_sq)

# Cluster analysis
sf_list$draws$orc <- (sf_list$draws$or - 1)
sf_list$draws$orc_lag <- (sf_list$draws$or - 1) %*% t(nrfs$W_sa2/rowSums(nrfs$W_sa2))

## Model coefficients ## ----
l_a_pc_reshape <- matrix(aperm(s2LN_list$its$l_a_pcfe, c(1,3,2)), nrow = dim(s2LN_list$its$l_a_pcfe)[1], ncol = prod(dim(s2LN_list$its$l_a_pcfe)[2], dim(s2LN_list$its$l_a_pcfe)[3]))
sf_list$coefs$l_a_pcfe <- getMCMCsummary(l_a_pc_reshape) %>% mutate(parameter = subsetSummary(s2LN_list$summary, "l_a_pcfe\\[")$parameter); rm(l_a_pc_reshape)
sf_list$coefs$sigmarho <- 
getMCMCsummary(matrix(c(s2LN_list$its$sigma_sa2,
                        s2LN_list$its$rho_sa2,
                        s2LN_list$its$sigma_sa3,
                        s2LN_list$its$rho_sa3), 
                      byrow = F, nrow = length(s2LN_list$its$sigma_sa2))) %>% 
  mutate(parameter = c("sigma_sa2", "rho_sa2", "sigma_sa3", "rho_sa3")) %>% 
  relocate(parameter)

## Posterior summary ## ----

mu <- getMCMCsummary(sf_list$draws$mu, prefix = "mu_", model_name = rf) %>% apa()
muspo1 <- getMCMCsummary(sf_list$draws$mu_spo1, prefix = "muspo1_") %>% apa()
muspo2 <- getMCMCsummary(sf_list$draws$mu_spo2, prefix = "muspo2_") %>% apa()
or <- getMCMCsummary(sf_list$draws$or, prefix = "or_") %>% apa()
sf_list$summ$sa4 <- s2LN_list$sa4_summ %>% mutate(ps_sa4 = 1:nrow(.)) %>% relocate(ps_sa4)
sf_list$summ$msb <- getMCMCsummary(sf_list$draws$mu_msb, prefix = "mu_", model_name = rf) %>% mutate(ps_majorstatebench = 1:nrow(.)) %>% relocate(ps_majorstatebench)
sf_list$summ$mus1 <- s2LN_list$data$s1_summ
  
## Difference in posterior probability ## ----
    
DPP_mu <- bind_cols(getDPP(sf_list$draws$mu, null_value = rfs$HT_mu)) %>% 
  setNames(paste0("mu_", names(.))) %>% apa() 
DPP_or <- bind_cols(getDPP(sf_list$draws$or, null_value = 1)) %>% 
  setNames(paste0("or_", names(.))) %>% apa()

## Summarize for all ## ----

sf_list$summ$sa2 <- list(mu, muspo1, muspo2, DPP_mu, or, DPP_or) %>% 
  reduce(inner_join, by = "ps_area") %>% 
  mutate(LISA = as.factor(getLISA(sf_list$draws$orc, sf_list$draws$orc_lag))) %>% 
  left_join(.,sf_list$census, by = "ps_area") %>% 
  left_join(.,rfs$sample_agg, by = "ps_area") %>% 
  left_join(.,sf_list$summ$mus1, by = "ps_area") %>% 
  mutate(RRSE = 100*(HT_SE - mu_sd)/HT_SE)
sf_list$summ$sa2_map <- sf_list$summ$sa2 %>% 
  left_join(.,nrfs$map_sa2, by = c("ps_area", "SA2")) %>% sf::st_as_sf()

## Plots ## ---
sf_list$plot$cat <- sf_list$summ$sa2 %>% 
  mutate(rank = rank(mu_median)) %>% 
  ggplot(aes(y = mu_median, ymin = mu_lower, ymax = mu_upper,
             x = rank, col = mu_median))+
  theme_bw()+
  geom_errorbar(col = "grey")+
  geom_point()+
  scale_color_viridis_c(begin = 0, end = 1,
                        direction = -1, 
                        option = "B")+
  geom_hline(yintercept = rfs$HT_mu) +
  labs(y = "Modeled prevalence estimates", 
       x = "")+
  theme(legend.position = "none")+
  facet_grid(.~ra_sa2_3c)+ylim(0,1)

## Metrics ## ----
s1mets <- readRDS(paste0("P:/R Projects/ACAriskfactors/output/perf_mets/", cur_dB, "/s1LN/", rf, "/rl_", rf, "_s1LN_RE.rds"))
s2mets <- readRDS(paste0("P:/R Projects/ACAriskfactors/output/perf_mets/", cur_dB, "/s2LN/", rf, "/rl_", rf, "_s2LN_RE.rds"))
sf_list$mets$s1 <- bind_rows(s1mets)
sf_list$mets$s2 <- bind_rows(s2mets)

## Add concordance data ## ----
sf_list$area_concor <- nrfs$area_sa_concor
sf_list$majorstatebench_concor <- nrfs$majorstatebench_concor

## Add direct estimates ## ----
sf_list$direct$national <- c(rfs$HT_mu, rfs$HT_mu_VAR)
sf_list$direct$sa2 <- rfs$sample_agg %>% 
  dplyr::select(ps_area, HT, n, N, unstable, HT_SE, HT_CV) %>% 
  filter(n > 10)  
sf_list$direct$sa4 <- rfs$sa4_agg %>% 
  dplyr::select(ps_sa4, HT, n, N, HT_SE, HT_CV) %>% 
  filter(n > 10)
sf_list$direct$msb <- rfs$majorstatebench_agg %>% 
  dplyr::select(ps_majorstatebench, HT, n, N, HT_SE, HT_CV) %>% 
  filter(n > 10)
sf_list$direct$state <- rfs$state_agg %>% 
  dplyr::select(ps_state, HT, n, N, HT_SE, HT_CV) %>% 
  filter(n > 10)
sf_list$direct$sd_comp <- data.frame(sa2_modelled = sd(sf_list$summ$sa2$mu_median),
                                     sa2_direct = sd(rfs$sample_agg$HT, na.rm = T),
                                     sa4_modelled = sd(sf_list$summ$sa4$median),
                                     sa4_direct = sd(rfs$sa4_agg$HT, na.rm = T),
                                     majorstatebench_modelled = sd(sf_list$summ$msb$mu_median),
                                     majorstatebench_direct = sd(rfs$majorstatebench_agg$HT, na.rm = T)) %>% 
  mutate(sa2_ratio = sa2_direct/sa2_modelled,
         sa4_ratio = sa4_direct/sa4_modelled,
         majorstatebench_ratio = majorstatebench_direct/majorstatebench_modelled)

## Save objects to export ## ----
global_obj <- sf_list[c("area_concor", "majorstatebench_concor", "census")]
global_obj$W <- nrfs$W_sa2
saveRDS(global_obj, file = paste0(base_folder, "/summary_fit/global_obj.rds"))

modelled_est <- sf_list$draws[c("mu", "or", "mu_sa4", "mu_msb")]
saveRDS(modelled_est, file = paste0(base_folder, "/summary_fit/modelled_est", bench_status, "_", rf, ".rds"))

raw_est <- sf_list$direct
raw_est$sd_comp <- raw_est$sd_comp %>% dplyr::select(-contains("ratio"))
saveRDS(raw_est, file = paste0(base_folder, "/summary_fit/raw_est_", rf, ".rds"))

model_building <- list()
model_building$s1 <- dplyr::select(sf_list$mets$s1, contains(c("WOLS2", "SR", "elpdloo_est"))) %>% rename(SR = SR, ALC = WOLS2, LOOCV = elpdloo_est)
model_building$s2 <- dplyr::select(sf_list$mets$s2, contains(c("MARB", "MRRMSE", "overlap")))
saveRDS(model_building, file = paste0(base_folder, "/summary_fit/model_building_", rf, ".rds"))

## Save full object ## ----
rm(or, mu, DPP_or, DPP_mu)
saveRDS(sf_list, file = paste0(base_folder, "/summary_fit/sf_list_b", bench_status, ".rds"), compress = FALSE)
    
## END SCRIPT ## ---------------------------------------------------------------
    