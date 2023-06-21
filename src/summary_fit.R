## -----------------------------------------------------------------------------
## summary_fit ## --------------------------------------------------------------
## -----------------------------------------------------------------------------

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

# Benchmark status
bench_status <- ifelse(QwS == QwS_length, "", "_nb")

## Posterior draws ## ----

sf_list$draws$mu <- modelled_est$mu
sf_list$draws$or <- modelled_est$or
sf_list$draws$mu_sa4 <- modelled_est$mu_sa4 
sf_list$draws$mu_msb <- modelled_est$mu_msb

# spatially lagged values
sf_list$draws$mu_spo1 <- (sf_list$draws$mu) %*% t(global_obj$W/rowSums(global_obj$W))
W_sq <- global_obj$W %*% global_obj$W
sf_list$draws$mu_spo2 <- (sf_list$draws$mu) %*% t(W_sq/rowSums(W_sq))
rm(W_sq)

# Cluster analysis
sf_list$draws$orc <- (sf_list$draws$or - 1)
sf_list$draws$orc_lag <- (sf_list$draws$or - 1) %*% t(global_obj$W/rowSums(global_obj$W))

## Posterior summary ## ----

mu <- getMCMCsummary(sf_list$draws$mu, prefix = "mu_", model_name = rf) %>% apa()
muspo1 <- getMCMCsummary(sf_list$draws$mu_spo1, prefix = "muspo1_") %>% apa()
muspo2 <- getMCMCsummary(sf_list$draws$mu_spo2, prefix = "muspo2_") %>% apa()
or <- getMCMCsummary(sf_list$draws$or, prefix = "or_") %>% apa()
sf_list$summ$sa4 <- s2LN_list$sa4_summ %>% mutate(ps_sa4 = 1:nrow(.)) %>% relocate(ps_sa4)
sf_list$summ$msb <- getMCMCsummary(sf_list$draws$mu_msb, prefix = "mu_", model_name = rf) %>% mutate(ps_majorstatebench = 1:nrow(.)) %>% relocate(ps_majorstatebench)
sf_list$summ$mus1 <- s2LN_list$data$s1_summ
  
## Difference in posterior probability ## ----
    
DPP_mu <- bind_cols(getDPP(sf_list$draws$mu, null_value = raw_est$activityleis$national[1])) %>% 
  setNames(paste0("mu_", names(.))) %>% apa() 
DPP_or <- bind_cols(getDPP(sf_list$draws$or, null_value = 1)) %>% 
  setNames(paste0("or_", names(.))) %>% apa()

## Summarize for all ## ----

sf_list$summ$sa2 <- list(mu, muspo1, muspo2, DPP_mu, or, DPP_or) %>% 
  reduce(inner_join, by = "ps_area") %>% 
  mutate(LISA = as.factor(getLISA(sf_list$draws$orc, sf_list$draws$orc_lag))) %>% 
  left_join(.,global_obj$census, by = "ps_area") %>% 
  left_join(.,sf_list$summ$mus1, by = "ps_area") %>% 
  mutate(RRSE = 100*(HT_SE - mu_sd)/HT_SE)
sf_list$summ$sa2_map <- sf_list$summ$sa2 %>% 
  left_join(.,map_sa2, by = c("ps_area", "SA2")) %>% sf::st_as_sf()

## Save sf_list object ## ----
rm(or, mu, DPP_or, DPP_mu)
saveRDS(sf_list, file = paste0("data/", rf, "_sf_list_b", bench_status, ".rds"), compress = FALSE)
    
## END SCRIPT ## ---------------------------------------------------------------
    