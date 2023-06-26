## -----------------------------------------------------------------------------
## summary_fit ## --------------------------------------------------------------
## -----------------------------------------------------------------------------

# sparse matrices for cluster analysis
W_sparse <- as(t(global_obj$W/rowSums(global_obj$W)), "sparseMatrix")
W_sq <- global_obj$W %*% global_obj$W
W_sparse_sq <- as(t(W_sq/rowSums(W_sq)), "sparseMatrix")

# across benchmarks
for(b in c(0,1)){

# start for loop
for(i in 1:8){
  
rf <- names(modelled_est)[i]

# create empty list
sf_list <- list()

## Posterior draws ## ----
if(b == 0){
  sf_list$draws$mu <- modelled_est_nb[[i]]$mu
  sf_list$draws$or <- modelled_est_nb[[i]]$or
  sf_list$draws$mu_sa4 <- modelled_est_nb[[i]]$mu_sa4 
  sf_list$draws$mu_msb <- modelled_est_nb[[i]]$mu_msb
}else if(b == 1){
  sf_list$draws$mu <- modelled_est[[i]]$mu
  sf_list$draws$or <- modelled_est[[i]]$or
  sf_list$draws$mu_sa4 <- modelled_est[[i]]$mu_sa4 
  sf_list$draws$mu_msb <- modelled_est[[i]]$mu_msb
}

# spatially lagged values
sf_list$draws$mu_spo1 <- (sf_list$draws$mu) %*% W_sparse
sf_list$draws$mu_spo2 <- (sf_list$draws$mu) %*% W_sparse_sq

# Cluster analysis
sf_list$draws$orc <- (sf_list$draws$or - 1)
sf_list$draws$orc_lag <- (sf_list$draws$or - 1) %*% t(global_obj$W/rowSums(global_obj$W))

## Posterior summary ## ----

mu <- getMCMCsummary(sf_list$draws$mu, prefix = "mu_", model_name = rf) %>% apa()
muspo1 <- getMCMCsummary(sf_list$draws$mu_spo1, prefix = "muspo1_") %>% apa()
muspo2 <- getMCMCsummary(sf_list$draws$mu_spo2, prefix = "muspo2_") %>% apa()
or <- getMCMCsummary(sf_list$draws$or, prefix = "or_") %>% apa()
sf_list$summ$sa4 <- getMCMCsummary(sf_list$draws$mu_sa4) %>% mutate(ps_sa4 = 1:nrow(.)) %>% relocate(ps_sa4)
sf_list$summ$msb <- getMCMCsummary(sf_list$draws$mu_msb, prefix = "mu_", model_name = rf) %>% 
  mutate(ps_majorstatebench = 1:nrow(.)) %>% 
  relocate(ps_majorstatebench)
  
## Difference in posterior probability ## ----
    
DPP_mu <- bind_cols(getDPP(sf_list$draws$mu, null_value = raw_est[[i]]$national[1])) %>% 
  setNames(paste0("mu_", names(.))) %>% apa() 
DPP_or <- bind_cols(getDPP(sf_list$draws$or, null_value = 1)) %>% 
  setNames(paste0("or_", names(.))) %>% apa()

## Summarize for all ## ----

sf_list$summ$sa2 <- list(mu, muspo1, muspo2, DPP_mu, or, DPP_or) %>% 
  reduce(inner_join, by = "ps_area") %>% 
  mutate(LISA = as.factor(getLISA(sf_list$draws$orc, sf_list$draws$orc_lag))) %>% 
  left_join(.,global_obj$census, by = "ps_area")
sf_list$summ$sa2_map <- sf_list$summ$sa2 %>% 
  left_join(.,map_sa2, by = c("ps_area", "SA2")) %>% sf::st_as_sf()

## Save sf_list object ## ----
rm(or, mu, DPP_or, DPP_mu)
saveRDS(sf_list, file = paste0("data/summary_files/", rf, "_b", b, ".rds"), compress = FALSE)

## Message ## -----
message(i, ": bench ", b, ": finished ", rf)

}
}
    
## END SCRIPT ## ---------------------------------------------------------------
    