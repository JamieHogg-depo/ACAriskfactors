## -----------------------------------------------------------------------------
## summary_fit ## --------------------------------------------------------------
## -----------------------------------------------------------------------------

# Source master
source("src/ms.R")

# Load modelled estimates - nonbenchmarked
modelled_est_nb <- pbapply::pblapply(list.files("data/DataLabExport",
                                             pattern = "modelled_est_nb_*", full.names = T), readRDS)
names(modelled_est_nb) <- str_remove(
  str_remove(
    list.files("data/DataLabExport", pattern = "modelled_est_nb_"), "modelled_est_nb_"), ".rds")

# Load modelled estimates
modelled_est <- pbapply::pblapply(list.files("data/DataLabExport",
                             pattern = "modelled_est_*", full.names = T), readRDS)
names(modelled_est) <- str_remove(
  str_remove(
    list.files("data/DataLabExport", pattern = "modelled_est_*"), "modelled_est_"), ".rds")
modelled_est <- modelled_est[!str_starts(names(modelled_est), "nb_")]

# sparse matrices for cluster analysis
W_sparse <- as(t(global_obj$W/rowSums(global_obj$W)), "sparseMatrix")
W_sq <- global_obj$W %*% global_obj$W
W_sparse_sq <- as(t(W_sq/rowSums(W_sq)), "sparseMatrix")

# across benchmarks
for(b in c(0,1)){

# start for loop
for(i in 1:8){
  
rf <- names(modelled_est)[i]

# create empty lists
sf_list <- list()
ll <- list()

## Posterior draws ## ----
if(b == 0){
  sf_list$draws$mu <- modelled_est_nb[[i]]$mu
  sf_list$draws$or <- modelled_est_nb[[i]]$or
  sf_list$draws$rr <- modelled_est_nb[[i]]$mu/raw_est[[i]]$national[1]
  sf_list$draws$count <- t(t(modelled_est_nb[[i]]$mu) * global_obj$census$N_persons)
  sf_list$draws$mu_sa4 <- modelled_est_nb[[i]]$mu_sa4 
  sf_list$draws$mu_msb <- modelled_est_nb[[i]]$mu_msb
}else if(b == 1){
  sf_list$draws$mu <- modelled_est[[i]]$mu
  sf_list$draws$or <- modelled_est[[i]]$or
  sf_list$draws$rr <- modelled_est[[i]]$mu/raw_est[[i]]$national[1]
  sf_list$draws$count <- t(t(modelled_est[[i]]$mu) * global_obj$census$N_persons)
  sf_list$draws$mu_sa4 <- modelled_est[[i]]$mu_sa4 
  sf_list$draws$mu_msb <- modelled_est[[i]]$mu_msb
}

# PHA level estimates
this <- aggregate(global_obj$census$N_persons, list(global_obj$census$pha), sum)
pha_unique <- this[,1]
this <- this[,2]
foo <- function(x){
  aggregate(x*global_obj$census$N_persons, list(global_obj$census$pha), sum)[,2]/this
}
pha_draws_mat <- t(pbapply::pbapply(sf_list$draws$mu, 1, foo))
sf_list$summ$pha <- getMCMCsummary(pha_draws_mat) %>% mutate(pha =pha_unique) %>% relocate(pha)
rm(this, pha_unique, foo, pha_draws_mat)

# spatially lagged values
sf_list$draws$mu_spo1 <- (sf_list$draws$mu) %*% W_sparse
sf_list$draws$mu_spo2 <- (sf_list$draws$mu) %*% W_sparse_sq

# Cluster analysis
sf_list$draws$orc <- (sf_list$draws$or - 1)
sf_list$draws$orc_lag <- (sf_list$draws$or - 1) %*% t(global_obj$W/rowSums(global_obj$W))

## Posterior summary ## ----

# prevalence
mu <- getMCMCsummary(sf_list$draws$mu, prefix = "mu_", model_name = rf) %>% apa()
muspo1 <- getMCMCsummary(sf_list$draws$mu_spo1, prefix = "muspo1_") %>% apa()
muspo2 <- getMCMCsummary(sf_list$draws$mu_spo2, prefix = "muspo2_") %>% apa()

# or
or <- getMCMCsummary(sf_list$draws$or, prefix = "or_") %>% apa()
logor <- getMCMCsummary(log(sf_list$draws$or), prefix = "logor_") %>% apa()

# rr
rr <- getMCMCsummary(sf_list$draws$rr, prefix = "rr_") %>% apa()
logrr <- getMCMCsummary(log(sf_list$draws$rr), prefix = "logrr_") %>% apa()

# count
count <- getMCMCsummary(sf_list$draws$count, prefix = "count_") %>% apa()

# sa4
sf_list$summ$sa4 <- getMCMCsummary(sf_list$draws$mu_sa4) %>% mutate(ps_sa4 = 1:nrow(.)) %>% relocate(ps_sa4)
sf_list$summ$msb <- getMCMCsummary(sf_list$draws$mu_msb, prefix = "mu_", model_name = rf) %>% 
  mutate(ps_majorstatebench = 1:nrow(.)) %>% 
  relocate(ps_majorstatebench)
  
## Difference in posterior probability ## ----
    
DPP_mu <- bind_cols(getDPP(sf_list$draws$mu, null_value = raw_est[[i]]$national[1])) %>% 
  setNames(paste0("mu_", names(.))) %>% apa() 
DPP_or <- bind_cols(getDPP(sf_list$draws$or, null_value = 1)) %>% 
  setNames(paste0("or_", names(.))) %>% apa()
DPP_rr <- bind_cols(getDPP(sf_list$draws$rr, null_value = 1)) %>% 
  setNames(paste0("rr_", names(.))) %>% apa()

## Summarize for all ## ----

sf_list$summ$sa2 <- list(mu, muspo1, muspo2, DPP_mu, or, logor, DPP_or, rr, logrr, DPP_rr, count) %>% 
  reduce(inner_join, by = "ps_area") %>% 
  mutate(LISA = as.factor(getLISA(sf_list$draws$orc, sf_list$draws$orc_lag))) %>% 
  left_join(.,global_obj$census, by = "ps_area")

# keep empty geometries
sf_list$summ$sa2_map <- sf_list$summ$sa2 %>% 
  right_join(.,map_sa2, by = c("ps_area", "SA2")) %>% sf::st_as_sf()

## Save sf_list object ## ----
rm(or, mu, DPP_or, DPP_mu)
saveRDS(sf_list, file = paste0("data/summary_files/", rf, "_b", b, "_full.rds"), compress = FALSE)

# save smaller one
sf_list_small <- sf_list
sf_list_small$draws <- NULL
saveRDS(sf_list_small, file = paste0("data/summary_files/", rf, "_b", b, ".rds"), compress = FALSE)

## Message ## -----
message(i, ": bench ", b, ": finished ", rf)

}
}
    
## END SCRIPT ## ---------------------------------------------------------------
    