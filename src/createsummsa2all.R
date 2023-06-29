# createsummsa2all Packages

source('src/ms.R')

## Create object ## ------------------------------------------------------------
ll <- list()

for(k in 1:8){

  rf <- names(raw_est)[k]
  message("Started ", k, ": ", rf)
  
  modelled_est <- readRDS(file = paste0("data/summary_files/", rf, "_b1.rds"))
  ll[[k]] <- modelled_est$summ$sa2
}

# finalise and save
sa2_all <- bind_rows(ll) %>% mutate(irsd_5c = rep(irsd_5c, 8))
saveRDS(sa2_all, "data/summary_files/summsa2all.Rds")
