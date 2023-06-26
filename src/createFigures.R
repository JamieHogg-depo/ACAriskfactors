## -----------------------------------------------------------------------------
## FIGURES ## ------------------------------------------------------------------
## -----------------------------------------------------------------------------

source("src/ms.R")

lookup <- data.frame(rf = names(raw_est),
                     sha = c("exercise", "exercise", "alcohol", 
                             "fruit", "obese", "overweight",
                             "smoking", "overweight"))

## START FOR LOOP #### ---------------------------------------------------------
for(k in 1:8){
  
  rf <- names(raw_est)[k]
  sha_var = paste0("SHAPHA_estimate_", lookup[lookup$rf == rf,]$sha)
  message("Started ", k, ": ", rf)
  
  # load data
  modelled_est <- readRDS(file = paste0("data/summary_files/", rf, "_b1.rds"))
  
## Scatter: SHA PHA vs ACA #### ------------------------------------------------
  
modelled_est$summ$sa2 %>% 
    ggplot(aes(x = mu_median, xmin = mu_lower, xmax = mu_upper, 
               y = .data[[sha_var]]))+
    theme_bw()+
    geom_errorbar(col = "grey")+
    geom_abline()+
    geom_point()+
    labs(y = "Point estimtes from SHAA (PHA level)",
         x = "Current posterior medians (SA2 level)")+
    ylim(0,1)+xlim(0,1)
  
# save object
jsave(filename = paste0("scatter_shaphavsaca_", rf ,".png"), 
      base_folder = paste0(base_folder, "/figures"),
      square = F)

## Boxplot: ACA vs SEIFA and Remoteness #### -----------------------------------

seifa <- modelled_est$summ$sa2 %>% 
  mutate(ABS_irsd_decile_nation_complete = factor(ABS_irsd_decile_nation_complete, 
                                                  levels = 1:10,
                                         labels = c("Least advantaged", 
                                                    as.character(2:9), 
                                                    "Most advantaged"))) %>% 
  ggplot(aes(x = mu_median,
             y = ABS_irsd_decile_nation_complete))+
  theme_bw()+
  geom_boxplot()+
  labs(y = "",
       x = "")

ra <- modelled_est$summ$sa2 %>% 
  mutate(ra_sa2 = factor(ra_sa2, c("Major Cities", "Inner Regional", 
                                   "Outer Regional", "Remote", 
                                   "Very Remote"))) %>% 
  ggplot(aes(x = mu_median,
             y = ra_sa2))+
  theme_bw()+
  geom_boxplot()+
  labs(y = "",
       x = "Posterior medians")+
  scale_y_discrete(limits=rev)

# Create layout
lay <- rbind(c(1),
             c(2))
full_plt <- grid.arrange(grobs = list(seifa, ra), 
                               layout_matrix  = lay)

# save object
jsave(filename = paste0("boxplot_avaseifaremoteness_", rf ,".png"), 
      base_folder = paste0(base_folder, "/figures"),
      plot = full_plt,
      square = F)

# cleanup
rm(seifa, ra)

## Caterpillar plots #### ------------------------------------------------------

modelled_est$summ$sa2 %>% 
  mutate(rank = rank(mu_median, ties.method = "first")) %>%
  ggplot(aes(y = mu_median, ymin = mu_lower, ymax = mu_upper,
             x = rank, 
             col = mu_median))+
  theme_bw()+
  geom_errorbar(col = "grey")+
  geom_hline(yintercept = raw_est[[k]]$national[1])+
  geom_point()+
  scale_color_viridis_c(begin = 0.3, end = 1, 
                      direction = 1,
                      option = "B")+
  labs(y = "Modeled prevalence estimates",
       x = "")+
  theme(legend.position = "none")

# save object
jsave(filename = paste0("catterpillar_", rf ,".png"), 
      base_folder = paste0(base_folder, "/figures"),
      square = F)

## Scatter - SA4 direct vs SA4 modelled #### -----------------------------------

# setup
temp <- left_join(modelled_est$summ$sa4, raw_est[[k]]$sa4, by = "ps_sa4")
for_range <- with(temp, c(lower, upper, HT - 1.96 * HT_SE, HT + 1.96 * HT_SE))
st_range <- range(for_range)
  
# plot
  temp %>% 
  ggplot(aes(y = median, ymin = lower, ymax = upper,
             x = HT, 
             xmin = HT - 1.96 * HT_SE, 
             xmax = HT + 1.96 * HT_SE,
             col = log(HT_CV)))+
  theme_bw()+
  geom_errorbar(col = "grey")+
  geom_errorbarh(col = "grey")+
  geom_abline()+
  geom_point()+
  scale_color_viridis_c(begin = 0, end = 1, 
                        direction = -1,
                        option = "A")+
  labs(col = "Log\ndirect\nestimate\nCV",
       y = "Modelled",
       x = "Direct")+
  ylim(st_range[1],st_range[2])+xlim(st_range[1],st_range[2])

# save object
jsave(filename = paste0("scatter_sa4directvsmodelled_", rf ,".png"), 
      base_folder = paste0(base_folder, "/figures"),
      square = F)

# cleanup
rm(temp, for_range, st_range)

## Scatter - MSB direct vs modelled #### ---------------------------------------

# setup
temp <- left_join(modelled_est$summ$msb, raw_est[[k]]$msb, by = "ps_majorstatebench")
for_range <- with(temp, c(mu_lower, mu_upper, HT - 1.96 * HT_SE, HT + 1.96 * HT_SE))
st_range <- range(for_range)

# plot
temp %>% 
  ggplot(aes(y = mu_median, ymin = mu_lower, ymax = mu_upper,
             x = HT, 
             xmin = HT - 1.96 * HT_SE, 
             xmax = HT + 1.96 * HT_SE,
             col = log(HT_CV)))+
  theme_bw()+
  geom_errorbar(col = "grey")+
  geom_errorbarh(col = "grey")+
  geom_abline()+
  geom_point()+
  scale_color_viridis_c(begin = 0, end = 1, 
                        direction = -1,
                        option = "A")+
  labs(col = "Log\ndirect\nestimate\nCV",
       y = "Modelled",
       x = "Direct")+
  ylim(st_range[1],st_range[2])+xlim(st_range[1],st_range[2])

# save object
jsave(filename = paste0("scatter_msbdirectvsmodelled_", rf ,".png"), 
      base_folder = paste0(base_folder, "/figures"),
      square = F)

# cleanup
rm(temp, for_range, st_range)

## FINISH FOR LOOP #### --------------------------------------------------------

}

## END SCRIPT #### -------------------------------------------------------------