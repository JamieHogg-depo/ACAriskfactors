## -----------------------------------------------------------------------------
## FIGURES ## ------------------------------------------------------------------
## -----------------------------------------------------------------------------

source("src/ms.R")

## START FOR LOOP #### ---------------------------------------------------------
for(k in 1:8){
  
  rf <- names(raw_est)[k]
  rf_full <- lookup[k,]$rf_full
  sha_vars = paste0("shaout_", lookup[lookup$rf == rf,]$sha, "_", c("estimate", "lower", "upper"))
  message("Started ", k, ": ", rf)
  
  # load data
  modelled_est <- readRDS(file = paste0("data/summary_files/", rf, "_b1.rds"))
  modelled_est_nb <- readRDS(file = paste0("data/summary_files/", rf, "_b0.rds"))
  
## Scatter: SHA PHA vs ACA #### ------------------------------------------------

# pha level
modelled_est$summ$pha %>% 
    left_join(.,SHA_pha, by = "pha") %>% 
    ggplot(aes(x = median, xmin = lower, xmax = upper, 
               y = .data[[sha_vars[1]]], 
               ymin = .data[[sha_vars[2]]], 
               ymax = .data[[sha_vars[3]]]))+
    theme_bw()+
    geom_errorbar(col = "grey")+
    geom_errorbarh(col = "grey")+
    geom_abline()+
    geom_point()+
    labs(y = "Social Health Atlas",
         x = "Our estimates",
         col = "SES")+
    coord_obs_pred()+
    #ylim(0,1)+xlim(0,1)+
    theme(legend.position = "bottom")
  
# save object
jsave(filename = paste0("scatter_shaphavsaca_pha_", rf ,".png"), 
      base_folder = paste0(base_folder, "/figures"),
      square = F)
  
# sa2 level
modelled_est$summ$sa2 %>% 
    left_join(.,SHA_pha, by = "pha") %>% 
    left_join(.,irsd_5c, by = "ps_area") %>% 
    ggplot(aes(x = mu_median, xmin = mu_lower, xmax = mu_upper, 
               y = .data[[sha_vars[1]]], 
               ymin = .data[[sha_vars[2]]], 
               ymax = .data[[sha_vars[3]]],
               col = irsd_5c))+
    theme_bw()+
    geom_errorbar(col = "grey")+
    geom_errorbarh(col = "grey")+
    geom_abline()+
    geom_point()+
    labs(y = "SHAA (PHA level)",
         x = "Our estimates (SA2 level)",
         col = "SES")+
    ylim(0,1)+xlim(0,1)+
    theme(legend.position = "bottom")

# save object
jsave(filename = paste0("scatter_shaphavsaca_", rf ,".png"), 
      base_folder = paste0(base_folder, "/figures"),
      square = F)

## Boxplot: ACA vs SEIFA and Remoteness #### -----------------------------------

seifa <- modelled_est$summ$sa2 %>% 
  mutate(ABS_irsd_decile_nation_complete = factor(ABS_irsd_decile_nation_complete, 
                                                  levels = 1:10,
                                         labels = c("Most\ndisadvantaged", 
                                                    as.character(2:9), 
                                                    "Least\ndisadvantaged"))) %>% 
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
                               layout_matrix  = lay,
                         top = textGrob(rf_full,gp=gpar(fontsize=10)))

# save object
jsave(filename = paste0("boxplot_avaseifaremoteness_", rf ,".png"), 
      base_folder = paste0(base_folder, "/figures"),
      plot = full_plt,
      square = T)

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
       x = "", 
       title = rf_full)+
  theme(legend.position = "none")

# save object
jsave(filename = paste0("catterpillar_", rf ,".png"), 
      base_folder = paste0(base_folder, "/figures"),
      square = F)

## Scatter - SA4 direct vs SA4 modelled #### -----------------------------------

## Non-Benchmarked 
temp <- left_join(modelled_est_nb$summ$sa4, raw_est[[k]]$sa4, by = "ps_sa4")
for_range <- c(with(temp, c(lower, upper, HT - 1.96 * HT_SE, HT + 1.96 * HT_SE)),
               with(modelled_est$summ$sa4, c(lower, upper)))
st_range <- range(for_range, na.rm = T)

# plot
nb_plt <- temp %>% 
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
       x = "Direct",
       title = "Non-benchmarked")+
  ylim(st_range[1],st_range[2])+xlim(st_range[1],st_range[2])

# extract legend
llegend <- ggpubr::get_legend(nb_plt)
nb_plt <- nb_plt + theme(legend.position = "none")
  
## Benchmarked 
b_plt <- left_join(modelled_est$summ$sa4, raw_est[[k]]$sa4, by = "ps_sa4") %>% 
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
       x = "Direct",
       title = "Benchmarked")+
  ylim(st_range[1],st_range[2])+xlim(st_range[1],st_range[2])+
  theme(legend.position = "none")

## Full plot
lay <- rbind(c(1,1,1,3),
             c(2,2,2,3))
full_plt <- arrangeGrob(grobs = list(nb_plt, b_plt, llegend), 
                        layout_matrix  = lay)

# save object
jsave(filename = paste0("scatter_sa4directvsmodelled_", rf ,".png"), 
      base_folder = paste0(base_folder, "/figures"),
      plot = full_plt,
      square = F)

# cleanup
rm(temp, for_range, st_range, full_plt, nb_plt, b_plt, lay, llegend)

## Scatter - MSB direct vs modelled #### ---------------------------------------

## Non-Benchmarked 
temp <- left_join(modelled_est_nb$summ$msb, raw_est[[k]]$msb, by = "ps_majorstatebench")
for_range <- with(temp, c(mu_lower, mu_upper, HT - 1.96 * HT_SE, HT + 1.96 * HT_SE))
st_range <- range(for_range)

# plot
nb_plt <- temp %>% 
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
       x = "Direct",
       title = "Non-benchmarked")+
  ylim(st_range[1],st_range[2])+xlim(st_range[1],st_range[2])

# extract legend
llegend <- ggpubr::get_legend(nb_plt)
nb_plt <- nb_plt + theme(legend.position = "none")

## Benchmarked
b_plt <- left_join(modelled_est$summ$msb, raw_est[[k]]$msb, by = "ps_majorstatebench") %>% 
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
       x = "Direct",
       title = "Benchmarked")+
  ylim(st_range[1],st_range[2])+xlim(st_range[1],st_range[2])+
  theme(legend.position = "none")

## Full plot
lay <- rbind(c(1,1,1,3),
             c(2,2,2,3))
full_plt <- arrangeGrob(grobs = list(nb_plt, b_plt, llegend), 
                        layout_matrix  = lay)

# save object
jsave(filename = paste0("scatter_msbdirectvsmodelled_", rf ,".png"), 
      base_folder = paste0(base_folder, "/figures"),
      plot = full_plt, 
      square = F)

# cleanup
rm(temp, for_range, st_range, full_plt, nb_plt, b_plt, lay, llegend)

## Barchart - LISA by State ## -------------------------------------------------

modelled_est$summ$sa2 %>% 
  left_join(.,state_name_concor, by = "ps_state") %>% 
  mutate(LISA_or = case_when(
    LISA_or == "HH" ~ "HC",
    LISA_or == "LL" ~ "LC"
  ),
  lisa = ifelse(or_EP > 0.9, "H", 
                       ifelse(or_EP < 0.1, "L", NA)),
         out = factor(ifelse(is.na(LISA_or) & !is.na(lisa), 
                             lisa, as.character(LISA_or)),
                      levels = c("HC", "H", "L", "LC"))) %>% 
  filter(!is.na(out)) %>% 
  ggplot(aes(fill = out, y = as.factor(state_name))) + 
  theme_bw()+
  geom_bar(position = "fill")+
  labs(y = "",
       x = "",
       fill = "",
       title = rf_full)+
  theme(legend.position = "bottom")+
  scale_fill_manual(values = c("red", "coral", "skyblue", "royalblue"),
                    breaks = c("HC", "H", "L", "LC"))

# save object
jsave(filename = paste0("bar_lisastate_", rf ,".png"), 
      base_folder = paste0(base_folder, "/figures"),
      square = F)

## Binned prevalence ## ------------------------------------------------

# IRSD
modelled_est$summ$sa2 %>% 
  left_join(.,irsd_5c, by = "ps_area") %>% 
  mutate(c = cut_number(mu_median, 10, labels = FALSE)) %>% 
  ggplot(aes(x = as.factor(c), fill = irsd_5c))+
  theme_bw()+
  geom_bar(position = "fill")+
  addIRSDColor()+
  labs(y = "",
       x = "Prevalence point estimates (Percentiles)",
       fill = "",
       title = rf_full)

# save object
jsave(filename = paste0("binnedprev_irsd_", rf ,".png"), 
      base_folder = paste0(base_folder, "/figures"),
      square = F)

# Remoteness
modelled_est$summ$sa2 %>% 
  mutate(c = cut_number(mu_median, 10, labels = FALSE),
         ra_sa2 = fct_relevel(ra_sa2, "Major Cities")) %>% 
  ggplot(aes(x = as.factor(c), fill = ra_sa2))+
  theme_bw()+
  geom_bar(position = "fill")+
  addRemotenessColor()+
  labs(y = "",
       x = "Prevalence point estimates (Percentiles)",
       fill = "",
       title = rf_full)

# save object
jsave(filename = paste0("binnedprev_ra_", rf ,".png"), 
      base_folder = paste0(base_folder, "/figures"),
      square = F)

## FINISH FOR LOOP #### --------------------------------------------------------

}

## END SCRIPT #### -------------------------------------------------------------