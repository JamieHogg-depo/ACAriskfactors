## -----------------------------------------------------------------------------
## FIGURES ## ------------------------------------------------------------------
## -----------------------------------------------------------------------------

source("src/ms.R")

lookup <- data.frame(rf = names(raw_est),
                     sha = c("exercise", "exercise", "alcohol", 
                             "fruit", "obese", "overweight",
                             "smoking", "overweight"))

## Violin plots ## -------------------------------------------------------------

ll <- list()
for(k in 1:8){
  ll[[k]] <- data.frame(model = names(raw_est)[k],
                        point = raw_est[[k]]$national[1])
}
nat_di <- bind_rows(ll) %>% mutate(model = getRFFullNames(model))

# proportion
summsa2all %>% 
  mutate(model = getRFFullNames(model)) %>% 
  ggplot(aes(x = mu_median, y = model))+
  theme_bw()+
  geom_violin()+
  geom_point(data = nat_di, aes(x = point, y = model), col = "blue")+
  labs(y = "", 
       x = "Proportion (posterior median)")+
  xlim(0,1)

# save object
jsave(filename = paste0("summary_violin.png"), 
      base_folder = paste0(base_folder, "/figures"),
      square = F)
  
## Scatter: Two-way SHA PHA vs ACA  - smoking and obesity #### -----------------
  
  sm <- readRDS(file = paste0("data/summary_files/smoking_b1.rds"))
  ob <- readRDS(file = paste0("data/summary_files/obesity_b1.rds"))
  
## Smoking
  sm_plt <- sm$summ$sa2 %>% 
    left_join(.,SHA_pha, by = "pha") %>% 
    mutate(irsd_5c = irsd_5c) %>% 
    ggplot(aes(x = mu_median, xmin = mu_lower, xmax = mu_upper, 
               y = shaout_smoking_estimate, 
               ymin = shaout_smoking_lower, 
               ymax = shaout_smoking_upper,
               col = irsd_5c))+
    theme_bw()+
    geom_errorbar(col = "grey")+
    geom_errorbarh(col = "grey")+
    geom_abline()+
    geom_point()+
    labs(y = "SHAA (PHA level)",
         title = "Current smoking",
         x = "Our estimates (SA2 level)",
         col = "IRSD")+
    ylim(0,1)+xlim(0,1)
  
## legend
  llegend <- ggpubr::get_legend(sm_plt)
  sm_plt <- sm_plt + theme(legend.position = "none")
  
## Obesity
  ob_plt <- ob$summ$sa2 %>% 
    left_join(.,SHA_pha, by = "pha") %>% 
    mutate(irsd_5c = irsd_5c) %>% 
    ggplot(aes(x = mu_median, xmin = mu_lower, xmax = mu_upper, 
               y = shaout_obese_estimate, 
               ymin = shaout_obese_lower, 
               ymax = shaout_obese_upper,
               col = irsd_5c))+
    theme_bw()+
    geom_errorbar(col = "grey")+
    geom_errorbarh(col = "grey")+
    geom_abline()+
    geom_point()+
    labs(y = "SHAA (PHA level)",
         title = "Obesity",
         x = "",
         col = "SES")+
    ylim(0,1)+xlim(0,1)+
    theme(legend.position = "none")

## Full plot
lay <- rbind(c(1,1,3),
             c(1,1,3),
             c(2,2,3),
             c(2,2,3))
full_plt <- arrangeGrob(grobs = list(ob_plt, sm_plt, llegend), 
                        layout_matrix  = lay)

# save object
jsave(filename = paste0("scattersha_smokingobesity.png"), 
      base_folder = paste0(base_folder, "/figures"),
      plot = full_plt,
      square = F)

# cleanup
rm(full_plt, sm_plt, ob_plt, lay, llegend)

## Scatter: Non-benchmarked direct estimate ## ---------------------------------

# create list
ll <- list()

# complete forloop
for(k in 1:8){
  
  rf <- names(raw_est)[k]
  message("Started ", k, ": ", rf)
  
  # load data
  modelled_est <- readRDS(file = paste0("data/summary_files/", rf, "_b1.rds"))

  ll[[k]] <- rbind(
    modelled_est$summ$msb %>% 
    slice(13) %>% 
    dplyr::select(mu_median, mu_lower, mu_upper) %>%
    rename(point = mu_median, 
           lower = mu_lower, 
           upper = mu_upper) %>% 
    mutate(type = "Modelled",
           rf = rf),
  
  raw_est[[k]]$msb %>% 
    slice(13) %>% 
    mutate(point = HT, 
           lower = HT - 1.96 * HT_SE,
           upper = HT + 1.96 * HT_SE) %>% 
    dplyr::select(point, lower, upper) %>% 
    mutate(type = "Direct",
           rf = rf)
  )
  
}

# create full data
nb_data <- bind_rows(ll) %>% 
  mutate(rf = getRFFullNames(rf))
  
# create plot
nb_data %>% 
  ggplot(aes(x = point, xmin = lower, xmax = upper,
             y = rf,
             col = type))+
  theme_bw()+
  geom_pointrange(position=position_dodge(width=0.5))+
  labs(col = "",
       x = "Point estimate and 95% interval", 
       y = "")
  
# save object
jsave(filename = paste0("benchmarkcomp.png"), 
      base_folder = paste0(base_folder, "/figures"),
      square = F)

## Barchart - LISA ## ----------------------------------------------------------

# Remoteness
summsa2all %>% 
  mutate(lisa = ifelse(or_EP > 0.9, "H", 
                       ifelse(or_EP < 0.1, "L", NA)),
         out = factor(ifelse(is.na(LISA) & !is.na(lisa), 
                             lisa, as.character(LISA)),
                      levels = c("HH", "H", "L", "LL")),
         model = getRFFullNames(model),
         ra_sa2 = fct_relevel(ra_sa2, "Major Cities")) %>% 
  filter(!is.na(out)) %>% 
  ggplot()+theme_bw()+
  geom_bar(aes(x = out, fill = ra_sa2), position = position_dodge2())+
  facet_wrap(.~model)+
  labs(fill = "",
       x = "",
       y = "")+
  theme(legend.position = "bottom")+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  addRemotenessColor()

# save object
jsave(filename = paste0("barchart_lisa_ra.png"), 
      base_folder = paste0(base_folder, "/figures"),
      square = F, ratio = 9:6)

# cities on yaxis instead
summsa2all %>% 
  mutate(lisa = ifelse(or_EP > 0.9, "H", 
                       ifelse(or_EP < 0.1, "L", NA)),
         out = factor(ifelse(is.na(LISA) & !is.na(lisa), 
                             lisa, as.character(LISA)),
                      levels = c("HH", "H", "L", "LL")),
         model = getRFFullNames(model),
         ra = factor(ifelse(ra_sa2_3c == "Outer regional to very remote", "Outer regional\nto very remote", 
                     as.character(ra_sa2_3c)),
         levels = c("Major Cities",
                    "Inner Regional",
                    "Outer regional\nto very remote"))) %>% 
  filter(!is.na(out)) %>% 
  ggplot()+theme_bw()+
  geom_bar(aes(y = ra, fill = out), position = position_dodge2())+
  facet_wrap(.~model)+
  labs(fill = "",
       x = "",
       y = "")+
  theme(legend.position = "bottom")+
  scale_fill_manual(values = c("red", "coral", "skyblue", "royalblue"),
                    breaks = c("HH", "H", "L", "LL"))

# save object
jsave(filename = paste0("barchart_lisa_ra2.png"), 
      base_folder = paste0(base_folder, "/figures"),
      square = F, ratio = 9:6)

# Socioeconomic status
summsa2all %>% 
  mutate(lisa = ifelse(or_EP > 0.9, "H", 
                       ifelse(or_EP < 0.1, "L", NA)),
         out = factor(ifelse(is.na(LISA) & !is.na(lisa), lisa, as.character(LISA)),
                      levels = c("HH", "H", "L", "LL")),
         model = getRFFullNames(model)) %>% 
  filter(!is.na(out)) %>% 
  ggplot()+theme_bw()+
  geom_bar(aes(x = out, fill = irsd_5c), position = position_dodge2())+
  facet_wrap(.~model)+
  labs(fill = "",
       x = "",
       y = "")+
  theme(legend.position = "bottom")+
  addIRSDColor()


# save object
jsave(filename = paste0("barchart_lisa_irsd.png"), 
      base_folder = paste0(base_folder, "/figures"),
      square = F, ratio = 9:6)

## Point estimate ranges ## ----------------------------------------------------

summsa2all %>% 
  group_by(model, ra_sa2_3c, irsd_5c) %>% 
  summarise(variance = var(mu_median), .groups = "drop") %>%
  mutate(model = getRFFullNames(model),
         ra_sa2_3c = fct_relevel(ra_sa2_3c, "Major Cities")) %>% 
  ggplot(aes(x = variance, y = ra_sa2_3c, col = irsd_5c))+
  theme_bw()+
  geom_jitter()+
  facet_wrap(.~model)+
  labs(y = "",
       col = "",
       x = "Variance of modelled point estimates")+
  theme(legend.position = "bottom")

# save object
jsave(filename = paste0("variance_irsdra.png"), 
      base_folder = paste0(base_folder, "/figures"),
      square = F)

## END SCRIPT #### -------------------------------------------------------------