## -----------------------------------------------------------------------------
## FIGURES ## ------------------------------------------------------------------
## -----------------------------------------------------------------------------

source("src/ms.R")

## Violin plots ## -------------------------------------------------------------

ll <- list()
for(k in 1:8){
  ll[[k]] <- data.frame(model = names(raw_est)[k],
                        point = raw_est[[k]]$national[1],
                        lower = raw_est[[k]]$national[1] - 1.96*sqrt(raw_est[[k]]$national[2]),
                        upper = raw_est[[k]]$national[1] + 1.96*sqrt(raw_est[[k]]$national[2]))
}
nat_di <- bind_rows(ll) %>% mutate(model = getRFFullNames(model))

# proportion
summsa2all %>% 
  mutate(model = getRFFullNames(model)) %>% 
  ggplot(aes(x = mu_median, y = model))+
  theme_bw()+
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  geom_point(data = nat_di, aes(x = point, y = model), col = "blue")+
  labs(y = "", 
       x = "Posterior medians")+
  xlim(0,1)

# save object
jsave(filename = paste0("summary_violin.png"), 
      base_folder = paste0(base_folder, "/figures"),
      square = F)
  
## Scatter: Two-way SHA PHA vs ACA  - smoking and obesity - SA2 #### -----------
  
  sm <- readRDS(file = paste0("data/summary_files/smoking_b1.rds"))
  ob <- readRDS(file = paste0("data/summary_files/obesity_b1.rds"))
  
## Smoking
  sm_plt <- sm$summ$sa2 %>% 
    left_join(.,SHA_pha, by = "pha") %>% 
    left_join(.,irsd_5c, by = "ps_area") %>% 
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
    left_join(.,irsd_5c, by = "ps_area") %>% 
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
         title = "Obese",
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

## Scatter: Two-way SHA PHA vs ACA  - smoking and obesity - PHA #### -----------

## Smoking
sm_plt <- sm$summ$pha %>% 
  left_join(.,SHA_pha, by = "pha") %>% 
  ggplot(aes(x = median, xmin = lower, xmax = upper, 
             y = shaout_smoking_estimate, 
             ymin = shaout_smoking_lower, 
             ymax = shaout_smoking_upper))+
  theme_bw()+
  geom_errorbar(col = "grey")+
  geom_errorbarh(col = "grey")+
  geom_abline()+
  geom_point()+
  labs(y = "SHAA",
       title = "Current smoking",
       x = "Our estimates")+
  ylim(0,1)+xlim(0,1)

## Obesity
ob_plt <- ob$summ$pha %>% 
  left_join(.,SHA_pha, by = "pha") %>% 
  ggplot(aes(x = median, xmin = lower, xmax = upper, 
             y = shaout_obese_estimate, 
             ymin = shaout_obese_lower, 
             ymax = shaout_obese_upper))+
  theme_bw()+
  geom_errorbar(col = "grey")+
  geom_errorbarh(col = "grey")+
  geom_abline()+
  geom_point()+
  labs(y = "SHAA",
       title = "Obese",
       x = "Our estimates")+
  ylim(0,1)+xlim(0,1)+
  theme(legend.position = "none")

## Full plot
lay <- rbind(c(1),
             c(2))
full_plt <- arrangeGrob(grobs = list(ob_plt, sm_plt), 
                        layout_matrix  = lay)

# save object
jsave(filename = paste0("scattersha_smokingobesity_pha.png"), 
      base_folder = paste0(base_folder, "/figures"),
      plot = full_plt,
      square = T)

# cleanup
rm(full_plt, sm_plt, ob_plt, sm, ob)

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

## Non-benchmarked areas #### --------------------------------------------------

summsa2all %>% 
  left_join(.,state_name_concor, by = "ps_state") %>% 
  mutate(bench = factor(ifelse((ra_sa2 != "Very Remote" & 
                                  state_name != "Northern Territory"),
                        "Benchmarked", "Non\nbenchmarked"),
                        levels = c("Non\nbenchmarked", "Benchmarked")),
         model = getRFFullNames(model)) %>% 
  group_by(model) %>% 
  mutate(cv_c = cut_number(mu_CV, 10, labels = FALSE)*10) %>%
  ungroup() %>% 
  ggplot(aes(y = cv_c, fill = bench))+
  theme_bw()+
  geom_bar(position = "fill")+
  facet_wrap(.~model)+
  labs(y = "Coefficient of Variation (Percentiles)",
       x = "",
       fill = "")

# save object
jsave(filename = paste0("benchmark_CV.png"), 
      base_folder = paste0(base_folder, "/figures"),
      square = F)


## PC1 vs IRSD #### ------------------------------------------------------------

global_obj$census %>% 
  mutate(pc1_c = cut_number(PC1, 5, labels = FALSE),
         ABS_irsd_decile_nation_complete = factor(ABS_irsd_decile_nation_complete, 
                                                  levels = 1:10,
                                                  labels = c("Most\ndisadvantaged", 
                                                             as.character(2:9), 
                                                             "Least\ndisadvantaged"))) %>% 
  ggplot(aes(x = PC1, y = ABS_irsd_decile_nation_complete))+
  theme_bw()+
  geom_violin()+
  labs(y = "")

# save object
jsave(filename = paste0("pc1_vs_irsd.png"), 
      base_folder = paste0(base_folder, "/figures"),
      square = F)

## Indigenous status #### ------------------------------------------------------

summsa2all %>% 
  left_join(.,indig, by = "SA2") %>% 
  mutate(over20 = factor(case_when(
    Indigenous >= 20 ~ ">=20%",
    Indigenous >=10 &  Indigenous<20 ~ "10%-20%",
    Indigenous <10 ~ "<10%"),
    levels = c("<10%","10%-20%", ">=20%")),
    model = getRFFullNames(model)) %>% 
  filter(!is.na(over20)) %>% 
  ggplot(aes(x = mu_median,
             fill = over20,
             y = model))+
  theme_bw()+
  geom_violin(draw_quantiles = c(0.25,0.5,0.75))+
  labs(y = "",
       x = "Posterior medians",
       fill = "Proportion of\nFirst Nations\nAustralians")+
  scale_y_discrete(limits=rev) + 
  scale_fill_manual(values = c("skyblue", "royalblue", "purple"),
                    breaks = c("<10%","10%-20%", ">=20%"))

# save object
jsave(filename = paste0("boxplot_fna.png"), 
      base_folder = paste0(base_folder, "/figures"),
      square = F)

## Correlation plot #### -------------------------------------------------------
library(corrplot)

# For major cities
cor_data <- summsa2all %>% 
  filter(ra_sa2_3c == "Major Cities") %>% 
  dplyr::select(ps_area, model, mu_median) %>% 
  pivot_wider(names_from = model, values_from = mu_median) %>% 
  dplyr::select(-c(ps_area, activityleis, waist_circum, obesity))

res <- cor(cor_data)
res
corrplot(res, type = "upper", 
         tl.col = "black", tl.srt = 45)

# For outer regional
cor_data <- summsa2all %>% 
  filter(ra_sa2_3c == "Inner Regional") %>% 
  dplyr::select(ps_area, model, mu_median) %>% 
  pivot_wider(names_from = model, values_from = mu_median) %>% 
  dplyr::select(-c(ps_area, activityleis, waist_circum, obesity))

res <- cor(cor_data)
res
corrplot(res, type = "upper", 
         tl.col = "black", tl.srt = 45)

# For major cities
cor_data <- summsa2all %>% 
  filter(ra_sa2_3c == "Outer regional to very remote") %>% 
  dplyr::select(ps_area, model, mu_median) %>% 
  pivot_wider(names_from = model, values_from = mu_median) %>% 
  dplyr::select(-c(ps_area, activityleis, waist_circum, obesity))

res <- cor(cor_data)
res
corrplot(res, type = "upper", 
         tl.col = "black", tl.srt = 45)

## END SCRIPT #### -------------------------------------------------------------