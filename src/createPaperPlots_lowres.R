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
  xlim(0,1)+
  theme(text = element_text(size = 8))

# save object
jsave(filename = paste0("summary_violin.png"), 
      base_folder = paste0(base_folder, "/figures_lowres"),
      square = F,
      square_size = 1200,
      dpi = 300)
jsave(filename = "fig4.jpeg", 
      base_folder = paste0(base_folder, "/paper"),
      square = F,
      square_size = 1200,
      dpi = 300)
jsave(filename = "fig4_300dpi.tiff", 
      base_folder = paste0(base_folder, "/paper"),
      square = F,
      square_size = 1200,
      dpi = 300)
jsave(filename = "fig4_500dpi.tiff", 
      base_folder = paste0(base_folder, "/paper"),
      square = F,
      square_size = 1200,
      scale = 1.67,
      dpi = 500)
  
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
    ylim(0,1)+xlim(0,1)+
    theme(text = element_text(size = 8))
  
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
    theme(legend.position = "none",
          text = element_text(size = 8))

## Full plot
lay <- rbind(c(1,1,3),
             c(1,1,3),
             c(2,2,3),
             c(2,2,3))
full_plt <- arrangeGrob(grobs = list(ob_plt, sm_plt, llegend), 
                        layout_matrix  = lay)

# save object
jsave(filename = paste0("scattersha_smokingobesity.png"), 
      base_folder = paste0(base_folder, "/figures_lowres"),
      plot = full_plt,
      square = F,
      square_size = 1200,
      dpi = 300)

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
  ylim(0,1)+xlim(0,1)+
  theme(text = element_text(size = 8))

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
  theme(legend.position = "none",
        text = element_text(size = 8))

## Full plot
lay <- rbind(c(1),
             c(2))
full_plt <- arrangeGrob(grobs = list(ob_plt, sm_plt), 
                        layout_matrix  = lay)

# save object
jsave(filename = paste0("scattersha_smokingobesity_pha.png"), 
      base_folder = paste0(base_folder, "/figures_lowres"),
      plot = full_plt,
      square = T,
      square_size = 1200,
      dpi = 300)
jsave(filename = "fig9.jpeg", 
      base_folder = paste0(base_folder, "/paper"),
      plot = full_plt,
      square = T,
      square_size = 1200,
      dpi = 300)
jsave(filename = "fig9.tiff", 
      base_folder = paste0(base_folder, "/paper"),
      plot = full_plt,
      square = T,
      square_size = 1200,
      scale = 1.67,
      dpi = 500)

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
       y = "")+
  theme(text = element_text(size = 8))
  
# save object
jsave(filename = paste0("benchmarkcomp.png"), 
      base_folder = paste0(base_folder, "/figures_lowres"),
      square = F,
      square_size = 1200,
      dpi = 300)
jsave(filename = "fig10.jpeg", 
      base_folder = paste0(base_folder, "/paper"),
      square = F,
      square_size = 1200,
      dpi = 300)
jsave(filename = "fig10.tiff", 
      base_folder = paste0(base_folder, "/paper"),
      square = F,
      square_size = 1200,
      scale = 1.67,
      dpi = 500)

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
  theme(legend.position = "bottom",
        text = element_text(size = 8))

# save object
jsave(filename = paste0("variance_irsdra.png"), 
      base_folder = paste0(base_folder, "/figures_lowres"),
      square = F,
      square_size = 1200,
      dpi = 300)

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
       fill = "")+
  theme(text = element_text(size = 8))

# save object
jsave(filename = paste0("benchmark_CV.png"), 
      base_folder = paste0(base_folder, "/figures_lowres"),
      square = F,
      square_size = 1200,
      dpi = 300)


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
  labs(y = "")+
  theme(text = element_text(size = 8))

# save object
jsave(filename = paste0("pc1_vs_irsd.png"), 
      base_folder = paste0(base_folder, "/figures_lowres"),
      square = F,
      square_size = 1200,
      dpi = 300)

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
                    breaks = c("<10%","10%-20%", ">=20%"))+
  theme(text = element_text(size = 8))

# save object
jsave(filename = paste0("boxplot_fna.png"), 
      base_folder = paste0(base_folder, "/figures_lowres"),
      square = F,
      square_size = 1200,
      dpi = 300)

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

## RSE vs ERP ## ---------------------------------------------------------------

summsa2all %>% 
  mutate(ra_sa2_3c = fct_relevel(ra_sa2_3c, "Major Cities")) %>% 
  ggplot(aes(y = rr_CV_b, log(N_persons), col = ra_sa2_3c))+
  theme_bw()+
  geom_point()+
  facet_wrap(.~model)+
  geom_hline(yintercept = 50)+
  labs(y = "Bayesian RSE for Relative Ratios",
       x = "log(ERP)",
       col = "")+
  theme(legend.position = "bottom")

jsave(filename = "rse vs pop.png", 
      base_folder = paste0(base_folder, "/figures_lowres"),
      square = T,
      square_size = 1200,
      dpi = 300)

## QLD vs National ## ----------------------------------------------------------

# Write function to derive columns for direct estimate table
column_of_direct <- function(i){
  temp_list <- raw_est[[i]]
  rf <- names(raw_est)[i]
  
  temp_data <- bind_rows(data.frame(state_name = "Australia", 
                                    point = 100 * temp_list$national[1],
                                    lower = 100 * (temp_list$national[1] - 1.96 * sqrt(temp_list$national[2])),
                                    upper = 100 * (temp_list$national[1] + 1.96 * sqrt(temp_list$national[2]))),
                         (left_join(temp_list$state, state_name_concor, by = "ps_state") %>% 
                            filter(state_name_short == "QLD")) %>% 
                           mutate(point = 100 * HT, 
                                  lower = 100 * (HT - 1.96 * HT_SE),
                                  upper = 100 * (HT + 1.96 * HT_SE)) %>% 
                           dplyr::select(state_name, point, lower, upper)) %>% 
    mutate(rf = rf)
  
  return(temp_data)
}

# Return all colunns and make large table
ll <- bind_rows(lapply(1:8, column_of_direct))

# create plot
ll %>% 
  mutate(rf = getRFFullNames(rf)) %>% 
  ggplot(aes(x = point, xmin = lower, xmax = upper,
             y = rf, col = state_name))+
  theme_bw()+
  geom_errorbar(position=position_dodge(width=0.5))+
  geom_point(position=position_dodge(width=0.5))+
  #geom_pointrange(position=position_dodge(width=0.5))+
  labs(col = "",
       x = "Prevalence (%)", 
       y = "")+
  theme(text = element_text(size = 8),
        legend.position = "bottom")+
  xlim(0,100)
jsave(filename = "qld vs national direct.png", 
      base_folder = paste0(base_folder, "/figures_lowres"),
      square = F,
      square_size = 1200,
      dpi = 300)

## END SCRIPT #### -------------------------------------------------------------