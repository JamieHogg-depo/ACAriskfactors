## -----------------------------------------------------------------------------
## FIGURES ## ------------------------------------------------------------------
## -----------------------------------------------------------------------------

# SA4 level - HT vs modeled ####
b_est$summ_mu_sa4 %>% 
  ggplot(aes(y = median, ymin = lower, ymax = upper,
             x = HT, xmin = HT_lower, xmax = HT_upper))+
  theme_bw()+geom_abline(col="red")+
  geom_errorbar(col = "grey")+geom_errorbarh(col = "grey")+
  geom_point()+
  facet_grid(.~model)+
  labs(y = "Modelled prevalence estimate",
       x = "Direct prevalence estimate")
if(export) jsave("sa4_directvsmodeled.png", square = F)

# SA4 level - ARB, RRMSE #### 
(b_est$summ_mu_sa4 %>% 
   dplyr::select(ps_sa4, n, RRMSE, model) %>% 
   pivot_wider(names_from = model, values_from = RRMSE) %>% 
   arrange(TSLN) %>% mutate(x = 1:nrow(.)) %>% dplyr::select(-ps_sa4) %>% 
   pivot_longer(-c(x, n)) %>% 
   ggplot(aes(y = 10*value, x = x, color = name))+
   theme_bw()+
   geom_path()+geom_point(aes(size = n))+
   scale_color_manual(values = jcol$color,
                      breaks = jcol$model)+
   labs(y = "RRMSE (x10)", x = "", col = "",
        size = "Sample size")+
   theme(legend.position = "bottom",
         legend.box = "vertical")+
   scale_size_binned(n.breaks = 5))+
  (b_est$summ_mu_sa4 %>% 
     dplyr::select(ps_sa4, n, ARB, model) %>% 
     pivot_wider(names_from = model, values_from = ARB) %>% 
     arrange(TSLN) %>% mutate(x = 1:nrow(.)) %>% dplyr::select(-ps_sa4) %>% 
     pivot_longer(-c(x, n)) %>% 
     ggplot(aes(y = 10*value, x = x, color = name))+
     theme_bw()+
     geom_path()+geom_point(aes(size = n))+
     scale_color_manual(values = jcol$color,
                        breaks = jcol$model)+
     labs(y = "ARB (x10)", x = "", col = "",
          size = "Sample size")+
     theme(legend.position = "bottom",
           legend.box = "vertical")+
     scale_size_binned(n.breaks = 5))
if(export) jsave("sa4_rrmse_arb.png", square = F)

# What about relative measures??

# Violin plots ####
b_est$summ_mu %>% 
  ggplot(aes(y = model, fill = model, x = median))+
  theme_bw()+
  geom_violin()+
  labs(y = "",
       x = "Modeled prevalence estimate")+
  theme(legend.position = "none")+
  scale_fill_manual(values = jcol$color,
                    breaks = jcol$model)
if(export) jsave("violin.png", square = F)

# Boxplot of posterior medians - IRSD ####
prev_median_wide %>%
  left_join(.,dplyr::select(aux2, ps_area, ABS_irsd_decile_nation, ra_sa2_3c),
            by = "ps_area") %>%
  pivot_longer(-c(ps_area, ABS_irsd_decile_nation, ra_sa2_3c)) %>%
  # rename IRSD categories
  mutate(ABS_irsd_decile_nation = factor(ABS_irsd_decile_nation, levels = 1:10,
                                         labels = c("Least advantaged", 
                                                    as.character(2:9), 
                                                    "Most advantaged"))) %>% 
  ggplot(aes(x = value, fill = name, y = ABS_irsd_decile_nation))+
  theme_bw()+
  geom_boxplot()+
  labs(x = "Modeled prevalence estimate",
       y = "IRSD deciles",
       fill = "")+
  theme(legend.position = "bottom")+
  scale_fill_manual(values = jcol$color,
                    breaks = jcol$model)
if(export) jsave("boxplot_byseifa.png", square = F)

# Boxplot of posterior medians - Remoteness ####
prev_median_wide %>%
  left_join(.,dplyr::select(aux2, ps_area, ABS_irsd_decile_nation, ra_sa2_3c),
            by = "ps_area") %>%
  pivot_longer(-c(ps_area, ABS_irsd_decile_nation, ra_sa2_3c)) %>%
  ggplot(aes(x = value, fill = name, y = ra_sa2_3c))+
  theme_bw()+
  geom_boxplot()+
  #facet_grid(.~ra_sa2_3c, labeller = label_wrap_gen(multi_line = TRUE))+
  labs(x = "Modeled prevalence estimate",
       y = "",
       fill = "")+
  theme(legend.position = "bottom")+
  scale_fill_manual(values = jcol$color,
                    breaks = jcol$model)
if(export) jsave("boxplot_byremoteness.png", square = F)

# Compare estimates from models ####
dplyr::select(b_est$summ_mu, median, lower, upper, model, ps_area) %>% 
  pivot_wider(names_from = model, values_from = c(median, lower, upper)) %>% 
  ggplot(aes(y = median_TSLN, ymin = lower_TSLN, ymax = upper_TSLN,
             x = median_ELN, xmin = lower_ELN, xmax = upper_ELN))+
  theme_bw(base_size = 20)+
  geom_abline(col = "red")+
  geom_hline(yintercept = HT_mu)+
  geom_vline(xintercept = HT_mu)+
  geom_errorbar(col="grey")+
  geom_errorbarh(col="grey")+
  geom_point()+
  xlim(0,1)+ylim(0,1)+
  labs(y = "TSLN model",
       x = "ELN model")
if(export) jsave("scatter_TSLNvsELN.png", square = T)

dplyr::select(b_est$summ_mu, median, lower, upper, model, ps_area) %>% 
  pivot_wider(names_from = model, values_from = c(median, lower, upper)) %>% 
  ggplot(aes(y = median_TSLN, ymin = lower_TSLN, ymax = upper_TSLN,
             x = median_LOG, xmin = lower_LOG, xmax = upper_LOG))+
  theme_bw(base_size = 20)+
  geom_abline(col = "red")+
  geom_hline(yintercept = HT_mu)+
  geom_vline(xintercept = HT_mu)+
  geom_errorbar(col="grey")+
  geom_errorbarh(col="grey")+
  geom_point()+
  xlim(0,1)+ylim(0,1)+
  labs(y = "TSLN model",
       x = "LOG model")
if(export) jsave("scatter_TSLNvsLOG.png", square = T)

dplyr::select(b_est$summ_mu, median, lower, upper, model, ps_area) %>% 
  pivot_wider(names_from = model, values_from = c(median, lower, upper)) %>% 
  ggplot(aes(y = median_ELN, ymin = lower_ELN, ymax = upper_ELN,
             x = median_LOG, xmin = lower_LOG, xmax = upper_LOG))+
  theme_bw(base_size = 20)+
  geom_abline(col = "red")+
  geom_hline(yintercept = HT_mu)+
  geom_vline(xintercept = HT_mu)+
  geom_errorbar(col="grey")+
  geom_errorbarh(col="grey")+
  geom_point()+
  xlim(0,1)+ylim(0,1)+
  labs(y = "ELN model",
       x = "LOG model")
if(export) jsave("scatter_ELNvsLOG.png", square = T)

# Caterpillar plots - by model - with CI  #### ---------------------------------

# Prevalence
b_est$summ_mu %>% 
  group_by(model) %>% 
  mutate(rank = rank(median, ties.method = "first")) %>%
  ggplot(aes(y = median, ymin = lower, ymax = upper,
             x = rank, 
             col = median))+theme_bw()+
  geom_errorbar(col = "grey")+
  geom_point()+
  facet_wrap(.~model)+
  scale_color_viridis_c(begin = 0.3, end = 1, 
                        direction = 1,
                        option = "B")+
  geom_hline(yintercept = HT_mu)+
  labs(y = "Modeled prevalence estimates",
       x = "")+
  theme(legend.position = "none")
if(export) jsave("cat_prev_medianci.png", square = F)

## END SCRIPT ## --------------------------------------------------------------