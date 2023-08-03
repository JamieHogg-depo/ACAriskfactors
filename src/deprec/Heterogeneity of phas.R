
summsa2all %>% 
  group_by(model, pha) %>% 
  mutate(mu = mean(mu_median)) %>% 
  ungroup() %>% 
  group_by(model, pha) %>%
  summarise(n = n(),
            MAD = mean(abs(mu - mu_median)),
            .groups = "drop") %>% 
  filter(n > 1) %>% 
  ggplot(aes(x = MAD, y = model))+
  geom_boxplot()+
  facet_wrap(.~n)

summsa2all %>% 
  group_by(model, pha) %>% 
  mutate(mu = mean(mu_median)) %>% 
  ungroup() %>% 
  group_by(model, pha, ra_sa2) %>%
  summarise(n = n(),
            MAD = mean(abs(mu - mu_median)),
            .groups = "drop") %>% 
  filter(n > 1) %>% 
  group_by(model, n, ra_sa2) %>% 
  summarise(MMAD = median(MAD)) %>% 
  ggplot(aes(y = MMAD, x = n, col = model, group = model))+
  geom_point()+
  geom_line()+
  facet_grid(.~ra_sa2)

  ggplot(aes(x = MAD, y = model))+
  geom_boxplot()+
  facet_wrap(.~n)

anova(lm(mu_median ~ as.factor(pha), data = summsa2all[summsa2all$model == "smoking",]))


gr1 <- rnorm(1000, 0, 1)
gr2 <- rnorm(500, 0, 4)
gr3 <- rnorm(200, 0, 0.3)
out <- data.frame(x = c(gr1, gr2, gr3),
           cat = c(rep("group1", 1000),
                   rep("group2", 500),
                   rep("group3", 200)))
ggplot(out, aes(y = x, x = cat))+geom_boxplot()
anova(lm(x ~ cat, data = out))
# rejecting means more variance between groups than within
# rejecting more more hetergeneity between phas than within them


# Example of high variance in major cities
summsa2all %>% 
  group_by(model, pha) %>% 
  mutate(mu = mean(mu_median),
         MAD = mean(abs(mu - mu_median)),
         number_phas = n()) %>% 
  ungroup() %>% 
  relocate(MAD, mu, number_phas, pha, ra_sa2) %>% 
  filter(number_phas > 4, #ra_sa2 == "Major Cities",
         MAD > 0.1) %>% 
  view()
# pha 30018 alcohol
# pha 80012 obesity

# Creat pHA MAP ## -------------------------------------------------------------

mod_re <- readRDS("data/summary_files/smoking_b1.rds")
pha_map <- suppressMessages(mod_re$summ$sa2_map %>% 
                              group_by(pha) %>% 
                              summarise(geometry = st_union(geometry)))
W <- nb2mat(poly2nb(pha_map, queen = T), zero.policy = T, style = "B")

## Alcohol - pha 30018 ## ------------------------------------------------------

# smoking 30213

#pha_code <- 10030 #80024 #30018
pha_code <- c(30213, 30183)
phas_to_map <- c(pha_code, pha_map$pha[which(W[which(pha_map$pha == pha_code),] == 1)])
phas_to_map <- phas_to_map[!is.na(phas_to_map)]

# some new datasets and objects
pha_estimates <- mod_re$summ$pha %>% 
  dplyr::select(pha, median, cisize) %>% 
  filter(pha %in% phas_to_map) %>% 
  left_join(.,pha_map) %>% 
  st_as_sf()
sa2_estimates <- mod_re$summ$sa2_map %>% 
  filter(pha %in% phas_to_map)
lims_mu <- range(c(sa2_estimates$mu_median, pha_estimates$median))
lims_u <- range(c(pha_estimates$cisize, sa2_estimates$mu_cisize))

# SA2 plot
sa2_plot <- sa2_estimates %>% 
  ggplot(aes(fill = mu_median, geometry = geometry))+
  geom_sf()+
  scale_fill_viridis_c(direction = -1,
                       option = "F", 
                       limit = lims_mu)+
  theme_void()+
  labs(title = "SA2",
       fill = "Prevalence")+
  theme(text = element_text(size = 8),
        plot.title = element_text(margin = margin(0,0,2,0)),
        plot.margin = unit(c(1,1,1,1), "mm"))
llegend_pp <- ggpubr::get_legend(sa2_plot)
sa2_plot <- sa2_plot + theme(legend.position = "none")
sa2_plotu <- sa2_estimates %>% 
  ggplot(aes(fill = mu_cisize, geometry = geometry))+
  geom_sf()+
  scale_fill_viridis_c(direction = -1,
                       option = "D", 
                       limit = lims_u)+
  theme_void()+
  labs(fill = "Width of\n95% HPDI")+
  theme(text = element_text(size = 8),
        plot.title = element_text(margin = margin(0,0,2,0)),
        plot.margin = unit(c(1,1,1,1), "mm"))
llegend_ci <- ggpubr::get_legend(sa2_plotu)
sa2_plotu <- sa2_plotu + theme(legend.position = "none")

# PHA plot
pha_plot <- pha_estimates %>% 
  ggplot(aes(fill = median, geometry = geometry))+
  geom_sf()+
  scale_fill_viridis_c(direction = -1,
                       option = "F", 
                       limit = lims_mu)+
  theme_void()+
  labs(title = "PHA")+
  theme(legend.position = "none")
pha_plotu <- pha_estimates %>% 
  ggplot(aes(fill = cisize, geometry = geometry))+
  geom_sf()+
  scale_fill_viridis_c(direction = -1,
                       option = "D", 
                       limit = lims_u)+
  theme_void()+
  theme(legend.position = "none")

# population
pop_plot <- sa2_estimates %>% 
  ggplot(aes(fill = log(N_persons), geometry = geometry))+
  geom_sf()+
  scale_fill_viridis_c(option = "A",
                       direction = -1)+
  theme_void()+
  labs(title = "Log population",
       fill = "")

# Combine plots
lay <- rbind(c(1,1,1,2,2,2,3),
             c(1,1,1,2,2,2,3),
             c(1,1,1,2,2,2,3),
             c(4,4,4,5,5,5,6),
             c(4,4,4,5,5,5,6),
             c(4,4,4,5,5,5,6))
full_inset_plt <- arrangeGrob(grobs = list(sa2_plot, pha_plot, llegend_pp, 
                                           sa2_plotu, pha_plotu, llegend_ci), 
                              layout_matrix  = lay)
jsave(filename = paste0("what.png"), 
      base_folder = paste0(base_folder, "/maps"),
      plot = full_inset_plt, square = F)
  



