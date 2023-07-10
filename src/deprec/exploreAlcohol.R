# Explore alcohol - high rates around Sydney habour

# Sydney areas that are high - 25
some_red_areas <- c("Mosman",
  "Cremorne - Cammeray",
  "Neutral Bay - Kirribilli",
  "North Sydney - Lavender Bay",
  "Crows Nest - Waverton",
  "St Leonards - Naremburn",
  "Lane Cove - Greenwich",
  "Hunters Hill - Woolwich",
  "Gladesville - Huntleys Point",
  "Drummoyne - Rodd Point",
  "Five Dock - Abbotsford",
  "Haberfield - Summer Hill",
  "Lilyfield - Rozelle",
  "Leichhardt - Annandale",
  "Petersham - Stanmore",
  "Potts Point - Woolloomooloo",
  "Darlinghurst",
  "Surry Hills",
  "Paddington - Moore Park",
  "Woollahra",
  "Double Bay - Bellevue Hill",
  "Rose Bay - Vaucluse - Watsons Bay",
  "Manly - Fairlight",
  "Balgowlah - Clontarf - Seaforth",
  "Freshwater - Brookvale")

# Subset data
temp <- alcohol_b1$summ$sa2_map %>% 
  filter(ps_state.y == 1) %>%
  st_drop_geometry() %>% 
  mutate(habour = as.factor(ifelse(SA2_NAME %in% some_red_areas, 1, 0))) %>% 
  relocate(SA2_NAME) %>% 
  left_join(.,ocu, by = "SA2") %>% 
  left_join(.,averages, by = "SA2")

# some plots
temp %>% 
  ggplot(aes(y = PC1, x = habour))+
  geom_boxplot()

# Occuptaion
(temp %>% 
  ggplot(aes(y = Managers, x = habour))+
  geom_boxplot())+
(temp %>% 
  ggplot(aes(y = Professionals, x = habour))+
  geom_boxplot())+
(temp %>% 
    ggplot(aes(y = ClericalAdminis_W, x = habour))+
    geom_boxplot())+
(temp %>% 
   ggplot(aes(y = Sales_W, x = habour))+
   geom_boxplot())

# Professionals 
temp %>% 
  ggplot(aes(y = log(or_EP), x = Professionals, col = habour))+
  geom_point()+
  facet_wrap(.~ra_sa2)

# Other census variables
temp %>% 
  group_by(habour) %>% 
  summarise_at(vars(Median_age_persons:Average_household_size),
               list(median = median, min = min, max = max)) %>% 
  t(.)

# Interaction of income, professionals
temp %>% 
  ggplot(aes(y = or_EP, x = log(Median_tot_prsnl_inc_weekly), col = cut_number(Professionals, 10)))+
  geom_point()+
  facet_wrap(.~habour)

# Interaction of income, professionals
temp %>% 
  filter(ra_sa2 == "Major Cities") %>% 
  ggplot(aes(x = or_EP, y = cut_number(Median_tot_prsnl_inc_weekly, 10), fill = cut_number(Professionals, 10)))+
  geom_boxplot()+
  facet_grid(.~habour)

## for all Australia ## --------------------------------------------------------

# professsionals
alcohol_b1$summ$sa2 %>% 
  left_join(.,ocu, by = "SA2") %>% 
  mutate(upper_prof = as.factor(ifelse(Professionals > 35, "High 10", "Low")),
         PC1_high = as.factor(ifelse(PC1 > 1.336, "High 10", "low"))) %>% 
  group_by(PC1_high, upper_prof) %>% 
  tally()

# income
alcohol_b1$summ$sa2 %>% 
  left_join(.,ocu, by = "SA2") %>% 
  left_join(.,averages, by = "SA2") %>% 
  mutate(upper_prof = as.factor(ifelse(Professionals > 35, "High 10", "Low")),
         PC1_high = as.factor(ifelse(PC1 > 1.336, "High 10", "low"))) %>%
  ggplot(aes(y = Median_tot_prsnl_inc_weekly, x = upper_prof))+
  geom_boxplot()

# Interaction of income, professionals
alcohol_b1$summ$sa2 %>% 
  left_join(.,ocu, by = "SA2") %>% 
  left_join(.,averages, by = "SA2") %>% 
  filter(ra_sa2 == "Major Cities") %>% 
  ggplot(aes(x = mu_median, y = cut_number(Median_tot_prsnl_inc_weekly, 5), fill = cut_number(Professionals, 5)))+
  geom_boxplot()

# LISA
alcohol_b1$summ$sa2 %>% 
  left_join(.,ocu, by = "SA2") %>% 
  ggplot(aes(y = Professionals, x = log(or_EP)))+
  geom_point()

# interactive map
alcohol_b1$summ$sa2_map %>% 
  left_join(.,ocu, by = "SA2") %>% 
  left_join(.,averages, by = "SA2") %>% 
  mutate(upper_prof = as.factor(ifelse(Professionals > 35, "High 10", "Low"))) %>% 
  tm_shape(.)+
  tm_polygons(col = "Median_tot_prsnl_inc_weekly")


