# New population from CCQ - email on the 20230831

s9_to_s5 <- function(x){paste0(str_sub(x, 1, 1), str_sub(x, start = -4))}

Aust_pop9620_16ASGS_90pl <- read_dta("~/OneDrive - Queensland University of Technology/DataLab/Requested file load/Aust_pop9620_16ASGS_90pl.dta")
global_obj <- readRDS("C:/r_proj/ACAriskfactors/data/DataLabExport/global_obj.rds")

newpop <- Aust_pop9620_16ASGS_90pl %>% 
  dplyr::select(-state) %>% 
  filter(sex != 3,
         year %in% c(2017, 2018),
         !ragegrp %in% c(1,2,3)) %>%
  pivot_wider(values_from = pop, names_from = sex) %>% 
  group_by(sa2, sa2name, ragegrp) %>% 
  summarise(`1` = mean(`1`),
            `2` = mean(`2`), 
            .groups = "drop") %>% 
  mutate(N_persons = `1`+`2`) %>% 
  group_by(sa2, sa2name) %>% 
  summarise(N = sum(N_persons)) %>% 
  arrange(sa2)

oldpop <- global_obj$census %>% 
  mutate(SA2 = ifelse(SA2 == 114011271, 901031003, SA2),
         SA2_5d = as.numeric(s9_to_s5(SA2))) %>% 
  dplyr::select(SA2_5d, N_persons) %>% 
  arrange(SA2_5d)

## join
all <- full_join(newpop, oldpop, by = c("sa2" = "SA2_5d"))
cor(all[,c(3,4)], use = "complete.obs")
