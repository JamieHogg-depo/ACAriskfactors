## Get N_persons_adults

# Load data
sa2agepop <- suppressMessages(read_csv("~/OneDrive - Queensland University of Technology/DataLab/Requested file load/ERP/j_ERP_2016-2019.csv") %>% 
  filter(year %in% c(2017,2018)) %>% 
  addGroupID(A_id, age) %>% # remove 1, 2 and 10
  filter(!A_id %in% c("1", "2", "10")) %>% 
  dplyr::select(-age))

# Population for aux - 20 years and above
sa2pop <- sa2agepop %>% 
  group_by(SA2, spec, year) %>% 
  summarise(N = sum(N),
            .groups = "drop") %>% 
  pivot_wider(values_from = N,
              names_from = spec) %>% 
  dplyr::select(-persons) %>% 
  group_by(SA2) %>% 
  summarise(N_males = mean(males),
            N_females = mean(females)) %>% 
  mutate(N_persons_adults = N_males + N_females) %>% 
  filter(N_males > 0, N_females > 0) %>% 
  dplyr::select(SA2, N_persons_adults)

# Population for aux - 20 years and above
sa2popadult <- sa2agepop %>% 
  mutate(N = ifelse(A_id == 3, N*0.4, N)) %>% 
  group_by(SA2, spec, year) %>% 
  summarise(N = sum(N),
            .groups = "drop") %>% 
  pivot_wider(values_from = N,
              names_from = spec) %>% 
  dplyr::select(-persons) %>% 
  group_by(SA2) %>% 
  summarise(N_males = mean(males),
            N_females = mean(females)) %>% 
  mutate(N_persons_adults = N_males + N_females) %>% 
  filter(N_males > 0, N_females > 0) %>% 
  dplyr::select(SA2, N_persons_adults)

# remove objects
rm(sa2agepop)
