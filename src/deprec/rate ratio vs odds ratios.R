library(ggplot2)
library(dplyr)
library(tidyr)

lab_prev <- 
  function (labels, multi_line = TRUE) {
    value <- label_value(labels, multi_line = multi_line)
    if (multi_line) {
      out <- vector("list", length(value))
      for (i in seq_along(out)) {
        out[[i]] <- paste0("Prevalence: ", value[[i]])
      }
    }
    else {
      value <- inject(paste(!!!value, sep = ", "))
      out <- Map(paste, "Prevalence: ", value, sep = sep)
      out <- list(unname(unlist(out)))
    }
    out
  }

data.frame(
    aust.prev = rep(c(0.15, 0.4, 0.5, 0.6, 0.85), each = 2150),
    Odds.Ratio = rep(exp(rnorm(2150)), 5)) %>%
  mutate(aust.odds = aust.prev / (1 - aust.prev),
         area.odds = Odds.Ratio * aust.odds,
         area.prev = area.odds / (1 + area.odds),
         Rate.Ratio = area.prev / aust.prev) %>%
  select("Odds.Ratio", "Rate.Ratio", "aust.prev") %>% 
  gather(key, val, -aust.prev) %>% # head()
  ggplot(aes(x = val, group = key, colour = key, fill = key)) +
  geom_vline(xintercept = 1) +
  geom_density(alpha = 0.2) +
  scale_x_continuous("Ratio", trans = "log2", 
                     breaks = 2^c(-4, -2, 0, 2, 4),
                     labels = 2^c(-4, -2, 0, 2, 4)) +
  facet_wrap(~factor(aust.prev), labeller = lab_prev) +
  theme_bw() 


# Converting from RRs to ORs

# Jamie's version
data.frame(
  aust.prev = rep(c(0.15, 0.4, 0.5, 0.6, 0.85), each = 2150),
  Rate.Ratio = rep(exp(rnorm(2150)), 5)) %>%
  mutate(
    area.prev = Rate.Ratio * aust.prev,
    area.odds = area.prev / (1 - min(area.prev, 0.99)),
    aust.odds = aust.prev / (1 - aust.prev),
    Odds.Ratio = area.odds/aust.odds) %>% 
  select("Odds.Ratio", "Rate.Ratio", "aust.prev") %>% 
  gather(key, val, -aust.prev) %>% # head()
  ggplot(aes(x = val, group = key, colour = key, fill = key)) +
  geom_vline(xintercept = 1) +
  geom_density(alpha = 0.2) +
  scale_x_continuous("Ratio", trans = "log2", 
                     breaks = 2^c(-4, -2, 0, 2, 4),
                     labels = 2^c(-4, -2, 0, 2, 4)) +
  facet_wrap(~factor(aust.prev), labeller = lab_prev) +
  theme_bw() 

# Jess's version
data.frame(
  aust.prev = rep(c(0.15, 0.4, 0.5, 0.6, 0.85), each = 2150),
  Rate.Ratio = rep(exp(rnorm(2150)), 5)) %>%
  mutate(
    area.prev = Rate.Ratio * aust.prev,
    area.odds = area.prev / (1 - min(area.prev, 0.99)),
    aust.odds = aust.prev / (1 - aust.prev),
    Odds.Ratio = area.prev / (1 - min(area.prev, 0.99))) %>% 
  select("Odds.Ratio", "Rate.Ratio", "aust.prev") %>% 
  gather(key, val, -aust.prev) %>% # head()
  ggplot(aes(x = val, group = key, colour = key, fill = key)) +
  geom_vline(xintercept = 1) +
  geom_density(alpha = 0.2) +
  scale_x_continuous("Ratio", trans = "log2", 
                     breaks = 2^c(-4, -2, 0, 2, 4),
                     labels = 2^c(-4, -2, 0, 2, 4)) +
  facet_wrap(~factor(aust.prev), labeller = lab_prev) +
  theme_bw() 


## More experiments
aust.prev <- 0.15; aust.odds <- aust.prev/(1-aust.prev)
draws.prev <- rnorm(6000, aust.prev + 0.04, 0.04)
draws.prev <- ifelse(draws.prev > 1, 0.99, draws.prev)
draws.prev <- ifelse(draws.prev < 0, 0.01, draws.prev)
draws.rr <- draws.prev/aust.prev
draws.odds <- draws.prev/(1-draws.prev)
draws.or = draws.odds/aust.odds

data.frame(
  OR_p = median(draws.or),
  OR_sd = sd(draws.or),
  RR_p = median(draws.rr),
  RR_sd = sd(draws.rr)
)

data.frame(rr = draws.rr, 
           or = draws.or) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(x = value, col = name)) + 
  geom_density()+
  xlim(0, 5)



