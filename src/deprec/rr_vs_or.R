
smoking_b1_full <- readRDS("C:/r_proj/ACAriskfactors/data/summary_files/smoking_b1_full.rds")


foo <- function(which_full, ps_area, rf){
SA2_selected = global_obj$area_concor[global_obj$area_concor$ps_area == ps_area,]$SA2
data.frame(rr = which_full$draws$mu[,ps_area]/raw_est[[rf]]$national[1],
           or = which_full$draws$or[,ps_area]) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(x = value, col = name))+theme_bw()+
  geom_density()+
  #xlim(0,30)+
  geom_vline(xintercept = 1)+
  labs(subtitle = paste0("National average: ", round(raw_est[[rf]]$national[1], 3)),
       title = paste0("Posterior proportion median for SA2: ", SA2_selected, " is ", round(median(which_full$draws$mu[,ps_area]), 3)))
}

foo(obesity_b1_full, 343, "obesity")




# More 
getIT <- function(aust.prev){
  aust.odds <- aust.prev/(1-aust.prev)
  adj_amt <- 0.5-aust.prev
  signn <- sample(c(-1,1), 6000, replace = T)
  draws.prev <- rnorm(6000, aust.prev + signn*0.1, 0.04)
  draws.prev <- ifelse(draws.prev > 1, 0.99, draws.prev)
  draws.prev <- ifelse(draws.prev < 0, 0.01, draws.prev)
  #draws.rr <- (draws.prev + adj_amt)/(aust.prev + adj_amt)
  draws.rr <- draws.prev/aust.prev
  draws.odds <- draws.prev/(1-draws.prev)
  draws.or = draws.odds/aust.odds
  
  data.frame(
    OR_p = median(draws.or),
    lOR_p = median(log2(draws.or)),
    OR_sd = sd(draws.or),
    RR_p = median(draws.rr),
    lRR_p = median(log2(draws.rr)),
    RR_sd = sd(draws.rr)
  )
}

# for loop
grid <- data.frame(aust.prev = rep(seq(0.05, 0.95, 0.01), 100))
out <- list()
pb <- txtProgressBar(min = 0, max = nrow(grid), style = 3)
for(i in 1:nrow(grid)){
  out[[i]] <- getIT(grid$aust.prev[i])
  setTxtProgressBar(pb, i)
}
close(pb)
data <- bind_cols(grid, bind_rows(out))
data %>% 
  ggplot(aes(RR_p, OR_p))+
  geom_point()+
  geom_abline()+
  facet_grid(.~cut_width(aust.prev, 0.1))
with(data, plot(RR_p, OR_p)); abline(a=0,b=1)

with(data, plot(aust.prev, OR_p))
with(data, plot(aust.prev, lOR_p))
with(data, plot(aust.prev, RR_p))
with(data, plot(aust.prev, lRR_p))
