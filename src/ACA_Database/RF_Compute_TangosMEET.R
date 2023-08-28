#==========================================================================
# Derive Tango's MEET for Risk factor data
#        
# Authors: Jamie Hogg
# Updated: 28/08/23
#          
#==========================================================================

# Packages
library(tidyverse)
library(readr)
library(readxl)
library(sf)
library(openxlsx)
rm(list = ls())

# function to convert 9DIG to 5DIG
s9_to_s5 <- function(x){paste0(str_sub(x, 1, 1), str_sub(x, start = -4))}

# tango function
source("src/ACA_Database/tangoFunction.R")

## Data ## ---------------------------------------------------------------------

# Load SA2 concordance file
SA2_concordance <- read_csv("data/ViseR_Input_Data/Shapefile concordance.csv")

# Load SA2 long_lat
SA2_lat_lon <- read_csv("src/ACA_Database/fromJess/SA2_2016_Aust_lat_long.csv")

# Load adult pop
SA2_ERP <- read_csv("data/DataLabExport/SA2_ERP.csv", col_types = cols(X1 = col_skip())) %>% 
  dplyr::select(SA2, N_persons_adults)

# Load global data
global_obj <- readRDS("data/DataLabExport/global_obj.rds")
global_obj$census <- left_join(global_obj$census, SA2_ERP)

# Fix jervis bay SA2 and get 5dig sa2s
area_concor <- global_obj$area_concor %>% 
    # recode Jervis Bay
  mutate(SA2 = ifelse(SA2 == 114011271, 901031003, SA2),
         SA2_5d = as.numeric(s9_to_s5(SA2))) %>% 
  full_join(.,SA2_concordance, by = c("SA2_5d" = "SA2_Code_Short")) %>% 
  filter(!SA2_NAME16 %in% c("Norfolk Island", "Lord Howe Island", "Christmas Island", "Cocos (Keeling) Islands"))

# Load raw estimates
raw_est <- pbapply::pblapply(list.files("data/DataLabExport", 
                                        pattern = "raw_est_*", full.names = T), readRDS)
names(raw_est) <- str_remove( 
  str_remove(
    list.files("data/DataLabExport", pattern = "raw_est_*"), "raw_est_"), ".rds")

# SA2s not modelled
na.sa2 <- as.numeric(area_concor$SA2_5d[is.na(area_concor$ps_area)])
na.sa2name <- area_concor$SA2_NAME16[is.na(area_concor$ps_area)]

# Load all modelled estimates
summsa2all <- readRDS("data/summary_files/summsa2all.rds") %>% 
  mutate(SA2 = ifelse(SA2 == 114011271, 901031003, SA2),
         SA2_5d = as.numeric(s9_to_s5(SA2)))
# all risk factors - long format

# Expected counts
E_list <- list()
for(i in 1:length(raw_est)){
  if(names(raw_est)[i] == "waist_circum"){
    E_list[[i]] <- data.frame(ps_area = global_obj$census$ps_area,
                              model = names(raw_est)[i],
                              e = global_obj$census$N_persons_adults*raw_est[[i]]$national[1])
  }else{
    E_list[[i]] <- data.frame(ps_area = global_obj$census$ps_area,
                              model = names(raw_est)[i],
                              e = global_obj$census$N_persons*raw_est[[i]]$national[1])
  }
}
E_dat <- bind_rows(E_list)

# Join to all data
rf_all <- left_join(summsa2all, E_dat) %>% 
  dplyr::select(ps_area, SA2, model, count_median, e) %>% 
  left_join(.,area_concor) %>% 
  arrange(ID_2238)
# all risk factors - long format

## Derive Tango's MEET ## ------------------------------------------------------

j_verbose <- TRUE

## At the current time all  values in MEET_p are 0.166667

# create empty dataset
MEET_p <- data.frame(combo = names(raw_est),
                     tm1 = NA,
                     tm2 = NA,
                     tm3 = NA)

# Three replicates 
for(y in 1:3){
  
message(paste0("y: ", y))
  
# All risk factors
for(z in MEET_p$combo){
  
message(paste0("RF: ", z))

# filter data to risk factor
rf_fl <- rf_all %>% 
  filter(model == z)
# create new dataset
dat.tango <- data.frame(id = rf_fl$ID_2238[!is.na(rf_fl$ID_2238)],
                        d = rf_fl$count_median[!is.na(rf_fl$ID_2238)],
                        e = rf_fl$e[!is.na(rf_fl$ID_2238)],
                        j = 1) %>% 
  left_join(.,SA2_lat_lon, by = "id") %>% 
  slice(1:100)

#MEET_p[MEET_p$combo == z, paste0("tm", y)] <- acaTangoMeet(dat.tango, nloop = 5, j_verbose = FALSE)

#}
#}

# Code from `2E Tango's MEET`  
  nloop<-999
  nk<- 1
  kvec<- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
  kvec<- kvec*100 #as lambda is supposed to vary up to half the area, and the max. distance is >3963, altered the constant from 2 to 60. 
  
  #
  #
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  nr<-length(dat.tango$id)
  qdat<-rep(0,nr)
  p<-rep(0,nr)
  w<-matrix(0,nr,nr)
  nn<-sum(dat.tango$d)
  R<-6371
  
  kkk<- length(kvec)
  dc<-matrix(0,nr,nr)
  regid<-dat.tango$id
  frq<-matrix(0,nloop,nr)
  pval<-matrix(0,nloop,kkk)
  pxx<-rep(0,nloop)
  pcc<-pxx
  
  if(j_verbose) pb <- txtProgressBar(min = 0, max = nr, style = 3)
  for (i in 1:nr){
    for (j in i:nr){
      #formula 1.10 p.18 in Banerjee et al 2004
      dij <-suppressWarnings(acos(sin(dat.tango$yrad[i])*sin(dat.tango$yrad[j])+cos(dat.tango$yrad[i])*cos(dat.tango$yrad[j])*cos(dat.tango$xrad[i]-dat.tango$xrad[j]))*R)
      if (dij=="NaN") {dij=0} else {dij=dij}; 
      dc[j,i]<- dij
      dc[i,j]<- dij
      #Check largest value
      #which(dc==max(dc), arr.ind=TRUE)
      #dc[1829,238]
    }
    if(j_verbose) setTxtProgressBar(pb, i)
  }
  if(j_verbose) close(pb)
  
  #
  # Calculation of (1) null distribution of test statistic Pmin
  #
  for (j in 1:nk){
    jdat<- dat.tango$d[dat.tango$j == j]
    nj<-sum(jdat)
    poo <- dat.tango$e[dat.tango$j == j]
    p1<-poo/sum(poo)
    pp<-matrix(p1)
    w1<-diag(p1)-pp%*%t(pp)
    qdat<- qdat + jdat
    p<- p+nj*p1
    w<- w+nj*w1
    py<-p1
    vv<-py/sum(py)
    cvv<-cumsum(vv)
    
    if(j_verbose) pb <- txtProgressBar(min = 0, max = nloop, style = 3)
    for (ijk in 1:nloop) {
      r1<-hist(runif(nj),breaks=c(0,cvv),plot=F)
      frq[ijk,]<- frq[ijk,] + r1$count
      if(j_verbose) setTxtProgressBar(pb, ijk)
    }
    if(j_verbose) close(pb)
  }
  frq<-frq/nn
  ori<- qdat
  qdat<-qdat/nn
  w<- w/nn
  p<-p/nn
  
  #
  for (k in 1:kkk){
    rad<- kvec[k]
    ac <- exp( -4 * (dc/rad)^2 )
    hh <- ac%*%w
    hh2<- hh%*%hh
    av <- sum(diag( hh ))
    av2<- sum(diag( hh2 ))
    av3<- sum(diag( hh2%*%hh ))
    skew1<- 2*sqrt(2)*av3/ (av2)^1.5
    df1<- 8/skew1/skew1
    eg1<-av
    vg1<-2*av2
    
    if(j_verbose) pb <- txtProgressBar(min = 0, max = nloop, style = 3)
    for (ijk in 1:nloop){
      q<-frq[ijk,]
      gt<-(q-p)%*%ac%*%(q-p)*nn
      stat1<-(gt-eg1)/sqrt(vg1)
      aprox<-df1+sqrt(2*df1)*stat1
      pval[ijk,k]<- 1-pchisq(aprox, df1)
      if(j_verbose) setTxtProgressBar(pb, ijk)
    }
    if(j_verbose) close(pb)
  }
  
  if(j_verbose) pb <- txtProgressBar(min = 0, max = nloop, style = 3)
  for (ijk in 1:nloop){
    pcc[ijk]<-min( pval[ijk,] )
    if(j_verbose) setTxtProgressBar(pb, ijk)
  }
  if(j_verbose) close(pb)
  
  pxx<- sort( pcc )
  #
  # Calculation of (2) test statistic Pmin and its adjusted p-value
  # of observed data
  #
  ptan<-rep(0,kkk)
  gtt<-rep(0,kkk)
  stat2<-rep(0,kkk)
  
  jjj<-1:kkk
  ep<-p*nn
  po<-ppois(ori,ep)
  pa<-(po-0.90)/0.10
  p1<-ifelse(po > 0.9 & ori>0, pa^3, 0)
  
  if(j_verbose) pb <- txtProgressBar(min = 0, max = kkk, style = 3)
  for (k in 1:kkk) {
    rad<- kvec[k]
    ac <- exp( -4 * (dc/rad)^2 )
    hh <- ac%*%w
    hh2<- hh%*%hh
    av <- sum(diag( hh ))
    av2<- sum(diag( hh2 ))
    av3<- sum(diag( hh2%*%hh ))
    skew1<- 2*sqrt(2)*av3/ (av2)^1.5
    df1<- 8/skew1/skew1
    eg1<-av
    vg1<-2*av2
    gt<-(qdat-p)%*%ac%*%(qdat-p)*nn
    gtt[k]<-gt
    stat1<-(gt-eg1)/sqrt(vg1)
    stat2[k]<-stat1
    aprox<-df1+sqrt(2*df1)*stat1
    ptan[k]<- 1-pchisq(aprox, df1)
    if(j_verbose) setTxtProgressBar(pb, k)
  }
  if(j_verbose) close(pb)
  #
  # Figure 3
  p22<-min( ptan )
  at<-c(pxx,p22)
  rm(pres)
  pres<-length(at[at<=p22])/(length(pxx)+1)
  
  MEET_p[MEET_p$combo == z, paste0("tm", y)] = pres
  
  for (i in 1:kkk){
    if ( abs(p22 - ptan[i] )< 0.0000001 ){pind<-i}
  }
  
  
}
}

MEET_p

## SCRIPT END ## ---------------------------------------------------------------