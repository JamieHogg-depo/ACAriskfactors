# S-PLUS code for the detection of spatial clustering program
# called MEET (Maximized Excess Events Test)
#
# August 2000 (version 2.00)
#
# Programmed by
#
# Toshiro Tango
# Division of Theoretical Epidemiology
# Department of Epidemiology
# The Institute of Public Health
# 4-6-1 Shirokanedai, Minatoku, Tokyo, 108, Japan
# E-mail: tango@iph.go.jp
#
# ----------------------------------------------------------------
# ( README FIRST )
# 1. Please read my paper carefully before using this program.
# 2. You are free to use this program for noncommercial purposes
# even to modify it but please cite this original program
#
# Reference:
# Tango, T. A test for spatial disease clustering adjusted for
# multiple testing. Statistics in Medicine 19, 191-204 (2000).
# -----------------------------------------------------------------
# INPUT:
#
# nloop: The number of Monte Carlo replications, e.g., 999, 9999.
# us$x: x-value of region centroid, e.g., longitude
# us$y: y-value of region centroid, e.g., latitude
# dc: distance matrix calculated from us$x and us$y
# ( Depending on the user's situation, this should be modified !)
# dat$j: j-th category of the confounders included (j=1,2,...,nk )
# dat$d: observed number of cases
# dat$e: expected number of cases ( or population size)
# kvec: a sequence of lambda values used for the profile p-values
#
# ------------------------------------------------------------------
# Major variables:
#
# nk: the number of strata or categories of all the confounders
# included. For example, if age and sex are the confounders
# included, with 18 different age groups, then there should be
# 18x2 = 36 ( = nk ) categories.
# nr: the number of regions
# nn: the number of cases
# kkk: the number of lambda values
# pxx: sorted p-values due to Monte Carlo replications under
# the null
# ss : per cent contribution to C (equation 14)
# top: region id sorted in the order of the magnitude of ss
# (e.g., top[1] denote the most likely location of cluster,
# if any.)
# pres: The resultant adjusted p-value based on Monte Carlo
# replicates
#
# +++++ An example of execution ++++++++++++++++++++++++++++++++++++
#
# 100 cases: the random sample from clustering model A shown in
# Figure 2 of my paper. The result will be the one
# similar to Figure 3 and 4. The difference will be
# due to Monte Carlo replicates produced.
# These data files are added at the end of email.
#

library(lattice)
library(grDevices)


fp.wd <- "G:/Screening/BreastScreen/Results/2016 ASGS/Tango/"
fp.out <- paste0(fp.wd, "/Tango_BreastScreen_1920.csv")
fp.dat <- "G:/Screening/BreastScreen/Data/2-year/tango_data_BreastScreen.Rds"

# Obtain lat and long for areas included in BreastScreen modelling
aust <- read.csv("G:/Geography 2016/SA2_2016_Aust_lat_long.csv")
aust <- aust[order(aust$id),]

load("G:/Screening/BreastScreen/Geography/Adj_matrix_concordance_BreastScreen.Rdata")
conc.breast.screen <- concordance
concordance <- read.csv("G:/Geography 2016/Shapefile concordance.csv")

concord <- dplyr::full_join(concordance, conc.breast.screen, by = "SA2_Code_Short")

id.drop <- which(is.na(concord$ID_2190[!is.na(concord$ID_2238)]))

concord[concord$ID_2238 %in% id.drop,]
any(is.na(concord$ID_2238[concord$ID_2238 %in% id.drop]))
all(is.na(concord$ID_2190[concord$ID_2238 %in% id.drop]))

aust = aust[-id.drop,]
aust$id = order(order(aust$id))
all(aust$id == 1:nrow(aust))

if (!file.exists(fp.wd)) dir.create(fp.wd)

par(mar=c(1,1,1,1))

MEET_p <- data.frame(combo = "BreastScreen_1920",
                     tm1 = NA,
                     tm2 = NA,
                     tm3 = NA)

for (y in 1:3){

  f = paste0(fp.wd, "/Run",y,"/")
  if (!dir.exists(f)) dir.create(f)
  setwd(f)

#All cancer types

for (z in MEET_p$combo) {
  
    print(z)
    
    nloop<-999
    nk<- 1
    kvec<- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
    kvec<- kvec*100 #as lambda is supposed to vary up to half the area, and the max. distance is >3963, altered the constant from 2 to 60. 
    
    dat <- readRDS(fp.dat) 
    plotfilename <- gsub("\\.csv", paste0("_Run", y, "_fig.pdf"), fp.out)
    
    #
    #
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    dev.new()
    par(mfrow=c(2,2))
    start<-date()
    nr<-length(aust$id)
    qdat<-rep(0,nr)
    p<-rep(0,nr)
    w<-matrix(0,nr,nr)
    nn<-sum(dat$d)
    R<-6371
    
    kkk<- length(kvec)
    dc<-matrix(0,nr,nr)
    regid<-aust$id
    frq<-matrix(0,nloop,nr)
    pval<-matrix(0,nloop,kkk)
    pxx<-rep(0,nloop)
    pcc<-pxx
    
    for (i in 1:nr){
      for (j in i:nr){
        dij <- acos(sin(aust$yrad[i])*sin(aust$yrad[j])+cos(aust$yrad[i])*cos(aust$yrad[j])*cos(aust$xrad[i]-aust$xrad[j]))*R #formula 1.10 p.18 in Banerjee et al 2004
        if (dij=="NaN") {dij=0} else {dij=dij}; 
        dc[j,i]<- dij
        dc[i,j]<- dij
        #Check largest value
        #which(dc==max(dc), arr.ind=TRUE)
        #dc[1829,238]
      }
    }
    
    #
    # Calculation of (1) null distribution of test statistic Pmin
    #
    for (j in 1:nk){
      jdat<- dat$d[dat$j == j]
      nj<-sum(jdat)
      poo <- dat$e[dat$j == j]
      p1<-poo/sum(poo)
      pp<-matrix(p1)
      w1<-diag(p1)-pp%*%t(pp)
      qdat<- qdat + jdat
      p<- p+nj*p1
      w<- w+nj*w1
      py<-p1
      vv<-py/sum(py)
      cvv<-cumsum(vv)
      for (ijk in 1:nloop) {
        r1<-hist(runif(nj),breaks=c(0,cvv),plot=F)
        frq[ijk,]<- frq[ijk,] + r1$count
      }
    }
    frq<-frq/nn
    ori<- qdat
    qdat<-qdat/nn
    w<- w/nn
    p<-p/nn
    symbols(aust$x,aust$y,circles=p,inch=0.50) # Figure 1
    text(aust$x,aust$y,regid,adj= -0.5, col=8)
    print(symbols)
    
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
      for (ijk in 1:nloop){
        q<-frq[ijk,]
        gt<-(q-p)%*%ac%*%(q-p)*nn
        stat1<-(gt-eg1)/sqrt(vg1)
        aprox<-df1+sqrt(2*df1)*stat1
        pval[ijk,k]<- 1-pchisq(aprox, df1)
      }
    }
    for (ijk in 1:nloop){
      pcc[ijk]<-min( pval[ijk,] )
    }
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
    symbols(aust$x,aust$y,circle=p1,inch=0.2) # Figure 2
    text(aust$x, aust$y, ori, col=8)
    par(cex=0.5)
    par(mar=c(4,3,3,5))
    #par(mfrow=c(1,1))
    par(mgp=c(1.5,0.5,0))
    print(symbols)
    #
    for (k in 1:kkk) {
      ti<-date()
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
    }
    #
    # Figure 3
    p22<-min( ptan )
    at<-c(pxx,p22)
    pres<-length(at[at<=p22])/(length(pxx)+1)
    
    MEET_p[MEET_p$combo == z, paste0("tm", y)] = pres
    
    for (i in 1:kkk){
      if ( abs(p22 - ptan[i] )< 0.0000001 ){pind<-i}
    }
    # pind<-jjj[ptan==min(ptan)]
    
    
    plot(kvec,ptan,pch=1,type="b",xlab="cluster size ( lambda values ) ",
         ylab=" Unadjusted p-values")
    abline(v=kvec[pind])
    text((max(kvec)+2*min(kvec))/3,(3*max(ptan)+min(ptan))/4,
         "Adjusted p-value = ",col=8)
    text((1.5*max(kvec)+min(kvec))/2.5,(3*max(ptan)+min(ptan))/4, pres,col=8)
    print(plot)
    #
    # Figure 4
    rad<- kvec[pind]
    ac <- exp( -4 * (dc/rad)^2 )
    ss<-ac%*%(qdat-p)*nn
    ss<-(qdat-p)*ss
    ss2<-ss
    ss<-ss/sum(ss)*100
    plot(regid,ss,pch=1,xlab=" region ID ", ylab=" Percent contribution ")
    text(regid+2,ss+1,regid,col=8)
    top<-regid[sort.list(-ss)]
    print(plot)
    
    dev.copy2pdf(file=plotfilename)
    
    
    dev.off()
    end<- date()
  }

}


# Remove cancer-sex combinations that don't exist
# These combinations should have "NA"s for all tm's
# MEET_p <- MEET_p[-which(apply(is.na(MEET_p[, grep("tm[[:digit:]]", colnames(MEET_p))]), 1, all)),]

write.csv(MEET_p, file = fp.out, row.names = F)

#
# End of S-plus file
# ----------------------------------------------------------------
# -------------- End of all files ----------------------
