acaTangoMeet <- function(input_df,
                         nloop = 999,
                         j_verbose = TRUE){
  
  # set fixed parameters
  nk<- 1
  kvec<- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
  kvec<- kvec*100 #as lambda is supposed to vary up to half the area, and the max. distance is >3963, altered the constant from 2 to 60.
  
  nr<-length(input_df$id)
  qdat<-rep(0,nr)
  p<-rep(0,nr)
  w<-matrix(0,nr,nr)
  nn<-sum(input_df$d)
  R<-6371
  
  kkk<- length(kvec)
  dc<-matrix(0,nr,nr)
  regid<-input_df$id
  frq<-matrix(0,nloop,nr)
  pval<-matrix(0,nloop,kkk)
  pxx<-rep(0,nloop)
  pcc<-pxx
  
  if(j_verbose) pb <- txtProgressBar(min = 0, max = nr, style = 3)
  for (i in 1:nr){
    for (j in i:nr){
      #formula 1.10 p.18 in Banerjee et al 2004
      dij <-suppressWarnings(acos(sin(input_df$yrad[i])*sin(input_df$yrad[j])+cos(input_df$yrad[i])*cos(input_df$yrad[j])*cos(input_df$xrad[i]-input_df$xrad[j]))*R)
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
    jdat<- input_df$d[input_df$j == j]
    nj<-sum(jdat)
    poo <- input_df$e[input_df$j == j]
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
  return(length(at[at<=p22])/(length(pxx)+1))
}