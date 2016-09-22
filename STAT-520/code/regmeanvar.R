# =============================================================================
# File Name:     regmeanvar.R
# Author:        Evan Pete Walsh
# Contact:       epwalsh10@gmail.com
# Creation Date: 2016-09-21
# Last Modified: 2016-09-21 14:05:31
# =============================================================================

regmeanvar<-function(x,y,g){
  #group means and variances for regression with
  #one covariate x and response y (both vectors)
  #g is number of groups desired
  #
  #output is data frame with names mean (group means)
  #vars (group variances) and ns (no. observations in group)
  r<-range(x)
  binxint<-round((r[2]-r[1])/g)
  oo<-order(x)
  x<-x[oo]
  y<-y[oo]
  gmns<-NULL; gvs<-NULL; gns<-NULL
  lowt<-min(x)
  cnt<-0
  repeat{
    cnt<-cnt+1
    tint<-lowt+binxint
    tys<-y[(x>lowt)&(x<=tint)]
    tmn<-mean(tys)
    tv<-var(tys)
    gmns<-c(gmns,tmn)
    gvs<-c(gvs,tv)
    gns<-c(gns,length(tys))
    lowt<-tint
    if(cnt==g) break
  }
  res<-data.frame(mean=gmns,vars=gvs,ns=gns)
  return(res)
}
