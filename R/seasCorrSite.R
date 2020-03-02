# seasCorrSite function
# function to run seascorr on a site by site basis

seasCorrSite<-function(site,t_span=c((1940,2005))) {
  
  clim<-
    climPrep(site) %>% 
    as.data.frame()
  
  # CRN PREP
  # Combine sites together
  ew<-fdi2020$crn$ew
  names(ew)=paste("ew",names(ew),sep=".")
    
  lw<-fdi2020$crn$lw
  names(lw)=paste("lw",names(lw),sep=".")
  
  rw<-fdi2020$crn$rw
  names(rw)=paste("rw",names(rw),sep=".")
  
  crn<-
    combine.rwl(list(rw,ew,lw)) %>% 
    dplyr::select(ends_with(site)) %>% 
    rownames_to_column("year") %>% 
    filter(year>=min(t_span) & year<=max(t_span)) %>% 
    column_to_rownames("year") %>% 
  as.data.frame() %>% 
    setNames(c("rw","ew","lw"))
  

  # Now run for different variables and chronologies
  crnType="ew"
  
  y=crn[,c(crnType,"lw")]
  
  x1<-seascorr(y,clim,primary="Tmax",secondary="Tmin")
  x1<-seascorr(y,clim,primary="Tmin",secondary="Tmax")
  x1<-seascorr(y,clim,primary="Tave",secondary="Tmin")
  x1<-seascorr(y,clim,primary="PPT",secondary="Tmin")
  x1<-seascorr(y,clim,primary="DD_0_",secondary="Tmin")
  x1<-seascorr(y,clim,primary="DD5_",secondary="Tmax")
  x1<-seascorr(y,clim,primary="DD_18_",secondary="Tmin")
  x1<-seascorr(y,clim,primary="DD18_",secondary="Tmin")
  x1<-seascorr(y,clim,primary="DD18_",secondary="Tmin")
  x1<-seascorr(y,clim,primary="DD18_",secondary="Tmin")
  x1<-seascorr(y,clim,primary="DD18_",secondary="Tmin")
  
    
    
  }
  
  
 
  

}