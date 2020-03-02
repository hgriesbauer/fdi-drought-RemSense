# Climate preparation function 
# Prepare climate for input into correlation and response function analysis

climPrep<-function(site,yrs=c(1940,2005)) {
  
  clim<-
    fdi2020$clim$annual %>% 
    filter(Site==site) %>% # filter for site
    filter(Year<=max(yrs)) %>% # filter for maximum year 
    filter(Year>=(min(yrs)-1))  # filter for minimum year, minus one to allow for climate in prior year to be analyze
  
    # Add some small values to columns with SD=0
  sd.sub<-
    clim %>% 
    summarise_if(is.numeric,sd) %>% 
    t() %>% 
    as.data.frame() %>% 
    rownames_to_column("var") %>% 
    setNames(c("var","sd")) %>% 
    filter(sd==0) # shortlist for standard deviation ==0
  
    # add very small numbers to columns with SD==0, so that seascorr and other functions will work
    clim[,names(clim)%in%sd.sub$var]<-apply(clim[,names(clim)%in%sd.sub$var],2,function(x) runif(nrow(clim),min=0.0000000001,max=0.0000000009))
    
    
    # Reshape
  clim<-
    clim %>% 
    dplyr::select(Site:RH12) %>% 
    pivot_longer(cols=Tmax01:RH12,names_to="var",values_to="values") %>% 
    
    # extract months
    mutate(month=str_sub(var,-2,-1)) %>% 
    mutate(var=str_sub(var,1,-3)) %>% 
    mutate(month=as.integer(month)) %>% 
  
    # select variables
    dplyr::select(-Latitude,-Longitude,-Elevation,site=Site,year=Year,month,var,values) %>% 
    
    # Reshape back to wide
    pivot_wider(names_from="var",values_from="values") %>% 
    
    # remove certain columns
    dplyr::select(-Rad,-site)
  
  return(clim)
  
}