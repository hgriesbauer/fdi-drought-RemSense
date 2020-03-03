# seasCorrSite function
# function to run seascorr on a site by site basis

# Climate preparation function 
# Prepare climate for input into correlation and response function analysis

climPrep<-function(site,t_span=c(1938,2005)) {
  
  clim<-
    fdi2020$clim$annual %>% 
    filter(Site==site) %>%   # filter for site
    filter(Year>=min(t_span)) %>% 
    filter(Year<=max(t_span))
  
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
    dplyr::select(-Rad,-site) %>% # remove certain variables
    
    dplyr::select(year,month,Tmax,Tmin,Tave,PPT,Eref,CMD,PAS) %>% 
    
    # convert to data frame
    as.data.frame()
  
  return(clim)
  
}


# seasCorrExt - function to extract coefficients and other data from the
# seascorr() function
seasCorrExt<-function(crn,clim,var1,var2,t_span=c(1940,2005),winLength=c(1,3,6,9,12),growth.complete=9) {
  
  # Use growth.complete to create a vector
  # of 14 months ending with growth.complete month
  end.month=c(paste("prior",month.name[(13-(14-growth.complete)):12],sep="."),
              month.name[1:growth.complete]
  )
  
  # Derive a climVar list
  climVar=names(clim)[3:ncol(clim)]
    
  # run seasCorr on climVar
  
  for (j in 1:length(climVar)) { # for each clim Var entry
    
    # Set the climate variables
    var1=climVar[j]
    var2=ifelse(j==1,climVar[2],climVar[1]) # use first climvar for most runs
    
    # Run seascorr function
    x.out<-
      seascorr(crn,clim,primary=var1,secondary=var2,
               season_length=winLength,
               complete=growth.complete) %>% 
      pluck("coef") 
    
    # Now create a list of dataframes
    for (k in 1:length(winLength)) {
      
      if(j==1 & k==1) seas.coef<-data.frame()
      
     
      seas.coef<-
        rbind(seas.coef,
        data.frame(crn=names(crn)[1],
                   climVar=var1,
                   window=winLength[k],
                   end.month=end.month,
                   coef=x.out[[k]]$primary$coef,
                   sig=x.out[[k]]$primary$significant)
        )
        
    } # close k loop
    } # close j loop
    
    # return from function
    return(seas.coef)
    
  }

# crnExtract function
crnExt<-function(crnType,site) {
  fdi2020$crn[[crnType]] %>% 
  rownames_to_column("year") %>% 
  .[,c("year",site)] %>% 
    filter(year>=min(t_span) & year<=max(t_span)) %>% 
  column_to_rownames("year") %>% 
  setNames(paste(site,crnType,sep=".")) %>% 
  drop_na() %>% 
    as.data.frame() %>% 
    return()
}

# Define seasCorrSite function - use this with seasCorrExt to extract coefficients for sites    
seasCorrSite<-function(Site) {
  
  # calculate correlation coefficients using rw,lw and ew chronologies
  clim1=climPrep(Site)
  
  print(Site)
  
 return(rbind(seasCorrExt(crn=crnExt("ew",site=Site),clim1),
                seasCorrExt(crn=crnExt("lw",site=Site),clim1),
                seasCorrExt(crn=crnExt("rw",site=Site),clim1)))
 
}    
  