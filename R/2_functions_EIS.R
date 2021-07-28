compute_retention<- function(data=NULL,
                             development_type="New", #accepts "Old" and "New"
                             rel_drift_vel=0.9, #0.9 or 1 were used by Craig
                             anoxic_incl=FALSE) #accepts TRUE or FALSE
{
  ## ERROR HANDLING OF INPUTS
  if(development_type!="Old" & development_type!="New")
  {
    return(print("development_type must be specified as 'Old' or 'New'."))
  }
  if(anoxic_incl!=TRUE & anoxic_incl!=FALSE)
  {
    return(print("anoxic_incl must be of type logical."))
  }
  
  ## CLEAN THE HEC-RAS OUTPUT DATA
  dat<- data
  ### CHECK DATA COLUMNS
  if(!grepl("velocity", names(dat)[6]) | 
     !grepl("Temperature", names(dat)[7]) | 
     !grepl("mass", names(dat)[8]))
  {
    print(return("Data column names are inconsistent with function."))
  }
  ### CHECK FOR INITIAL TIME STEP
  if(length(which(dat[,8]!=0))!=1)
  {
    print(return("Initial time step might not be included."))
  }
  ### REMOVE EXTRA ROWS
  if(nrow(dat)==565 & dat[nrow(dat),5]==0){dat<- dat[1:(nrow(dat)-1),]}
  indx<- which(dat[,5]==0 | is.na(dat[,5]))
  if(length(indx)>0)
  {
    #### CHECK ROW CONSISTENCY
    if(!all(indx %in% c(1, 20:22, 179:181, 364:366, 575)) | length(indx)!=11)
    {
      print(return("Row breaks are different than expected."))
    }
    dat<- dat[-indx,]
  }
  ### CHECK DATA COLUMNS
  if(!grepl("velocity", names(dat)[6]) | 
     !grepl("Temperature", names(dat)[7]) | 
     !grepl("mass", names(dat)[8]))
  {
    print(return("Data column names are inconsistent with function."))
  }
  ### NUMBER OF OBSERVATIONS
  n<- length(dat[1,6:ncol(dat)])/3
  
  ## DEVELOPMENTAL TREATMENT OF ANOXIC ZONE
  ### CHECK ANOXIC ZONE VALUE
  AZ<- 459
  if(!grepl("1528.05", dat[459, 4]) | !grepl("1527.4", dat[459, 4]))
  {
    #print(return("Anoxic zone row is inconsistent."))
    #Override for EIS Alt2 7/5/97 (464), 6/25/11 (460), 7/2/11 (460), and 6/21/12 (455)
    AZ<- which(grepl("1528.05", dat[,4]) & grepl("1527.4", dat[,4]))
  }
  ### LAST ROW OF DATA TO INCLUDE FOR DEVELOPMENT
  M<- ifelse(anoxic_incl, nrow(dat), AZ)#459)
  
  ## COMPUTE RETENTION
  ### NEW
  if(development_type=="New")
  {
    PITU<- sapply(1:n, function(i)
    {
      sum((3/24*(1/rel_drift_vel)*dat[1:M,3*i+4]/(1923*dat[1:M,3*i+4]^(-0.74)))*(dat[1:M,3*i+5]/sum(dat[1:M,3*i+5])))
    })
    PCTU<- cumsum(PITU)
    ret<- sapply(1:n, function(i)
    {
      sum(dat[1:AZ,3*i+5])/sum(dat[,3*i+5])
    })
    indx<- min(which(PCTU>1))
    out<- (ret[indx]*(1-PCTU[indx-1])+ret[indx-1]*(PCTU[indx]-1))/(PCTU[indx]-PCTU[indx-1])
  }
  ### OLD
  if(development_type=="Old")
  {
    ITU<- sapply(1:n, function(i)
    {
      sum(3/24*(1/rel_drift_vel)*dat[1:M,3*i+4]*dat[1:M,3*i+5]/sum(dat[1:M,3*i+5]))
    })
    CTU<- cumsum(ITU)
    ret<- sapply(1:n, function(i)
    {
      sum(dat[1:AZ,3*i+5])/sum(dat[,3*i+5])
    })
    indx<- min(which(CTU>200))
    out<- (ret[indx-1]*(CTU[indx]-200) + ret[indx]*(200-CTU[indx-1]))/(CTU[indx]-CTU[indx-1])
  }
  return(out)
}
