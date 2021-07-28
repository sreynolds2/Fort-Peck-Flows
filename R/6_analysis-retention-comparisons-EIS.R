
test<- function(data=NULL,
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
    if(rel_drift_vel==1)
    {
      PITU<- sapply(1:n, function(i)
      {
        sum((3/24*dat[1:M,3*i+4]/(1923*dat[1:M,3*i+4]^(-0.74)))*(dat[1:M,3*i+5]/sum(dat[1:M,3*i+5])))
      })
    }
    if(rel_drift_vel==0.9)
    {
      PITU<- sapply(1:n, function(i)
      {
        sum((3.33/24*dat[1:M,3*i+4]/(1923*dat[1:M,3*i+4]^(-0.74)))*(dat[1:M,3*i+5]/sum(dat[1:M,3*i+5])))
      })
    }
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
    n_UB<- max(n, 20)
    if(rel_drift_vel==1)
    {
      ITU<- sapply(1:n, function(i)
      {
        if(i %in% 1:19)
        {
          out<- sum(3/24*dat[1:M,3*i+4]*dat[1:M,3*i+5]/sum(dat[1:M,3*i+5]))
        }
        if(i %in% 20:n_UB)
        {
          out<- sum(3/24000000*dat[1:M,3*i+4]*dat[1:M,3*i+5])
        }
        return(out)
      })
    }
    if(rel_drift_vel==0.9)
    {
      ITU<- sapply(1:n, function(i)
      {
        if(i %in% 1:19)
        {
          out<- sum(3.33/24*dat[1:M,3*i+4]*dat[1:M,3*i+5]/sum(dat[1:M,3*i+5]))
        }
        if(i %in% 20:n_UB)
        {
          out<- sum(3.33/24000000*dat[1:M,3*i+4]*dat[1:M,3*i+5])
        }
        return(out)
      })
    }
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

## WITH NEW SETUP MOST DO NOT REACH 200 CTUs

# COMPUTE ALL TYPES OF RETENTION VALUES
alts<- c("1", "1a", "1b", "2", "2a", "2b", "NA")
comp_ret<- lapply(alts, function(alt)
{
  ## PULL SHEET NAMES
  nms<- read.xlsx(paste0("./dat/R3_Output_Data_", alt, "P.xlsx"), 
                  "Sheet_Names")
  ## DETERMINE RETENTION FOR EACH SHEET
  alt_ret<- lapply(nms[,1], function(x)
  {
    ### PULL PARTICULAR SHEET
    tmp<- read.xlsx(paste0("./dat/R3_Output_Data_", alt, "P.xlsx"), x)
    ### GATHER HATCH DATE INFORMATION FOR THE SHEET
    dt<- strsplit(names(tmp)[8], "\\(")[[1]][2]
    dt<- strsplit(dt, "\\)")[[1]][1]
    dt<- strsplit(dt, "\\.")[[1]][1]
    dt<- as.Date(as.Date(dt, "%d%b%Y"))
    yr<- as.numeric(format(dt, "%Y"))
    ### INCLUDE VARIOUS TYPES OF RETENTION VALUES
    dev<- c("New", "Old")
    dev_ret<- lapply(dev, function(dv)
    {
      drft<- c(0.9, 1)
      drft_ret<- lapply(drft, function(dr)
      {
        #### COMPUTE RETENTION
        ret<- test(data=tmp, development_type=dv,
                   rel_drift_vel=dr, anoxic_incl=FALSE)
        out<- data.frame(Alt=alt, Year=yr, Hatch_Date=dt, Retention=ret,
                         Develop_Mod=dv, Drift_Mod=dr, anoxic_layer=FALSE)
        return(out)
      })
      drft_ret<- do.call(rbind, drft_ret)
      return(drft_ret)
    })
    dev_ret<- do.call(rbind, dev_ret)
    return(dev_ret)
  })
  alt_ret<- do.call(rbind, alt_ret)
  return(alt_ret)
})
comp_ret<- do.call(rbind, comp_ret)

## TABLE LIKE CRAIGS
tmp<- dcast(comp_ret, Alt+Year+Hatch_Date~Develop_Mod+Drift_Mod, mean, 
            value.var="Retention")
write.csv(tmp, "./output/Retention_Comparison_Rounding.csv",
          row.names = FALSE)

