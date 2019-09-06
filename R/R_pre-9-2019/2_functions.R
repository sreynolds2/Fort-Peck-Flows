
# THIS FILE CONTAINS THE FUNCTIONS USED IN THE FT. PECK FLOWS ANALYSIS
# BASED ON AGE-1+ DEMOGRAPHIC INPUTS, DRIFT-DEVELOPMENT DYNAMICS, AND 
# AGE-0 MISSOURI RIVER AND LAKE SAK SURVIVAL
## ANY REGIONS OF HOPE?
### 0. regions_of_hope
## FUNCTIONS TO FIND: 
### 1. find_age1plus_id
### 2. find_drift_id
### 3. find_survival_id
## FUNCTIONS TO CREATE NEW SCENARIOS:
### 4. new_age1plus_scenario
### 5. new_drift_scenario
### 6. new_survival_scenario
## FUNCTION TO CREATE THE LESLIE MATRIX 
## AND GIVE ASSOCIATED DEMOGRAPHIC OUTPUTS:
### 7. demog_output
### 8. param_sens_elas
### 9. 



#0.
regions_of_hope<- function(maxage=NULL,
                           sexratio=NULL,
                           phi=NULL,
                           # spnProb=NULL,
                           # eggs=NULL,
                           birthR=NULL,
                           phi0=NULL
                           )
{
  # BUILD LESLIE MATRIX
  A<-matrix(0,maxage,maxage)
  ## SURVIVAL RATES
  A[cbind(2:maxage,1:(maxage-1))]<- phi
  ## FECUNDITIES
  if(length(birthR)==1){birthV<- c(rep(0,8), rep(birthR, maxage-8))}
  if(length(birthR)==maxage){birthV<- birthR}
  A[1,] <- sexratio*birthV*phi0
  # A[1,] <- sexratio*spnProb*eggs*phi0
  
  # EIGENANALYSIS 
  ea<- eigen.analysis(A)
  ea$A<- A
  ea$birth_rate<- round(sum(birthV*ea$stable.age)/sum(ea$stable.age[9:maxage]))
  # ea$birth_rate<- sum(spnProb*eggs*ea$stable.age)
  
  # PARAMETER SENSITIVITIES
  sens<- ea$sensitivities 
  ea$sensitivities<- list()
  ea$sensitivities$entries<- sens
  ea$sensitivities$phi<- rep(0, maxage-1)
  for(i in 1:(maxage-1))
  {
    ea$sensitivities$phi[i]<- sens[i+1,i]
  }
  ea$sensitivities$birthR<- sens[1,]*sexratio*phi0
  ea$sensitivities$phi0<- sum(sens[1,]*sexratio*birthR)
  ea$sensitivities$sexratio<- sum(sens[1,]*birthR*phi0)
  rm(sens)
  # PARAMETER ELASATICITIES
  elas<- ea$elasticities
  ea$elasticities<- list()
  ea$elasticities$entries<- elas
  rm(elas)
  ea$elasticities$phi<- ea$sensitivities$phi*phi/ea$lambda1
  ea$elasticities$birthR<- ea$sensitivities$birthR*birthR/ea$lambda1
  ea$elasticities$phi0<- ea$sensitivities$phi0*phi0/ea$lambda1
  ea$elasticities$sexratio<- ea$sensitivities$sexratio*sexratio/ea$lambda1
  # LABEL AND SAVE
  phi0_id<- phi0*1000
  if(length(birthR)==1)
  {
    bR_id<- birthR/100
    saveRDS(ea, paste0("./output/Regions_of_Hope/phi0_", phi0_id,
                       "_birthR_", bR_id, ".rds"))
  }
  if(length(birthR)==maxage)
  {
    bR_id<- round(ea$birth_rate/100)
    saveRDS(ea, paste0("./output/Regions_of_Hope/phi0_", phi0_id,
                       "_birthR_Variety_", bR_id, ".rds"))
  }
  out<- data.frame(age0_survival=phi0, birth_rate=ea$birth_rate, 
                   lambda=ea$lambda1, phi0_id=phi0_id, input_bR_id=bR_id)
  return(out)
}


#1. 
find_age1plus_id<- function(maxage=NULL,
                            sexratio=NULL,
                            phi=NULL, #AGE-SPECIFIC SURVIVALS, A VECTOR OF LENGTH maxage-1
                            psi=NULL, #PROPORTION OF MATURE FEMALES OF A GIVEN AGE, A VECTOR OF LENGTH maxage
                            eta=NULL, #FRACTION OF THE MATURE FEMALES (OF THE GIVEN AGE) THAT WILL SPAWN, A VECTOR OF LENGTH maxage
                            fec=NULL #AVERAGE NUMBER OF EGGS PRODUCED BY A SPAWNING FEMALE OF A GIVEN AGE, A VECTOR OF LENGTH maxage
                           )
{
  ## ADD INPUTS TO A LIST
  dat<-list()
  dat$maxage<- maxage
  dat$sexratio<- sexratio
  dat$phi<- phi
  dat$psi<- psi
  dat$eta<- eta
  dat$fec<- fec
  ### THROW AN ERROR IF ANY INPUTS ARE NULL
  if(length(dat)!=6)
  {
    return(print("In order to create a new age-1+ demographic input 
                  scenario, specify an input value for each of: maxage, 
                  sexratio, phi, psi, eta, and fec."))
  }
  
  ## PULL SCENARIOS
  codes<- readRDS("./dat/scenario_codes.rds") 
  
  ## CHECK FOR MATCHING AGE-1+ SCENARIOS
  out<-sapply(1:length(codes$demograhpic), function(i)
  {
    tmp<- NULL
    if(codes$age1plus[[i]]$maxage==dat$maxage & 
       codes$age1plus[[i]]$sexratio==dat$sexratio & 
       codes$age1plus[[i]]$phi==dat$phi & 
       codes$age1plus[[i]]$psi==dat$psi &
       codes$age1plus[[i]]$eta==dat$eta & 
       codes$age1plus[[i]]$fec==dat$fec)
    {
      tmp<- i
    }
    return(tmp)
  })
  return(out)
}


# 2. 
find_drift_id<- function(U_mps=NULL,
                         temp_C=NULL,
                         Exc_value=NULL,
                         spawn_rkm=NULL)
{
  ## PULL DRIFT-DEVELOPMENT SCENARIOS
  codes<- readRDS("./dat/scenario_codes.rds")
  dat<- codes$drift
  ## FIND THOSE THAT MATCH INPUT VALUES
  indx<- which(dat$U_mps==U_mps & dat$temp_C==tmp_C & 
                 dat$Exc_value==Exc_value & dat$spawn_rkm==spawn_rkm)
  indx<- dat$id[indx]
  return(indx)
}


# 3. 
find_survival_id<- function(phi0_MR=NULL,
                            phi0_LS=NULL)
{
  ## PULL SURVIVAL SCENARIOS
  codes<- readRDS("./dat/scenario_codes.rds")
  dat<- codes$survival
  ## FIND THOSE THAT MATCH INPUT VALUES
  indx<- which(dat$phi0_MR==phi0_MR & dat$phi0_LS==phi0_LS)
  indx<- dat$id[indx]
  return(indx)
}



# 4. 
new_age1plus_scenario<- function(maxage=NULL,
                                 sexratio=NULL,
                                 phi=NULL, #AGE-SPECIFIC SURVIVALS, A VECTOR OF LENGTH maxage-1
                                 psi=NULL, #PROPORTION OF MATURE FEMALES OF A GIVEN AGE, A VECTOR OF LENGTH maxage
                                 eta=NULL, #FRACTION OF THE MATURE FEMALES (OF THE GIVEN AGE) THAT WILL SPAWN, A VECTOR OF LENGTH maxage
                                 fec=NULL #AVERAGE NUMBER OF EGGS PRODUCED BY A SPAWNING FEMALE OF A GIVEN AGE, A VECTOR OF LENGTH maxage
                                )
{
  ## PULL PREVIOUS DATA SCENARIOS
  codes<- readRDS("./dat/scenario_codes.rds")
  M<- length(codes$age1plus)
  
  ## ADD NEW INPUTS TO A LIST
  out<-list()
  out$maxage<- maxage
  out$sexratio<- sexratio
  out$phi<- phi
  out$psi<- psi
  out$eta<- eta
  out$fec<- fec
  ### THROW AN ERROR IF ANY INPUTS ARE NULL
  if(length(out)!=6)
  {
    return(print("In order to create a new age-1+ demographic input 
                  scenario, specify an input value for each of: maxage, 
                  sexratio, phi, psi, eta, and fec."))
  }
  
  ## ADD A UNIQUE ID
  out$id<- M+1
  
  ## UPDATE AGE-1+ DEMOGRAPHIC INPUT DATA SCENARIOS AND SAVE
  codes$age1plus[[M+1]]<- out
  saveRDS(codes, "./dat/scenario_codes.rds")
  
  ## RETURN ID OF NEW SCENARIO
  return(M+1)
  }



# 5.
new_drift_scenario<- function(drift_dat=NULL)
{
  ## PULL PREVIOUS DATA SCENARIOS
  codes<- readRDS("./dat/scenario_codes.rds")
  ## OBTAIN NEW DRIFT DATA ONLY 
  ## (ADD OLD DRIFT DATA TO MOST DRIFT DATA AND REMOVE DUPLICATES)
  new<- rbind(dat, codes$drift[,names(dat)])
  indx<- which(duplicated(new))
  indx<- c(indx, which(duplicated(new, fromLast = TRUE)))
  if(length(indx>0)){new<- new[-indx,]}
  if(nrow(new)==0)
  {
    return(print("No new drift scenarios are available to add."))
  }
  ## ADD A UNIQUE ID TO THE NEW DATA
  M<- max(codes$drift$id)
  new$id<- (M+1):(M+nrow(new))
  ## ADD NEW DATA TO CODES FILE
  codes$drift<- rbind(codes$drift, new)
  codes$drift<- codes$drift[order(codes$drift$id),]
  ## SAVE THE NEW DATA
  saveRDS(codes, "./dat/scenario_codes.rds")
  ## RETURN NEW DRIFT SCENARIO IDS
  return(new$ids)
}


# 6.
new_survival_scenario<- function(phi0_MR=NULL,
                                 phi0_LS=NULL)
{
  ## ERROR HANDLING
  if(length(phi0_LS)!=length(phi0_MR))
  {
    return(print("Inputs must be of the same length."))
  }
  ## PULL PREVIOUSLY USED SURVIVAL SCENARIOS
  codes<- readRDS("./dat/scenario_codes.rds")
  M<- max(codes$survival$id)
  ## GENERATE NEW SURVIVAL DATAFRAME WITH UNIQUE IDS
  ids<- (M+1):(M+length(phi0_MR))
  new<-data.frame(phi0_MR=phi0_MR, phi0_LS=phi0_LS, id=ids)
  ## ADD TO SURVIVAL SCENARIOS AND SAVE
  codes$survival<- rbind(codes$survival, new)
  saveRDS(codes, "./dat/scenario_codes.rds")
  ## RETURN SURVIVAL IDS ASSOCIATED WITH EACH NEW SURVIVAL SCENARIO
  return(ids)
}



# 7.
# UTILIZE AN AGE-1+ DEMOGRAPHIC SCENARIO, A DRIFT SCENARIO, AND A 
# SURVIVAL SCENARIO COMBINATION
demog_output<- function(age1plus_id=NULL,
                        drift_id=NULL,
                        survival_id=NULL,
                        codes=NULL)
{
  # ERROR HANDLING
  if(length(codes$age1plus)<age1plus_id)
  {
    return(print("Age-1+ ID not found."))
  }
  if(codes$age1plus[[age1plus_id]]$id!=age1plus_id)
  {
    return(print("Mismatch in Age-1+ Scenario IDs and order of 
                 codes$age1plus, please check the codes file."))
  }
  if(!any(codes$drift$id==drift_id))
  {
    return(print("Drift ID not found."))
  }
  if(!any(codes$survival$id==survival_id))
  {
    return(print("Survival ID not found."))
  }
  ## PULL AGE-1+ DEMOGRAPHIC INPUTS AND FURTHER ERROR HANDLING
  inputs<- codes$age1plus[[age1plus_id]]
  if(length(inputs$sexratio)!=inputs$maxage & length(inputs$sexratio)!=1)
  {
    return(print("The sex ratio must be a either a constant value or a 
                 vector of length maxage."))
  }
  if(length(inputs$phi)!=inputs$maxage-1)
  {
    return(print("The annual survival vector, phi, must be of length 
                  maxage-1, giving one survival value for each age: 1, 
                  2, ..., maxage-1."))
  }
  if(length(inputs$psi)!=inputs$maxage | 
      length(inputs$eta)!=inputs$maxage | 
      length(inputs$fec)!=inputs$maxage)
  {
    return(print("Age-1+ inputs psi, eta, and fec must all be of length 
                  maxage, i.e., there must be one maturation, spawning, 
                  and fecundity value for each age: 1, 2, ..., maxage."))
  }
  if(any(c(inputs$sexratio, inputs$phi, inputs$psi, inputs$eta)<0) | 
     any(c(inputs$sexratio, inputs$phi, inputs$psi, inputs$eta)>1))
  {
    return(print("All Age-1+ input entries for sexratio, phi, psi, and 
                  eta must be between 0 and 1, inclusive.")) 
  }
  
  ## PULL DRIFT DATA AND SURVIVAL VALUES
  drift_data<- codes$drift
  drift_data<- subset(drift_data, id==drift_id)
  survival_data<- codes$survival
  survival_data<- subset(survival_data, id==survival_id)
  
  # BUILD EGG TO AGE-1 SURVIVAL
  names(drift_data)[which(names(drift_data)=="id")]<- "drift_id"
  names(survival_data)[which(names(survival_data)=="id")]<- "survival_id"
  dat<- merge(drift_data, survival_data)
  rm(drift_data, survival_data)
  dat$phi0<- dat$p_retained*dat$phi0_MR + 
                (1-dat$p_retained)*dat$phi0_MR*dat$phi0_LS
  
  

  # BUILD LESLIE MATRIX
  A<-matrix(0,inputs$maxage,inputs$maxage)
  ## SURVIVAL RATES
  A[cbind(2:inputs$maxage,1:(inputs$maxage-1))]<- inputs$phi
  ## FECUNDITIES
  A[1,] <- inputs$sexratio*inputs$psi*inputs$eta*inputs$fec*dat$phi0
  rm(inputs)
  # EIGENANALYSIS 
  ea<- eigen.analysis(A)
  ## ADD MATRIX A
  ea$A<- A
  ## ADD ID AND SAVE
  ea$id<- paste0(age1plus_id, "-", drift_id, "-", survival_id)
  saveRDS(ea, paste0("./output/Eigen_Analysis_", ea$id, ".rds"))
  ## UPDATE ANALYSIS RECORDS AND SAVE
  rec<- readRDS("./output/analysis_records.rds")
  if(any(rec$id==ea$id))
  {
    rec$demographic[which(rec$id==ea$id)]<- TRUE
  }
  if(all(rec$id!=ea$id))
  {
    rec<- rbind(rec, data.frame(demographic=TRUE,
                                parameter_specific=FALSE,
                                id=ea$id))
  }
  saveRDS(rec, "./output/analysis_records.rds")
  return(ea)
}
 


# 8.
param_sens_elas<- function(demog_output=NULL,
                           params=c("sexratio", "phi", "psi", "eta", 
                                    "fec", "phi0", "phi0_MR", "phi0_LS", 
                                    "p_retained"),
                           codes=NULL)
{
  # STORE OUTPUTS IN A LIST, INCLUDING PREVIOUS ANALYSES
  out<- list(sens=demog_output$param_sensitivities, 
             elas=demog_output$param_elasticities)
  if(any(names(out$sens)=="sr"))
  {
    names(out$sens)[[which(names(out$sens)=="sr")]]<- "sexratio"
  }
  params2<- intersect(names(out$sens), names(out$elas))
  
  # ELIMINATE ANY PREVIOUSLY RAN ANALYSES
  params<- setdiff(params, params2)
  
  if(length(params)!=0)
  {
    # PARAMETER VALUES
    age1plus_id<- as.numeric(strsplit(demog_output$id, "-")[[1]][1])
    age1dat<- codes$age1plus[[age1plus_id]]
    sexratio<- age1dat$sexratio
    psi<- age1dat$psi
    eta<- age1dat$eta
    fec<- age1dat$fec
    drift_id<- as.numeric(strsplit(demog_output$id, "-")[[1]][2])
    p_retained<- codes$drift[which(codes$drift$id==drift_id),]$p_retained
    survival_id<- as.numeric(strsplit(demog_output$id, "-")[[1]][3])
    phi0dat<- codes$survival[which(codes$survival$id==survival_id),]
    phi0_MR<- phi0dat$phi0_MR
    phi0_LS<- phi0dat$phi0_LS
    phi0<- p_retained*phi0_MR + (1-p_retained)*phi0_MR*phi0_LS
    if(length(unique(age1dat$phi[2:length(age1dat$phi)]))==1)
    {
      phi2plus<- age1dat$phi[2]
    }
    rm(age1plus_id, age1dat, drift_id, survival_id, phi0dat)
    # SENSITIVITY MATRIX
    sens<- demog_output$sensitivities
    # ELASTICITY MATRIX
    # elas<- demog_output$elasticities
    
    # PARAMETER SENSITIVITIES & ELASTICITIES
    if("sexratio" %in% params)
    {
      out$sens$sexratio<- sens[1,]*psi*eta*fec*phi0
      out$sens$sexratio_total<- sum(out$sens$sexratio)
      out$elas$sexratio<- out$sens$sexratio*sexratio/demog_output$lambda1
      out$elas$sexratio_total<- out$sens$sexratio_total*sexratio/demog_output$lambda1
    }
    if("phi" %in% params)
    {
      out$sens$phi<-rep(0, nrow(sens)-1)
      for(i in 1:length(out$sens$phi))
      {
        out$sens$phi[i]<-sens[i+1,i]
      }
      vals<- cbind(2:nrow(demog_output$A), 1:(ncol(demog_output$A)-1))
      out$elas$phi<- out$sens$phi*demog_output$A[vals]/demog_output$lambda1 
      if(exists("phi2plus"))
      {
        out$sens$phi2plus<-sum(out$sens$phi[2:length(out$sens$phi)])
        out$elas$phi2plus<- out$sens$phi2plus*phi2plus/demog_output$lambda1
      }
    }
    if("psi" %in% params)
    {
      out$sens$psi<- sens[1,]*sexratio*eta*fec*phi0
      out$elas$psi<- out$sens$psi*psi/demog_output$lambda1
    }
    if("eta" %in% params)
    {
      out$sens$eta<- sens[1,]*sexratio*psi*fec*phi0
      out$elas$eta<- out$sens$eta*eta/demog_output$lambda1
    }
    if("fec" %in% params)
    {
      out$sens$fec<- sens[1,]*sexratio*psi*eta*phi0
      out$elas$fec<- out$sens$fec*fec/demog_output$lambda1
    }
    if("phi0" %in% params)
    {
      out$sens$phi0<- sens[1,]*sexratio*psi*eta*fec
      out$sens$phi0_total<- sum(out$sens$phi0)
      out$elas$phi0<- out$sens$phi0*phi0/demog_output$lambda1
      out$elas$phi0_total<- out$sens$phi0_total*phi0/demog_output$lambda1
    }
    if("phi0_MR" %in% params)
    {
      out$sens$phi0_MR<- sens[1,]*sexratio*psi*eta*fec*(p_retained+(1-p_retained)*phi0_LS)
      out$sens$phi0_MR_total<- sum(out$sens$phi0_MR)
      out$elas$phi0_MR<- out$sens$phi0_MR*phi0_MR/demog_output$lambda1
      out$elas$phi0_MR_total<- out$sens$phi0_MR_total*phi0_MR/demog_output$lambda1
    }
    if("phi0_LS" %in% params)
    {
      out$sens$phi0_LS<- sens[1,]*sexratio*psi*eta*fec*(1-p_retained)*phi0_MR
      out$sens$phi0_LS_total<- sum(out$sens$phi0_LS)
      out$elas$phi0_LS<- out$sens$phi0_LS*phi0_LS/demog_output$lambda1
      out$elas$phi0_LS_total<- out$sens$phi0_LS_total*phi0_LS/demog_output$lambda1
    }
    if("p_retained" %in% params)
    {
      out$sens$p_retained<- sens[1,]*sexratio*psi*eta*fec*phi0_MR*(1-phi0_LS)
      out$sens$p_retained_total<- sum(out$sens$p_retained)
      out$elas$p_retained<- out$sens$p_retained*p_retained/demog_output$lambda1
      out$elas$p_retained_total<- out$sens$p_retained_total*p_retained/demog_output$lambda1
    }
    # SAVE RESULTS TO THE DEMOGRAPHIC FILE
    demog_output$param_sensitivities<- out$sens
    demog_output$elas_sensitivities<- out$elas
    saveRDS(demog_output, 
            paste0("./output/Eigen_Analysis_", demog_output$id, ".rds"))
    ## UPDATE ANALYSIS RECORDS AND SAVE
    rec<- readRDS("./output/analysis_records.rds")
    if(any(rec$id==demog_output$id))
    {
      rec$parameter_specific[which(rec$id==demog_output$id)]<- TRUE
    }
    if(all(rec$id!=demog_output$id))
    {
      rec<- rbind(rec, data.frame(demographic=TRUE,
                                  parameter_specific=TRUE,
                                  id=demog_output$id))
    }
    saveRDS(rec, "./output/analysis_records.rds")
  }
  return(out)
}
                                    