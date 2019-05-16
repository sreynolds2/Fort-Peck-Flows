
# THIS FILE CONTAINS THE FUNCTIONS USED IN THE FT. PECK FLOWS ANALYSIS
# BASED ON AGE-1+ DEMOGRAPHIC INPUTS, DRIFT-DEVELOPMENT DYNAMICS, AND 
# AGE-0 MISSOURI RIVER AND LAKE SAK SURVIVAL
## FUNCTIONS TO FIND: 
### 1. find_age1plus_id
### 2. find_drift_id
### 3. find_survival_id
## FUNCTIONS TO CREATE NEW SCENARIOS:
### 4. new_age1plus_scenario
### 5. new_survival_scenario
## FUNCTION TO CREATE THE LESLIE MATRIX 
## AND GIVE ASSOCIATED DEMOGRAPHIC OUTPUTS:
### 6. demog_output
### 7. parameter sensitivities...




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



# 6.
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
  inputs<- code$age1plus[[age1plus_id]]
  if(length(inputs$sexratio)!=maxage & length(inputs$sexratio)!=1)
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
  drift_data<- code$drift
  drift_data<- subset(drift_data, id==drift_id)
  survival_data<- code$survival
  survival_data<- subset(survival_data, id==survival_id)
  
  # BUILD EGG TO AGE-1 SURVIVAL
  names(drift_data)[which(names(drift_data)==id)]<- "drift_id"
  names(survival_data)[which(names(survival_data)==id)]<- "survival_id"
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
  ## ADD ID
  ea$id<- paste0(age1plus_id, "-", drift_id, "-", survival_id)
  saveRDS(ea, paste0("./output/Eigen_Analysis_", ea$id, ".rds"))
  return(ea)
}
  