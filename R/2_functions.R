


## GENERATE NEW INPUTS
create_inputs<-function(max_age=60,  
                        phi=c(0.64, 0.69, 0.72, 0.76, 0.79, 0.82, 0.84, 
                              0.86, 0.88, 0.895, 0.91, 0.92, 0.93, 0.935,
                              rep(0.94, 45)), #Age-1+ survivals (vector length max_age-1)
                        a_mat_min=8,      #Minimum age of maturation
                        a_mat_max=21,     #Maximum age at which a fish will mature
                        a_mat_h=15,     #Age at which half of the females are mature
                        mat_var_k=TRUE,      #Control the variance in terms of k (measure of maturation rate) or directly?
                        k_mat=1,       #Measure of 
                        three_sigma=5, #Age width
                        direct_maturation=FALSE, #Input a probability distribution for age at first maturation vs. build from cumulative distribution inputs
                        m_i=c(rep(0,7), 0.0009110512, 0.0015615720, 
                              0.0042202278, 0.0112933590, 0.0294396632, 
                              0.0717770488, 0.1497384993, 0.2310585786, 
                              0.2310585786, 0.1497384993, 0.0717770488,
                              0.0294396632, 0.0112933590, 0.0066928509,
                              rep(0,39)),  #Proportion of females that first mature at the given age
                        max_period=5,
                        direct_pt=FALSE,  #Directly give reproductively ready period probabilities vs. build from data and max_period? 
                        p_t=c(0, 0.38095238, 0.37142857, 0.16507937, 
                              0.08253968, rep(0, 47)), #Probability the reproductively ready period is t years
                        direct_psi=FALSE,              #Build psi from direct vector input vs. maturation and reproductively ready period inputs?
                        psi=NULL,         #Proportion of reproductively ready females by age (vector length max_age)
                        gamma=0.01,       #Probability a female spawns in the MR given reproductively readiness
                        eggs=c(rep(0,7), 17858.32, 19196.99, 20567.28,
                               21929.43, 23349.27, 24775.18, 26243.11, 
                               27685.86, 29146.68, 30671.10, 32132.68, 
                               33675.72, 35201.90, 36758.49, 38313.39, 
                               39869.35, 41409.45, 42992.59, 44535.03, 
                               46151.91, 47672.25, 49359.17, 50889.91, 
                               52537.71, 54138.78, 55773.74, 57365.43, 
                               58985.37, 60526.52, 62200.39, 63870.55, 
                               65417.74, 67086.64, 68720.32, 70346.88, 
                               71994.93, 73619.97, 75342.93, 76795.55, 
                               78508.78, 80057.36, 81574.96, 83282.14, 
                               85053.37, 86523.37, 88186.36, 89790.52, 
                               91420.39, 92923.74, 94449.11, 96010.70,
                               97906.26, 99150.91),  #Number of eggs produced per female by age (vector length max_age)
                        sexratio=0.32,    #Probability a pallid sturgeon is female
                        p_retained=0.001,  #Probability a free embryo is retained in the MR
                        phi0_MR=0.000075)     #Age-0 survival, given free embryo retained in MR 
{
    inputs<- list()
    inputs$max_age<- max_age
    inputs$phi<- phi
    if(direct_psi)
    {
      inputs$psi<- psi
    }
    if(!direct_psi)
    {
      if(direct_maturation)
      {
        inputs$mat$m_i<- m_i
      }
      if(!direct_maturation)
      {
        if(!mat_var_k)
        {
          k_mat<- sqrt(3)*pi/three_sigma 
        }
        mat_Cdist<-1/(1+exp(-k_mat*(a_mat_min:(a_mat_max-1)-a_mat_h)))
        m<-rep(0, a_mat_max-a_mat_min+1)
        m[1]<-mat_Cdist[1]
        for(i in 2:length(mat_Cdist))
        {
          m[i]<- mat_Cdist[i]-mat_Cdist[i-1]
        }
        rm(i)
        m[length(mat_Cdist)+1]<- 1-mat_Cdist[length(mat_Cdist)]
        inputs$mat$a_min<- a_mat_min
        inputs$mat$a_max<- a_mat_max
        inputs$mat$a_h<- a_mat_h
        inputs$mat$k<- k_mat
        inputs$mat$m_i<- c(rep(0,a_mat_min-1), m, rep(0, max_age-a_mat_max))
      }
      if(direct_pt)
      {
        inputs$reproduction$p_t<- p_t
      }
      if(!direct_pt)
      {
        probs<-c(0, 8/21, 13/21*3/5, 
                 13/21*2/5*1/sum(2^(0:(max_period-4)))*2^((max_period-4):0))
        inputs$reproduction$max_period<- max_period
        inputs$reproduction$p_t<- c(probs, rep(0, max_age-a_mat_min-max_period))
      }
      inputs$psi<- rep(0, max_age)
      inputs$psi[a_mat_min]<- inputs$mat$m_i[a_mat_min]
      for(i in (a_mat_min+1):60)
      {
        inputs$psi[i]<- inputs$mat$m_i[i]+
          sum(inputs$psi[a_mat_min:(i-1)]*inputs$reproduction$p_t[i-a_mat_min:(i-1)])
      }
      rm(i)
    }
    inputs$probF<- sexratio
    inputs$eggs<- eggs
    inputs$p_retained<- p_retained
    inputs$phi0_MR<- phi0_MR
    inputs$gamma<- gamma
    return(inputs)
}



## CONSTRUCT THE LESLIE MATRIX, PERFORM EIGEN-, SENSITIVITY, & 
## ELASTICITY ANALYSES
matrix_eigen_analysis<- function(inputs)
                                 # max_age=NULL,  
                                 # phi=NULL,        #Age-1+ survivals (vector length maxage-1)
                                 # psi=NULL,        #Proportion of reproductively ready females by age (vector length maxage)
                                 # gamma=NULL,      #Probability a female spawns in the MR given reproductively readiness
                                 # eggs=NULL,       #Number of eggs produced per female by age (vector length maxage)
                                 # sexratio=NULL,   #Probability a pallid sturgeon is female
                                 # p_retained=NULL, #Probability a free embryo is retained in the MR
                                 # phi0_MR=NULL)    #Age-0 survival, given free embryo retained in MR 
{
  # ERROR HANDLING
  if(!all(sapply(c("max_age", "phi", "psi", "gamma", "eggs", "probF", 
                   "p_retained", "phi0_MR"), exists, where=inputs)))
  {
    return(print("The inputs file must contain all of the following:
                  'max_age', 'phi', 'psi', 'gamma', 'eggs', 'probF', 
                   'p_retained', 'phi0_MR'."))
  }
  maxage<- inputs$max_age  
  phi<- inputs$phi
  psi<- inputs$psi
  gamma<- inputs$gamma
  eggs<- inputs$eggs
  sexratio<- inputs$probF
  p_retained<- inputs$p_retained
  phi0_MR<- inputs$phi0_MR
  if(!(all(sapply(c(maxage, gamma, sexratio, p_retained, phi0_MR), 
                  length)==1) & 
       is.numeric(c(maxage, gamma, sexratio, p_retained, phi0_MR))))
  {
    return(print("Inputs 'maxage', 'gamma', 'sexratio', 'p_retained', and 'phi0_MR' should all be numerical values of length 1."))
  }
  if(!all(c(length(psi), length(eggs))==maxage))
  {
    return(print("Inputs 'psi' and 'eggs' need to be a vector of length 'maxage' (one value for each age)."))
  }
  if(length(phi)!=maxage-1)
  {
    return(print("Input 'phi' needs to be a vector of survivals with length maxage-1."))
  }
  
  # BUILD LESLIE MATRIX
  A<- matrix(0,maxage,maxage)
  ## SURVIVAL VALUES
  A[cbind(2:maxage,1:(maxage-1))]<- phi
  ## FERTILITY VALUES
  A[1,] <- psi*gamma*eggs*sexratio*p_retained*phi0_MR
  
  # EIGENANALYSIS 
  ea<- eigen.analysis(A)
  ea$A<- A
  ea$birth_rate<- round(sum(psi*gamma*eggs*ea$stable.age)/sum(psi*ea$stable.age))
  
  # PARAMETER SENSITIVITIES
  sens<- ea$sensitivities 
  ea$sensitivities<- list()
  ea$sensitivities$entries<- sens
  ea$sensitivities$phi<- rep(0, maxage-1)
  for(i in 1:(maxage-1))
  {
    ea$sensitivities$phi[i]<- sens[i+1,i]
  }
  ea$sensitivities$psi<- sens[1,]*gamma*eggs*sexratio*p_retained*phi0_MR
    # ANALYZE A_MAT_H, K_MAT, AND RR PERIOD PROBS HERE...USE "INPUTS" 
    # AS FUNCTION INPUT
  ea$sensitivities$gamma<- sum(sens[1,]*psi*eggs*sexratio*p_retained*phi0_MR)
  ea$sensitivities$eggs<- sens[1,]*psi*gamma*sexratio*p_retained*phi0_MR
  ea$sensitivities$sexratio<- sum(sens[1,]*psi*gamma*eggs*p_retained*phi0_MR)
  ea$sensitivities$p_retained<- sum(sens[1,]*psi*gamma*eggs*sexratio*phi0_MR)
  ea$sensitivities$phi0_MR<- sum(sens[1,]*psi*gamma*eggs*sexratio*p_retained)
  ea$sensitivities$prod<- sum(sens[1,]*psi*eggs*sexratio)
  if(exists("mat", inputs) & exists("reproduction", inputs))
  {
    a_min<- inputs$mat$a_min
    a_max<- inputs$mat$a_max
    a_h<- inputs$mat$a_h
    k<- inputs$mat$k
    pt<- inputs$reproduction$p_t
    Tmax<- inputs$reproduction$max_period
    # dm_i/dk
    dmi_dk<- rep(0, maxage)
    dmi_dk[a_min]<- (a_min-a_h)*exp(-k*(a_min-a_h))/(1+exp(-k*(a_min-a_h)))^2
    dmi_dk[(a_min+1):(a_max-1)]<- ((a_min+1):(a_max-1)-a_h)*exp(-k*((a_min+1):(a_max-1)-a_h))/(1+exp(-k*((a_min+1):(a_max-1)-a_h)))^2-
      ((a_min+1):(a_max-1)-1-a_h)*exp(-k*((a_min+1):(a_max-1)-1-a_h))/(1+exp(-k*((a_min+1):(a_max-1)-1-a_h)))^2
    dmi_dk[a_max]<- -(a_max-1-a_h)*exp(-k*(a_max-1-a_h))/(1+exp(-k*(a_max-1-a_h)))^2
    # dm_i/dh
    dmi_dh<- rep(0, maxage)
    dmi_dh[a_min]<- -k*exp(-k*(a_min-a_h))/(1+exp(-k*(a_min-a_h)))^2
    dmi_dh[(a_min+1):(a_max-1)]<- k*exp(-k*((a_min+1):(a_max-1)-1-a_h))/(1+exp(-k*((a_min+1):(a_max-1)-1-a_h)))^2-
      k*exp(-k*((a_min+1):(a_max-1)-a_h))/(1+exp(-k*((a_min+1):(a_max-1)-a_h)))^2
    dmi_dk[a_max]<- k*exp(-k*(a_max-1-a_h))/(1+exp(-k*(a_max-1-a_h)))^2
    # dpsi_i/dk
    dpsii_dk<- rep(0, maxage)
    dpsii_dk[a_min]<- dmi_dk[a_min]
    for(i in (a_min+1):maxage)
    {
      dpsii_dk[i]<- dmi_dk[i]+sum(dpsii_dk[a_min:(i-1)]*pt[i-a_min:(i-1)])
    }
    rm(i)
    # dpsi_i/dh
    dpsii_dh<- rep(0, maxage)
    dpsii_dh[a_min]<- dmi_dh[a_min]
    for(i in (a_min+1):maxage)
    {
      dpsii_dh[i]<- dmi_dh[i]+sum(dpsii_dh[a_min:(i-1)]*pt[i-a_min:(i-1)])
    }
    rm(i)
    # dpsi_i/dp_t
    dpsii_dpt<- lapply(1:Tmax, function(t)
    {
      out<- rep(0, maxage)
      out[a_min+t]<- psi[a_min]
      for(i in (a_min+t+1):maxage)
      {
        out[i]<- psi[i-t]+sum(out[(a_min+t):(i-1)]*pt[i-(a_min+t):(i-1)])
      }
      return(out) 
    })
    #dlambda/dk_mat, dlambda/da_h, dlambda/dp_t
    ea$sensitivities$k_mat<- sum(ea$sensitivities$psi*dpsii_dk)
    ea$sensitivities$a_h_mat<- sum(ea$sensitivities$psi*dpsii_dh)
    ### THE FOLLOWING DOES NOT ACCOUNT FOR TRADE-OFFS IN THE FACT THAT THE
    ### p_t's NEED TO SUM TO 1 SO CHANGING ONE WILL CHANGE AT LEAST ONE OTHER
    ea$sensitivities$p_t_reproductive<-sapply(1:Tmax, function(t)
    {
      sum(ea$sensitivities$psi*dpsii_dpt[[t]])
    })
  }
  rm(sens)
  
  # PARAMETER ELASATICITIES
  elas<- ea$elasticities
  ea$elasticities<- list()
  ea$elasticities$entries<- elas
  rm(elas)
  ea$elasticities$phi<- ea$sensitivities$phi*phi/ea$lambda1
  ea$elasticities$psi<- ea$sensitivities$psi*psi/ea$lambda1
  ea$elasticities$gamma<- ea$sensitivities$gamma*gamma/ea$lambda1
  ea$elasticities$eggs<- ea$sensitivities$eggs*eggs/ea$lambda1
  ea$elasticities$sexratio<- ea$sensitivities$sexratio*sexratio/ea$lambda1
  ea$elasticities$p_retained<- ea$sensitivities$p_retained*p_retained/ea$lambda1
  ea$elasticities$phi0_MR<- ea$sensitivities$phi0_MR*phi0_MR/ea$lambda1
  ea$elasticities$prod<- ea$sensitivities$prod*phi0_MR*gamma*p_retained/ea$lambda1
  if(exists("mat", inputs) & exists("reproduction", inputs))
  {
    ea$elasticities$k_mat<- ea$sensitivities$k_mat*k/ea$lambda1
    ea$elasticities$a_h_mat<- ea$sensitivities$a_h_mat*a_h/ea$lambda1
    ### THE FOLLOWING DOES NOT ACCOUNT FOR TRADE-OFFS IN THE FACT THAT THE
    ### p_t's NEED TO SUM TO 1 SO CHANGING ONE WILL CHANGE AT LEAST ONE OTHER
    ea$elasticities$p_t_reproductive<- ea$sensitivities$p_t_reproductive*pt[1:Tmax]/ea$lambda1
  }
  
  # ADD INPUTS
  ea$inputs<- inputs
  
  # RETURN
  return(ea)
}

sens_elas_table<- function(data=NULL,
                           number=10,
                           sensitvities=TRUE,
                           elasticities=TRUE)
{
  sens<-data$sensitivities
  sens$F_i<- sens$entries[1,]
  sens$entries<-NULL
  elas<-data$elasticities
  elas$F_i<- elas$entries[1,] 
  elas$entries<-NULL
  if(exists("mat", data$inputs) & exists("reproduction", data$inputs))
  {
    indx1<- which.max(sens$p_t_reproductive[2:length(sens$p_t_reproductive)])
    indx2<- which.min(sens$p_t_reproductive[2:length(sens$p_t_reproductive)])
    sens$p_t_reproductive<- sens$p_t_reproductive[indx1+1]-sens$p_t_reproductive[indx2+1]
    elas$p_t_reproductive<- sens$p_t_reproductive*inputs$reproduction$p_t[indx1+1]/data$lambda1
  }
  # SENSITIVITIES
  sens_tbl<- data.frame(Parameter=names(unlist(sens)),
                        Sensitivity=unlist(sens))
  sens_tbl$Parameter<-gsub("F_i", "F_", sens_tbl$Parameter)
  sens_tbl$Type<- ifelse(sens_tbl$Parameter %in% paste0("F_", 1:60),
                         "Entry", 
                         ifelse(sens_tbl$Parameter %in% paste0("phi", 1:59),
                                "Both", "Parameter"))
  sens_tbl<- sens_tbl[order(abs(sens_tbl$Sensitivity), decreasing = TRUE),]
  sens_tbl$rank<-1:nrow(sens_tbl)
  rownames(sens_tbl)<- sens_tbl$rank
  sens_tbl$Sensitivity<- round(sens_tbl$Sensitivity,6)
  indx<-max(which(sens_tbl$Sensitivity[number]==sens_tbl$Sensitivity))
  top_sens<- sens_tbl[1:indx,]
  indx<-which(sens_tbl$Parameter %in% c("phi0_MR", "p_retained", "gamma"))
  if(!(all(indx %in% top_sens$rank)))
  {
    top_sens<- rbind(top_sens, sens_tbl[indx,])
    top_sens<- top_sens[!duplicated(top_sens),]
  }
    
  # ELASTICITIES
  elas_tbl<- data.frame(Parameter=names(unlist(elas)),
                        Elasticity=unlist(elas))
  elas_tbl$Parameter<-gsub("F_i", "F_", elas_tbl$Parameter)
  elas_tbl$Type<- ifelse(elas_tbl$Parameter %in% paste0("F_", 1:60),
                         "Entry", 
                         ifelse(elas_tbl$Parameter %in% paste0("phi", 1:59),
                                "Both", "Parameter"))
  elas_tbl<- elas_tbl[order(abs(elas_tbl$Elasticity), decreasing = TRUE),]
  elas_tbl$rank<-1:nrow(elas_tbl)
  rownames(elas_tbl)<- elas_tbl$rank
  elas_tbl$Elasticity<- round(elas_tbl$Elasticity,6)
  indx<-max(which(elas_tbl$Elasticity[number]==elas_tbl$Elasticity))
  top_elas<- elas_tbl[1:indx,]
  indx<-which(elas_tbl$Parameter %in% c("phi0_MR", "p_retained", "gamma"))
  if(!(all(indx %in% top_elas$rank)))
  {
    top_elas<- rbind(top_elas, elas_tbl[indx,])
    top_elas<- top_elas[!duplicated(top_elas),]
  }
  out<-list(Sensitivities=top_sens, Elasticities=top_elas)
  return(out)
}


## BOUNDARY PRODUCT VALUE
boundary_product<- function(inputs=NULL)
{
  # ERROR HANDLING
  if(!all(sapply(c("max_age", "phi", "psi", "eggs", "probF"), exists, where=inputs)))
  {
    return(print("The inputs file must contain all of the following:
                 'max_age', 'phi', 'psi', 'eggs', 'probF'."))
  }
  maxage<- inputs$max_age
  phi<- inputs$phi
  psi<- inputs$psi
  eggs<- inputs$eggs
  sexratio<- inputs$probF
  if(!(all(sapply(c(maxage, sexratio),length)==1) &
       is.numeric(c(maxage, sexratio))))
  {
    return(print("Inputs 'maxage' and 'sexratio' should be numerical values of length 1."))
  }
  if(!all(c(length(psi), length(eggs))==maxage))
  {
    return(print("Inputs 'psi' and 'eggs' need to be a vector of length 'maxage' (one value for each age)."))
  }
  if(length(phi)!=maxage-1)
  {
    return(print("Input 'phi' needs to be a vector of survivals with length maxage-1."))
  }
  # REARRANGE EULER-LOTKA EQUATION FOR CHARACTERISTIC EQUATION WITH LAMBDA=1
  denom<- sapply(2:length(psi), function(i)
  {
      out<- psi[i]*eggs[i]*sexratio*prod(phi[1:(i-1)])
      return(out)
  })
  denom<- c(psi[1]*eggs[1]*sexratio, denom) 
  prod<- 1/sum(denom)
  
  # CHECK LAMBDA=1 IS THE LARGEST E-VALUE
  ## BUILD LESLIE MATRIX
  A<- matrix(0,maxage,maxage)
  ### SURVIVAL VALUES
  A[cbind(2:maxage,1:(maxage-1))]<- phi
  ### FERTILITY VALUES
  A[1,] <- psi*eggs*sexratio*prod
  ## EIGENANALYSIS 
  ea<- eigen.analysis(A)
  check<- round(abs(ea$lambda1-1),14)
  
  # REPORT PRODUCT AND CHECK
  inputs$boundary$product<- prod
  inputs$boundary$check<- check
  return(inputs)
}

## SPAWNING-SURVIVAL CURVES
spawning_survival_retention_curves<- function(boundary_inputs=NULL,
                                              p_retained=seq(0.1, 1, 0.1))
{
  # ERROR HANDLING
  if(!all(sapply(c("product", "check"), exists, where=boundary_inputs$boundary)))
  {
    return(print("Boundary inputs must contain the list boundary with entries 'product' and 'check'."))
  }
  if(boundary_inputs$boundary$check!=0)
  {
    return(print("Boundary check failed."))
  }
  if(!all(sapply(c("max_age", "phi", "psi", "eggs", "probF"), exists, where=boundary_inputs)))
  {
    return(print("The boundary_inputs file must contain all of the following:
                 'max_age', 'phi', 'psi', 'eggs', 'probF'."))
  }
  if(!exists("p_retained", boundary_inputs) & is.null(p_retained))
  {
    return(print("A numeric value or vector 'p_retained', of retention probabilities 
                 needs to be specified within the boundary inputs file."))
  }
  maxage<- boundary_inputs$max_age
  phi<- boundary_inputs$phi
  psi<- boundary_inputs$psi
  eggs<- boundary_inputs$eggs
  sexratio<- boundary_inputs$probF
  prod<-boundary_inputs$boundary$product
  if(!is.null(p_retained)){boundary_inputs$p_retained<- p_retained}
  if(is.null(p_retained)){p_retained<- boundary_inputs$p_retained}
  if(any(p_retained<=0 | p_retained>1)){p_retained<- p_retained[-which(p_retained<=0 | p_retained>1)]}
  if(length(p_retained)==0)
  {
    return(print("Values of 'p_retained' greater than 0 and less than equal to 1
                 must be specified."))
  }
  if(!(all(sapply(c(maxage, sexratio, prod),
                  length)==1) &
       is.numeric(c(maxage, sexratio, prod))))
  {
    return(print("Boundary inputs 'maxage', 'sexratio', and 'boundary$product' should all be numerical values of length 1."))
  }
  if(!all(c(length(psi), length(eggs))==maxage))
  {
    return(print("Boundary inputs 'psi' and 'eggs' need to be a vector of length 'maxage' (one value for each age)."))
  }
  if(length(phi)!=maxage-1)
  {
    return(print("Boundary input 'phi' needs to be a vector of survivals with length maxage-1."))
  }
  curve_dat<- lapply(1:length(p_retained), function(p)
  {
    gamma<- c(0.000001,seq(0.01, 1, 0.01))
    phi0<- prod/(p_retained[p]*gamma)
    indx<-which(phi0>1)
    phi0<- phi0[-indx]
    gamma<-gamma[-indx]
    #SYMMETRICAL SO CAN EXPAND
    tmp<-phi0
    phi0<- c(phi0, gamma)
    gamma<- c(gamma, tmp)
    out<-NULL
    if(length(phi0>0))
    {
      out<-data.frame(gamma=gamma, phi0_MR=phi0, p_retained=p_retained[p])
    }
    return(out)
  })
  curve_dat<- do.call(rbind, curve_dat)
  return(curve_dat)
}

plot_boundary_curves<- function(curve_dat=NULL,
                                gamma_upper=0.75,
                                phi0_upper=0.01,
                                xlabel=expression(paste("MR Spawning Probability  (", gamma, ")")),
                                ylabel=expression(paste("MR Survival Probability  (  ", phi[0], ")")),
                                xaxis="s")
{
  pret<-unique(curve_dat$p_retained)
  pret<- pret[order(pret)]
  tmp<-curve_dat[which(curve_dat$p_retained==pret[1]),]
  tmp<-tmp[order(tmp$gamma),]
  plot(tmp$gamma, tmp$phi0_MR, type="l", 
       xlim=c(0,gamma_upper), ylim=c(0,phi0_upper),
       xlab=xlabel, ylab=ylabel, xaxt=xaxis,
       tck=0.02, mgp=c(1.5,0.1,0))
  #legend(0.4, tmp[which(tmp$gamma==0.4),"phi0_MR"]+0.001, 
  #       paste("retention =", pret[1]), bty='n')
  if(length(pret>1))
  {
    invisible(lapply(2:length(pret), function(i)
    {
      tmp<-curve_dat[which(curve_dat$p_retained==pret[i]),]
      tmp<-tmp[order(tmp$gamma),]
      points(tmp$gamma, tmp$phi0_MR, type="l")
      #if(i==length(pret))
      #{
      #  legend(0, tmp[which(tmp$gamma==0.2),"phi0_MR"]+0.0005, 
      #         paste("retention =", pret[i]), bty='n')
      #}
    })) 
  }
}
