### CORE FUNCTION TO DO SIMULATIONS	
sim<- function(inputs=NULL, dyn=NULL,
               recruitmentFreq=1,
               stockingFreq=1,
               sizeStructure=FALSE, 
               demographicOnly=FALSE,
               weightCalc=TRUE)
{
  inputs$recruitmentFreq<-recruitmentFreq
  # INSTEAD OF RECRUITMENT FREQUENCY, PERHAPS WE WANT RECRUITMENT FAILURE
  # EVEN MORE SPECIFICALLY, WE MAY WANT SPAWNING FAILURE AND SURVIVAL 
  # FAILURE:  IN THE CASE OF SPAWNING FAILURE, SPN<-0 FOR ALL FISH AND
  # MPS FOR MATURE FISH WOULD INCREASE.  DOES THIS MAKE SENSE?  IF A FISH
  # PRODUCES EGGS, WILL SHE  ALWAYS SPAWN OR CAN SHE REABSORB?  IF THE 
  # LATTER CAN TAKE PLACE HOW DOES THIS EFFECT THE SPAWNING PROBABILITY 
  # FOR THE FOLLOWING YEAR?
  # IN THE CASE OF SURVIVAL FAILURE PHI0<- 0 FOR THAT TIME STEP
  inputs$stockingFreq<-stockingFreq

  
 
  ## ASSIGN OBJECTS FROM DYN INPUT
  k_H<-dyn$k_H
  k_N<-dyn$k_N
  
  Linf_H<-dyn$Linf_H
  Linf_N<-dyn$Linf_N
  
  LEN_H<-dyn$LEN_H
  LEN_N<-dyn$LEN_N
  
  Z_H<-dyn$Z_H
  Z_N<-dyn$Z_N
  
  AGE_H<-dyn$AGE_H
  AGE_N<-dyn$AGE_N
  
  MAT_H<-dyn$MAT_H 
  MAT_N<-dyn$MAT_N
  
  MPS_H<-dyn$MPS_H 
  MPS_N<-dyn$MPS_N 
  
  SEX_H<-dyn$SEX_H
  SEX_N<-dyn$SEX_N
  
  SPN_H<-dyn$SPN_H
  SPN_N<-dyn$SPN_N
  
  EGGS_H<-dyn$EGGS_H 
  EGGS_N<-dyn$EGGS_N 
  
  AGE_0_N_BND<-dyn$AGE_0_N_BND
  AGE_0_H<-inputs$hatchery_age0
  
  inBasinH<-1
  inBasinN<-1
  

  
  m<-dyn$m
  
  
  #rm(list='dyn')	
  
  # SET UP SUMMARIES
  RECRUITS<-matrix(0,nrow=length(m),ncol=inputs$nreps)
  
  MEANLENGTH_N<-matrix(0,nrow=length(m),ncol=inputs$nreps)
  MEANLENGTH_H<-matrix(0,nrow=length(m),ncol=inputs$nreps)
  MEANLENGTH<-matrix(0,nrow=length(m),ncol=inputs$nreps)
  
  N_NAT<- matrix(0,nrow=length(m),ncol=inputs$nreps)
  N_HAT<- matrix(0,nrow=length(m),ncol=inputs$nreps)
  
  N_MAT_N<- matrix(0,nrow=length(m),ncol=inputs$nreps)
  N_MAT_H<- matrix(0,nrow=length(m),ncol=inputs$nreps)
  # END SUMMARIES
  
  
  # SET UP INDICES PRIOR TO RUNNING MODEL
  indx_H<- lapply(1:inputs$nreps,
                  function(x){which(Z_H[,x]==1)}) # ROW INDICES
  tmp<- unlist(lapply(1:inputs$nreps,
                      function(x) rep(x,length(indx_H[[x]])))) # COLUMN INDICES
  indx_H<- cbind(unlist(indx_H),tmp)#row,column
  
  indx_N<- lapply(1:inputs$nreps,
                  function(x){which(Z_N[,x]==1)}) # ROW INDICES
  tmp<- unlist(lapply(1:inputs$nreps,
                      function(x) rep(x,length(indx_N[[x]])))) # COLUMN INDICES
  indx_N<- cbind(unlist(indx_N),tmp)#row,column
  
  
  # PROGRESS BAR
  pb<-txtProgressBar(min=1,max=length(m),initial=0,char="*",style=3)
  
  
  
  
  
  # SIMULATE POPULATION DYNAMICS GIVEN INITIAL STATES
  for(i in 1:length(m))
  {
    setTxtProgressBar(pb, i)
    
    #[1] UPDATE SURVIVAL
    Z_H[indx_H]<- dSurvival(phi_age=inputs$phi,
                            age=AGE_H[indx_H],
                            maxAge=inputs$maxage)
    Z_N[indx_N]<- dSurvival(phi_age=inputs$phi,
                            age=AGE_N[indx_N],
                            maxAge=inputs$maxage)
    
    
    #[6] UPDATE AGE IN MONTHS (AND ZERO OUT DEAD FISH)
    AGE_H[indx_H]<- (AGE_H[indx_H]+1)*Z_H[indx_H]
    AGE_N[indx_N]<- (AGE_N[indx_N]+1)*Z_N[indx_N]
    
    # IF JANUARY, UPDATE MATURITY AND PROJECTED JUNE 
    # MPS & SPAWNING STATUS
    if(m[i]==1)
    {
      tmp_H<-dMaturity(mature = MAT_H[indx_H],
                       age = AGE_H[indx_H],
                       live = Z_H[indx_H],
                       cond_mat_dist = inputs$pMatC)
      tmp_N<-dMaturity(mature = MAT_N[indx_N],
                       age = AGE_N[indx_N],
                       live = Z_N[indx_N],
                       cond_mat_dist = inputs$pMatC)
      
      MAT_H[indx_H] <- tmp_H$mature
      MAT_N[indx_N] <- tmp_N$mature
      
      ### UPDATE MONTHS SINCE SPAWNING
      MPS_H[indx_H] <- dMPS(mps = MPS_H[indx_H],
                            spawn = SPN_H[indx_H],
                            mature = MAT_H[indx_H])
      MPS_N[indx_N] <- dMPS(mps = MPS_N[indx_N],
                            spawn = SPN_N[indx_N],
                            mature = MAT_N[indx_N])
      
      ### UPDATE SPAWNING [NO|YES]
      ### GIVEN SEXUAL MATURITY AND 
      ### TIME SINCE LAST SPAWNING EVENT
      SPN_H[indx_H] <- spawn(mps = MPS_H[indx_H],
                             a=-5,b=2.55,
                             mature = MAT_H[indx_H],
                             FirstSpawn = tmp_H$FirstSpawn)
      SPN_N[indx_N] <- spawn(mps = MPS_N[indx_N],
                             a=-5,b=2.55,
                             mature = MAT_N[indx_N],
                             FirstSpawn = tmp_N$FirstSpawn)
    }
    
    # IF JUNE, NATURAL RECRUITMENT AND SPAWNING MODULES
    if(recruitmentFreq>0 & m[i]==6)
    {
      ### ZERO OUT FISH THAT DIED
      MAT_H<-MAT_H*Z_H 
      MAT_N<-MAT_N*Z_N
      
      MPS_H<-MPS_H*Z_H 
      MPS_N<-MPS_N*Z_N 
      
      SPN_H<-SPN_H*Z_H
      SPN_N<-SPN_N*Z_N
      
      EGGS_H<-EGGS_H*Z_H
      EGGS_N<-EGGS_N*Z_N
      
      ### AGE-1 RECRUITMENT: NATURALLY SPAWNED FISH
      #### SURVIVAL FROM AGE-0 TO AGE-1
      AGE_0_N_BND[]<- rbinom(length(AGE_0_N_BND),
                             AGE_0_N_BND,
                             inputs$phi0)
      if(sum(AGE_0_N_BND)>0)
      {
        chk<-min(nrow(Z_N)-colSums(Z_N)-colSums(AGE_0_N_BND))
        if(chk<0)
        {
          z0<-matrix(0,ncol=ncol(Z_N), nrow=abs(chk))
          Z_N<-rbind(Z_N,z0)
          AGE_N<-rbind(AGE_N,z0)
          MAT_N<-rbind(MAT_N,z0)
          MPS_N<-rbind(MPS_N,z0)
          SPN_N<-rbind(SPN_N,z0)
          SEX_N<-rbind(SEX_N,z0)
          EGGS_N<-rbind(EGGS_N,z0)
          k_N<-rbind(k_N,z0)
          Linf_N<-rbind(Linf_N,z0)
          LEN_N<-rbind(LEN_N,z0)
        }
        # INDEX OF OPEN SLOTS
        indxr<- unlist(sapply(1:inputs$nreps,function(x)
        {
          tmp<-NULL
          if(sum(AGE_0_N_BND[,x])>0)
          {
            tmp<-which(Z_N[,x]==0)[1:sum(AGE_0_N_BND[,x])]
          }
          return(tmp)
        }))		
        indxr<- cbind(c(indxr),sort(rep(1:inputs$nreps,colSums(AGE_0_N_BND))))
        # ADD NEW 1 YEAR OLD RECRUITS
        Z_N[indxr]<-1    
        # ADD AGE OF RECRUITS
        AGE_N[indxr]<-12 	
        # UPDATE GROWTH COEFFICIENTS
        tmp<- ini_growth(n=sum(AGE_0_N_BND),
                         mu_ln_Linf=inputs$ln_Linf_mu,
                         mu_ln_k=inputs$ln_k_mu,
                         vcv=inputs$vcv,
                         maxLinf=inputs$maxLinf) 
        Linf_N[indxr]<-tmp$linf
        k_N[indxr]<-tmp$k
        # ADD  INITIAL LENGTH OF RECRUITS
        ## METHOD 1: RECRUIT LENGTH DISTRIBUTION
        LEN_N[indxr]<-rnorm(length(indxr[,1]),
                            inputs$recruit_mean_length,
                            inputs$recruit_length_sd)				
        ## METHOD 2: LENGTH FROM AGE AND VB GROWTH 
        #LEN_N[indxr]<-dLength(k=k_N[indxr], 
        #                         linf=Linf_N[indxr],
        #                          length1=7,
        #                           dT=1)
        # ASSIGN SEX
        SEX_N[indxr]<-ini_sex(n=length(indxr[,1]),
                              prob_F=inputs$sexratio)
      }
      #### ZERO OUT AGE-0's AFTER THEY MOVE TO AGE-1
      AGE_0_N_BND[]<-0
      
      ##### NATURAL REPRODUCTION
      ### UPDATE THE NUMBER OF EGGS IN A FEMALE 
      ### GIVEN SEX AND SPAWNING STATUS
      EGGS_H[indx_H]<-fecundity(fl=LEN_H[indx_H],
                                a=inputs$fec_a,
                                b=inputs$fec_b,
                                er=inputs$fec_er,
                                sex=SEX_H[indx_H],
                                spawn=SPN_H[indx_H])*inBasinH
      EGGS_N[indx_N]<-fecundity(fl=LEN_N[indx_N],
                                a=inputs$fec_a,
                                b=inputs$fec_b,
                                er=inputs$fec_er,
                                sex=SEX_N[indx_N],
                                spawn=SPN_N[indx_N])*inBasinN
      
      ## NUMBER OF EGGS
      AGE_0_N_BND<- matrix(colSums(EGGS_N)+colSums(EGGS_H),nrow=1)
      
      ### eggs --> embryos	
      ### STATUS: DONE NEEDS TO BE MODIFIED WITH DENSITY<-DENSITY DEPENDENT SURVIVAL OR JUST BY BEND??? (NOW DONE BY BEND)
      AGE_0_N_BND[]<- rbinom(length(AGE_0_N_BND),
                             c(AGE_0_N_BND),
                             inputs$pr_embryo) 
      
      ### embryos --> free embryos
      AGE_0_N_BND[]<- rbinom(length(AGE_0_N_BND),
                             c(AGE_0_N_BND),
                             inputs$phi_embryo)
      ### free embryos --> exogenously feeding age0's
      AGE_0_N_BND[]<- rbinom(length(AGE_0_N_BND),
                             c(AGE_0_N_BND),
                             inputs$phi_free_embryo) 
      
      
      ### ADJUST FOR THE AGE-0 THAT WERE INTERCEPTED AND RETAINED IN
      ### THE BASIN AND KEEP TRACK OF HOW MANY DRIFTED OUT OF THE BASIN
      AGE_0_N_BND[]<- rbinom(length(AGE_0_N_BND),
                             c(AGE_0_N_BND),
                             inputs$p_retained)
    }
    
    
    # IF JUNE, RECRUITMENT OF STOCKED FINGERLINGS AND 
    # BROODSTOCK REPRODUCTION 
    if(stockingFreq>0 & m[i]==6)
    {
      ### ZERO OUT FISH THAT DIED
      MAT_H<-MAT_H*Z_H 
      MAT_N<-MAT_N*Z_N
      
      MPS_H<-MPS_H*Z_H 
      MPS_N<-MPS_N*Z_N 
      
      SPN_H<-SPN_H*Z_H
      SPN_N<-SPN_N*Z_N
      
      EGGS_H<-EGGS_H*Z_H
      EGGS_N<-EGGS_N*Z_N
      
      ### AGE-1 RECRUITMENT: STOCKED FINGERLINGS
      #### SURVIVAL FROM AGE-0 TO AGE-1
      if(sum(AGE_0_H$number)>0)
      {
        AGE_0_H_DAT<- matrix(rbinom(inputs$nreps*nrow(AGE_0_H),
                                    AGE_0_H$number,
                                    AGE_0_H$survival_est),
                             ncol=inputs$nreps,
                             nrow=nrow(AGE_0_H))
      }
      if(sum(AGE_0_H$number)==0)
      {
        AGE_0_H_DAT<- matrix(0, ncol=inputs$nreps, nrow=1)
      }
      #### ADD SURVIVING FISH TO POPULATION
      if(sum(AGE_0_H_DAT)>0)
      {
        chk<-min(nrow(Z_H)-colSums(Z_H)-colSums(AGE_0_H_DAT))
        if(chk<0)
        {
          z0<-matrix(0,ncol=ncol(Z_H), nrow=abs(chk))
          Z_H<-rbind(Z_H,z0)
          AGE_H<-rbind(AGE_H,z0)
          MAT_H<-rbind(MAT_H,z0)
          MPS_H<-rbind(MPS_H,z0)
          SPN_H<-rbind(SPN_H,z0)
          SEX_H<-rbind(SEX_H,z0)
          EGGS_H<-rbind(EGGS_H,z0)
          k_H<-rbind(k_H,z0)
          Linf_H<-rbind(Linf_H,z0)
          LEN_H<-rbind(LEN_H,z0)
        }
        ### INDEX OF OPEN SLOTS
        indxr<- unlist(sapply(1:inputs$nreps,function(x)
        {
          tmp<-NULL
          if(sum(AGE_0_H_DAT[,x])>0)
          {
            tmp<-which(Z_H[,x]==0)[1:sum(AGE_0_H_DAT[,x])]
          }
          return(tmp)
        }))		
        indxr<- cbind(c(indxr),sort(rep(1:inputs$nreps,colSums(AGE_0_H_DAT))))
        # ADD NEW 1 YEAR OLD RECRUITS
        Z_H[indxr]<-1 
        # ADD AGE OF RECRUITS
        AGE_H[indxr]<-12 	
        # UPDATE GROWTH COEFFICIENTS
        tmp<- ini_growth(n=sum(AGE_0_H_DAT),
                         mu_ln_Linf=inputs$ln_Linf_mu,
                         mu_ln_k=inputs$ln_k_mu,
                         vcv=inputs$vcv,
                         maxLinf=inputs$maxLinf) 
        Linf_H[indxr]<-tmp$linf
        k_H[indxr]<-tmp$k
        # ADD  INITIAL LENGTH OF RECRUITS
        ## METHOD 1: RECRUIT DISTRIBUTION
        LEN_H[indxr]<-rnorm(length(indxr[,1]),
                            inputs$recruit_mean_length,
                            inputs$recruit_length_sd)				
        ### METHOD 2: LENGTH FROM AGE AND VB GROWTH 
        #LEN_H[indxr]<-rnorm(length(indxr[,1]),
        #                    rep(rep(AGE_0_H$length_mn,inputs$nreps), 
        #                        as.vector(AGE_0_H_DAT)),
        #                    rep(rep(AGE_0_H$length_sd,inputs$nreps), 
        #                        as.vector(AGE_0_H_DAT)))
        #Linf_H[indxr]<-ifelse(LEN_H[indxr]<Linf_H[indxr], 
        #                      Linf_H[indxr],
        #                      LEN_H[indxr]*1.1)
        #LEN_H[indxr]<-dLength(k=k_H[indxr], 
        #                      linf=Linf_H[indxr],
        #                      length1=LEN_H[indxr]
        #                      dT=0.5)
        # ASSIGN SEX
        SEX_H[indxr]<-ini_sex(n=length(indxr[,1]),
                              prob_F=inputs$sexratio)
      }
      #### ZERO OUT AGE-0's AFTER THEY MOVE TO AGE-1
      AGE_0_H_DAT[]<-0
      AGE_0_H<-NULL
    }
    
    
    # IF SEPTEMBER, STOCKING MODULE
    ## NEED TO UPDATE IF WANT TO INCLUDE OTHER MONTHS
    if(stockingFreq>0 & m[i]==9)
    {
      ### YEARLING STOCKING (AGE-1)
      if(any(inputs$yearling$month==m[i]))
      {
        yearling<-inputs$yearling[which(inputs$yearling$month==m[i]),]
        if(any(yearling$stocking_no>0))
        {
          #### CREATE ENOUGH OPEN SPACE FOR NEW AGE-1 FISH
          chk<-min(nrow(Z_H)-colSums(Z_H)-sum(yearling$stocking_no))
          if(chk<0)
          {
            z0<-matrix(0,ncol=ncol(Z_H), nrow=abs(chk))
            Z_H<-rbind(Z_H,z0)
            AGE_H<-rbind(AGE_H,z0)
            MAT_H<-rbind(MAT_H,z0)
            MPS_H<-rbind(MPS_H,z0)
            SPN_H<-rbind(SPN_H,z0)
            SEX_H<-rbind(SEX_H,z0)
            EGGS_H<-rbind(EGGS_H,z0)
            k_H<-rbind(k_H,z0)
            Linf_H<-rbind(Linf_H,z0)
            LEN_H<-rbind(LEN_H,z0)
          }
          names(yearling)[which(names(yearling)=="stocking_no")]<-"number"
          #### GET INDEXES OF OPEN SLOTS TO STICK STOCKED INDIVIDUALS
          indx_R<- lapply(1:inputs$nreps,
                          function(x){out<- which(Z_H[,x]==0)[1:sum(yearling$number)]}) 
          indx_R<- cbind(unlist(indx_R),
                          rep(1:inputs$nreps, each=sum(yearling$number)))
            
          #### ADD IN NEWLY STOCKED INDIVIDUALS	
          Z_H[indx_R]<- 1
          ##### INITIALIZE LENGTH
          LEN_H[indx_R]<- rnorm(sum(yearling$number),
                                rep(yearling$length_mn, 
                                    yearling$number),
                                rep(yearling$length_sd,
                                    yearling$number))
          LEN_H[indx_R]<-ifelse(LEN_H[indx_R]<0, 100, LEN_H[indx_R])
          
          ##### AGE	
          AGE_H[indx_R]<- rep(yearling$age, yearling$number)
          ##### GROWTH COEFFICIENTS
          tmp<- ini_growth(n=sum(yearling$number),
                           mu_ln_Linf=inputs$ln_Linf_mu,
                           mu_ln_k=inputs$ln_k_mu,
                           vcv=inputs$vcv,
                           maxLinf=inputs$maxLinf) 
          Linf_H[indx_R]<-tmp$linf
          k_H[indx_R]<-tmp$k
          ##### ASSIGN MATURATION, MPS, SPAWNING STATUS, & EGGS
          MAT_H[indx_R]<- 0	
          MPS_H[indx_R]<- 0 
          SPN_H[indx_R]<- 0 
          EGGS_H[indx_R]<- 0 
          
          ##### ASSIGN SEX
          SEX_H[indx_R]<-rbinom(length(indx_R[,1]), 1, p=0.5)
        }
      }
      ### ZERO OUT AGE-1 FISH IN HATCHERY
      BROOD_1<-NULL #OK UNLESS USING MULTIPLE STOCKING MONTHS OR AGES ABOVE AGE-1
      yearling<-NULL
      ### FINGERLING STOCKING (AGE-0)
      #### UPDATE NUMBER OF OFFSPRING AVAILABLE FOR STOCKING
      #### DETERMINE NUMBER OF FISH STOCKED IN EACH BEND
      if(any(inputs$fingerling$month==m[i]))
      {
        fingerling<-inputs$fingerling[which(inputs$fingerling$month==m[i]),]
        ### STOCKED AGE-0's
        AGE_0_H<-data.frame(number=sum(fingerling$stocking_no),
                            survival_est=sum(fingerling$stocking_no*fingerling$phi0_mn)/sum(fingerling$stocking_no))
      }
      
    }
    
    ### UPDATE INDICES FOR FISH THAT ARE ALIVE; INCLUDING RECRUITS
    indx_H<- lapply(1:inputs$nreps,
                    function(x){which(Z_H[,x]==1)}) # ROW INDICES
    tmp<- unlist(lapply(1:inputs$nreps,
                        function(x) rep(x,length(indx_H[[x]])))) # COLUMN INDICES
    indx_H<- cbind(unlist(indx_H),tmp)#row,column
    
    indx_N<- lapply(1:inputs$nreps,
                    function(x){which(Z_N[,x]==1)}) # ROW INDICES
    tmp<- unlist(lapply(1:inputs$nreps,
                        function(x) rep(x,length(indx_N[[x]])))) # COLUMN INDICES
    indx_N<- cbind(unlist(indx_N),tmp)#row,column

    #[3] UPDATE TOTAL LENGTH (AND ZERO OUT DEAD FISH) 
    LEN_H[indx_H]<-dLength(k=k_H[indx_H],
                           linf=Linf_H[indx_H],
                           dT=1/12,
                           length1=LEN_H[indx_H])*Z_H[indx_H]
    LEN_N[indx_N]<-dLength(k=k_N[indx_N],
                           linf=Linf_N[indx_N],
                           dT=1/12,
                           length1=LEN_N[indx_N])*Z_N[indx_N]
    ## END POPULATION DYNAMICS
    
    ## SUMMARIES 
    ### ABUNDANCE AGE-1+
    N_NAT[i,]<-colSums(Z_N*inBasinN) 
    N_HAT[i,]<-colSums(Z_H*inBasinH) 
    
    N_MAT_N[i,]<-colSums(Z_N*MAT_N*inBasinN)
    N_MAT_H[i,]<-colSums(Z_H*MAT_H*inBasinH)
    
    ### MEAN LENGTH IN MM
    MEANLENGTH_N[i,]<- colSums(LEN_N*inBasinN)/colSums(Z_N*inBasinN)	
    MEANLENGTH_H[i,]<- colSums(LEN_H*inBasinH)/colSums(Z_H*inBasinH)
    MEANLENGTH[i,]<-(colSums(LEN_H*inBasinH)+ colSums(LEN_N*inBasinN))/(colSums(Z_H*inBasinH)+colSums(Z_N*inBasinN))

    ### AGE-1 RECRUITS; NATURAL ORIGIN
    indx_R<- lapply(1:inputs$nreps,function(x)
    {
      if(!inputs$migration)
      {
        out<- which(AGE_N[,x]>0 & AGE_N[,x]<24 & Z_N[,x]==1)
      }
      if(inputs$migration)
      {
        out<- which(AGE_N[,x]>0 & AGE_N[,x]<24 & Z_N[,x]==1 & inBasinN[,x]==1)
      }
    }) 		
    RECRUITS[i,]<- sapply(1:inputs$nreps,
                          function(x)
                          { 
                            length(indx_R[[x]])
                          })
  }# end i 

  out<-list(
    total_N=N_NAT,
    total_H=N_HAT,
    total_mat_N=N_MAT_N,
    total_mat_H=N_MAT_H,
    months=m,
    recruits=RECRUITS,
    mean_length_n=MEANLENGTH_N,
    mean_length_h=MEANLENGTH_H,
    mean_length=MEANLENGTH,
    post_length=c(LEN_H[LEN_H[,1]>0,1],LEN_N[LEN_N[,1]>0,1]), # LENGTH DISTRIBUTION POST SIMULATION
    years=inputs$startYear+cumsum(rep(1,length(m))/12),
    #init_summary=init_summary, ####fixme#####
    LeslieMatrix=list(lambda=lambda, stable_age_dist=sad),
    inputs=inputs)
  return(out)	
}
