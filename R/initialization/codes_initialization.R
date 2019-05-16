
# THIS FILE CREATES THE INITIAL INPUT "CODES" FILE TO BUILD UPON

## AGE-1+ UPPER RIVER DEMOGRAPHIC INPUT DATA AND CODES 
## VALUES FROM POPULATION MODEL DEFAULT INPUTS ON 5/16/19 WITH NOTED EXCEPTIONS
id1<-list()
id1$maxage<- 60
id1$sexratio<- 0.32 
id1$phi<- rep(0.95, id1$maxage-1)
  age_mat_50<-8 
  mat_k<-0.2 	
  age_mat_min<-8 
  age_mat_max<-16
  propM<- 1/(1+exp(-mat_k*(1:id1$maxage-age_mat_50)))
  propM[1:(age_mat_min-1)]<-0
  propM[age_mat_max:id1$maxage]<-1
id1$psi<- propM
  yps<-matrix(0, id1$maxage-age_mat_min, id1$maxage-age_mat_min)
  p<-plogis(-5+2.55*1:nrow(yps))
  yps[1,]<-p
  for(j in 1:(ncol(yps)-1)){yps[j+1,j]<-1-plogis(-5+2.55*j)}
  # ASSUME FISH SPAWNS THE FIRST YEAR IT MATURES (DIFFERENT IN PROCESS 
  # INPUTS FILE AS OF 5/16/19)
  pMat<- rep(0, length(propM))
  pMat[1]<- propM[1]
  for(i in 2:length(pMat))
  {
    pMat[i]<-propM[i]-propM[i-1]
  }
  first_spawn_dist<- pMat[age_mat_min:id1$maxage]/propM[age_mat_min:id1$maxage]
  mat_yrs<-age_mat_min:id1$maxage
  PropSpawn<-matrix(0,length(mat_yrs)-1,length(mat_yrs))
  PropSpawn[1,1]<-first_spawn_dist[1]
  for(i in 2:length(mat_yrs)){PropSpawn[,i]<-
    c(first_spawn_dist[i], rep(0,length(mat_yrs)-2))+yps%*%PropSpawn[,i-1]}
  PropSpawn<-PropSpawn[1,]
id1$eta<- c(rep(0, age_mat_min-1), PropSpawn)
  # NEEDS REWORKING HERE AND IN THE SIMUALATION FILE AS OF 5/16/19
  L_min<- (300 - 1260.167)/277.404
  L_max<- (1200 - 1260.167)/277.404 
  L_mu<-(750- 1260.167)/277.404
  fec_a<- 11.26
  fec_b<- 0.57
  fec_er<- 0.39
  egg_mu_min<- exp(fec_a + fec_b*L_min+fec_er^2/2)
  egg_mu_max<- exp(fec_a + fec_b*L_max+fec_er^2/2)
  egg_mu<- exp(fec_a + fec_b*L_mu+fec_er^2/2)
id1$fec<- c(rep(0, age_mat_min-1), 
               seq(egg_mu_min, egg_mu,
                   length.out = ceiling((id1$maxage-age_mat_min+1)/2)),
               seq(egg_mu, egg_mu_max,
                   length.out = floor((id1$maxage-age_mat_min+1)/2)))
  rm(age_mat_50, mat_k, age_mat_min, age_mat_max, propM, yps, p, pMat, 
     first_spawn_dist, mat_yrs, PropSpawn, i, j, L_min, L_max, L_mu,
     fec_a, fec_b, fec_er, egg_mu_min, egg_mu_max, egg_mu)
id1$id<- 1 
age1plus<-list(id1)
rm(id1)

# DRIFT DATA CODES
source("./R/3_load-and-clean.r") # LOAD AND CLEAN CURRENT ON 5/16/19...
# WILL UPDATE CODE WHEN LOAD-AND-CLEAN UPDATED
drift<- dat
drift$id<- 1:nrow(drift)
rm(dat)

# AGE-0 SURIVIVAL CODES
survival<- data.frame(phi0_MR=c(0.00001, 0.00005, 0.0001, 0.0005, 0.001),
                      phi0_LS=rep(0.001, 5),
                      id=1:5)

# CREATE CODES FILE
codes<-list(age1plus=age1plus, drift=drift, survival=survival)
rm(age1plus, drift, survival)
saveRDS(codes, "./dat/scenario_codes.rds")
rm(codes)

