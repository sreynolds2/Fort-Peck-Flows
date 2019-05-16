
source("./R/1_global.r")
source("./R/2_functions.r")
source("./R/3_load-and-clean.r")

codes<-readRDS("./dat/scenario_codes.rds")

out<- demog_output(1,1,1,codes)




# ids_age1<- 1:length(codes$age1plus)
# ids_drift<- unique(codes$drift$id)
# ids_surv<- unique(codes$survival$id)


# inputs<-list()
# inputs$maxage<- 60
# inputs$sexratio<- 0.5 
# inputs$phi<- c(0.2, 0.5, 0.7, rep(0.9, inputs$maxage-4)) 
# inputs$psi<- c(rep(0, 7), 0.2, 0.5, 0.7, 0.9, 1, rep(1, inputs$maxage-12))
# inputs$eta<- c(rep(0, 7), rep(1/3, inputs$maxage-7))
# inputs$fec<- c(rep(0, 7), seq(12000, 24000, length.out = inputs$maxage-7))
# 
# phi0_MR<- c(0.00001, 0.00005, 0.0001, 0.0005, 0.001)
# phi0_LS<- 0.001

