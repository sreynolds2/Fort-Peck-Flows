
source("./R/1_global.r")
source("./R/2_functions.r")
source("./R/3_load-and-clean.r")

# # INITIAL RUN
# source("./R/initialization/codes_initialization.R")
# codes<-readRDS("./dat/scenario_codes.rds")
# ids_age1<- 1:length(codes$age1plus)
# ids_drift<- unique(codes$drift$id)
# ids_surv<- unique(codes$survival$id)
# 
# invisible(
#   lapply(ids_age1, function(i)
#   {
#     lapply(ids_drift, function(j)
#     {
#       lapply(ids_surv, function(k)
#       {
#         out<- demog_output(i,j,k,codes)
#         out2<- param_sens_elas(demog_output=out,
#                                codes=codes)
#         rm(out, out2)
#       })
#     })
#   })
# )


# ADD NEW SCENARIOS TO ANALYZE
## AGE-1+ DEMOGRAPHIC INPUT SCENARIOS
new_age1_ids<-
  new_age1plus_scenario(maxage=60, sexratio=0.5,
                        phi=c(0.2, 0.5, 0.7, rep(0.9, 56)),
                        psi=c(rep(0, 7), 0.2, 0.5, 0.7, 0.9, 1, rep(1, 48)),
                        eta=c(rep(0, 7), rep(1/3, 53)),
                        fec=c(rep(0, 7), seq(12000, 24000, length.out = 53)))

## DRIFT SCENARIOS
### THIS REQUIRES AN UPDATED DRIFT DATA EXCEL FILE TO BE ADDED TO THE REPO
fullrun<- TRUE
source("./R/3_load-and-clean.r")
rm(fullrun)
new_drift_ids<- new_drift_scenario(dat)

## SURVIVAL SCENARIOS
new_surv_ids<-
  new_survival_scenario(phi0_MR=c(0.00001, 0.00005, 0.0001, 0.0005, 0.001),
                        phi0_LS=rep(0.01,5))



# ANALYZE NEW SCENARIO COMBINATIONS
codes<- readRDS("./dat/scenario_codes.rds")
ids_age1<- 1:length(codes$age1plus)
ids_drift<- unique(codes$drift$id)
ids_surv<- unique(codes$survival$id)
rec<- readRDS("./output/analysis_records.rds")
analyzed_ids<-rec$id[which(rec$demographic==TRUE)]

invisible(
  lapply(ids_age1, function(i)
  {
    lapply(ids_drift, function(j)
    {
      lapply(ids_surv, function(k)
      {
        if(all(analyzed_ids != paste0(i, "-", j, "-", k)))
        {
          out<- demog_output(i,j,k,codes)
          out2<- param_sens_elas(demog_output=out, codes=codes)
          rm(out)
        }
      })
    })
  })
)


rec<- readRDS("./output/analysis_records.rds")
rec<- subset(rec, parameter_specific==FALSE)
lapply(1:nrow(rec), function(x)
{
  
})
