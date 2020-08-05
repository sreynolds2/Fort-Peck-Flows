
source("./r/1_global_DSM.R")
source("./r/2_functions_DSM.R")

alts<- c("1", "1a", "1b", "2", "2a", "2b", "NA")
DSM_ret<- lapply(alts, function(alt)
{
  ## PULL SHEET NAMES
  nms<- read.xlsx(paste0("./dat/R2_Output_Data_", alt, "P.xlsx"), 
                  "Sheet_Names")
  ## DETERMINE RETENTION FOR EACH SHEET
  alt_ret<- lapply(nms[,1], function(x)
  {
    tmp<- read.xlsx(paste0("./dat/R2_Output_Data_", alt, "P.xlsx"), x)
    ret<- compute_retention(data=tmp, development_type="New",
                            rel_drift_vel=0.9, anoxic_incl=FALSE)
    dt<- strsplit(names(tmp)[8], "\\(")[[1]][2]
    dt<- strsplit(dt, "\\)")[[1]][1]
    dt<- strsplit(dt, "\\.")[[1]][1]
    dt<- as.Date(as.Date(dt, "%d%b%Y"))
    yr<- as.numeric(format(dt, "%Y"))
    out<- data.frame(Alt=alt, Year=yr, Hatch_Date=dt, Retention=ret,
                     Develop_Mod="New", Drift_Mod=0.9, anoxic_layer=FALSE)
    return(out)
  })
  alt_ret<- do.call(rbind, alt_ret)
  return(alt_ret)
})
DSM_ret<- do.call(rbind, DSM_ret)

#alt1<- test
#write.csv(alt1, "./output/Alt1_Retentions.csv")
