
source("./r/1_global_DSM.R")
source("./r/2_functions_DSM.R")

# COMPUTE ALL TYPES OF RETENTION VALUES
anx<- FALSE
dev<- c("New", "Old")
DSM_ret<- lapply(dev, function(dv)
{
  drft<- c(0.9, 1)
  dev_ret<- lapply(drft, function(dr)
  {
    alts<- c("1", "1a", "1b", "2", "2a", "2b", "NA")
    drft_ret<- lapply(alts, function(alt)
    {
      ## PULL SHEET NAMES
      nms<- read.xlsx(paste0("./dat/R2_Output_Data_", alt, "P.xlsx"), 
                      "Sheet_Names")
      ## DETERMINE RETENTION FOR EACH SHEET
      alt_ret<- lapply(nms[,1], function(x)
      {
        tmp<- read.xlsx(paste0("./dat/R2_Output_Data_", alt, "P.xlsx"), x)
        ret<- compute_retention(data=tmp, development_type=dv,
                                rel_drift_vel=dr, anoxic_incl=anx)
        dt<- strsplit(names(tmp)[8], "\\(")[[1]][2]
        dt<- strsplit(dt, "\\)")[[1]][1]
        dt<- strsplit(dt, "\\.")[[1]][1]
        dt<- as.Date(as.Date(dt, "%d%b%Y"))
        yr<- as.numeric(format(dt, "%Y"))
        out<- data.frame(Alt=alt, Year=yr, Hatch_Date=dt, Retention=ret,
                         Develop_Mod=dv, Drift_Mod=dr, anoxic_layer=anx)
        return(out)
      })
      alt_ret<- do.call(rbind, alt_ret)
      return(alt_ret)
    })
    drft_ret<- do.call(rbind, drft_ret)
    return(drft_ret)
  })
  dev_ret<- do.call(rbind, dev_ret)
  return(dev_ret)
})
DSM_ret<- do.call(rbind, DSM_ret)

# ## FOR anx=TRUE
# write.csv(DSM_ret, "./output/All_Temps_Retentions.csv", row.names = FALSE)
# ## FOR anx=FALSE
# write.csv(DSM_ret, "./output/Above_Anoxic_Retentions.csv", row.names = FALSE)

## FULL TABLE
al<- read.csv("./output/All_Temps_Retentions.csv", stringsAsFactors = FALSE)
al[is.na(al$Alt),]$Alt<- "NA"
ab<- read.csv("./output/Above_Anoxic_Retentions.csv", stringsAsFactors = FALSE)
ab[is.na(ab$Alt),]$Alt<- "NA"
DSM_full<- rbind(al, ab)
write.csv(DSM_full, "./output/Retentions.csv", row.names = FALSE)

# KEY RETENTIONS ONLY
# alts<- c("1", "1a", "1b", "2", "2a", "2b", "NA")
# DSM_ret<- lapply(alts, function(alt)
# {
#   ## PULL SHEET NAMES
#   nms<- read.xlsx(paste0("./dat/R2_Output_Data_", alt, "P.xlsx"), 
#                   "Sheet_Names")
#   ## DETERMINE RETENTION FOR EACH SHEET
#   alt_ret<- lapply(nms[,1], function(x)
#   {
#     tmp<- read.xlsx(paste0("./dat/R2_Output_Data_", alt, "P.xlsx"), x)
#     ret<- compute_retention(data=tmp, development_type="New",
#                             rel_drift_vel=0.9, anoxic_incl=FALSE)
#     dt<- strsplit(names(tmp)[8], "\\(")[[1]][2]
#     dt<- strsplit(dt, "\\)")[[1]][1]
#     dt<- strsplit(dt, "\\.")[[1]][1]
#     dt<- as.Date(as.Date(dt, "%d%b%Y"))
#     yr<- as.numeric(format(dt, "%Y"))
#     out<- data.frame(Alt=alt, Year=yr, Hatch_Date=dt, Retention=ret,
#                      Develop_Mod="New", Drift_Mod=0.9, anoxic_layer=FALSE)
#     return(out)
#   })
#   alt_ret<- do.call(rbind, alt_ret)
#   return(alt_ret)
# })
# DSM_ret<- do.call(rbind, DSM_ret)
# 
# write.csv(DSM_ret, "./output/New_9_above_Retentions.csv")


## COMPARISON TABLES (COMPARE WITH CRAIGS FILES)
DSM_ret<- read.csv("./output/All_Temps_Retentions.csv", stringsAsFactors = FALSE)
DSM_ret$Hatch_Date<- as.Date(DSM_ret$Hatch_Date)
DSM_ret[is.na(DSM_ret$Alt),]$Alt<- "NA"
alts<- c("1", "1a", "1b", "2", "2a", "2b", "NA")
invisible(lapply(alts, function(alt)
{
  tmp<- subset(DSM_ret, Alt==alt)
  # SEE NOTES ON REPEATED HATCH DATES
  tbl<- dcast(tmp, Hatch_Date~Develop_Mod+Drift_Mod, mean,
              value.var="Retention")
  tbl[,2:5]<- tbl[,2:5]
  write.csv(tbl, paste0("./output/Alt", alt, "_Ret_Comp.csv"), row.names = FALSE)
}))

