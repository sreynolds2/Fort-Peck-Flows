
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

