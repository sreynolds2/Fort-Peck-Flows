source("./r/1_global_DSM.R")
source("./r/2_functions_EIS.R")

if(fullrun_EIS)
{
  # COMPUTE ALL TYPES OF RETENTION VALUES
  alts<- c("1", "1a", "1b", "2", "2a", "2b", "NA")
  EIS_ret<- lapply(alts, function(alt)
  {
    ## PULL SHEET NAMES
    nms<- read.xlsx(paste0("./dat/R3_Output_Data_", alt, "P.xlsx"), 
                    "Sheet_Names")
    ## DETERMINE RETENTION FOR EACH SHEET
    alt_ret<- lapply(nms[,1], function(x)
    {
      ### PULL PARTICULAR SHEET
      tmp<- read.xlsx(paste0("./dat/R3_Output_Data_", alt, "P.xlsx"), x)
      ### GATHER HATCH DATE INFORMATION FOR THE SHEET
      dt<- strsplit(names(tmp)[8], "\\(")[[1]][2]
      dt<- strsplit(dt, "\\)")[[1]][1]
      dt<- strsplit(dt, "\\.")[[1]][1]
      dt<- as.Date(as.Date(dt, "%d%b%Y"))
      yr<- as.numeric(format(dt, "%Y"))
      ### INCLUDE VARIOUS TYPES OF RETENTION VALUES
      anoxic_temps<- c(TRUE, FALSE)
      anx_ret<- lapply(anoxic_temps, function(anx)
      {
        dev<- c("New", "Old")
        dev_ret<- lapply(dev, function(dv)
        {
          drft<- c(0.9, 1)
          drft_ret<- lapply(drft, function(dr)
          {
            #### COMPUTE RETENTION
            ret<- compute_retention(data=tmp, development_type=dv,
                                    rel_drift_vel=dr, anoxic_incl=anx)
            out<- data.frame(Alt=alt, Year=yr, Hatch_Date=dt, Retention=ret,
                             Develop_Mod=dv, Drift_Mod=dr, anoxic_layer=anx)
            return(out)
          })
          drft_ret<- do.call(rbind, drft_ret)
          return(drft_ret)
        })
        dev_ret<- do.call(rbind, dev_ret)
        return(dev_ret)
      })
      anx_ret<- do.call(rbind, anx_ret)
      return(anx_ret)
    })
    alt_ret<- do.call(rbind, alt_ret)
    return(alt_ret)
  })
  EIS_ret<- do.call(rbind, EIS_ret)
  # SAVE FILES
  write.csv(EIS_ret, "./output/EIS_Retentions.csv", row.names = FALSE)
  
  ## FOR anx=TRUE
  tmp<- subset(EIS_ret, anoxic_layer==TRUE)
  write.csv(tmp, "./output/EIS_All_Temps_Retentions.csv", row.names = FALSE)
  ## FOR anx=FALSE
  tmp<- subset(EIS_ret, anoxic_layer==FALSE)
  write.csv(tmp, "./output/EIS_Above_Anoxic_Retentions.csv", row.names = FALSE)
  
  ## FOR KEY RETENTIONS ONLY
  tmp<- subset(tmp, Develop_Mod=="New" & Drift_Mod==0.9)
  write.csv(tmp, "./output/EIS_New_9_above_Retentions.csv", 
            row.names = FALSE)
}

if(!fullrun_EIS)
{
  ## FULL DATA
  EIS_full<- read.csv("./output/EIS_Retentions.csv", 
                      stringsAsFactors = FALSE)
  EIS_full$Hatch_Date<- as.Date(EIS_full$Hatch_Date)
  EIS_full[is.na(EIS_full$Alt),]$Alt<- "NA"
  ## KEY DATA
  EIS_ret<- read.csv("./output/EIS_New_9_above_Retentions.csv", 
                     stringsAsFactors = FALSE)
  EIS_ret$Hatch_Date<- as.Date(EIS_ret$Hatch_Date)
  EIS_ret[is.na(EIS_ret$Alt),]$Alt<- "NA"
}
