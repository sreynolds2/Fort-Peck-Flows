
source("./R/0_default-parameters.r")
source("./R/1_global.r")

## RUNS OUT OF MEMORY
#if(full_run)
#{
  alt_files<- list.files("./dat", "_Retention_Out_12.xlsx")
  #dat<- lapply(1:length(alt_files), function(x)
  #{
  x<- 1
    alt_dat<- read.xlsx(paste0("./dat/", alt_files[x]), "Retention")
    #alt_dat<- alt_dat[,1:2]
    alt_dat<- alt_dat[,c(1,4)]
    ## ERROR HANDLING
    if(names(alt_dat)[1]!="Hatch.Date"){return(print("First column does not give hatch date."))}
    if(as.character(alt_dat[1,2])!="New;0.9"){return(print("Issues with extracting correct retention probabilities."))}
    ## CLEAN FILE GIVEN NO ERRORS
    alt_dat<- alt_dat[2:nrow(alt_dat),]
    names(alt_dat)<- c("Hatch_Date", "Retention_Probability")
    if(any(is.na(alt_dat$Retention_Probability) & is.na(alt_dat$Hatch_Date)))
    {
      indx<- which(is.na(alt_dat$Retention_Probability) & is.na(alt_dat$Hatch_Date))
      alt_dat<- alt_dat[-indx, ]
      rm(indx)
    }
    ## MISSING DATA HANDLING
    if(any(is.na(alt_dat$Retention_Probability) | is.na(alt_dat$Hatch_Date)))
    {
      return(print("NA's present in data."))
    }
    ## ADD ESTIMATE TYPE
    alt_dat$Estimate_Type<- "New;0.9"
    ## ADD ALTERNATIVE/VARIANT NAME
    alt_dat$Flow_Scenario<- unlist(strsplit(alt_files[x], "_Retention_Out_12.xlsx"))
    ## ADD IN FLOW YEAR
    alt_dat$Year<- as.numeric(format(alt_dat[,1], "%Y"))
    #return(alt_dat)
  #})
    write.csv(alt_dat, paste0("./dat/",
                              unlist(strsplit(alt_files[x], "_Retention_Out_12.xlsx")),
                              "_retention_data.csv"), row.names = FALSE)
  #dat<-do.call(rbind, dat)
  #write.csv(dat, "./dat/scenario_retention_data.csv", row.names = FALSE)
#}


if(full_run)
{
  alt_files<- list.files("./dat", "_retention_data.csv")
  ## REMOVE OLD FILE
  if(length(grep("scenario_retention_data.csv", alt_files))>0)
  {
   alt_files<- alt_files[-grep("scenario_retention_data.csv", alt_files)] 
  }
  dat<- lapply(1:length(alt_files), function(x)
  {
    alt_dat<- read.csv(paste0("./dat/", alt_files[x]))
    return(alt_dat)
  })
  dat<-do.call(rbind, dat)
  write.csv(dat, "./dat/scenario_retention_data.csv", row.names = FALSE)
  print("NOTE DUE TO MEMORY LIMITS THIS IS RAN FROM THE .CSV FILES; UPDATE THESE FILES PRIOR TO COMPLETING A FULL RUN!")
}

dat<- read.csv("./dat/scenario_retention_data.csv")







# ### (OLD) PRE-FEBRUARY 2020 DATA 
# # CLEAN FILES RUNS OUT OF MEMORY...
#   alt_files<- list.files("./dat", "_Summary.xlsx")
#   # EXCLUDE OLD FILE
#   alt_files<- setdiff(alt_files, "Alt1_CTU_Ret_Summary.xlsx")
#   # CLEAN DATA FILES (REMOVE LOADED NAs)
# invisible(lapply(1:length(alt_files), function(x)
# {
#   dat<- read.xlsx(paste0("./dat/", alt_files[x]), "Outputs")
#   NA_rows<-which(sapply(1:nrow(dat), function(x){all(is.na(dat[x,]))}))
#   if(length(NA_rows)>0){dat<- dat[-NA_rows, ]}
#   NA_cols<- which(sapply(1:ncol(dat), function(x){all(is.na(dat[2:nrow(dat),x]))}))
#   if(length(NA_cols)>0){dat<- dat[,-NA_cols]}
#   write.csv(dat, paste0("./dat/", strsplit(alt_files[x], ".xlsx")[[1]][1],
#                         "_Cleaned.csv"), row.names=FALSE)
# }))
#   SA_files<- list.files("./dat", "_Ret_SA.xlsx")
#   invisible(lapply(1:length(SA_files), function(x)
#   {
#      SA_dat<- read.xlsx(paste0("./dat/", SA_files[x]), "Retention",
#                         endRow = 3)
#      SA_dat<-SA_dat[,1:10]
#      write.csv(SA_dat, paste0("./dat/", strsplit(SA_files[x], ".xlsx")[[1]][1],
#                               "_Cleaned.csv"), row.names=FALSE)
#    }))


# if(full_run)
# {
#   alt_files<- list.files("./dat", "_Summary_Cleaned.csv")
#   # EXCLUDE OLD FILE
#   alt_files<- setdiff(alt_files, "Alt1_CTU_Ret_Summary_Cleaned.csv")
#   alt_files<- setdiff(alt_files, "Alt2b_Summary_Cleaned.csv")
#   dat<- lapply(1:length(alt_files), function(x)
#   {
#     alt_dat<- read.csv(paste0("./dat/", alt_files[x]))
#     p_ret<- sapply(seq(3,ncol(alt_dat),3), function(i)
#     {
#       indx<- min(which(as.numeric(as.character(alt_dat[2:nrow(alt_dat),i]))>200))
#       tmp<- alt_dat[indx:(indx+1),i:(i+1)]
#       weights<- abs(as.numeric(as.character(tmp[,1]))-200)
#       weights<- weights/sum(weights)
#       out<- weights[1]*as.numeric(as.character(tmp[2,2]))+
#         weights[2]*as.numeric(as.character(tmp[1,2]))
#       return(out) 
#     })
#     names1<-names(alt_dat[,seq(2,ncol(alt_dat),3)])
#     if(!grepl("NA", alt_files[x]))
#     {
#       if(any(grepl("NA", names1))){names1<-names1[-grep("NA", names1)]}
#     }
#     if(grepl("NA", alt_files[x]))
#     {
#       names1<- gsub("NA", "NoAct", names1)
#       names1[5]<- "NoAct_11_LF" #CORRECT FIX???
#     }
#     tmp<- strsplit(names1, "_")
#     alt<- sapply(tmp, "[[", 1)
#     yr<- sapply(tmp, "[[", 2)
#     temp<- sapply(tmp, "[[", 3)
#     out<- data.frame(scenario=alt, year=yr, temperature_flow=temp,
#                      p_retained=p_ret)
#     return(out)
#   })
#   dat<-do.call(rbind, dat)
#   
#   SA_files<- list.files("./dat", "_Ret_SA_Cleaned.csv")
#   SA<- lapply(1:length(SA_files), function(x)
#   {
#     SA_dat<- read.csv(paste0("./dat/", SA_files[x]))
#     tmp<- strsplit(SA_files[x], "_")[[1]]
#     alt<- rep(tmp[1], 9)
#     yr<- rep(as.numeric(tmp[2]), 9)
#     temp<- c("LF", "MF", "HF", "L", "M", "H", "LP", "MP", "HP")
#     sday<- rep(as.Date(SA_dat[2,1]),9)
#     pret<- sapply(2:10, function(y){as.numeric(as.character(SA_dat[2,y]))})
#     out<- data.frame(scenario=alt, year=yr, temperature_flow=temp,
#                      p_retained=pret, spawn_date=sday)
#     return(out)
#   })
#   SA<-do.call(rbind, SA)
#   
#   dat<-rbind.fill(dat, SA)
#   dat$id<-1:nrow(dat)
#   write.csv(dat, "./dat/scenario_retention_data.csv", row.names = FALSE)
# }
# 
# dat<- read.csv("./dat/scenario_retention_data.csv")
# 
# 


# if(full_run)
# {
#   alt1<- read.xlsx("./dat/Alt1_CTU_Ret_Summary.xlsx", "Outputs")
#   all(is.na(alt1[,(ncol(alt1)-3):ncol(alt1)]))
#   alt1<-alt1[,1:(ncol(alt1)-4)]
#   all(is.na(alt1[(nrow(alt1)-2):nrow(alt1),]))
#   alt1<-alt1[1:(nrow(alt1)-3),]
#   names1<-names(alt1[,seq(2,ncol(alt1),3)])
#   p_ret<- sapply(seq(3,ncol(alt1),3), function(i)
#   {
#     indx<- min(which(as.numeric(as.character(alt1[2:nrow(alt1),i]))>200))
#     tmp<- alt1[indx:(indx+1),i:(i+1)]
#     weights<- abs(as.numeric(as.character(tmp[,1]))-200)
#     weights<- weights/sum(weights)
#     out<- weights[1]*as.numeric(as.character(tmp[2,2]))+
#       weights[2]*as.numeric(as.character(tmp[1,2]))
#     return(out) 
#   })
#   dat<- data.frame(scenario=names1, p_retained=p_ret)
#   dat$id<-1:nrow(dat)
#   rm(alt1, names1, p_ret)
#   write.csv(dat, "./dat/scenario_retention_data.csv", row.names = FALSE)
# }
# 
# dat<- read.csv("./dat/scenario_retention_data.csv")
