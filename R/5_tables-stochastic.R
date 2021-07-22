

## BASELINE TIME TO QUASIEXTINCTION RESULTS
dat<- readRDS("./output/_stochastic/Full_Data_1-1.rds")

vdat<- lapply(1:7, function(y)
{
  nm<- names(dat$extinction)[y]
  yrs<- dat$extinction[[y]]$extinction_yr
  out<- data.frame(flow_scenario=rep(nm,length(yrs)),
                   extinction_yr=yrs)
  return(out)
})
vdat<- do.call("rbind", vdat)
vdat$flow_scenario<- factor(vdat$flow_scenario, 
                            c("NoAct", "1a", "1", "1b", "2a", "2", "2b"))

tbl<- aggregate(extinction_yr~flow_scenario, vdat, mean)
names(tbl)[2]<- "Uniform_N0"

datP<- readRDS("./output/_stochastic/Full_Data_2020_PSPAP_1-1.rds")

vdatP<- lapply(1:7, function(y)
{
  nm<- names(datP$extinction)[y]
  yrs<- datP$extinction[[y]]$extinction_yr
  out<- data.frame(flow_scenario=rep(nm,length(yrs)),
                   extinction_yr=yrs)
  return(out)
})
vdatP<- do.call("rbind", vdatP)
vdatP$flow_scenario<- factor(vdatP$flow_scenario, 
                             c("NoAct", "1a", "1", "1b", "2a", "2", "2b"))

tblP<- aggregate(extinction_yr~flow_scenario, vdatP, mean)
names(tblP)[2]<- "PSPAP_N0"
tbl<- merge(tbl, tblP)
tbl<- tbl[order(tbl$Uniform_N0, decreasing = TRUE),c(1,3,2)]

write.csv(tbl, "./tables/baseline_quasiextinction_outcomes.csv",
          row.names = FALSE)

tbl<- aggregate(extinction_yr~flow_scenario, vdat, var)
names(tbl)[2]<- "Uniform_N0"
tblP<- aggregate(extinction_yr~flow_scenario, vdatP, var)
names(tblP)[2]<- "PSPAP_N0"
tbl<- merge(tbl, tblP)
tbl<- tbl[order(tbl$Uniform_N0, decreasing = TRUE),c(1,3,2)]


