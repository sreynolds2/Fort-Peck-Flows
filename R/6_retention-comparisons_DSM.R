
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


## HISTOGRAM OF RETENTIONS BY ALTERNATIVE
### PULL RETENTION VALUES
DSM_ret<- read.csv("./output/New_9_Above_Retentions.csv", stringsAsFactors = FALSE)
DSM_ret$Hatch_Date<- as.Date(DSM_ret$Hatch_Date)
DSM_ret[is.na(DSM_ret$Alt),]$Alt<- "NoAct"
#### ADD SPAWN DATE
DSM_ret$Spawn_Date<- DSM_ret$Hatch_Date-7
### PULL SPAWN DATE SUMMARY
spn<- read.csv("./output/Spawn_Dates_Summary.csv", stringsAsFactors = FALSE)
#### USE ONLY STANDARD SPAWN DATES
spn<- spn[,c("Year", "Weather_Pattern", "Flow_Scenario", "Standard")]
spn[which(spn$Flow_Scenario=="Alt1"),]$Flow_Scenario<- "Alt.1"
spn$Flow_Scenario<- gsub("Alt.", "", spn$Flow_Scenario)
spn[which(spn$Flow_Scenario=="No.Act"),]$Flow_Scenario<- "NoAct"
spn<- subset(spn, Standard!="Fail")
spn$Standard<- as.Date(paste(spn$Year, spn$Standard, sep="-"))
spn[which(spn$Year==1975),]$Standard<- "1975-07-02" #NEEDS CHANGING EVENTUALLY
spn[which(spn$Year==1976),]$Standard<- "1976-06-14" #NEEDS CHANGING EVENTUALLY
spn[which(spn$Year==1997),]$Standard<- "1997-06-25" #NEEDS CHANGING EVENTUALLY
spn[which(spn$Year==1997 & spn$Flow_Scenario %in% c("1b", "2a")),]$Standard<- "1997-06-30" #NEEDS CHANGING EVENTUALLY
### COMBINE STANDARD SPAWN DATES AND RETENTION DATA
std_ret<- merge(spn, DSM_ret, by.x=c("Year", "Flow_Scenario", "Standard"), 
            by.y=c("Year", "Alt", "Spawn_Date"), all.x=TRUE)
std_ret[which(std_ret$Year==1997 & std_ret$Flow_Scenario=="1b"), 5:9]<- #APPROXIMATE BASED ON SIMILAR FLOWS AND SAME HATCH DATE
  std_ret[which(std_ret$Year==1997 & std_ret$Flow_Scenario=="2a"),5:9] #NEEDS CHANGING EVENTUALLY
### HISTOGRAM OF RETENTIONS
alts<- c("1", "1a", "1b", "2", "2a", "2b", "NoAct")
x<- alts[1]
tmp<- std_ret[which(std_ret$Flow_Scenario==x),]
hist(tmp$Retention, 10, freq=FALSE, ylim=c(0,5), xlab="Retention", 
       main="", col="darkgray")
legend("top", paste0("Alternative ", x), text.font=2, bty="n")
#legend("top", "No Action Alternative", text.font=2, bty="n")
rm(x)
par(mfrow=c(2,3),
    mar=c(3,3,1,1)+0.1)
#mar=c(5,4,4,2)+0.1)
invisible(lapply(alts[1:6], function(x)
{
  tmp<- std_ret[which(std_ret$Flow_Scenario==x),]
  hist(tmp$Retention, 10, freq=FALSE, ylim=c(0,5), xlab="Retention", 
       main=paste0("Alternative ", x), col = "darkgray")
}))

par(mfrow=c(3,3))
invisible(lapply(alts, function(x)
{
  tmp<- std_ret[which(std_ret$Flow_Scenario==x),]
  hist(tmp$Retention, 10, freq=FALSE, ylim=c(0,5), xlab="Retention", 
       main=paste0("Alternative ", x))
}))
par(mfrow=c(1,1))
tmp<- std_ret[which(std_ret$Flow_Scenario=="NoAct"),]
hist(tmp$Retention, 10, freq=FALSE, ylim=c(0,5), xlab="Retention", 
     main=paste0("No Action Alternative"), col="darkgray")

par(mfrow=c(3,3))
tmp<- std_ret[which(std_ret$Flow_Scenario=="NoAct"),]
hist(tmp$Retention, 10, freq=FALSE, ylim=c(0,5), xlab="Retention", 
     main=paste0("Alternative ", "NoAct"), col="darkgray")
plot.new()
plot.new()
invisible(lapply(alts[1:6], function(x)
{
  tmp<- std_ret[which(std_ret$Flow_Scenario==x),]
  hist(tmp$Retention, 10, freq=FALSE, ylim=c(0,5), xlab="Retention", 
       main=paste0("Alternative ", x), col="darkgray")
}))


## HISTOGRAM OF SPAWNING BY ALTERNATIVE
tmp<- expand.grid(Year=1930:2012, Flow_Scenario=unique(std_ret$Flow_Scenario))
std_ret<- merge(std_ret, tmp, by=c("Year", "Flow_Scenario"), all=TRUE)
std_ret[which(is.na(std_ret$Retention)),"Retention"]<- 0
std_ret$spawning_prob<- ifelse(std_ret$Retention==0, 0, 0.5)

spn_prob<- aggregate(spawning_prob~Flow_Scenario, std_ret, sum)
spn_prob$spawning_prob<- spn_prob$spawning_prob*2/83 
barplot(spn_prob$spawning_prob, space=0, ylim=c(0,0.2), 
        names.arg=spn_prob$Flow_Scenario, 
        xlab="Flow Alternative", 
        ylab="Percentage of Years with Spawning")

x<- alts[3]
tmp<- std_ret[which(std_ret$Flow_Scenario==x),]
par(mfrow=c(1,1),
    mar=c(5,4,4,2)+0.1)
hist(tmp$spawning_prob, seq(0, 1, 0.1), freq=FALSE, ylim=c(0,10), xlim=c(0,1),
     xlab="Spawning Probability", main="", col="darkgray")
legend("top", paste0("Alternative ", x), text.font=2, bty="n")
#legend("top", "No Action Alternative", text.font=2, bty="n")
rm(x)
par(mfrow=c(3,3))
invisible(lapply(alts, function(x)
{
  tmp<- std_ret[which(std_ret$Flow_Scenario==x),]
  hist(tmp$spawning_prob, 10, freq=FALSE, ylim=c(0,20), xlim=c(0,1),
       xlab="Spawning Probability", main=paste0("Alternative ", x))
}))


## SPAWNING WEIGHTS BY ALTERNATIVE
library(plyr)
std_ret$spawning_feq<- ifelse(std_ret$spawning_prob==0, 0, 1)
std_ret$freq<- 1
wgts<- ddply(std_ret, .(Flow_Scenario), summarize,
             Weight=sum(spawning_feq)/sum(freq))
### REMOVE ZEROES IN RETENTION DATA DUE TO LACK OF SPAWNING
std_ret<- merge(spn, DSM_ret, by.x=c("Year", "Flow_Scenario", "Standard"), 
                by.y=c("Year", "Alt", "Spawn_Date"), all.x=TRUE)
std_ret[which(std_ret$Year==1997 & std_ret$Flow_Scenario=="1b"), 5:9]<- #APPROXIMATE BASED ON SIMILAR FLOWS AND SAME HATCH DATE
  std_ret[which(std_ret$Year==1997 & std_ret$Flow_Scenario=="2a"),5:9] #NEEDS CHANGING EVENTUALLY
std_ret$freq<- 1
### OBTAIN HISTORGRAM BINS
alts<- c("1", "1a", "1b", "2", "2a", "2b", "NoAct")
par(mfrow=c(2,3),
    mar=c(3,3,1,1)+0.1)
    #mar=c(5,4,4,2)+0.1)
invisible(lapply(alts[1:6], function(x)
{
  tmp<- std_ret[which(std_ret$Flow_Scenario==x),]
  test<- hist(tmp$Retention, seq(0,1,0.1), plot=FALSE)
  test$vals<- test$density*wgts[which(wgts$Flow_Scenario==x),]$Weight
  if(grepl("1", x))
  {
    barplot(test$vals, space=0, ylim=c(0,0.5))
    axis(1, seq(0,10, 1), labels=FALSE)
  }
  if(grepl("2", x))
  {
    barplot(test$vals, space=0, ylim=c(0,0.5))
    axis(1, seq(0,10, 1), labels=seq(0,1,0.1))
  }
  legend("top", paste0("Alternative ", x), text.font = 2, bty="n")
}))
par(mfrow=c(1,1))
tmp<- std_ret[which(std_ret$Flow_Scenario=="NoAct"),]
test<- hist(tmp$Retention, seq(0,1,0.1), plot=FALSE)
test$vals<- test$density*wgts[which(wgts$Flow_Scenario=="NoAct"),]$Weight
barplot(test$vals, space=0, ylim=c(0,0.5))
axis(1, seq(0,10, 1), labels=seq(0,1,0.1))
legend("top", "No Action Alternative", text.font = 2, bty="n")
par(mfrow=c(3,3),
    mar=c(3,3,1,1)+0.1)
#mar=c(5,4,4,2)+0.1)
plot.new()
plot.new()
tmp<- std_ret[which(std_ret$Flow_Scenario=="NoAct"),]
test<- hist(tmp$Retention, seq(0,1,0.1), plot=FALSE)
test$vals<- test$density*wgts[which(wgts$Flow_Scenario=="NoAct"),]$Weight
barplot(test$vals, space=0, ylim=c(0,0.5))
axis(1, seq(0,10, 1), labels=FALSE)
legend("top", "No Action Alternative", text.font = 2, bty="n")
invisible(lapply(alts[1:6], function(x)
{
  tmp<- std_ret[which(std_ret$Flow_Scenario==x),]
  test<- hist(tmp$Retention, seq(0,1,0.1), plot=FALSE)
  test$vals<- test$density*wgts[which(wgts$Flow_Scenario==x),]$Weight
  if(grepl("1", x))
  {
    barplot(test$vals, space=0, ylim=c(0,0.5))
    axis(1, seq(0,10, 1), labels=FALSE)
  }
  if(grepl("2", x))
  {
    barplot(test$vals, space=0, ylim=c(0,0.5))
    axis(1, seq(0,10, 1), labels=seq(0,1,0.1))
  }
  legend("top", paste0("Alternative ", x), text.font = 2, bty="n")
}))
