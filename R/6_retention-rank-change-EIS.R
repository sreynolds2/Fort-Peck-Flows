

## NEED TO REDO TO JUST PULL STANDARD DATES
library(reshape2)
#ret<- read.csv("./output/Above_Anoxic_Retentions.csv")

tmp<- subset(EIS_ret, anoxic_layer==FALSE)
#tmp<- ret

test<- dcast(tmp, Alt+Hatch_Date~Develop_Mod+Drift_Mod, mean, value.var = "Retention")

dt<- unique(test$Hatch_Date)
test2<- lapply(dt, function(x)
{
  tmp<- test[which(test$Hatch_Date==x),]
  rank1<- tmp[order(tmp$New_0.9, decreasing = TRUE),]$Alt
  out<- lapply(4:6, function(y)
  {
    r<- tmp[order(tmp[,y], decreasing = TRUE),]$Alt
    return(all(r==rank1))
  })
  out<- unlist(out)
  return(out)
})
indx<- which(unlist(test2)==FALSE)
indx<- unique(ceiling(indx/3))
dt[indx]
indx<- indx[c(4,7,9,10,11)]
dt[indx]
test2[[indx[1]]]
test2[[indx[2]]]
test2[[indx[3]]]
test2[[indx[4]]]
test2[[indx[5]]]

x<- dt[indx[3]]
tmp<- test[which(test$Hatch_Date==x),]
rank1<- tmp[order(tmp$New_0.9, decreasing = TRUE),]$Alt
rank1
r<- lapply(4:6, function(y)
{
  return(tmp[order(tmp[,y], decreasing = TRUE),]$Alt)
})
r
tmp

x<- dt[indx[4]]
tmp<- test[which(test$Hatch_Date==x),]
rank1<- tmp[order(tmp$New_0.9, decreasing = TRUE),]$Alt
rank1
r<- lapply(4:6, function(y)
{
  return(tmp[order(tmp[,y], decreasing = TRUE),]$Alt)
})
r
tmp
