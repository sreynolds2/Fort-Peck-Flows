# STOCHASTIC ANALYSES MANUSCRIPT FIGURES
library(ggplot2)

## RANK COMPARISON (BASELINE PARAMETERS)
### UNIFORM
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

ggplot(vdat, aes(x=flow_scenario, y=extinction_yr)) + 
  geom_violin(width=1) +
  geom_boxplot(width=0.2, color="grey", alpha=0.2) +
  theme(
    legend.position="none",
    plot.title = element_text(size=12)
  ) +
  xlab("Management Alternative") + 
  ylab("Time to Pseudoextinction") +
  ylim(0,100)

### 2020 PSPAP 
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

ggplot(vdatP, aes(x=flow_scenario, y=extinction_yr)) + 
  geom_violin(width=1.1) +
  geom_boxplot(width=0.15, color="grey", alpha=0.2) +
  theme(
    legend.position="none",
    plot.title = element_text(size=12)
  ) +
  xlab("Alternative") + 
  ylab("Time to Quasiextinction") +
  ylim(60,100)



vdat$N0_type<- "Uniform"
vdatP$N0_type<- "PSPAP"
vdat<- rbind(vdat, vdatP)
tmp<- ddply(vdat, .(flow_scenario, N0_type), summarize,
            mn_yr=mean(extinction_yr))
vdat<- merge(vdat, tmp, all.x=TRUE)

dodge<- position_dodge(width=1)
ggplot(vdat, aes(x=flow_scenario, y=extinction_yr, 
                 group=interaction(N0_type, flow_scenario))) + 
  geom_violin(position="dodge", width=1, aes(color=N0_type)) +
  #geom_boxplot(position="dodge", width=1, color="grey", alpha=0.2) +
  geom_point(aes(x=flow_scenario, y=mn_yr, fill=N0_type), 
             position=dodge) +
  theme(
    legend.position="topleft",
    plot.title = element_text(size=12)
  ) +
  xlab("Management Alternative") + 
  ylab("Time to Pseudoextinction") +
  ylim(50,100)

alts<- c("No Action", "1a", "1", "1b", "2a", "2", "2b")
par(cex.lab=1.5,
    cex.axis=1.5,
    mar=c(5,5,1,1)+0.1)
boxplot(extinction_yr~N0_type+flow_scenario, vdat, 
        xlab="", xaxt="n", ylab="Time to Quasiextinction (Years)", 
        col=rep(c("gray", "white"),7), range=1.5, 
        at=c(1,2,4,5,7,8,10,11,13,14,16,17,19,20))
axis(1,c(1.5, 4.5, 7.5, 10.5, 13.5, 16.5, 19.5), alts,
     cex.lab=1.5)
mtext("Alternative", 1, padj=3, cex=1.5)
legend("topleft", c("2020 PSPAP N0", "Uniform N0"), fill=c("gray", "white"), 
       bty="n", cex=1.25)

## PLOT N0 VALUES
par(mfrow=c(2,1),
    mar=c(3,4,1,0)+0.1,
    oma=c(1,0,0,0))
barplot(dat$inputs$N0, xlab="", ylab="Initial Female Abundance", las=1,
        width=1, xlim=c(0,100), ylim=c(0,100), space=0)
axis(1, seq(5, 100, 5), tick = FALSE, las=0)
legend("topleft", "A", bty="n")

barplot(datP$inputs$N0, xlab="", ylab="Initial Female Abundance", las=1, 
        width=1, xlim=c(0,100), space=0)
axis(1, seq(5, 100, 5), tick = FALSE, las=0)
legend("topleft", "B", bty="n")
mtext("Age", 1, outer=TRUE, padj=-1)

par(mfrow=c(1,1))
barplot(datP$inputs$N0[65:100], xlab="", ylab="Initial Female Abundance", las=1, 
        width=1, space=0)
axis(1, at=seq(1.5,36.5,2), labels=seq(66,100,2), tick = FALSE, las=0)
legend("topleft", "C", bty="n")
mtext("Age", 1, outer=TRUE, padj=-1)

## SENSITIVITIES
params<- read.csv("./output/_stochastic/sens_elas_vals.csv")
params[params$param_id==1,c("max_age", "probF", "gamma")]<- c(100, 0.5, 0.5)
source("./R/0_default-parameters.r")
### UNIFORM
uni<- read.csv("./output/_stochastic/extinction_time_data_Uniform.csv",
               stringsAsFactors = FALSE)
alts<- unique(uni$flow_scenario)
sens_avg<- readRDS("./output/_stochastic/average_sensitivities_uniform.rds")

### NEED TO CHANGE PLOT ORDER
par(mfrow=c(3,3),
    oma=c(3,1,1,0),
    mar=c(2,4,2,2))
invisible(lapply(c(7,2,1,3,5,4,6), function(y)
{
  tmp<- sens_avg[[y]]
  nms<- ifelse(is.na(tmp$age), tmp$param, 
               paste0(tmp$param, "-", tmp$age))
  barplot(tmp$sens[1:5],names.arg = nms[1:5], las=1, horiz = TRUE)
  if(y %in% 1:6)
  {
  legend("topright", paste("Alternative", tmp$flow_scenario[1]),
         bty="n", cex=1.5)
  }
  if(y==7)
  {
    legend("topright", "No Action", bty="n", cex=1.5)
    plot.new()
    plot.new()
  }
}))
mtext("Sensitivity", 1, outer=TRUE, padj=1, cex=1.25)
mtext("Uniform N0", 3, outer=TRUE, padj=1, cex=1.25)

### 2020 PSPAP
pap<- read.csv("./output/_stochastic/extinction_time_data_2020_PSPAP.csv",
               stringsAsFactors = FALSE)
sens_avgP<- readRDS("./output/_stochastic/average_sensitivities_2020_pspap.rds")

par(mfrow=c(3,3),
    oma=c(3,1,1,0),
    mar=c(2,4,2,2))
invisible(lapply(c(7,2,1,3,5,4,6), function(y)
{
  tmp<- sens_avgP[[y]]
  nms<- ifelse(is.na(tmp$age), tmp$param, 
               paste0(tmp$param, "-", tmp$age))
  barplot(tmp$sens[1:5],names.arg = nms[1:5], las=1, horiz = TRUE)
  if(y %in% 1:6)
  {
  legend("topright", paste("Alternative", tmp$flow_scenario[1]),
         bty="n", cex=1.5)
  }
  if(y==7)
  {
    legend("topright", "No Action", bty="n", cex=1.5)
    plot.new()
    plot.new()
  }
}))
mtext("Sensitivity", 1, outer=TRUE, padj=1)
mtext("2020 PSPAP N0", 3, outer=TRUE, padj=1, cex=1.25)

## ELASTICITIES
### UNIFORM
elas_avg<- readRDS("./output/_stochastic/average_elasticities_uniform.rds")
par(mfrow=c(3,3),
    oma=c(3,2,1,0),
    mar=c(2,4,2,2),
    cex.lab=1.25,
    cex.axis=1.25)
invisible(lapply(c(7,2,1,3,5,4,6), function(y)
{
  tmp<- elas_avg[[y]]
  nms<- ifelse(is.na(tmp$age), tmp$param, 
               paste0(tmp$param, "-", tmp$age))
  barplot(tmp$elas[1:5],names.arg = nms[1:5], las=1, horiz = TRUE)
  if(y %in% 1:6)
  {
  legend("topright", paste("Alternative", tmp$flow_scenario[1]),
         bty="n", cex=1.5)
  }
  if(y==7)
  {
    legend("topright", "No Action", bty="n", cex=1.5)
    plot.new()
    plot.new()
  }
}))
mtext("Elasticity", 1, outer=TRUE, padj=1, cex=1.25)
mtext("Uniform N0", 3, outer=TRUE, padj=1, cex=1.25)

par(mfrow=c(2,2),
    oma=c(3,1,0,0),
    mar=c(2,4,2,2))
invisible(lapply(c(7,1:3), function(y)
{
  tmp<- elas_avg[[y]]
  indx<- which(tmp$param=="phi0_MR")+4
  nms<- ifelse(is.na(tmp$age), tmp$param, 
               paste0(tmp$param, "-", tmp$age))
  barplot(tmp$elas[1:indx],
          names.arg = c(nms[1], rep("",indx-6), nms[indx-4], rep("", 4)), 
          las=1, horiz = TRUE, xlim=c(0,0.35))
  if(y %in% 1:6)
  {
    legend("topright", paste("Alternative", tmp$flow_scenario[1]),
           bty="n")
  }
  if(y==7)
  {
    legend("topright", "No Action", bty="n")
  }
}))
mtext("Elasticity", 1, outer=TRUE, padj=1)

par(mfrow=c(2,2),
    oma=c(3,1,0,0),
    mar=c(2,4,2,2))
invisible(lapply(c(7,4:6), function(y)
{
  tmp<- elas_avg[[y]]
  indx<- which(tmp$param=="phi0_MR")+4
  nms<- ifelse(is.na(tmp$age), tmp$param, 
               paste0(tmp$param, "-", tmp$age))
  barplot(tmp$elas[1:indx],
          names.arg = c(nms[1], rep("",indx-6), nms[indx-4], rep("", 4)), 
          las=1, horiz = TRUE, xlim=c(0,0.35))
  if(y %in% 1:6)
  {
    legend("topright", paste("Alternative", tmp$flow_scenario[1]),
           bty="n")
  }
  if(y==7)
  {
    legend("topright", "No Action", bty="n")
  }
}))
mtext("Elasticity", 1, outer=TRUE, padj=1)

### 2020 PSPAP
elas_avgP<- readRDS("./output/_stochastic/average_elasticities_2020_pspap.rds")
par(mfrow=c(3,3),
    oma=c(3,1,1,0),
    mar=c(2,4,3,2))
invisible(lapply(c(7,2,1,3,5,4,6), function(y)
{
  tmp<- elas_avgP[[y]]
  nms<- ifelse(is.na(tmp$age), tmp$param, 
               paste0(tmp$param, "-", tmp$age))
  barplot(tmp$elas[1:5],names.arg = nms[1:5], las=1, horiz = TRUE)
  if(y %in% 1:6)
  {
    legend("topright", paste("Alternative", tmp$flow_scenario[1]),
           bty="n", cex=1.5)
  }
  if(y==7)
  {
    legend("topright", "No Action", bty="n", cex=1.5)
    plot.new()
    plot.new()
  }
}))
mtext("Elasticity", 1, outer=TRUE, padj=1, cex=1.25)
mtext("2020 PSPAP N0", 3, outer=TRUE, padj=1, cex=1.25)

par(mfrow=c(2,2),
    oma=c(3,1,0,0),
    mar=c(2,4,2,2))
invisible(lapply(c(7,1:3), function(y)
{
  tmp<- elas_avgP[[y]]
  indx<- which(tmp$param=="phi0_MR")+4
  nms<- ifelse(is.na(tmp$age), tmp$param, 
               paste0(tmp$param, "-", tmp$age))
  barplot(tmp$elas[1:indx],
          names.arg = c(nms[1], rep("",indx-6), nms[indx-4], rep("", 4)), 
          las=1, horiz = TRUE, xlim=c(0,0.35))
  if(y %in% 1:6)
  {
    legend("topright", paste("Alternative", tmp$flow_scenario[1]),
           bty="n")
  }
  if(y==7)
  {
    legend("topright", "No Action", bty="n")
  }
}))
mtext("Elasticity", 1, outer=TRUE, padj=1)

par(mfrow=c(2,2),
    oma=c(3,1,0,0),
    mar=c(2,4,2,2))
invisible(lapply(c(7,4:6), function(y)
{
  tmp<- elas_avgP[[y]]
  indx<- which(tmp$param=="phi0_MR")+4
  nms<- ifelse(is.na(tmp$age), tmp$param, 
               paste0(tmp$param, "-", tmp$age))
  barplot(tmp$elas[1:indx],
          names.arg = c(nms[1], rep("",indx-6), nms[indx-4], rep("", 4)), 
          las=1, horiz = TRUE, xlim=c(0,0.35))
  if(y %in% 1:6)
  {
    legend("topright", paste("Alternative", tmp$flow_scenario[1]),
           bty="n")
  }
  if(y==7)
  {
    legend("topright", "No Action", bty="n")
  }
}))
mtext("Elasticity", 1, outer=TRUE, padj=1)

#ALT 1b
## PSPAP SENSITIVITY
par(mfrow=c(2,2),
    oma=c(3,2,0,0),
    mar=c(2,4,4,2))
tmp<- sens_avgP[[3]]
nms<- ifelse(is.na(tmp$age), tmp$param, 
             paste0(tmp$param, "-", tmp$age))
barplot(tmp$sens[1:5],names.arg = nms[1:5], las=1, horiz = TRUE,
        main="2020 PSPAP")
mtext("Sensitivity", 1, padj=3)
## UNIFORM SENSITIVITY
tmp<- sens_avg[[3]]
nms<- ifelse(is.na(tmp$age), tmp$param, 
             paste0(tmp$param, "-", tmp$age))
barplot(tmp$sens[1:5],names.arg = nms[1:5], las=1, horiz = TRUE,
        main="Uniform")
mtext("Sensitivity", 1, padj=3)
## PSPAP ELASTICITY
tmp<- elas_avgP[[3]]
nms<- ifelse(is.na(tmp$age), tmp$param, 
             paste0(tmp$param, "-", tmp$age))
barplot(tmp$elas[1:5],names.arg = nms[1:5], las=1, horiz = TRUE)
mtext("Elasticity", 1, padj=3)
## UNIFORM ELASTICITY
tmp<- elas_avg[[3]]
nms<- ifelse(is.na(tmp$age), tmp$param, 
             paste0(tmp$param, "-", tmp$age))
barplot(tmp$elas[1:5],names.arg = nms[1:5], las=1, horiz = TRUE)
mtext("Elasticity", 1, padj=3)
#mtext(paste("Alternative", tmp$flow_scenario[3]),3, outer=TRUE)

#NO ACTION ALTERNATIVE
## PSPAP SENSITIVITY
par(mfrow=c(2,2),
    oma=c(3,2,0,0),
    mar=c(2,4,4,2))
tmp<- sens_avgP[[7]]
nms<- ifelse(is.na(tmp$age), tmp$param, 
             paste0(tmp$param, "-", tmp$age))
barplot(tmp$sens[1:5],names.arg = nms[1:5], las=1, horiz = TRUE,
        main="2020 PSPAP")
mtext("Sensitivity", 1, padj=3)
## UNIFORM SENSITIVITY
tmp<- sens_avg[[7]]
nms<- ifelse(is.na(tmp$age), tmp$param, 
             paste0(tmp$param, "-", tmp$age))
barplot(tmp$sens[1:5],names.arg = nms[1:5], las=1, horiz = TRUE,
        main="Uniform")
mtext("Sensitivity", 1, padj=3)
## PSPAP ELASTICITY
tmp<- elas_avgP[[7]]
nms<- ifelse(is.na(tmp$age), tmp$param, 
             paste0(tmp$param, "-", tmp$age))
barplot(tmp$elas[1:5],names.arg = nms[1:5], las=1, horiz = TRUE)
mtext("Elasticity", 1, padj=3)
## UNIFORM ELASTICITY
tmp<- elas_avg[[7]]
nms<- ifelse(is.na(tmp$age), tmp$param, 
             paste0(tmp$param, "-", tmp$age))
barplot(tmp$elas[1:5],names.arg = nms[1:5], las=1, horiz = TRUE)
mtext("Elasticity", 1, padj=3)
#mtext(paste("Alternative", tmp$flow_scenario[3]),3, outer=TRUE)


## RANK ORDER ROBUSTNESS
### UNIFORM
### 2020 PSPAP
#### EXPECTED TIME TO EXTINCTION
pap<- read.csv("./output/_stochastic/age-0_extinction_time_2020_PSPAP.csv",
               stringsAsFactors = FALSE)
params<- read.csv("./output/_stochastic/sens_elas_vals.csv")
pap$phi0_MR<- sapply(pap$param_id, function(i){params[which(params$param_id==i),]$phi0_MR})
tmp<- pap[pap$param_id %in% 1:11,]
alts<- c("NoAct", "1a", "1", "1b", "2a", "2", "2b")
cls<- c("red", rep("black", 3), rep("gray", 3))
typ<- c(1, 3, 1, 2, 3, 1, 2)

tmp2<- subset(tmp, flow_scenario==alts[1])
tmp2<- tmp2[order(tmp2$phi0_MR),]
par(mfrow=c(1,1))
plot(tmp2$phi0_MR, tmp2$E_time,
     xlab="Age-0 Survival Given Retention",
     ylab="Expected Time to Quasiextinction (Years)", type="b",
     pch=19, lty=typ[1], col=cls[1], ylim=c(0, 300))
invisible(lapply(2:7, function(y)
{
  tmp2<- subset(tmp, flow_scenario==alts[y])
  tmp2<- tmp2[order(tmp2$phi0_MR),]
  points(tmp2$phi0_MR, tmp2$E_time, type="b",
         pch=19, lty=typ[y], col=cls[y])
}))
legend("topleft", alts, lty=typ, col=cls, bty="n", cex=1.5)

legend("topleft", alts[c(4,7,6,3,5,2,1)], 
       lty=typ[c(4,7,6,3,5,2,1)], col=cls[c(4,7,6,3,5,2,1)], 
       bty="n", cex=1.5)
mtext("Age-0 Survival Given Retention", 1, cex=1.5, padj=3)

#### TIME TO 80%
tmp2<- subset(pap, flow_scenario==alts[1])
tmp2<- tmp2[order(tmp2$phi0_MR),]
par(mfrow=c(1,1))
plot(tmp2$phi0_MR, tmp2$time_80,
     xlab="Age-0 Survival Given Retention",
     ylab="Time when 80% of Replicates Are Quasiextinct (Years)", type="b",
     pch=19, lty=typ[1], col=cls[1], ylim=c(0, 600))
invisible(lapply(2:7, function(y)
{
  tmp2<- subset(pap, flow_scenario==alts[y])
  tmp2<- tmp2[order(tmp2$phi0_MR),]
  points(tmp2$phi0_MR, tmp2$time_80, type="b",
         pch=19, lty=typ[y], col=cls[y])
}))
legend("topleft", alts, lty=typ, col=cls, bty="n")

legend("topleft", alts[c(4,7,6,3,5,2,1)], 
       lty=typ[c(4,7,6,3,5,2,1)], col=cls[c(4,7,6,3,5,2,1)], 
       bty="n",cex=1.5)
mtext("Age-0 Survival Given Retention", 1, cex=1.5, padj=3)

