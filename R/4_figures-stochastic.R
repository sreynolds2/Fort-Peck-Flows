# STOCHASTIC ANALYSES MANUSCRIPT FIGURES
library(ggplot2)

## RANK COMPARISON (BASELINE PARAMETERS)
### UNIFORM
dat<- readRDS("./output/_stochastic/Full_Data_0-1.rds")

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
  geom_violin(width=1) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  theme(
    legend.position="none",
    plot.title = element_text(size=12)
  ) +
  xlab("Management Alternative") + 
  ylab("Time to Pseudoextinction") +
  ylim(0,100)


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
    oma=c(3,1,0,0),
    mar=c(2,4,2,2))
invisible(lapply(c(7,2,1,3,5,4,6), function(y)
{
  tmp<- sens_avg[[y]]
  nms<- ifelse(is.na(tmp$age), tmp$param, 
               paste0(tmp$param, "-", tmp$age))
  barplot(tmp$sens[1:5],names.arg = nms[1:5], las=1, horiz = TRUE)
  legend("topright", paste("Alternative", tmp$flow_scenario[1]),
         bty="n")
  if(y==7)
  {
    plot.new()
    plot.new()
  }
}))
mtext("Sensitivity", 1, outer=TRUE, padj=1)

### 2020 PSPAP
pap<- read.csv("./output/_stochastic/extinction_time_data_2020_PSPAP.csv",
               stringsAsFactors = FALSE)
sens_avgP<- readRDS("./output/_stochastic/average_sensitivities_2020_pspap.rds")

par(mfrow=c(3,3),
    oma=c(3,1,0,0),
    mar=c(2,4,2,2))
invisible(lapply(c(7,2,1,3,5,4,6), function(y)
{
  tmp<- sens_avgP[[y]]
  nms<- ifelse(is.na(tmp$age), tmp$param, 
               paste0(tmp$param, "-", tmp$age))
  barplot(tmp$sens[1:5],names.arg = nms[1:5], las=1, horiz = TRUE)
  legend("topright", paste("Alternative", tmp$flow_scenario[1]),
         bty="n")
  if(y==7)
  {
    plot.new()
    plot.new()
  }
}))
mtext("Sensitivity", 1, outer=TRUE, padj=1)

## ELASTICITIES
### UNIFORM
elas_avg<- readRDS("./output/_stochastic/average_elasticities_uniform.rds")
par(mfrow=c(3,3),
    oma=c(3,1,0,0),
    mar=c(2,4,2,2))
invisible(lapply(c(7,2,1,3,5,4,6), function(y)
{
  tmp<- elas_avg[[y]]
  nms<- ifelse(is.na(tmp$age), tmp$param, 
               paste0(tmp$param, "-", tmp$age))
  barplot(tmp$elas[1:5],names.arg = nms[1:5], las=1, horiz = TRUE)
  if(y %in% 1:6)
  {
  legend("topright", paste("Alternative", tmp$flow_scenario[1]),
         bty="n")
  }
  if(y==7)
  {
    legend("topright", "No Action", bty="n")
    plot.new()
    plot.new()
  }
}))
mtext("Elasticity", 1, outer=TRUE, padj=1)

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
    oma=c(3,1,0,0),
    mar=c(2,4,2,2))
invisible(lapply(c(7,2,1,3,5,4,6), function(y)
{
  tmp<- elas_avgP[[y]]
  nms<- ifelse(is.na(tmp$age), tmp$param, 
               paste0(tmp$param, "-", tmp$age))
  barplot(tmp$elas[1:5],names.arg = nms[1:5], las=1, horiz = TRUE)
  if(y %in% 1:6)
  {
    legend("topright", paste("Alternative", tmp$flow_scenario[1]),
           bty="n")
  }
  if(y==7)
  {
    legend("topright", "No Action", bty="n")
    plot.new()
    plot.new()
  }
}))
mtext("Elasticity", 1, outer=TRUE, padj=1)

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

## RANK ORDER ROBUSTNESS
### UNIFORM
### 2020 PSPAP
pap<- read.csv("./output/_stochastic/extinction_time_data_2020_PSPAP.csv",
               stringsAsFactors = FALSE)
params<- read.csv("./output/_stochastic/sens_elas_vals.csv")
tmp<- pap[pap$param_id %in% 1:13,]
tmp$phi0_MR<- sapply(tmp$param_id, function(i){params[which(params$param_id==i),]$phi0_MR})
alts<- c("NoAct", "1a", "1", "1b", "2a", "2", "2b")
cls<- c("red", rep("black", 3), rep("gray", 3))
typ<- c(1, 3, 1, 2, 3, 1, 2)

tmp2<- subset(tmp, flow_scenario==alts[1])
tmp2<- tmp2[order(tmp2$phi0_MR),]
par(mfrow=c(1,1))
plot(tmp2$phi0_MR, tmp2$E_time,
     xlab="Age-0 Survival Given Retention",
     ylab="Expected Time to Psuedoextinction (Years)", type="b",
     pch=19, lty=typ[1], col=cls[1], ylim=c(0, 125))
invisible(lapply(2:7, function(y)
{
  tmp2<- subset(tmp, flow_scenario==alts[y])
  tmp2<- tmp2[order(tmp2$phi0_MR),]
  points(tmp2$phi0_MR, tmp2$E_time, type="b",
         pch=19, lty=typ[y], col=cls[y])
}))
legend("bottomright", alts, lty=typ, col=cls, bty="n")

