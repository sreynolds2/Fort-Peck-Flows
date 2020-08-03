

dat<- read.xlsx("./dat/Alt1_Retention_Out_12.xlsx", "OO")
#COL 6: FIRST VELOCITY
#COL 7: FIRST TEMPERATURE
#COL 8: FIRST SPATIAL DISTRIBUTION


## FIND THE PULSE AND PULSE ABOVE ANOXIC ZONE
test<- lapply(1:96, function(x)
{
  pulse1<- which.max(dat[,3*x+5])
  temp1<- dat[pulse1,3*x+4]
  pulse2<- which.max(dat[1:459,3*x+5])
  temp2<- dat[pulse2,3*x+4]
  out<- data.frame(column_name=x,
                   pulse_location=pulse1, pulse_temperature=temp1, 
                   pulse_aboveLS=pulse2, temperature_aboveLS=temp2)
  return(out)
})
test<- do.call(rbind,test)
pulse_dd<- cumsum(3/24*test$pulse_temperature)
dat[test[min(which(pulse_dd>200)), "pulse_location"], "RS"]
pulse_dd[max(which(test$pulse_location<460))]

pulse_dd_aboveLS<- cumsum(3/24*test$temperature_aboveLS)
test[min(which(pulse_dd_aboveLS>200)),"pulse_aboveLS"]
pulse_ret<- dat[test[min(which(pulse_dd_aboveLS>200)),"pulse_aboveLS"], 
    as.character(min(which(pulse_dd_aboveLS>200)))]/sum(
      dat[,as.character(min(which(pulse_dd_aboveLS>200)))])
pulse_ret

ret_pulse_below<- sum(dat[1:test[min(which(pulse_dd_aboveLS>200)),"pulse_aboveLS"], 
                      as.character(min(which(pulse_dd_aboveLS>200)))])/sum(
                        dat[,as.character(min(which(pulse_dd_aboveLS>200)))])
ret_pulse_below




pulse_dd9<- cumsum(30/(9*24)*test$pulse_temperature)
dat[test[min(which(pulse_dd9>200)), "pulse_location"], "RS"]
pulse_dd9[max(which(test$pulse_location<460))]

pulse_dd9_aboveLS<- cumsum(30/(9*24)*test$temperature_aboveLS)
min(which(pulse_dd9_aboveLS>200))
test[min(which(pulse_dd9_aboveLS>200)),"pulse_aboveLS"]
pulse_ret9<- dat[test[min(which(pulse_dd9_aboveLS>200)),"pulse_aboveLS"], 
                as.character(min(which(pulse_dd9_aboveLS>200)))]/sum(
                  dat[,as.character(min(which(pulse_dd9_aboveLS>200)))])
pulse_ret9


Calcs<- lapply(1:96, function(x)
{ 
  out<- 3/24*dat[,3*x+4]*dat[,3*x+5]/sum(dat[,3*x+5])/(1923*dat[,3*x+4]^(-0.74))
  return(out)
})
Calcs<- do.call(cbind, Calcs)
IPCTUs<- colSums(Calcs)
PCTUs<- cumsum(IPCTUs)
Ret<- sapply(1:96, function(x)
{
  sum(dat[1:459,3*x+5])/sum(dat[,3*x+5])
})

ret1<- (Ret[min(which(PCTUs>1))-1]*(PCTUs[min(which(PCTUs>1))]-1) +
  Ret[min(which(PCTUs>1))]*(1-PCTUs[min(which(PCTUs>1))-1]))/
  (PCTUs[min(which(PCTUs>1))]-PCTUs[min(which(PCTUs>1))-1])


Calcs_9<- lapply(1:96, function(x)
{ 
  out<- 30/(9*24)*dat[1:459,3*x+4]*dat[1:459,3*x+5]/sum(dat[1:459,3*x+5])/(1923*dat[1:459,3*x+4]^(-0.74))
  return(out)
})
Calcs_9<- do.call(cbind, Calcs_9)
IPCTUs_9<- colSums(Calcs_9)
PCTUs_9<- cumsum(IPCTUs_9)


ret9<- (Ret[min(which(PCTUs_9>1))-1]*(PCTUs_9[min(which(PCTUs_9>1))]-1) +
    Ret[min(which(PCTUs_9>1))]*(1-PCTUs_9[min(which(PCTUs_9>1))-1]))/
  (PCTUs_9[min(which(PCTUs_9>1))]-PCTUs_9[min(which(PCTUs_9>1))-1])


Calcs_200<- lapply(1:96, function(x)
{ 
  out=3/24*dat[1:459,3*x+4]*dat[1:459,3*x+5]/sum(dat[1:459,3*x+5])
  return(out)
})
Calcs_200<- do.call(cbind, Calcs_200)
ITUs<- colSums(Calcs_200)
CTUs<- cumsum(ITUs)


ret200<- (Ret[min(which(CTUs>200))-1]*(CTUs[min(which(CTUs>200))]-200) +
    Ret[min(which(CTUs>200))]*(200-CTUs[min(which(CTUs>200))-1]))/
  (CTUs[min(which(CTUs>200))]-CTUs[min(which(CTUs>200))-1])
ret_pulse_below


Calcs_200_9<- lapply(1:96, function(x)
{ 
  out=30/(9*24)*dat[1:459,3*x+4]*dat[1:459,3*x+5]/sum(dat[1:459,3*x+5])
  return(out)
})
Calcs_200_9<- do.call(cbind, Calcs_200_9)
ITUs_9<- colSums(Calcs_200_9)
CTUs_9<- cumsum(ITUs_9)


ret200_9<- (Ret[min(which(CTUs_9>200))-1]*(CTUs_9[min(which(CTUs_9>200))]-200) +
    Ret[min(which(CTUs_9>200))]*(200-CTUs_9[min(which(CTUs_9>200))-1]))/
  (CTUs_9[min(which(CTUs_9>200))]-CTUs_9[min(which(CTUs_9>200))-1])


### LOWER 0.1%
ITUs_Lower<- sapply(1:96, function(x)
{
  perc_below<- cumsum(dat[,3*x+5])/sum(dat[,3*x+5])
  indx<- min(which(perc_below>0.001))
  keep<- (0.001-perc_below[indx-1])/(perc_below[indx]-perc_below[indx-1])
  tmp<- dat[1:(indx-1),3*x+5]
  tmp<- c(tmp, keep*dat[indx,3*x+5])
  out<- sum(3/24*dat[1:indx,3*x+4]*tmp/sum(tmp))
  return(out)
})

CTUs_Lower<- cumsum(ITUs_Lower)
indx<- min(which(CTUs_Lower>200))
perc_below<- cumsum(dat[,3*indx+5])/sum(dat[,3*indx+5])
indx2<- min(which(perc_below>0.001))
dat[indx2,"RS"]
### SHOULD BE RETAINED!!! :)


IPTUs_Lower<- sapply(1:96, function(x)
{
  perc_below<- cumsum(dat[,3*x+5])/sum(dat[,3*x+5])
  indx<- min(which(perc_below>0.001))
  keep<- (0.001-perc_below[indx-1])/(perc_below[indx]-perc_below[indx-1])
  tmp<- dat[1:(indx-1),3*x+5]
  tmp<- c(tmp, keep*dat[indx,3*x+5])
  out<- sum(3/24*dat[1:indx,3*x+4]*(tmp/sum(tmp))/(1923.5*dat[1:indx,3*x+4]^(-0.739)))
  return(out)
})

PTUs_Lower<- cumsum(IPTUs_Lower)
indx<- min(which(PTUs_Lower>1))
perc_below<- cumsum(dat[,3*indx+5])/sum(dat[,3*indx+5])
indx2<- min(which(perc_below>0.001))
dat[indx2,"RS"]
### NOT REALLY DIFFERENT FROM 200 ABOVE



### RERUN AVERAGE CALCS WITHOUT INCLUSION OF RIVER MILES PAST ANOXIC ZONE
n<- length(dat[1,6:ncol(dat)])/3
PITU<- sapply(1:n, function(i)
{
  sum((3/24*dat[1:459,3*i+4]/(1923*dat[1:459,3*i+4]^(-0.74)))*(dat[1:459,3*i+5]/sum(dat[1:459,3*i+5])))
})
PCTU<- cumsum(PITU)
ret<- sapply(1:n, function(i)
{
  sum(dat[1:459,3*i+5])/sum(dat[,3*i+5])
})
indx<- min(which(PCTU>1))
new_ret<- (ret[indx]*(1-PCTU[indx-1])+ret[indx-1]*(PCTU[indx]-1))/(PCTU[indx]-PCTU[indx-1])
new_ret
ret1
(new_ret-ret1)*100

## LOAD DATA OUTPUT TYPE PRIOR TO OO SHEET
dat<- read.xlsx("./dat/R2_Output_Data_1.xlsx", "Alt1R2_12")
### CHECK COLUMN NUMBERS ARE CORRECT
grepl("velocity", names(dat)[6]) & grepl("Temperature", names(dat)[7]) & 
  grepl("mass", names(dat)[8])
### REMOVE INITIAL TIME STEP
dat<- dat[,-c(6:8)]
### REMOVE EXTRA ROWS
indx<- which(dat[,5]==0 | is.na(dat[,5]))
#### CHECK ROW CONSISTENCY
all(indx %in% c(1, 20:22, 179:181, 364:366, 575)) & length(indx)==11
dat<- dat[-indx,]
### CHECK ANOXIC ZONE
grepl("1528.05", dat[459, 4]) & grepl("1527.4", dat[459, 4])
### CHECK OUTPUTS
n<- length(dat[1,6:ncol(dat)])/3
#### NEW
PITU<- sapply(1:n, function(i)
{
  sum((3/24*dat[1:459,3*i+4]/(1923*dat[1:459,3*i+4]^(-0.74)))*(dat[1:459,3*i+5]/sum(dat[1:459,3*i+5])))
})
PCTU<- cumsum(PITU)
ret<- sapply(1:n, function(i)
{
  sum(dat[1:459,3*i+5])/sum(dat[,3*i+5])
})
indx<- min(which(PCTU>1))
new_ret2<- (ret[indx]*(1-PCTU[indx-1])+ret[indx-1]*(PCTU[indx]-1))/(PCTU[indx]-PCTU[indx-1])
new_ret2
new_ret

#### NEW; 0.9
PITU_9<- sapply(1:n, function(i)
{
  sum((3/24*(1/0.9)*dat[1:459,3*i+4]/(1923*dat[1:459,3*i+4]^(-0.74)))*(dat[1:459,3*i+5]/sum(dat[1:459,3*i+5])))
})
PCTU_9<- cumsum(PITU_9)
indx<- min(which(PCTU_9>1))
ret9_2<- (ret[indx]*(1-PCTU_9[indx-1])+ret[indx-1]*(PCTU_9[indx]-1))/(PCTU_9[indx]-PCTU_9[indx-1])
ret9_2
ret9

#### OLD
ITU<- sapply(1:n, function(i)
{
  sum(3/24*dat[1:459,3*i+4]*dat[1:459,3*i+5]/sum(dat[1:459,3*i+5]))
})
CTU<- cumsum(ITU)
indx<- min(which(CTU>200))
ret200_2<- (ret[indx-1]*(CTU[indx]-200) + ret[indx]*(200-CTU[indx-1]))/(CTU[indx]-CTU[indx-1])
ret200_2
ret200

#### OLD; 0.9
ITU_9<- sapply(1:n, function(i)
{
  sum(3/24*(1/0.9)*dat[1:459,3*i+4]*dat[1:459,3*i+5]/sum(dat[1:459,3*i+5]))
})
CTU_9<- cumsum(ITU_9)
indx<- min(which(CTU_9>200))
ret200_9_2<- (ret[indx-1]*(CTU_9[indx]-200) + ret[indx]*(200-CTU_9[indx-1]))/(CTU_9[indx]-CTU_9[indx-1])
ret200_9_2
ret200_9
