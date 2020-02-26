

dat<- read.xlsx("./dat/Alt1_Retention_Out_12.xlsx", "OO")
#COL 6: FIRST VELOCITY
#COL 7: FIRST TEMPERATURE
#COL 8: FIRST SPATIAL DISTRIBUTION

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
  out=3/24*dat[1:459,3*x+4]*dat[1:459,3*x+5]/sum(dat[1:459,3*x+5])/(1923*dat[1:459,3*x+4]^(-0.74))
  return(out)
})
Calcs<- do.call(cbind, Calcs)
IPCTUs<- colSums(Calcs)
PCTUs<- cumsum(IPCTUs)
Ret<- sapply(1:96, function(x)
{
  sum(dat[1:459,3*x+5])/sum(dat[,3*x+5])
})

(Ret[min(which(PCTUs>1))-1]*(PCTUs[min(which(PCTUs>1))]-1) +
  Ret[min(which(PCTUs>1))]*(1-PCTUs[min(which(PCTUs>1))-1]))/
  (PCTUs[min(which(PCTUs>1))]-PCTUs[min(which(PCTUs>1))-1])


Calcs_9<- lapply(1:96, function(x)
{ 
  out=30/(9*24)*dat[1:459,3*x+4]*dat[1:459,3*x+5]/sum(dat[1:459,3*x+5])/(1923*dat[1:459,3*x+4]^(-0.74))
  return(out)
})
Calcs_9<- do.call(cbind, Calcs_9)
IPCTUs_9<- colSums(Calcs_9)
PCTUs_9<- cumsum(IPCTUs_9)


(Ret[min(which(PCTUs_9>1))-1]*(PCTUs_9[min(which(PCTUs_9>1))]-1) +
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


(Ret[min(which(CTUs>200))-1]*(CTUs[min(which(CTUs>200))]-200) +
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


(Ret[min(which(CTUs_9>200))-1]*(CTUs_9[min(which(CTUs_9>200))]-200) +
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
