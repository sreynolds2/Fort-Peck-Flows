

#######################################
#                                     #
#           MATURATION AGE            #
#                                     #
#######################################

# k=1/s
# var=s^2*pi^2/3
# sigma=pi/(k*sqrt(3))
# k=pi/(sigma*sqrt(3))
# 3*sigma=8, then k approx. 0.68

m<- 19
k<- 0.77
a_m<- 14
a_M<- 27
x<-a_m:a_M
a_max<- 100
Cdist<- 1/(1+exp(k*(m-x)))
mi<-rep(0, length(x))
mi[1]<-Cdist[1]
for(i in 2:(length(x)-1))
{
  mi[i]<- Cdist[i]-Cdist[i-1]
}
mi[length(x)]<- 1-sum(mi[1:(length(x)-1)])
barplot(mi, names.arg=a_m:a_M, ylim=c(0,0.2),
        xlab="Maturation Age (Years)", ylab="Probability Density")
dat<-data.frame(Age=1:a_max,
                m_i=c(rep(0,min(x)-1), mi, rep(0, a_max-max(x))))


#######################################
#                                     #
#           SPAWNING PERIOD           #
#                                     #  
#######################################
barplot(c(0,0.38,0.37,0.17,0.08,rep(0,5)), names.arg=1:10)

max_period<- 5

tau_dat<- data.frame(period=1:max_period,
                     number=c(0, 8, 3, rep(0, max_period-3)))
unkn_dat<- data.frame(period=c(">1", ">2", ">3", "2 or 4"),
                      number=c(6, 8, 2, 1))

par(mfrow=c(2,2),
    mar=c(2,4,1,0),
    oma=c(2.5,1,1,1))
## UNKNOWNS ALL WEIGHTED EVENLY (FOR AVAIABLE PERIODS)
counts<- sapply(1:4, function(i)
{
   if(unkn_dat$period[i]==">1")
   {
     out<- c(0, rep(unkn_dat$number[i]/(max_period-1), 
                    max_period-1))
   }
  if(unkn_dat$period[i]==">2")
  {
    out<- c(0, 0, rep(unkn_dat$number[i]/(max_period-2), 
                      max_period-2))
  }
  if(unkn_dat$period[i]==">3")
  {
    out<- c(0, 0, 0, rep(unkn_dat$number[i]/(max_period-3), 
                         max_period-3))
  }
  if(unkn_dat$period[i]=="2 or 4")
  {
    out<- c(0, unkn_dat$number[i]/2, 0, unkn_dat$number[i]/2,
            rep(0, max_period-4))
  }
  return(out)
})

counts<- tau_dat$number+rowSums(counts)
barplot(counts/sum(counts), names.arg = 1:max_period,
        ylim=c(0,0.5))
#barplot(c(counts/sum(counts),0), names.arg = 1:(max_period+1),
#        ylim=c(0,0.5))
legend("topright", 
       as.character(round(sum(counts*1:max_period)/sum(counts),
                          3)),
  
       bty="n", text.font=2)


## UNKNOWNS WEIGHTED AS 8/21 PERIOD 2 FOR ">1" 
## AND 3/5 PERIOD 3 FOR ">2" BASED ON KNOWN DATA
## AND WEIGHTED EVENLY BEYOND THAT
counts<- sapply(1:4, function(i)
{
  if(unkn_dat$period[i]==">1")
  {
    out<- c(0, 8/21, 13/21*3/5, 
            rep(13/21*2/5*1/(max_period-3), max_period-3)
            )*unkn_dat$number[i]
  }
  if(unkn_dat$period[i]==">2")
  {
    out<- c(0, 0, 3/5, 
            rep(2/5*1/(max_period-3), max_period-3)
            )*unkn_dat$number[i]
  }
  if(unkn_dat$period[i]==">3")
  {
    out<- c(0, 0, 0, rep(unkn_dat$number[i]/(max_period-3), 
                         max_period-3))
  }
  if(unkn_dat$period[i]=="2 or 4")
  {
    out<- c(0, unkn_dat$number[i]/2, 0, unkn_dat$number[i]/2,
            rep(0, max_period-4))
  }
  return(out)
})

counts<- tau_dat$number+rowSums(counts)
barplot(counts/sum(counts), names.arg = 1:max_period,
        ylim=c(0,0.5))
#barplot(c(counts/sum(counts),0), names.arg = 1:(max_period+1),
#        ylim=c(0,0.5))
legend("topright", 
       as.character(round(sum(counts*1:max_period)/sum(counts),
                          3)),
       bty="n", text.font=2)





## OR SPECIFIC PERIOD 1-3 PROBS WITH RELATIONSHIP AMONG 4-MAX
probs2<-c(0, 8/21, 13/21*3/5, rep(0, max_period-3))
### RULE EACH REDUCED BY HALF THE PROB
probs2<- probs2 +
  c(0, 0, 0, 
    13/21*2/5*1/sum(2^(0:(max_period-4)))*2^((max_period-4):0))
barplot(probs2, names.arg = 1:max_period,
        ylim=c(0,0.5))
#barplot(probs[2:max_period], names.arg = 2:max_period, 
#        xlab="Reproductive Period (Years)", 
#        ylim=c(0,0.5),
#        ylab="Probability Density")
#barplot(c(probs,0), names.arg = 1:(max_period+1),
#        ylim=c(0,0.5))
legend("topright", 
       as.character(round(sum(probs*1:max_period)/sum(probs),3)),
       bty="n", text.font=2)



## SAME AS ABOVE BUT USING THE FACT WE KNOW ONE IS 2 OR 4
probs<-c(0, 8.5/22, 13/22*3/5, 0.5/22, rep(0, max_period-4))
probs<- probs +
  c(0, 0, 0, 
    13/22*2/5*1/sum(2^(0:(max_period-4)))*2^((max_period-4):0))
barplot(probs, names.arg = 1:max_period,
        ylim=c(0,0.5))
#barplot(c(probs,0), names.arg = 1:(max_period+1),
#        ylim=c(0,0.5))
legend("topright", 
       as.character(round(sum(probs*1:max_period)/sum(probs),3)),
       bty="n", text.font=2)
mtext("Years Between Reproductive Readiness", 1, 
      outer=TRUE, padj=0.5)
mtext("Probability", 2, outer=TRUE, padj=1.5)


par(mfrow=c(1,1))
x<-seq(1,10,0.1)
y<-1/(1+exp(-2.7*(x-3)))
plot(x,y, type="l", xlab="Years Between Reproductive Readiness", 
     ylab="Cummulative Proportion")

x<-1:9
y<-1/(1+exp(-2.7*(x-3)))
p<- rep(0,length(y)+1)
p[2]<-y[2]
for(i in 3:length(y))
{
  p[i]<- y[i]-y[i-1]
}
p[length(p)]<- 1-y[length(y)]
sum(p)
barplot(p, names.arg = 1:length(p), ylab="Probability",
        xlab="Years Between Reproductive Readiness", ylim=c(0,0.55))
p



par(mfrow=c(1,1))
x<-seq(1,10,0.1)
y<-1/(1+exp(-5*(x-3)))
plot(x,y, type="l", xlab="Years Between Reproductive Readiness", 
     ylab="Cummulative Proportion")

x<-1:9
y<-1/(1+exp(-5*(x-3)))
p<- rep(0,length(y)+1)
p[2]<-y[2]
for(i in 3:length(y))
{
  p[i]<- y[i]-y[i-1]
}
p[length(p)]<- 1-y[length(y)]
sum(p)
barplot(p, names.arg = 1:length(p), ylab="Probability",
        xlab="Years Between Reproductive Readiness", ylim=c(0,0.55))
p


p_dat<- data.frame(Years=1:a_max,
                   Probability=c(probs2, rep(0, a_max-max_period)))
# write.csv(p_dat, "./baseline-parameters/reproductive_period_probs.csv",
#           row.names = FALSE)

dat$psi_i<- dat$m_i
start<-min(which(dat$m_i!=0))
for(i in (start+1):a_max)
{
  dat$psi_i[i]<- dat$m_i[i]+
    sum(dat$psi_i[start:(i-1)]*p_dat$Probability[i-start:(i-1)])
}

plot(dat$Age, dat$psi_i, xlab="Age (Years)", 
     ylab="Proportion of Females Reproductively Ready to Spawn",
     pch=19)

# write.csv(dat, "./baseline-parameters/Psi_by_Age_new.csv",
#           row.names = FALSE)

dat<- read.csv("./baseline-parameters/Psi_by_Age_new.csv")
