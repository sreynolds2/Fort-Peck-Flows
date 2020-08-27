
library(demogR)

## GENERATE BASELINE POPULATION TRANSITION MATRIX
A_base<- matrix(0,60,60)
### SURVIVAL VALUES
A_base[cbind(2:60,1:59)]<- c(0.64, 0.69, 0.72, 0.76, 0.79, 0.82, 0.84, 
                             0.86, 0.88, 0.895, 0.91, 0.92, 0.93, 
                             0.935, rep(0.94, 45))
### FERTILITY VALUES WITH FULL SPAWNING AND RETENTION
psi<- c(0.0009110512, 0.0015615720, 0.0045672949, 0.0122266340, 
        0.0319099933, 0.0784642192, 0.1673188663, 0.2751973410, 
        0.3302198159, 0.3323092533, 0.3338885331, 0.3379265874, 
        0.3391453206, 0.3415561692, 0.3372606143, 0.3394286578, 
        0.3392222014, 0.3389512062, 0.3391677126, 0.3389911381, 
        0.3391178293, 0.3390692029, 0.3390652542, 0.3390825082, 
        0.3390692824, 0.3390768185, 0.3390735233, 0.3390740041,
        0.3390747888, 0.3390739004, 0.3390744560, 0.3390742164, 
        0.3390742673, 0.3390743005, 0.3390742492, 0.3390742871, 
        0.3390742685, 0.3390742736, 0.3390742749, 0.3390742719, 
        0.3390742744, 0.3390742730, 0.3390742735, 0.3390742735, 
        0.3390742733, 0.3390742735, 0.3390742734, 0.3390742734, 
        0.3390742734, 0.3390742734, 0.3390742734, 0.3390742734, 
        0.3390742734)
eggs<- c(17858.32, 19196.99, 20567.28, 21929.43, 23349.27, 
         24775.18, 26243.11, 27685.86, 29146.68, 30671.10, 32132.68, 
         33675.72, 35201.90, 36758.49, 38313.39, 39869.35, 41409.45, 
         42992.59, 44535.03, 46151.91, 47672.25, 49359.17, 50889.91, 
         52537.71, 54138.78, 55773.74, 57365.43, 58985.37, 60526.52, 
         62200.39, 63870.55, 65417.74, 67086.64, 68720.32, 70346.88, 
         71994.93, 73619.97, 75342.93, 76795.55, 78508.78, 80057.36, 
         81574.96, 83282.14, 85053.37, 86523.37, 88186.36, 89790.52, 
         91420.39, 92923.74, 94449.11, 96010.70, 97906.26, 99150.91)
sexratio<- 0.32
phi0_MR<- 0.000075
A_base[1,8:60] <- psi*eggs*sexratio*phi0_MR
rm(psi, eggs, sexratio, phi0_MR)


## PULL ANNUAL RETENTION DATA BY ALTERNATIVE
dat<- read.csv("./output/_stochastic/Standard_Annual_Retentions_by_Alt.csv",
               stringsAsFactors = FALSE)

## DETERMINE THE AVERAGE MATRIX BY ALTERNATIVE
alts<- c("1", "1a", "1b", "2", "2a", "2b", "NoAct")
### ASSUMING IID
Avg_dat<- lapply(alts, function(x)
{
  tmp<- subset(dat, Alt==x)
  A_sum<- matrix(0,60,60)
  for(y in 1930:2012)
  {
    if(tmp[which(tmp$Year==y),]$Spawn==0)
    {
      A_t<- A_base
      A_t[1,]<- 0
    }
    if(tmp[which(tmp$Year==y),]$Spawn==1)
    {
      A_t<- A_base
      A_t[1,8:60]<- A_t[1,8:60]*0.5*tmp[which(tmp$Year==y),]$Retention
    }
    A_sum<- A_sum+A_t
  }
  A_avg<- A_sum/83
  ea<- eigen.analysis(A_avg)
  lambda<- ea$lambda1
  Y0<- ea$stable.age
  out<- list(A_avg=A_avg, lambda=lambda, stable_age=Y0)
  return(out)
})
names(Avg_dat)<- alts
saveRDS(Avg_dat, "./output/_stochastic/Average_Matrix_Data_IID.rds")


### ASSUMING STATIONARY MARKOV CHAIN
#### PULL TEMPERATURE TRANSITION MATRIX AND COMPUTE STABLE DISTRIBUTION
trans<- read.csv("./output/_stochastic/Temperature_Transition_Matrix.csv",
                 row.names = 1)
ea<- eigen.analysis(trans)
p<- ea$stable.age
names(p)<- c("Low", "Median", "High")
#### PULL ANNUAL DEIS POR TEMPERATURE DATA AND COMPUTE COUNTS 
temps<- read.csv("./dat/Observed_Annual_Temperature_Class.csv")
temps<- subset(temps, Year %in% 1930:2012)
temps$freq<- 1
temp_freq<- ddply(temps, .(Weather), summarize, Frequency=sum(freq))
temp_freq<- temp_freq[c(2,3,1),]

#### FIND STATIONARY DISTRIBUTION
AvgMC_dat<- lapply(alts, function(x)
{
  tmp<- subset(dat, Alt==x)
  A_sum<- matrix(0,60,60)
  for(y in 1930:2012)
  {
    if(tmp[which(tmp$Year==y),]$Spawn==0)
    {
      A_t<- A_base
      A_t[1,]<- 0
    }
    if(tmp[which(tmp$Year==y),]$Spawn==1)
    {
      A_t<- A_base
      A_t[1,8:60]<- A_t[1,8:60]*0.5*tmp[which(tmp$Year==y),]$Retention
    }
    py<- unname(p[tmp[which(tmp$Year==y),]$Weather])
    freq<- temp_freq[which(temp_freq$Weather==tmp[which(tmp$Year==y),]$Weather),]$Frequency
    A_sum<- A_sum+A_t*py*1/freq
  }
  ea<- eigen.analysis(A_sum)
  lambda<- ea$lambda1
  Y0<- ea$stable.age
  out<- list(A_avg=A_sum, lambda=lambda, stable_age=Y0)
  return(out)
})
names(AvgMC_dat)<- alts
saveRDS(AvgMC_dat, "./output/_stochastic/Average_Matrix_Data_MC.rds")
