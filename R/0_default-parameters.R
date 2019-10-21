
#####################################################
#                                                   #
#       BASELINE (DEFAULT) MODEL PARAMETERS         #
#                                                   #
#####################################################

inputs<- list()

# MAXIMUM AGE
inputs$max_age<- 60

# AGE-1+ SURVIVALS
phi<- read.csv("./dat/BaselinePhi.csv")
phi<- phi$Baseline.Value
inputs$phi<- c(phi, rep(phi[length(phi)], 
                        inputs$max_age-1-length(phi)))
rm(phi)

# FECUNDITIES
## PROPORTION OF FEMALES THAT ARE REPRODUCTIVELY READY
### MATURATION
a_min<- 8
a_max<- 21
a_h<- 15
k<- 1 #k=1 means 3*sigma is approximately 5 years
mat_Cdist<-1/(1+exp(-k*(a_min:(a_max-1)-a_h)))
m<-rep(0, a_max-a_min+1)
m[1]<-mat_Cdist[1]
for(i in 2:length(mat_Cdist))
{
  m[i]<- mat_Cdist[i]-mat_Cdist[i-1]
}
m[length(mat_Cdist)+1]<- 1-mat_Cdist[length(mat_Cdist)]
#### SAVE MATURATION INPUTS
inputs$mat$a_min<- a_min
inputs$mat$a_max<- a_max
inputs$mat$a_h<- a_h
inputs$mat$k<- k
inputs$mat$m_i<- c(rep(0,a_min-1), m, rep(0, inputs$max_age-a_max))
rm(a_max, a_h, k, mat_Cdist, m,i)
### REPRODUCTIVE PERIOD
max_period<- 5
  ## FROM FULLER ET AL. 2007 AND DELONAY ET AL. 2016:
  #tau_dat<- data.frame(period=1:max_period,
  #                     number=c(0, 8, 3, rep(0, max_period-3)))
  #unkn_dat<- data.frame(period=c(">1", ">2", ">3", "2 or 4"),
  #                      number=c(6, 8, 2, 1))
#### SPECIFIC PERIOD 1-3 PROBS FROM KNOWN DATA ABOVE WITH THE REMAINING 
#### PROBABILITY SPLIT AMONG PERIODS 4 TO MAX_PERIOD SUCH THAT
#### PROBS[T+1]=PROBS[T]/2 FOR T>4
probs<-c(0, 8/21, 13/21*3/5, 
         13/21*2/5*1/sum(2^(0:(max_period-4)))*2^((max_period-4):0))
#### SAVE REPRODUCTION PERIOD INPUTS
inputs$reproduction$max_period<- max_period
inputs$reproduction$p_t<- c(probs, rep(0, inputs$max_age-a_min-max_period))
rm(max_period, probs)
### CALCULATE PROPORTION FROM MATURATION PROBABILITIES 
### AND REPRODUCTION PERIODS
inputs$psi<- rep(0, inputs$max_age)
inputs$psi[a_min]<- inputs$mat$m_i[a_min]
for(i in (a_min+1):60)
{
  inputs$psi[i]<- inputs$mat$m_i[i]+
    sum(inputs$psi[a_min:(i-1)]*inputs$reproduction$p_t[i-a_min:(i-1)])
}
rm(i, a_min)

## SEX RATIO (PROBABILITY OF BEING FEMALE)
inputs$probF<- 0.5 #ASSUME EQUAL AND PROBABILITY BELOW REPRESENTS
                   #HIGHER HISTORICAL FISHING OF FEMALES FOR CAVIAR
  #40/125 #JAEGER ET AL. 2009  

## NUMBER OF EGGS PER SPAWNING FEMALE
E<- read.csv("./dat/fecundity_estimates_by_age.csv")
inputs$eggs<- c(rep(0,7), E$Mean_Eggs_Produced)
rm(E)

## AGE-0 SURVIVAL
### RETENTION IN MR
inputs$p_retained<-0.001
### SURVIVAL GIVEN RETENTION IN MR
inputs$phi0_MR<-0.000075
#inputs$phi0_MR$predation<- 0.8
#inputs$phi0_MR$phi0_NP<- 0.0001

## PROBABILITY A REPRODUCTIVELY READY FEMALE SPAWNS IN THE MISSOURI (BELOW THE DAM)
#inputs$gamma_upper<- 0.5
#inputs$gamma_lower<- 0.01
inputs$gamma<- 0.01

inputs$id<- 1
