

## FOR THE GIVEN AGE-0 SURVIVAL AND BIRTH RATE, FIND THE LOWER BOUND OF 
## THE 95% CONFIDENCE INTERVAL FOR THE NUMBER OF AGE-1 RECRUITS GIVEN
## THE NUMBER OF MATURE FEMALES

CI_lb<- function(phi0=NULL,
                 birthR=NULL,
                 Nf_mat=NULL,
                 CI=0.95)
{
  # ERROR HANDLING
  if(length(CI)!=1)
  {
    return(print("This function takes one confidence level at a time."))
  }
  # GENERATE TABLE
  out<- expand.grid(phi0=phi0, birthR=birthR, Nf_mat=Nf_mat)
  # ADD CIs
  out$lower_CI<- qbinom(CI, out$birthR*out$Nf_mat, out$phi0, lower.tail = FALSE)
  out$upper_CI<- qbinom(CI, out$birthR*out$Nf_mat, out$phi0, lower.tail = TRUE)
  out$CI<- CI
  return(out)
}

# TEST AND PLOT
test<- CI_lb(phi0 = c(0.000075, 0.00002),
             birthR = c(5000, 10000),
             Nf_mat = seq(0, 200, 2))

tmp<- subset(test, phi0==0.000075 & birthR==10000)
plot(tmp$Nf_mat, tmp$lower_CI, xlab="", ylab="")
mtext("Number of Mature Females", 1, outer=TRUE, padj=-3, adj=0.6, font=2)
mtext("Number of Age-1 Recruits (95% CI Lower Bound)", 2, outer=TRUE, padj=2.5, font=2)

library(demogR)
A<-matrix(0,60, 60)
## SURVIVAL RATES
A[cbind(2:60,1:59)]<- rep(0.95, 59)
## FECUNDITIES
birthV<- c(rep(0,8), rep(10000, 60-8))
A[1,] <- 0.32*birthV*0.000075

# EIGENANALYSIS 
ea<- eigen.analysis(A)
ea$A<- A

## PROJECT WITH DISCRETE NUMBERS
stoch_lambda<- function(maxage=NULL,
                        sexratio=NULL,
                        phi=NULL,
                        birthR=NULL,
                        phi0=NULL,
                        Nf_mat=NULL,
                        N_years=100,
                        N_reps=100,
                        CI=0.95)
{
  # BUILD LESLIE MATRIX
  A<-matrix(0,maxage,maxage)
  ## SURVIVAL RATES
  A[cbind(2:maxage,1:(maxage-1))]<- phi
  ## FECUNDITIES
  if(length(birthR)==1){birthV<- c(rep(0,8), rep(birthR, maxage-8))}
  if(length(birthR)==maxage){birthV<- birthR}
  A[1,] <- sexratio*birthV*phi0
  
  # EIGENANALYSIS 
  ea<- eigen.analysis(A)
  ea$A<- A
  ea$birth_rate<- round(sum(birthV*ea$stable.age)/sum(ea$stable.age[9:maxage]))
  
  #IDs
  phi0_id<- phi0*1000
  if(length(birthR)==1)
  {
    bR_id<- birthR/100
  }
  if(length(birthR)==maxage)
  {
    bR_id<- round(ea$birth_rate/100)
  }
  
  # NUMBER OF FISH BY AGE
  if(length(birthR)==1)
  {
    matV<- c(rep(0,8), ea$stable.age[9:length(ea$stable.age)])
    juvV<- c(ea$stable.age[1:8], rep(0, maxage-8))
  }
  if(length(birthR)==maxage)
  {
    indx<- which(birthR==0)
    matV<- ea$stable.age
    matV[indx]<- 0
    juvV<- ea$stable.age
    juvV[setdiff(1:maxage, indx)]<- 0
  }
  Nf_juv<- ceiling(Nf_mat/sum(matV))-Nf_mat
  matV<- matV/sum(matV)
  juvV<- juvV/sum(juvV)
  
  out<- sapply(1:N_reps, function(x)
  {
    lambdaT<- rep(0, N_years)
    N_mat<- rmultinom(1, Nf_mat, matV)
    N_juv<- rmultinom(1, Nf_juv, juvV)
    N<- N_juv+N_mat
    tmp<- data.frame(age=1:maxage, number=N)
    
    for(i in 1:N_years)
    {
      age2plus<- rbinom(length(phi), tmp$number[1:(nrow(tmp)-1)], phi)
      age1<- sum(rbinom(length(birthV), tmp$number*birthV, sexratio*phi0))
      lambdaT[i]<- sum(c(age1, age2plus))/sum(tmp$number)
      tmp$number<- c(age1, age2plus)
    }
    lambda<- prod(lambdaT)^(1/N_years)
    return(lambda)
  })
  return(out)
}




maxage<- 60
sexratio<- 0.32
phi<- rep(0.95, 59)
birthR<- 10000
phi0<- 0.000075
Nf_mat<- 100
N_years<- 10
N_reps<- 100
CI<- 0.95

