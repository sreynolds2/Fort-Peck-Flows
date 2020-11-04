setwd("./GitHub/Fort-Peck-Flows")

source("./R/1_global.r")
source("./R/2_functions.r")
full_run<- FALSE
source("./R/3_load-and-clean.r")

# tmp<- subset(dat, Retention_Probability>0)
#
# Long_Term_Data<- lapply(1:nrow(tmp), function(x)
# {
#   inps<- inputs
#   inps$gamma<- 0.5
#   inps$p_retained<- tmp$Retention_Probability[x]
#   ea<- matrix_eigen_analysis(inps)
# })
# sum(Long_Term_Data[[1]]$stable.age*Long_Term_Data[[1]]$A[1,])
# 
# x<- seq(0.01, 1, 0.01)
# y<- x
# z<- 0.000075
# vals1<- sapply(x, function(i)
# {
#   inps<- inputs
#   inps$gamma<- i
#   out<- sapply(y, function(j)
#   {
#     inps$p_retained<- j
#     ea<- matrix_eigen_analysis(inps)
#     rec<- sum(ea$stable.age*ea$A[1,])
#     return(rec)
#   })
#   return(out)
# })
# 
# contour(x = x, y = y, z = vals1, nlevels = 10, labels = NULL,
#         xlim=c(0,1), ylim=c(0,1), 
#         xlab="Spawning Probability below Fort Peck given Reproductive-Readiness",
#         ylab="Retention Probability",
#         main="Expected Age-1 Recruits per Female")
# 


x<- seq(0.01, 1, 0.01)
y<- x
phi0<- c(0.000025, 0.00005, 0.000075, 0.0001, 
         0.0002, 0.0004, 0.0006, 0.0008, 0.001, 0.002)
vals<- lapply(phi0, function(z)
{
  inps<- inputs
  inps$phi0_MR<- z
  out<- lapply(x, function(i)
  {
    inps$gamma<- i
    outt<- lapply(y, function(j)
    {
      inps$p_retained<- j
      ea<- matrix_eigen_analysis(inps)
      p_adults<- sum(ea$stable.age*cumsum(inputs$mat$m_i))
      p_RRfemale<- sum(ea$stable.age*inputs$probF*inputs$psi)
      rec<- data.frame(gamma=i, pret=j, phi0_MR=z, 
                       ENrec=sum(ea$stable.age*ea$A[1,]),
                       p_adult=p_adults,
                       p_RRfemale=p_RRfemale)
      return(rec)
    })
    outt<- do.call(rbind, outt)
    return(outt)
  })
  out<- do.call(rbind, out)
  return(out)
})
vals<- do.call(rbind, vals)

vals$ENrec_target<- round(5000*inputs$probF/vals$p_adult)*vals$ENrec
vals$ENrec_2019<- 7508*inputs$probF*vals$ENrec
vals$ENrec_wild<- round(275*inputs$probF/vals$p_adult)*vals$ENrec
vals$ENrec_100<- 100*vals$ENrec
vals$ENrec_100RR<- (inputs$probF*100/vals$p_RRfemale)*vals$ENrec

write.csv(vals, "./output/_long-term/recruitment/Recruitment_Values.csv",
          row.names = FALSE)

# vals<- read.csv("./output/_long-term/recruitment/Recruitment_Values.csv")

library(lattice)
library(grid)
z<- phi0[10]
tmp<- vals[which(vals$phi0_MR==z),]
contourplot(ENrec~gamma+pret, tmp, scales = list(tck = c(-1,-1)),
            #panel=panel.axis("top", ticks=FALSE),
            cuts=15,
            xlab="Spawning Probability below Fort Peck given Reproductive-Readiness", 
            ylab="Retention Probability",
            main="Expected Age-1 Recruits per Female")
ltext(540, 55,paste0("phi0_MR=", z), pos=1, font=2)
contourplot(ENrec_100~gamma+pret, tmp, scales = list(tck = c(-1,-1)),
            #panel=panel.axis("top", ticks=FALSE),
            cuts=15,
            xlab="Spawning Probability below Fort Peck given Reproductive-Readiness", 
            ylab="Retention Probability",
            main="Expected Age-1 Recruits per 100 Females")
ltext(130, 475,paste0("phi0_MR=", z), pos=1, font=2)
contourplot(ENrec_target~gamma+pret, tmp, scales = list(tck = c(-1,-1)),
            #panel=panel.axis("top", ticks=FALSE),
            cuts=30,
            xlab="Spawning Probability below Fort Peck given Reproductive-Readiness", 
            ylab="Retention Probability",
            main="Expected Age-1 Recuits Assuming 5,000 Adult Pallid Sturgeon")
ltext(130, 475,paste0("phi0_MR=", z), pos=1, font=2)

z<- phi0[10]
tmp<- vals[which(vals$phi0_MR==z),]
contourplot(ENrec_100RR~gamma+pret, tmp, scales = list(tck = c(-1,-1)),
            #panel=panel.axis("top", ticks=FALSE),
            cuts=30,
            xlab="Spawning Probability below Fort Peck given Reproductive-Readiness", 
            ylab="Retention Probability",
            main="Expected Age-1 Recruits per 100 Reproductively-Ready Females")
ltext(135, 360, paste0("phi0_MR=", z), pos=1, font=2)


## STABLE AGE DISTRIBUTIONS 
gam<- 0.5
z<- 0.000075
age_st<-lapply(c(0.00001, 0.0001, 0.001, 0.01, 0.05, seq(0.1, 1, 0.1)), function(j)
{
  inps<- inputs
  inps$gamma<- gam
  inps$p_retained<- j
  ea<- matrix_eigen_analysis(inps)
  p_adults<- sum(ea$stable.age*cumsum(inputs$mat$m_i))
  p_RRfemale<- sum(ea$stable.age*inputs$probF*inputs$psi)
  N_target<- ceiling(5000/p_adults)
  tmp<- data.frame(gamma=gam, pret=j, phi0_MR=z, lambda=ea$lambda1, 
                   ENrec=sum(ea$stable.age*ea$A[1,]),
                   p_adult=p_adults, p_RRfemale=p_RRfemale,
                   N_target=N_target)
  out<- list(vals=tmp, stable_age=ea$stable.age, 
             RR_age=inps$probF*inps$psi)
  return(out)
})

saveRDS(age_st, "./output/_long-term/age_structure/Target_Structure_Baseline.rds")

round(age_st[[1]]$vals$N_target*age_st[[1]]$stable_age)
lbl<- c(rep("topleft", 5), rep("topright", 10))
par(mfrow=c(3,3),
    mar = c(2, 2, 1, 1)+0.1,
    oma=c(0,2,0,0))
for(i in c(2,4:7,9,11,13,15))
{
  barplot(ceiling(age_st[[i]]$vals$N_target*age_st[[i]]$stable_age))
  legend(lbl[i], paste0("Retention Probability =", 
                        age_st[[i]]$vals$pret), bty="n")
}
mtext("Ages 1-60 (Years)", 1, outer=TRUE, padj=-1.5)
mtext("Abundance", 2, outer=TRUE, padj=-0.5)

age_st[[3]]$vals
round(age_st[[3]]$vals$p_RRfemale*age_st[[3]]$vals$N_target)


N_age<- ceiling(age_st[[1]]$vals$N_target*age_st[[1]]$stable_age)
N_age
N_RR<- ceiling(age_st[[1]]$RR_age*N_age)
N_RR
sum(N_RR)
round(age_st[[1]]$vals$p_RRfemale*age_st[[1]]$vals$N_target)
age_st[[1]]$vals$p_RRfemale
sum(N_RR)/sum(N_age)

lbl<- c(rep("topleft", 5), rep("top",2), rep("topright", 8))
par(mfrow=c(3,3),
    mar = c(2, 2, 1, 1)+0.1,
    oma=c(0,2,2,0))
for(i in c(2,4:7,9,11,13,15))
{
  N_age<- ceiling(age_st[[i]]$vals$N_target*age_st[[i]]$stable_age)
  N_RR<- ceiling(age_st[[i]]$RR_age*N_age)
  barplot(matrix(c(N_RR, N_age-N_RR), byrow = TRUE, 
                 ncol = length(N_RR)), col=c("blue", "gray"))
  legend(lbl[i], c(paste0("Retention Probability = ", 
                          age_st[[i]]$vals$pret),
                   paste0("Total Population Size = ", 
                          sum(N_age)),
                   paste0("Spawning Females = ", 
                          sum(N_RR))), bty="n")
}
mtext("Ages 1-60 (Years)", 1, outer=TRUE, padj=-1.5)
mtext("Abundance", 2, outer=TRUE, padj=-0.5)
par(mfrow=c(1,1), fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), 
    mar = c(0, 0, 0, 0), new=TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("topright", c("Reproductively Ready Females", 
                  "Males & Immature or Not Reproductively Ready Females"),
       fill=c("blue", "gray"), xpd=TRUE, cex=0.75, bty="n")

# 275 WILD
# 7233 HATCHERY

###############################################
#                                             #
#    PULL SUMMARY DATA FROM ROTELLA 2017      #
#                                             #
###############################################
library(tabulizer)
library(plyr)
rotella<- extract_tables("./baseline-parameters/Rotella_2017_Update.pdf", 
                                 pages=c(92))
rotella<- as.data.frame(rotella[[1]], stringsAsFactors=FALSE)
spc<- ifelse(rotella[1,]=="", "", "_")
nms<- paste0(rotella[1,], spc, rotella[2,])
nms<- gsub(" ", "_", nms)
rotella<- rotella[3:nrow(rotella),]
names(rotella)<- nms
rotella$Release_cohort<- as.numeric(rotella$Release_cohort)
rotella$Number_Released<- as.numeric(gsub(",", "", rotella$Number_Released))
rotella$Age_in_months<- as.numeric(rotella$Age_in_months)
rotella$Age_in_years<- as.numeric(rotella$Age_in_years)
rotella$N_Alive<- as.numeric(gsub(",", "", rotella$N_Alive))

rotella<- rotella[order(rotella$Age_in_years),]
rotella$age_class<- floor(rotella$Age_in_years)
abund<- aggregate(N_Alive~age_class, rotella, sum)
names(abund)[2]<- "N_2016"
M<- max(abund$age_class)
abund<- merge(abund, 
                     data.frame(age_class=1:M),
                     all=TRUE)
abund[is.na(abund$N_2016), ]$N_2016<- 0
par(mfrow=c(1,1))
barplot(abund$N_2016, names.arg = abund$age_class,
        ylim=c(0,5000), xlab= "Age (Years)", ylab="Abundance")
## PROJECT TO 2019 & 2020
abund<- merge(abund, 
              data.frame(age_class=M:(M+4)),
              all=TRUE)
abund[is.na(abund$N_2016), ]$N_2016<- 0
### 2019
abund$N_2019<- NA
for(i in 1:(M+1))
{
  abund$N_2019[i+3]<- round(abund$N_2016[i]*
                              prod(inputs$phi[abund$age_class[i]:
                                                (abund$age_class[i]+2)]))
}
#### ADD IN STOCKED FISH 
stock<- read.csv("./dat/All_Stocked_Data_Fall_2019_Cleaned.csv", 
                 stringsAsFactors = FALSE)
stock<- subset(stock, RPMA==2)
stock$STOCK.DATE<- as.Date(stock$STOCK.DATE)
stock$YEAR.STOCK<- as.numeric(format(stock$STOCK.DATE, "%Y"))
stock<- subset(stock, YEAR.STOCK>2015)
# ##### COMPUTE AVERAGE YEARLING AGE AT TIME OF STOCKING
# stock$SPAWN.DATE<- as.Date(stock$SPAWN.DATE)
# stock$AGE<- stock$STOCK.DATE-stock$SPAWN.DATE
# yr_age<- round(as.numeric(mean(stock[which(stock$AGE.AT.STOCKING=="Yearling"),]$AGE)))
# stock$YEARLING.STOCK.DATE<- ifelse(stock$AGE.AT.STOCKING=="Yearling",
#                                    stock$STOCK.DATE, stock$STOCK.DATE+yr_age)
##### COMPUTE AVERAGE YEARLING STOCKING DAY
stock$SPAWN.DATE<- as.Date(stock$SPAWN.DATE)
yr_day<- round(mean(as.numeric(format(stock[which(stock$AGE.AT.STOCKING=="Yearling"),]$STOCK.DATE, "%j"))))
library(chron)
stock$dt<- sapply(stock$YEAR.STOCK, function(x)
{
  paste(month.day.year(yr_day, c(1,1,x+1))$year, 
        paste0("0", month.day.year(yr_day, c(1,1,x+1))$month),
        month.day.year(yr_day, c(1,1,x+1))$day, sep="-")
})
stock$dt<- as.Date(stock$dt)
stock$YEARLING.STOCK.DATE<- ifelse(stock$AGE.AT.STOCKING=="Yearling",
                                   stock$STOCK.DATE, stock$dt)
stock$YEARLING.STOCK.DATE<- as.Date(stock$YEARLING.STOCK.DATE, 
                                    origin = "1970-01-01")
stock$YEARLING.STOCK.AGE<- as.numeric(stock$YEARLING.STOCK.DATE-stock$SPAWN.DATE)/365
stock$YEARLING.STOCK.YEAR<- as.numeric(format(stock$YEARLING.STOCK.DATE, "%Y"))
stock$dt<- sapply(stock$YEARLING.STOCK.YEAR, function(x)
{
  paste(x,"09-16", sep="-")
})
stock$dt<- as.Date(stock$dt)
stock$SEPTEMBER.AGE<- as.numeric(stock$dt-stock$SPAWN.DATE)/365
stock$age_class<- floor(stock$SEPTEMBER.AGE)
stock_sum<- aggregate(YEARLING.EQUIVALENTS~YEARLING.STOCK.YEAR+age_class, 
                      stock, sum)
stock_sum<- merge(stock_sum, 
                  data.frame(YEARLING.STOCK.YEAR=2016:2020,
                             age_class=1),
                  all=TRUE)
stock_sum[is.na(stock_sum$YEARLING.EQUIVALENTS),]$YEARLING.EQUIVALENTS<- 0
s<- abund[1,]$N_2016/stock_sum[which(stock_sum$YEARLING.STOCK.YEAR==2016),]$YEARLING.EQUIVALENTS
stock_sum$N_September<- round(stock_sum$YEARLING.EQUIVALENTS*s)

stock_dat<- data.frame(age_class=1:4)
stock_dat$N_2019<- 
  round(c(stock_sum[which(stock_sum$YEARLING.STOCK.YEAR==2019),]$N_September,
    stock_sum[which(stock_sum$YEARLING.STOCK.YEAR==2018),]$N_September*inputs$phi[1],
    stock_sum[which(stock_sum$YEARLING.STOCK.YEAR==2017),]$N_September*
      prod(inputs$phi[1:2]),
    stock_sum[which(stock_sum$YEARLING.STOCK.YEAR==2016),]$N_September*
      prod(inputs$phi[1:3])))
abund$N_2019[1:3]<- stock_dat$N_2019[1:3]

barplot(abund$N_2019, names.arg = abund$age_class,
        ylim=c(0,3000), xlab= "Age (Years)", ylab="Abundance")
sum(abund$N_2019)

### 2020
abund$N_2020<- NA
for(i in 3:(nrow(abund)-1))
{
  abund$N_2020[i+1]<- round(abund$N_2019[i]*inputs$phi[abund$age_class[i]])
}
#### ADD IN STOCKED FISH 
stock_dat$N_2020<- 
  round(c(stock_sum[which(stock_sum$YEARLING.STOCK.YEAR==2020),]$N_September,
          stock_dat$N_2019[1:3]*inputs$phi[1:3]))
abund$N_2020[1:4]<- stock_dat$N_2020[1:4]

barplot(abund$N_2020, names.arg = abund$age_class,
        ylim=c(0,3000), xlab= "Age (Years)", ylab="Abundance")
sum(abund$N_2020)


### UPDATE MATURATION INPUTS AND COMPUTE MATURE FEMALES
inps<- create_inputs(a_mat_min = 14,  a_mat_max=27, a_mat_h=19)

abund$matF_2020<- round(inps$probF*cumsum(inps$mat$m_i)[1:23]*abund$N_2020)
abund$RRF_2020<- round(inps$probF*inps$psi[1:23]*abund$N_2020)
sum(abund$matF_2020)
sum(abund$RRF_2020)
abund$immFM_2020<- abund$N_2020-abund$matF_2020 

barplot(matrix(c(abund$immFM_2020, abund$matF_2020), byrow = TRUE, 
               ncol = length(abund$matF_2020)), 
        names.arg = abund$age_class, col=c("gray", "blue"),
        legend.text = c("Males and Not Reproductively Ready Females",
                        "Reproductively Ready Females (37 HOPS)"),
        args.legend=list(bty="n"),
        ylim=c(0,3000), xlab= "Age (Years)", ylab="Abundance")

### ADD IN WILD FISH USING JAEGER ET AL. 2009 ESTIMATES FROM 2008
### AND AN INCREASED SURVIVAL OF 0.95
wld<- round(125*0.95^12)
wldF<- round(40*0.95^12)
age<- sample(63:100, wld, replace=TRUE)
  # BRAATEN ET AL. 2015 PRE-1957
sex<- sample(1:wld, wldF, replace = FALSE)
ageF<- age[sex]
wld_dat<- data.frame(age_class=24:100, 
                     N_2020=sapply(24:100, function(i){length(which(age==i))}),
                     matF_2020=sapply(24:100, function(i){length(which(ageF==i))}))
wld_dat$immFM_2020<- wld_dat$N_2020-wld_dat$matF_2020 
wld_dat$RRF_2020<- rbinom(nrow(wld_dat), wld_dat$matF_2020, inps$psi[60])
abund<- merge(abund, wld_dat, all=TRUE)
abund$matF_nonRR_2020<- abund$matF_2020-abund$RRF_2020
barplot(matrix(c(abund$immFM_2020[60:100], abund$matF_nonRR_2020[60:100], 
                 abund$RRF_2020[60:100]), 
               byrow = TRUE, nrow=3, ncol = length(abund$matF_2020[60:100])), 
        names.arg = abund$age_class[60:100], col=c("gray", "blue", "red"),
        legend.text = c("Males",
                        "Mature, Non-reproductive Females",
                        "Reproductively Ready Females"),
        args.legend=list(bty="n"), border=NA,
        ylim=c(0,8), xlab= "Age (Years)", ylab="Abundance")

barplot(matrix(c(abund$immFM_2020[1:25], abund$matF_nonRR_2020[1:25], 
                 abund$RRF_2020[1:25]), 
               byrow = TRUE, nrow=3, ncol = length(abund$matF_2020[1:25])), 
        names.arg = abund$age_class[1:25], col=c("gray", "blue", "red"),
        legend.text = c("Males and Immature Females",
                        "Mature, Non-reproductive Females",
                        "Reproductively Ready Females"),
        args.legend=list(bty="n"), border=NA,
        ylim=c(0,3000), xlab= "Age (Years)", ylab="Abundance")

barplot(matrix(c(abund$immFM_2020, abund$matF_nonRR_2020, 
                 abund$RRF_2020), 
               byrow = TRUE, nrow=3, ncol = length(abund$matF_2020)), 
        names.arg = abund$age_class, col=c("gray", "blue", "red"),
        legend.text = c("Males and Immature Females",
                        "Mature, Non-reproductive Females",
                        "Reproductively Ready Females"),
        args.legend=list(bty="n"), border=NA,
        ylim=c(0,3000), xlab= "Age (Years)", ylab="Abundance")



########################################################################
#                                                                      #
#               COMBINE RECRUITMENT & GROWTH CURVES                    #
#                                                                      #
########################################################################
# NEW FUNCTIONS SO THAT RETENTION PROBABILITY IS ON THE Y-AXIS
spawning_survival_retention_curves2<- function(boundary_inputs=NULL,
                                               phi0MR=seq(0.0002, 0.002, 0.0002))
{
  maxage<- boundary_inputs$max_age
  phi<- boundary_inputs$phi
  psi<- boundary_inputs$psi
  eggs<- boundary_inputs$eggs
  sexratio<- boundary_inputs$probF
  prod<-boundary_inputs$boundary$product
  phi0<-phi0MR
  curve_dat<- lapply(1:length(phi0MR), function(i)
  {
    gamma<- c(0.000001,seq(0.01, 1, 0.01))
    p_retained<- prod/(phi0[i]*gamma)
    indx<-which(p_retained>1)
    p_retained<- p_retained[-indx]
    gamma<-gamma[-indx]
    #SYMMETRICAL SO CAN EXPAND
    tmp<-p_retained
    p_retained<- c(p_retained, gamma)
    gamma<- c(gamma, tmp)
    out<-NULL
    if(length(p_retained>0))
    {
      out<-data.frame(gamma=gamma, phi0_MR=phi0[i], p_retained=p_retained)
    }
    return(out)
  })
  curve_dat<- do.call(rbind, curve_dat)
  return(curve_dat)
}


plot_boundary_curves2<- function(curve_dat=NULL,
                                 gamma_upper=1,
                                 pret_upper=1,
                                 xlabel=expression(paste("Spawning Probability  (", gamma, ")")),
                                 ylabel=expression(paste("Retention Probability  (  ", p[ret], ")")),
                                 xaxis="s")
{
  phi0_MR<-unique(curve_dat$phi0_MR)
  phi0_MR<- phi0_MR[order(phi0_MR)]
  tmp<-curve_dat[which(curve_dat$phi0_MR==phi0_MR[1]),]
  tmp<-tmp[order(tmp$gamma),]
  plot(tmp$gamma, tmp$p_retained, type="l", 
       xlim=c(0,gamma_upper), ylim=c(0,pret_upper),
       xlab=xlabel, ylab=ylabel, xaxt=xaxis,
       tck=0.02, mgp=c(1.5,0.1,0))
  if(length(phi0_MR>1))
  {
    invisible(lapply(2:length(phi0_MR), function(i)
    {
      tmp<-curve_dat[which(curve_dat$phi0_MR==phi0_MR[i]),]
      tmp<-tmp[order(tmp$gamma),]
      points(tmp$gamma, tmp$p_retained, type="l")
    })) 
  }
}

# TEST SURVIVAL VALUES FOR REGIONS OF GROWTH
inps<- inputs
inps$gamma<- 1
inps$p_retained<- 1
test<- sapply(phi0, function(x)
{
  inps$phi0_MR<- x
  ea<- matrix_eigen_analysis(inps)
  ea$lambda1>1
})

# FIND GROWTH-DECLINE CURVE FOR EACH SURVIVAL VALUE FOR WHICH GROWTH IS 
# POSSIBLE
bnd_inps<- boundary_product(inps)
bnd_inps$boundary$check==0
crvs<- spawning_survival_retention_curves2(bnd_inps, phi0MR = phi0)

# GENERATE PLOTS
#library(latticeExtra)
# z<- phi0[5]
# tmp<- vals[which(vals$phi0_MR==z),]
# contourplot(ENrec_100RR~gamma+pret, tmp, scales = list(tck = c(-1,-1)),
#             #panel=panel.axis("top", ticks=FALSE),
#             cuts=10,
#             xlab="Spawning Probability below Fort Peck given Reproductive-Readiness", 
#             ylab="Retention Probability",
#             main="Expected Age-1 Recruits per 100 Reproductively-Ready Females",
#             panel=panel.abline(tmp2$gamma, tmp2$p_retained, type="l", col="red", lwd=2))
# ltext(135, 340, paste0("phi0_MR=", z), pos=1, font=2)
# if(test[5])
# {
#   tmp2<- crvs[which(crvs$phi0_MR==z),]
#   xyplot(tmp2$p_retained~tmp2$gamma, type="l", col="red", lwd=2)
# }

indx<- 3
z<- phi0[indx]
tmp<- vals[which(vals$phi0_MR==z),]
x<- unique(tmp$gamma)
x<- x[order(x)]
y<- unique(tmp$pret)
y<- y[order(y)]
vals2<- sapply(y, function(j)
{
  out<- sapply(x, function(i)
  {
    tmp[which(tmp$gamma==i & tmp$pret==j),]$ENrec_100RR  
  })
  return(out)
})
contour(x, y, vals2, nlevels = 20, labels = NULL, 
        xlim=c(0,1), ylim=c(0,1), mgp=c(1.5,0.1,0), tck=0.02, 
        xlab="Spawning Probability below Fort Peck given Reproductive-Readiness",
        ylab="Retention Probability",
        main="Expected Age-1 Recruits per 100 Reproductively-Ready Females")
if(test[indx])
{
  tmp2<- crvs[which(crvs$phi0_MR==z),]
  points(tmp2$gamma, tmp2$p_retained, type="l", col="red", lwd=2)
}


vals3<- sapply(y, function(j)
{
  out<- sapply(x, function(i)
  {
    tmp[which(tmp$gamma==i & tmp$pret==j),]$ENrec_100  
  })
  return(out)
})
contour(x, y, vals3, nlevels = 10, labels = NULL, 
        xlim=c(0,1), ylim=c(0,1), 
        xlab="Spawning Probability below Fort Peck given Reproductive-Readiness",
        ylab="Retention Probability",
        main="Expected Age-1 Recruits per 100 Females")
if(test[indx])
{
  tmp2<- crvs[which(crvs$phi0_MR==z),]
  points(tmp2$gamma, tmp2$p_retained, type="l", col="red", lwd=2)
}



par(mfrow=c(3,3),
    mar = c(2, 2, 2, 2)+0.1,
    oma=c(2,2,3,0),
    tck=0.02, 
    mgp=c(1.5,0.1,0))
for(k in 2:10)
{
  z<- phi0[k]
  tmp<- vals[which(vals$phi0_MR==z),]
  x<- unique(tmp$gamma)
  x<- x[order(x)]
  y<- unique(tmp$pret)
  y<- y[order(y)]
  vals2<- sapply(y, function(j)
  {
    out<- sapply(x, function(i)
    {
      tmp[which(tmp$gamma==i & tmp$pret==j),]$ENrec_100RR  
    })
    return(out)
  })
  contour(x, y, vals2, nlevels = 15, labels = NULL, 
          xlim=c(0,1), ylim=c(0,1), 
          xlab="",
          ylab="")#,
          #main=paste0("Age-0 Survival Given Retention = ", z))
  if(test[k])
  {
    tmp2<- crvs[which(crvs$phi0_MR==z),]
    points(tmp2$gamma, tmp2$p_retained, type="l", col="red", lwd=2)
  }
  legend("bottomleft", paste0("phi0_MR = ", z),
         cex=1, bty="n", text.font=2)
}
mtext("Spawning Probability below Fort Peck given Reproductive-Readiness", 
      1, outer=TRUE, padj=0)
mtext("Retention Probability", 2, outer=TRUE, padj=0, las=0)
mtext("Expected Age-1 Recruits per 100 Reproductively-Ready Females",
      3, outer=TRUE, font=2)
