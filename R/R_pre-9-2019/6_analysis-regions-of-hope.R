
source("./R/1_global.r")
source("./R/2_functions.r")
library(lattice)


# LARGE, GENERAL PICTURE
phi0<- c(seq(0.00001, 0.001, 0.00001), seq(0.002, 0.1, 0.001))
# eggs<- seq(2000, 50000, 2000)
# spnProb<- seq(0.05, 0.75, 0.05)
bR<- c(seq(50, 950, 50), seq(1000, 15000, 100), seq(16000, 50000, 1000))

tbl<- lapply(phi0, function(x)
{
  out<-lapply(bR, function(y)
  {
    tmp<- regions_of_hope(maxage = 60,
                          sexratio = 0.32,
                          phi = c(0.6369613, 0.7767894, rep(0.95, 57)),
                          phi0 = x,
                          birthR = y)
    return(tmp)
  })
  out<- do.call(rbind, out)
  return(out)
})
tbl2<- do.call(rbind, tbl)

## ORIGINAL SAVE
saveRDS(tbl2, "./output/GeneralHopeTable.rds")
tbl<-readRDS("./output/GeneralHopeTable.rds")
# ## ADD NEW VALUES AND SAVE
# tbl<- rbind(tbl, tbl2)
# tbl<- tbl[!duplicated(tbl),]
# saveRDS(tbl, "./output/GeneralHopeTable.rds")

tmp<- subset(tbl, input_bR_id<=150 & phi0_id<=1)
dat<- dcast(tmp, birth_rate~age0_survival, value.var="lambda")
x<-dat[,1]
y<-sort(unique(tmp$age0_survival))
z<-as.matrix(dat[,-1])
contour(x=x,y=y,z=z,
        xlab="",
        ylab="",
        main="",cex.main=1.5,cex.lab=1.3,
        tck=0.03, mgp=c(2,0.5,0))
dat2<- readRDS("./output/p_retained_boundaries.rds")
col.l <- colorRampPalette(c('black', 'red', 'white', 'blue', 'black'))(200)
levelplot(lambda~birth_rate+age0_survival,tmp, col.regions=col.l,
          at=seq(0.75, 1.25, 0.01),  
          panel=function(...){panel.levelplot(...); 
            panel.axis("bottom",  half = FALSE,labels=F) 
            panel.axis("left",  half = FALSE,labels=F) 
            grid.lines(dat2[dat2$p_retained==1,]$birth_rate, 
                        dat2[dat2$p_retained==1,]$phi0_MR, 
                        default.units="native", 
                       gp=gpar(col="gray", lwd=2))
          }, scales = list(tck = c(-1,-1)),
          xlab="Fertilized Eggs per Mature Female per Year", 
          ylab="Age-0 Survival")


tmp2<- subset(tbl, input_bR_id<=150 & phi0_id<=0.1)
levelplot(lambda~birth_rate+age0_survival,tmp2, col.regions=col.l,
          at=seq(0.8, 1.2, 0.01),     
          panel=function(...){panel.levelplot(...); 
            panel.axis("bottom",  half = FALSE,labels=F) 
            panel.axis("left",  half = FALSE,labels=F)
            grid.lines(dat2[dat2$p_retained==1,]$birth_rate, 
                       dat2[dat2$p_retained==1,]$phi0_MR, 
                       default.units="native",
                       gp=gpar(col="gray", lwd=2))
          }, scales = list(tck = c(-1,-1)),
          xlab="Fertilized Eggs per Mature Female per Year", 
          ylab="Age-0 Survival")


# ## REFINE LOW SURVIVAL FIGURE -- NOT REALLY A REFINEMENT
# phi0<- seq(0.00001, 0.0001, 0.00001)
# bR<- seq(50, 15000, 50)
# 
# tbl<- lapply(phi0, function(x)
# {
#   out<-lapply(bR, function(y)
#   {
#     tmp<- regions_of_hope(maxage = 60,
#                           sexratio = 0.32,
#                           phi = c(0.6369613, 0.7767894, rep(0.95, 57)),
#                           phi0 = x,
#                           birthR = y)
#     return(tmp)
#   })
#   out<- do.call(rbind, out)
#   return(out)
# })
# tbl2<- do.call(rbind, tbl)
# 
# ## ORIGINAL SAVE
# saveRDS(tbl2, "./output/LowSurvivalHopeTable.rds")
# tbl<-readRDS("./output/LowSurvivalHopeTable.rds")
# # ## ADD NEW VALUES AND SAVE
# # tbl<- rbind(tbl, tbl2)
# # tbl<- tbl[!duplicated(tbl),]
# # saveRDS(tbl, "./output/LowSurvivalHopeTable.rds")
# 
# levelplot(lambda~birth_rate+age0_survival,tbl, col.regions=col.l,
#           at=seq(0.75, 1.25, 0.01), pretty=TRUE)


# tmp1$phi0_id<- round(tmp1$phi0_id, 2)
# tmp1$input_bR_id<- round(tmp1$input_bR_id)
# tmp1<- subset(tbl, input_bR_id<=50 & phi0_id<=1)
# my.at <- seq(0.75, 1.25, 0.01)
# levelplot(lambda~birth_rate+age0_survival,tmp1, at=my.at, col.regions=col.l)
# 
# tmp2<- subset(tbl, input_bR_id>=50 & input_bR_id<=150 & phi0_id<=0.1)
# levelplot(lambda~birth_rate+age0_survival,tmp2, col.regions=col.l)
# 
# dat1<- dcast(tmp1, birth_rate~age0_survival, value.var="lambda")
# x1<-dat1[,1]
# y1<-sort(unique(tmp1$age0_survival))
# z1<-as.matrix(dat1[,-1])
# contour(x=x1,y=y1,z=z1,
#         xlab="",
#         ylab="",
#         main="",cex.main=1.5,cex.lab=1.3,
#         tck=0.03, mgp=c(2,0.5,0))
# 
# dat2<- dcast(tmp2, birth_rate~age0_survival, value.var="lambda")
# 
# x2<-dat2[,1]
# y2<-sort(unique(tmp2$age0_survival))
# z2<-as.matrix(dat2[,-1])
# 
# 
# 
# 
# 
# 
# # pdf(file="Hope.pdf")
# par(mfrow=c(2,1),
#     oma = c(1,2,0,0) + 0.1,
#     mar = c(2,4,1,2) + 0.1,
#     las=1)
# contour(x=x1,y=y1,z=z1,
#         xlab="",
#         ylab="",
#         main="",cex.main=1.5,cex.lab=1.3,
#         tck=0.03, mgp=c(2,0.5,0))
# contour(x=x2,y=y2,z=z2,
#         xlab="",
#         ylab="",
#         main="",cex.main=1.5,cex.lab=1.3,
#         tck=0.03, mgp=c(2,0.5,0))
# mtext("        Birth Rate", 1, outer=TRUE, font=2)
# mtext("Age-0 survival", 2, las=0, padj=1, outer=TRUE, font=2)
# # dev.off()  
# 
# dat2<-readRDS("./output/Regions_of_Hope/phi0_3_birthR_3.rds")




## VARY THE FECUNDITIES BY AGE AND SEE HOW FIGURE CHANGES
phi0<- c(seq(0.00001, 0.001, 0.00001), seq(0.002, 0.1, 0.001))
# eggs<- seq(2000, 50000, 2000)
# spnProb<- seq(0.05, 0.75, 0.05)

bR<- lapply(c(seq(50, 950, 50), seq(1000, 15000, 100), seq(16000, 50000, 1000)),
            function(x)
            {
              out<- c(rep(0,8), seq(1/10*x, 2.25*x, length.out=60-8)) 
            })

tbl<- lapply(phi0, function(x)
{
  out<-lapply(1:length(bR), function(y) 
  {
    tmp<- regions_of_hope(maxage = 60,
                          sexratio = 0.32,
                          phi = c(0.6369613, 0.7767894, rep(0.95, 57)),
                          phi0 = x,
                          birthR = bR[[y]])
    return(tmp)
  })
  out<- do.call(rbind, out)
  return(out)
})
tbl2<- do.call(rbind, tbl)

## ORIGINAL SAVE
saveRDS(tbl2, "./output/GeneralHopeTable_BirthVariation.rds")
tbl<-readRDS("./output/GeneralHopeTable_BirthVariation.rds")
# ## ADD NEW VALUES AND SAVE
# tbl<- rbind(tbl, tbl2)
# tbl<- tbl[!duplicated(tbl),]
# saveRDS(tbl, "./output/GeneralHopeTable_BirthVariation.rds")

tmp<- subset(tbl, input_bR_id<=150 & phi0_id<=1)
dat<- dcast(tmp, birth_rate~age0_survival, value.var="lambda")
x<-dat[,1]
y<-sort(unique(tmp$age0_survival))
z<-as.matrix(dat[,-1])
contour(x=x,y=y,z=z,
        xlab="",
        ylab="",
        main="",cex.main=1.5,cex.lab=1.3,
        tck=0.03, mgp=c(2,0.5,0))
col.l <- colorRampPalette(c('black', 'red', 'white', 'blue', 'black'))(200)
levelplot(lambda~birth_rate+age0_survival,tmp, col.regions=col.l,
          at=seq(0.75, 1.25, 0.01),  
          panel=function(...){panel.levelplot(...); 
            panel.axis("bottom",  half = FALSE,labels=F) 
            panel.axis("left",  half = FALSE,labels=F) 
          }, scales = list(tck = c(-1,-1)),
          xlab="Fertilized Eggs per Mature Female per Year", 
          ylab="Age-0 Survival")

tmp2<- subset(tbl, input_bR_id<=150 & phi0_id<=0.1)
levelplot(lambda~birth_rate+age0_survival,tmp2, col.regions=col.l,
          at=seq(0.8, 1.2, 0.01),     
          panel=function(...){panel.levelplot(...); 
            panel.axis("bottom",  half = FALSE,labels=F) 
            panel.axis("left",  half = FALSE,labels=F) 
          }, scales = list(tck = c(-1,-1)),
          xlab="Fertilized Eggs per Mature Female per Year", 
          ylab="Age-0 Survival", pretty=TRUE)


## BIN UP AND REPLOT
tmp$bR_bin<-ifelse(tmp$birth_rate<=1000, floor(tmp$birth_rate/50)*50,
                   ifelse(tmp$birth_rate<=15000, floor(tmp$birth_rate/100)*100,
                          floor(tmp$birth_rate/1000)*1000))
dat<- dcast(tmp, bR_bin~age0_survival, mean, value.var="lambda")
x<-dat[,1]
y<-sort(unique(tmp$age0_survival))
z<-as.matrix(dat[,-1])
contour(x=x,y=y,z=z,
        xlab="",
        ylab="",
        main="",cex.main=1.5,cex.lab=1.3,
        tck=0.03, mgp=c(2,0.5,0))
col.l <- colorRampPalette(c('black', 'red', 'white', 'blue', 'black'))(200)
levelplot(lambda~bR_bin+age0_survival,tmp, col.regions=col.l,
          at=seq(0.75, 1.25, 0.01),  
          panel=function(...){panel.levelplot(...); 
            panel.axis("bottom",  half = FALSE,labels=F) 
            panel.axis("left",  half = FALSE,labels=F) 
          }, scales = list(tck = c(-1,-1)),
          xlab="Fertilized Eggs per Mature Female per Year", 
          ylab="Age-0 Survival")

tmp2$bR_bin<-ifelse(tmp2$birth_rate<=1000, floor(tmp2$birth_rate/50)*50,
                   ifelse(tmp2$birth_rate<=15000, floor(tmp2$birth_rate/100)*100,
                          floor(tmp2$birth_rate/1000)*1000))
levelplot(lambda~bR_bin+age0_survival,tmp2, col.regions=col.l,
          at=seq(0.8, 1.2, 0.01),     
          panel=function(...){panel.levelplot(...); 
            panel.axis("bottom",  half = FALSE,labels=F) 
            panel.axis("left",  half = FALSE,labels=F) 
          }, scales = list(tck = c(-1,-1)),
          xlab="Fertilized Eggs per Mature Female per Year", 
          ylab="Age-0 Survival", pretty=TRUE)
