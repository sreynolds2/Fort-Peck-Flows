
library(demogR)
library("scatterplot3d")
library(lattice)
library(reshape2)
drift<- readRDS("./dat/drift_data.rds")


#######################################
#                                     #
#     SURFACE PLOT OF LAMBDA = 1      #
#                                     #
#######################################
just_lambda<- function(maxage=NULL,
                       sexratio=NULL,
                       matage=NULL,
                       phi=NULL,
                       birthR=NULL,
                       phi0=NULL)
{
  vals<-sapply(phi0, function(x)
    {
      if(x==0){out<- 0}
      if(x!=0)
      {
        # BUILD LESLIE MATRIX
        A<-matrix(0,maxage,maxage)
        ## SURVIVAL RATES
        A[cbind(2:maxage,1:(maxage-1))]<- phi
        ## FECUNDITIES
        if(length(birthR)==1){birthV<- c(rep(0,matage-1), rep(birthR, maxage-matage+1))}
        if(length(birthR)==maxage){birthV<- birthR}
        A[1,] <- sexratio*birthV*x
        # EIGENANALYSIS 
        ea<- eigen.analysis(A)
        out<-ea$lambda1
      }
      return(out)
    })
  return(vals)
}

tmp<- data.frame(phi0_MR=seq(0.00003, 0.0001, 0.000005))
dat<-merge(drift, tmp, all=TRUE)
rm(tmp)
dat$phi0<- dat$phi0_MR*dat$p_retained
dat$lambda<- just_lambda(maxage = 60, 
                         sexratio = 0.32, 
                         matage = 9, 
                         phi = c(0.6369613, 0.7767894, rep(0.95, 57)), 
                         birthR = 10000,
                         phi0 = dat$phi0)
head(dat)

write.csv(dat, "./output/baseline_viability_data.csv",
          row.names = FALSE)



###################################
#                                 #
#         3D SCATTERPLOT          #
#                                 #
###################################
## USING BUILT-IN CONTOURLINES
phi0_MR<- seq(0.00003, 0.0001, 0.000005)
p_ret<- c(1.53886, 1.31902, 1.15414, 1.0259, 0.923313, 0.839376, 
          0.769428, 0.710241, 0.659509, 0.615542, 0.577071, 0.543125, 
          0.512952, 0.485954, 0.461657) #FROM findP IN MATHEMATICA
LS_RM<- 1535.89
tmp<- subset(drift, Lake_Sak_Upper_RM==LS_RM)
dat<- dcast(tmp, U_mps~temp_C, value.var="p_retained")
x<-dat[,1]
y<-sort(unique(tmp$temp_C))
z<-as.matrix(dat[,-1])
cp<- contourLines(x=x,y=y,z=z, levels=p_ret)
pts<- lapply(1:length(cp), function(i)
{
  indx<- which(p_ret==cp[[i]]$level)
  out<- data.frame(velocity=cp[[i]]$x, 
                   temperature=cp[[i]]$y,
                   phi0_MR=phi0_MR[indx])
  return(out)
})
pts<- do.call(rbind, pts)
###############################################################
# v <- ggplot(pts, aes(temperature, phi0_MR, z = velocity))
# v + geom_contour()
###############################################################
par(mfrow=c(1,1))
scatterplot3d(as.matrix(pts),pch = 16, color="steelblue", type="h")
scatterplot3d(z=pts$velocity, y=pts$temperature, x=pts$phi0_MR,
              pch = 16, color="steelblue", type="h",
              xlab="Age-0 Survival", ylab="Mean Temperature (C)",
              zlab="Mean Velocity (mps)")
scatterplot3d(z=pts$velocity, x=pts$temperature, y=pts$phi0_MR,
              pch = 16, color="steelblue", type="h",
              ylab="Age-0 Survival", xlab="Mean Temperature (C)",
              zlab="Mean Velocity (mps)")
pts<-pts[,c(2,1,3)]
scatterplot3d(as.matrix(pts),pch = 16, color="steelblue", type="h",   
              zlab="Age-0 Survival", xlab="Mean Temperature (C)",
              ylab="Mean Velocity (mps)")
pts<-pts[,c(1,3,2)]



wireframe(z=pts$velocity, x=pts$temperature, y=pts$phi0_MR,
          ylab="Age-0 Survival", xlab="Mean Temperature (C)",
          zlab="Mean Velocity (mps)")
pts2<-dcast(pts, temperature~phi0_MR, value.var="velocity")
pts3<-as.matrix(pts2[,-1])
row.names(pts3)<-pts2[,1]
wireframe(pts3, ylab="Age-0 Survival", xlab="Mean Temperature (C)",
          zlab="Mean Velocity (mps)", drape=TRUE)
pts2<-pts[which(pts$temperature %in% c(14,16,18,20,22,24)),]
pts2<-dcast(pts2, temperature~phi0_MR, value.var="velocity")
pts4<- as.matrix(pts2[,-1])
row.names(pts4)<-pts2[,1]
par(mfrow=c(1,1))
wireframe(pts4, ylab="phi0", xlab="degrees C",
          zlab="m/s", drape=TRUE)
any(pts$velocity<0.5)
pts2<-pts[which(pts$temperature %in% c(14,16,18,20,22,24)),]
###############################################################
#install.packages("rayshader")
library(rayshader)
library(ggplot2)
p<- ggplot(pts2)+
  geom_tile(aes(temperature,phi0_MR,fill=velocity))+
  geom_contour(aes(temperature,phi0_MR,z=velocity),binwidth = 0.01)
plot_gg(p, width = 5, height = 5, scale = 250, 
        theta = 10, phi = 30, windowsize = c(800, 800))
p<- ggplot(pts2)+
  geom_raster(aes(x=temperature, y=phi0_MR, fill = velocity)) +
  geom_contour(aes(temperature,phi0_MR,z=velocity),binwidth = 0.01, 
               colour = "white")
plot_gg(p, width = 5, height = 5, scale = 500, 
        theta = 10, phi = 30, windowsize = c(800, 800))
phivec <- 20 + 70 * 1/(1 + exp(seq(-5, 10, length.out = 180)))
phivecfull <- c(phivec, rev(phivec))
thetavec <- 90 * sin(seq(0,359,length.out = 360) * pi/180) #+270
zoomvec <- 0.5 + 0.5 * 1/(1 + exp(seq(-5, 10, length.out = 180)))
zoomvecfull <- c(zoomvec, rev(zoomvec))

for(i in 1:360) {
  render_camera(theta = thetavec[i],phi = phivecfull[i],zoom = zoomvecfull[i])
  render_snapshot(paste0("output/animation/frame", i, ".png"))
}

#Run this command in the command line using ffmpeg to stitch together a video:
#ffmpeg -framerate 60 -i frame%d.png -vcodec libx264 raymovie.mp4

#And run this command to convert the video to post to the web:
#ffmpeg -i raymovie.mp4 -pix_fmt yuv420p -profile:v baseline -level 3 -vf scale=-2:-2 rayweb.mp4
###############################################################
persp(seq(14, 24, 2), seq(0.00005, 0.0001, 0.000005), z=pts4,
      phi = 15, theta =-45,
      xlab = "degrees C", ylab = "phi0",
      zlab = "m/s")
par(mfrow=c(2,2),
    mar=c(1,0,1,0))
persp(seq(14, 24, 2), seq(0.00005, 0.0001, 0.000005), z=pts4,
      phi = 15, theta = 30,
      xlab = "degrees C", ylab = "phi0",
      zlab = "m/s")
persp(seq(14, 24, 2), seq(0.00005, 0.0001, 0.000005), z=pts4,
      phi = 15, theta = 60,
      xlab = "degrees C", ylab = "phi0",
      zlab = "m/s")
persp(seq(14, 24, 2), seq(0.00005, 0.0001, 0.000005), z=pts4,
      phi = 15, theta = 210,
      xlab = "degrees C", ylab = "phi0",
      zlab = "m/s")
persp(seq(14, 24, 2), seq(0.00005, 0.0001, 0.000005), z=pts4,
      phi = 15, theta = 230,
      xlab = "degrees C", ylab = "phi0",
      zlab = "m/s")


## EXPAND INTERPOLATION...PERHAPS USE TWO APPROXFUN...TRY THIS!
# findCurve<- function(p_ret=NULL,
#                      LS_RM=NULL,
#                      drift_data=NULL)
# {
#   dat<- drift_data[which(drift_data$Lake_Sak_Upper_RM==LS_RM),]
#   temps<- sort(unique(drift_data$temp_C))
#   vels<- sort(unique(drift_data$U_mps))
#   lapply(1:(length(temps)-1), function(y)
#   {
#     out<- lapply(1:(length(vels)-1), function(x)
#     {
#       p11<- dat[which(dat$temp_C==temps[y] & 
#                         dat$U_mps==vels[x]),]
#       p12<- dat[which(dat$temp_C==temps[y+1] & 
#                         dat$U_mps==vels[x]),]
#       p21<- dat[which(dat$temp_C==temps[y] & 
#                         dat$U_mps==vels[x+1]),]
#       p22<- dat[which(dat$temp_C==temps[y+1] & 
#                         dat$U_mps==vels[x+1]),]
#       pmax<-max(c(p11$p_retained, p12$p_retained,
#                   p21$p_retained, p22$p_retained))
#       pmin<-min(c(p11$p_retained, p12$p_retained,
#                   p21$p_retained, p22$p_retained))
#       outt<-NULL
#       if(pmin<=p_ret & pmax>=p_ret)
#       {
#         f<- function(p11,p12,p21,p22,v,t, p_ret)
#         {
#           ((p21$U_mps-v)*(p12$temp_C-t)*p11$p_retained + 
#             (p22$U_mps-v)*(t-p11$temp_C)*p12$p_retained +
#             (v-p11$U_mps)*(p22$temp_C-t)*p21$p_retained +
#             (v-p12$U_mps)*(t-p21$temp_C)*p22$p_retained)/
#             ((p21$U_mps-p11$U_mps)*(p12$temp_C-p11$temp_C)) -
#             p_ret
#         }
#         v<- seq(vels[x], vels[x+1], 0.01)
#         t<- sapply(1:length(v), function(i)
#           {
#             tmp2<- uniroot(f,c(0,50), v=v[i], 
#                            p11=p11, p12=p12, p21=p21, p22=p22, 
#                            p_ret=p_ret)$root
#         outt<- data.frame(p_retained=p_ret,
#                           Lake_Sak_Upper_RM=LS_RM)
#         
#       }
#     })
#   })
# }
# 
# phi0_MR<- seq(0.00003, 0.0001, 0.000005)
# slice1<- phi0_MR[10]
# p1<- 0.615542
# LS_RM<- 1535.89
# dat<- drift[which(drift$Lake_Sak_Upper_RM==LS_RM),]
# drift_data<- drift
# 
