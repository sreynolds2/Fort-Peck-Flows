
library(reshape2)
library(plyr)


# GENERATE THE TEMPERATURE TRANSITION MATRIX
## PULL TEMP DATA
temps<- read.csv("./dat/Observed_Annual_Temperature_Class.csv")
temps$freq<- 1
temp_sum<- ddply(temps, .(Weather), summarize, Frequency=sum(freq))
temp_sum<- temp_sum[c(2,3,1),]
## SAVE THE SUMMARY OF TEMPERATURE FREQUENCIES
write.csv(temp_sum, "./output/_stochastic/Temperature_Frequency.csv",
          row.names = FALSE)

## GENERATE YEAR TO YEAR TEMP TRANSITION DATA
trans<- lapply(1:(nrow(temps)-1), function(i)
{
  data.frame(Temp_1=temps$Weather[i], Temp_2=temps$Weather[i+1])
})
trans<- do.call(rbind, trans)
trans$Transitions<- paste0(trans$Temp_1, "-", trans$Temp_2)
trans$freq<- 1
### SUMMARIZE TRANSITIONS
trans_sum<- ddply(trans, .(Transitions), summarize, Frequency=sum(freq))
### ADD FREQUENCY OF STARTING YEARS
Temp_Freq<- aggregate(freq~Temp_1, trans, sum)
names(Temp_Freq)[2]<- "Temp_Freq"
trans<- merge(trans, Temp_Freq, all.x=TRUE) 
rm(Temp_Freq)
### CREATE THE TRANSITION MATRIX FROM THE TRANSITION PROBABILITIES
trans$Trans_Prob<- trans$freq/trans$Temp_Freq 
trans<- dcast(trans, Temp_1~Temp_2, sum, value.var="Trans_Prob")
trans<- trans[c(2,3,1),]
trans<- trans[,c("Low", "Median", "High")]
trans<- as.matrix(trans)
rownames(trans)<- c("Low", "Median", "High")
trans<- t(trans)
trans

# SAVE THE TRANSITION MATRIX
write.csv(trans, "./output/_stochastic/Temperature_Transition_Matrix.csv")

# SIMULATE 50 YEARS OF TEMPERATURE AND YEAR DATA
yrs<- 50
## TEMPERATURE DATA
W<- sample(c("Low", "Median", "High"), 1, 
           prob=temp_sum$Frequency/sum(temp_sum$Frequency))
for(i in 2:yrs)
{
  W[i]<- sample(c("Low", "Median", "High"), 1, 
                prob=trans[,W[i-1]])
}
## YEAR DATA
Y<- sapply(1:yrs, function(i)
{
  sample(temps[which(temps$Weather==W[i]),]$Year,1)
})
## SPAWNING/RETENTION DATA
### ADD-IN BINARY SPAWNING/RETENTION (COULD OCCURRED OR NOT) BY YEAR
### WHAT IS BEST WAY FOR CRAIG TO LOOK AT THIS??? ACTUAL RETENTION BY ALT.?
sp_yrs<- c(1930, 1949, 1953, 1966, 1975, 1976, 1980, 1983, 1985, 1986, 
           1987, 1994, 1997, 2000, 2011, 2012)
temps$Spawn_Year<- ifelse(temps$Year %in% sp_yrs, 1, 0) 
S<- sapply(1:yrs, function(i)
{
  temps[which(temps$Year==Y[i]),]$Spawn_Year
})

## SAVE AS TABLE
tbl<- data.frame(Temperature=W, Year=Y, Spawn_Year=S)
write.csv(tbl, "./output/_stochastic/paths/test_path_2.csv", 
          row.names = FALSE)




### SPLIT DATA TO COMPARE TRANSITION PROBABILITES FROM DIFFERENT TIME PERIODS
trans_comp<- function(temp_dat=NULL,
                      split_yr=NULL) #year of data split
{
  ## ERROR HANDLING
  if(split_yr<1931 | split_yr>2019)
  {
    return(print("Input for split_yr must be an integer between 1931 and
                 2019."))
  }
  temp_dat$freq<- 1
  ## ANALYZE FIRST DATA SET
  dat1<- subset(temp_dat, temp_dat$Year %in% 1930:split_yr)
  dat1_sum<- ddply(dat1, .(Weather), summarize, Counts=sum(freq))
  dat1_sum<- dat1_sum[c(2,3,1),]
  dat1_sum$Frequency<- dat1_sum$Counts/sum(dat1_sum$Counts)
  names(dat1_sum)[2:3]<- paste0(names(dat1_sum)[2:3], "_Pre_", split_yr)
  ### GENERATE YEAR TO YEAR TEMP TRANSITION DATA
  trans1<- lapply(1:(nrow(dat1)-1), function(i)
  {
    data.frame(Temp_1=dat1$Weather[i], Temp_2=dat1$Weather[i+1])
  })
  trans1<- do.call(rbind, trans1)
  trans1$Transitions<- paste0(trans1$Temp_1, "-", trans1$Temp_2)
  trans1$freq<- 1
  #### SUMMARIZE TRANSITIONS
  trans1_sum<- ddply(trans1, .(Transitions), summarize, Counts=sum(freq))
  trans1_sum$Frequency<- trans1_sum$Counts/sum(trans1_sum$Counts)
  names(trans1_sum)[2:3]<- paste0(names(trans1_sum)[2:3], "_Pre_", split_yr)
  #### ADD FREQUENCY OF STARTING YEARS
  Temp_Freq<- aggregate(freq~Temp_1, trans1, sum)
  names(Temp_Freq)[2]<- "Temp_Freq"
  trans1<- merge(trans1, Temp_Freq, all.x=TRUE) 
  rm(Temp_Freq)
  ### CREATE THE TRANSITION MATRIX FROM THE TRANSITION PROBABILITIES
  trans1$Trans_Prob<- trans1$freq/trans1$Temp_Freq 
  trans1<- dcast(trans1, Temp_1~Temp_2, sum, value.var="Trans_Prob")
  trans1<- trans1[c(2,3,1),]
  trans1<- trans1[,c("Low", "Median", "High")]
  trans1<- as.matrix(trans1)
  rownames(trans1)<- c("Low", "Median", "High")
  trans1<- t(trans1)
  ## ANALYZE SECOND DATA SET
  dat2<- subset(temp_dat, temp_dat$Year %in% split_yr:2020)
  dat2_sum<- ddply(dat2, .(Weather), summarize, Counts=sum(freq))
  dat2_sum<- dat2_sum[c(2,3,1),]
  dat2_sum$Frequency<- dat2_sum$Counts/sum(dat2_sum$Counts)
  names(dat2_sum)[2:3]<- paste0(names(dat2_sum)[2:3], "_Post_", split_yr)
  ### GENERATE YEAR TO YEAR TEMP TRANSITION DATA
  trans2<- lapply(1:(nrow(dat2)-1), function(i)
  {
    data.frame(Temp_1=dat2$Weather[i], Temp_2=dat2$Weather[i+1])
  })
  trans2<- do.call(rbind, trans2)
  trans2$Transitions<- paste0(trans2$Temp_1, "-", trans2$Temp_2)
  trans2$freq<- 1
  ### SUMMARIZE TRANSITIONS
  trans2_sum<- ddply(trans2, .(Transitions), summarize, Counts=sum(freq))
  trans2_sum$Frequency<- trans2_sum$Counts/sum(trans2_sum$Counts)
  names(trans2_sum)[2:3]<- paste0(names(trans2_sum)[2:3], "_Post_", split_yr)
  ### ADD FREQUENCY OF STARTING YEARS
  Temp_Freq<- aggregate(freq~Temp_1, trans2, sum)
  names(Temp_Freq)[2]<- "Temp_Freq"
  trans2<- merge(trans2, Temp_Freq, all.x=TRUE) 
  rm(Temp_Freq)
  ### CREATE THE TRANSITION MATRIX FROM THE TRANSITION PROBABILITIES
  trans2$Trans_Prob<- trans2$freq/trans2$Temp_Freq 
  trans2<- dcast(trans2, Temp_1~Temp_2, sum, value.var="Trans_Prob")
  trans2<- trans2[c(2,3,1),]
  trans2<- trans2[,c("Low", "Median", "High")]
  trans2<- as.matrix(trans2)
  rownames(trans2)<- c("Low", "Median", "High")
  trans2<- t(trans2)
  ## MERGE TRANSITION SUMMARY TABLES
  temp_sum<- merge(dat1_sum, dat2_sum, by="Weather", all=TRUE)
  temp_sum[is.na(temp_sum)]<- 0
  temp_sum<- temp_sum[c(2,3,1),]
  temp_sum<- temp_sum[,c(1:2,4,3,5)]
  trans_sum<- merge(trans1_sum, trans2_sum, by="Transitions", all=TRUE)
  trans_sum[is.na(trans_sum)]<- 0
  trans_sum<- trans_sum[c(5,6,4,8,9,7,2,3,1),]
  trans_sum<- trans_sum[,c(1:2,4,3,5)]
  ## RETURN ALL COMPONENTS
  out<- list(Matrix1=trans1, Matrix2=trans2, Temp_Summary=temp_sum,
             Transition_Summary=trans_sum)
  names(out)[1]<- paste0("Pre_", split_yr)
  names(out)[2]<- paste0("Post_", split_yr)
  return(out)
}

test<- trans_comp(temps, 1975)
test
par(mfrow=c(2,1))
barplot(test$Temp_Summary$Frequency_Pre_1975, 
        names.arg = c("Low", "Median", "High"), ylim=c(0,0.5))
barplot(test$Temp_Summary$Frequency_Post_1975, 
        names.arg = c("Low", "Median", "High"), ylim=c(0,0.5))

barplot(test$Transition_Summary$Frequency_Pre_1975,
        names.arg = c("L-L", "L-M", "L-H", "M-L", "M-M", "M-H", "H-L", 
                      "H-M", "H-H"), ylim=c(0,0.25))
barplot(test$Transition_Summary$Frequency_Post_1975,
        names.arg = c("L-L", "L-M", "L-H", "M-L", "M-M", "M-H", "H-L", 
                      "H-M", "H-H"), ylim=c(0,0.25))

heatmap(test$Pre_1975, Colv=NA, Rowv=NA, scale='none')
heatmap(test$Post_1975, Colv=NA, Rowv=NA, scale='none')


test2<- trans_comp(temps, 1990)
test2
par(mfrow=c(2,1))
barplot(test2$Temp_Summary$Frequency_Pre_1990, 
        names.arg = c("Low", "Median", "High"), ylim=c(0,0.6))
barplot(test2$Temp_Summary$Frequency_Post_1990, 
        names.arg = c("Low", "Median", "High"), ylim=c(0,0.6))

barplot(test2$Transition_Summary$Frequency_Pre_1990,
        names.arg = c("L-L", "L-M", "L-H", "M-L", "M-M", "M-H", "H-L", 
                      "H-M", "H-H"), ylim=c(0,0.35))
barplot(test2$Transition_Summary$Frequency_Post_1990,
        names.arg = c("L-L", "L-M", "L-H", "M-L", "M-M", "M-H", "H-L", 
                      "H-M", "H-H"), ylim=c(0,0.35))

heatmap(test2$Pre_1990, Colv=NA, Rowv=NA, scale='none')
heatmap(test2$Post_1990, Colv=NA, Rowv=NA, scale='none')
