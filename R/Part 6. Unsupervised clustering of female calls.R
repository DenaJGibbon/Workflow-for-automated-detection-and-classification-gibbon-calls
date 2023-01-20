library(plyr)
library(stringr)
library(ggpubr)
library(apcluster)
library(tuneR)
library(aricode)
library(clValid)
library(gibbonR)
library(dplyr)
library(tidyr)
library(ggpubr)

set.seed(14)

# True and False Positives ------------------------------------------------
subset.directory <-paste(input.dir,'UpdatedDanumDetectionsHQ99',sep='')

trainingdata <- gibbonR::MFCCFunction(input.dir=subset.directory , min.freq = 500, max.freq = 1500,
                                       n.windows=9, win.avg = 'standard')

traningdatanames <- list.files(subset.directory,
                               full.names = F,pattern = '.wav')

trainingdata$class <- str_split_fixed(traningdatanames,pattern = '_',n=3)[,2]

trainingdata$Class <- str_split_fixed(traningdatanames,pattern = '_',n=3)[,2]


# Unsupervised clustering -------------------------------------------------
# Adaptive returns q=0.2
# q.val.seq <- seq(from=0.1,to=0.9,by=0.1)
# 
# AcousticSignal.sil.df <- data.frame()
# for(a in 1:length(q.val.seq)){
#   print(a)
#   AcousticSignalsAP <-
#     apcluster::apcluster(negDistMat(r=2),q=q.val.seq[a],
#                          trainingdata[,-c(1,179)],
#                          maxits=100000,convits=10000)
# 
# 
#   sil <-
#     cluster::silhouette(x = AcousticSignalsAP@idx,
#                         dist = dist( trainingdata[,-c(1,179)]))
# 
#   sil.val <- (summary(sil)$avg.width)
#   temp.sil.df <-  cbind.data.frame(sil.val,q.val.seq[a])
#   AcousticSignal.sil.df <- rbind.data.frame(AcousticSignal.sil.df,temp.sil.df)
# }
# 
# 
# MaxSil <- which.max(AcousticSignal.sil.df$sil.val)
# max(AcousticSignal.sil.df$sil.val)

AcousticSignalsAPFemales <-
  apcluster::apcluster(negDistMat(r=2),q=0.2,#q.val.seq[MaxSil],
                       trainingdata[,-c(1,179)],
                       maxits=100000,convits=10000)

length(AcousticSignalsAPFemales@exemplars)
trainingdata[AcousticSignalsAPFemales@exemplars,]


for(a in 1:length(AcousticSignalsAPFemales@exemplars)){
png(paste(subset.directory,'Exemplars',a,'.png'),width = 1440, height = 960,res = 250)
#par(mfrow=c(5,2))
temp.name <- traningdatanamesfull[AcousticSignalsAPFemales@exemplars[a]]
temp.name.short <- traningdatanames[AcousticSignalsAPFemales@exemplars[a]]
temp.name.short.recorder <- str_split_fixed(temp.name.short,pattern = '_',n=3)[,2]
  short.wav <-readWave(temp.name)

  if(a==5 |a== 6 |a==7 |a== 8 ){
    dynamicrange =45
  } else{
    dynamicrange =60
  }
  phonTools::spectrogram(sound=short.wav@left,fs=short.wav@samp.rate,maxfreq = 1500,windowlength = 25,dynamicrange=dynamicrange,
                         maintitle = paste('Exemplar for Cluster',a,'from recorder',temp.name.short.recorder))
graphics.off()
}





source("R/gibbonID.R")
#set.seed(13)
gibbonID(input.dir=subset.directory,
         output.dir= paste(subset.directory,'/Thumbnails/', sep=''),
         win.avg='standard',class='affinity.fixed', q.fixed=0.2,add.spectrograms=TRUE,min.freq=500,max.freq=1500,
         spec.ratio=30)

 