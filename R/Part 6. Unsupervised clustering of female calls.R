library(plyr)
library(stringr)
library(ggpubr)
library(apcluster)
library(tuneR)
library(gibbonR)
library(dplyr)
library(tidyr)
library(ggpubr)
library(cluster)
library(phonTools)

# Set seed for reproducibility
set.seed(14)

# Gibbon Female Detections ------------------------------------------------
# NOTE: You must change this to the location where you have stored the downloaded data
input.dir <- '/Volumes/DJC Files/Clink et al Zenodo Data/'

# Create path to directory
subset.directory <-paste(input.dir,'UpdatedDanumDetectionsHQ99',sep='')

# Calculate MFCCs
trainingdata <- gibbonR::MFCCFunction(input.dir=subset.directory , min.freq = 500, max.freq = 1500,
                                      n.windows=9, win.avg = 'standard')

# Isolate names of .wav files
traningdatanames <- list.files(subset.directory,
                               full.names = F,pattern = '.wav')

# Add new column 'Class' with recorder information 
trainingdata$Class <- str_split_fixed(traningdatanames,pattern = '_',n=3)[,2]


# Unsupervised clustering -------------------------------------------------

# The commented code runs adaptive clustering; adaptive returns q=0.2
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

# Run apcluster with q=0.2 as obtained via adaptive clustering
AcousticSignalsAPFemales <-
  apcluster::apcluster(negDistMat(r=2),q=0.2,#q.val.seq[MaxSil],
                       trainingdata[,-c(1,179)],
                       maxits=100000,convits=10000)

# How many clusters are there?
length(AcousticSignalsAPFemales@exemplars)


# Save images of exemplar spectrograms to file ----------------------------
# Loop to save spectrogram images of the exemplars to a file
for(a in 1:length(AcousticSignalsAPFemales@exemplars)){
  png(paste(input.dir,'Exemplars/exemplar',a,'.png',sep=''),width = 1440, height = 960,res = 250)
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


# Create umap plot with spectrogram images --------------------------------

# Read in function
source("R/gibbonID.R")
gibbonID(input.dir=subset.directory,
         output.dir= paste(subset.directory,'/Thumbnails/', sep=''),
         win.avg='standard',class='affinity.fixed', q.fixed=0.2,add.spectrograms=TRUE,min.freq=500,max.freq=1500,
         spec.ratio=30)


# Create data frame of cluster assignment by recorder ---------------------

# Create new data frame that contains information about recorder and cluster assignment
NewDataFrame <- cbind.data.frame(trainingdata$Class,AcousticSignalsAPFemales@idx)

# New informative names
colnames(NewDataFrame) <- c('Recorder','Cluster')

# Convert cluster to factor
NewDataFrame$Cluster <- as.factor(NewDataFrame$Cluster)

# Revalue cluster levels starting at 1
levels(NewDataFrame$Cluster) <- seq(1,length(AcousticSignalsAPFemales@exemplars),1)

