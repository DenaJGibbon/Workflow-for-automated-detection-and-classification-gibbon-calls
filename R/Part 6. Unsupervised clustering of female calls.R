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

set.seed(13)

# True and False Positives ------------------------------------------------

source('R/MFCCFunctionMeanSD.R')
set.seed(14)
subset.directory <-'/Users/denaclink/Desktop/RStudio Projects/gibbonID/DanumDetections/UpdatedDanumDetectionsHQ99/'
#subset.directory <- "/Users/denaclink/Desktop/RStudio Projects/gibbonID/TrueFalsePositives/"

#trainingdata <- MFCCFunctionMeanSD(input.dir=subset.directory , min.freq = 500, max.freq = 1200)
trainingdata <- gibbonR::MFCCFunction(input.dir=subset.directory , min.freq = 500, max.freq = 1500,
                                       n.windows=9, win.avg = 'standard')


#write.csv(trainingdata,'femalesdanumHQ.csv',row.names = F)

traningdatanames <- list.files(subset.directory,
                               full.names = F,pattern = '.wav')

trainingdata$class <- str_split_fixed(traningdatanames,pattern = '_',n=3)[,2]

trainingdata$Class <- str_split_fixed(traningdatanames,pattern = '_',n=3)[,2]

# HQDf <- read.csv('gibbon.validation.df.HQstructureV2.csv')
# # V2 includes two intro notes or less
# HQDf <- droplevels(subset(HQDf,target.signal=='y'))
# HQDf$short.names <- str_split_fixed(HQDf$full.name,pattern = '//',n=2)[,2]
#
#
# trainingdata <- trainingdata[traningdatanames %in% HQDf$short.names,]
# traningdatanames <- traningdatanames[traningdatanames %in% HQDf$short.names]
#
#
# trainingdata$Class <- as.factor(trainingdata$Class)

# MetaData <- read.csv("/Users/denaclink/Desktop/RStudio Projects/T0010_SEAsia2018_2019.csv")
#
# # Save as new object
# trainingdataUpdate <- data.frame()
#
# UniqueClass <- unique(trainingdata$Class)
#
# for(b in 1:length(UniqueClass)){
#   TempClass <-  UniqueClass[b]
#   TempMeta <- subset(MetaData,Deployment.Comments==TempClass)
#   trainingdatasub <- subset(trainingdata,Class==TempClass)
#   trainingdatasub$lat <- TempMeta$LAT..decimal.degrees.
#   trainingdatasub$lon <- TempMeta$LON..decimal.degrees.
#
#   trainingdataUpdate <- rbind.data.frame(trainingdataUpdate, trainingdatasub)
# }
#
#
# head(trainingdataUpdate)
#
# trainingdata <- trainingdataUpdate

AcousticSignalsMFCC.umap <-
  umap::umap(trainingdata[,-c(1,179)],
             #labels=as.factor(trainingdata$Class),
             controlscale=TRUE,scale=3)

plot.for.AcousticSignalsMFCC <-
  cbind.data.frame(AcousticSignalsMFCC.umap$layout[,1:2],
                   trainingdata$Class)

colnames(plot.for.AcousticSignalsMFCC) <-
  c("Dim.1", "Dim.2", "Class")

plot.for.AcousticSignalsMFCC$Class <- as.factor(plot.for.AcousticSignalsMFCC$Class)



Plot1 <- ggpubr::ggscatter(data = plot.for.AcousticSignalsMFCC,x = "Dim.1",
                  y = "Dim.2",
                  color  = "Class", shape='Class', alpha=0.4)+ggtitle('Validated detections')+
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()  #remove y axis ticks
  )+scale_color_manual(values= matlab::jet.colors(length(unique(plot.for.AcousticSignalsMFCC$Class))) )

Plot1



# Unsupervised clustering -------------------------------------------------

library(apcluster)
#trainingdata <- trainingdataFemales

# Adaptive returns q=0.1
q.val.seq <- seq(from=0.1,to=0.9,by=0.1)

AcousticSignal.sil.df <- data.frame()
for(a in 1:length(q.val.seq)){
  print(a)
  AcousticSignalsAP <-
    apcluster::apcluster(negDistMat(r=2),q=q.val.seq[a],
                         trainingdata[,-c(1,179)],
                         maxits=100000,convits=10000)


  sil <-
    cluster::silhouette(x = AcousticSignalsAP@idx,
                        dist = dist( trainingdata[,-c(1,179)]))

  sil.val <- (summary(sil)$avg.width)
  temp.sil.df <-  cbind.data.frame(sil.val,q.val.seq[a])
  AcousticSignal.sil.df <- rbind.data.frame(AcousticSignal.sil.df,temp.sil.df)
}


MaxSil <- which.max(AcousticSignal.sil.df$sil.val)
max(AcousticSignal.sil.df$sil.val)

AcousticSignalsAPFemales <-
  apcluster::apcluster(negDistMat(r=2),q=q.val.seq[MaxSil],
                       trainingdata[,-c(1,179)],
                       maxits=100000,convits=10000)

length(AcousticSignalsAPFemales@exemplars)
trainingdata[AcousticSignalsAPFemales@exemplars,]

NewDataFrame <- cbind.data.frame(trainingdata$Class,AcousticSignalsAPFemales@idx)
colnames(NewDataFrame) <- c('Recorder','Cluster')
NewDataFrame$Cluster <- as.factor(NewDataFrame$Cluster)

levels(NewDataFrame$Cluster) <- seq(1,length(AcousticSignalsAPFemales@exemplars),1)

#NewDataFrame <- subset(NewDataFrame,Recorder != 'S10')
gghistogram(data=NewDataFrame,x='Cluster',fill='Recorder',stat="count",
            facet.by = 'Recorder',ncol=3)+guides(fill="none")+ylab('Number of calls')

gghistogram(data=NewDataFrame,x='Recorder',fill='Cluster',stat="count",
            facet.by = 'Cluster',ncol=2)+guides(fill="none")+
  ylab('Number of calls')

traningdatanamesfull <- list.files(subset.directory,
                                   full.names = T,pattern = '.wav')

for(a in 1:length(AcousticSignalsAPFemales@exemplars)){
png(paste('exemplars/exemplar',a,'.png'),width = 1440, height = 960,res = 250)
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





source("/Users/denaclink/Desktop/RStudio Projects/gibbonR/R/gibbonID.R")
#set.seed(13)
gibbonID(input.dir='/Users/denaclink/Desktop/RStudio Projects/gibbonID/DanumDetections/UpdatedDanumDetectionsHQ99/',
         output.dir='/Users/denaclink/Desktop/RStudio Projects/gibbonID/DanumDetections/UpdatedDanumDetectionsHQ99/Thumbnails/',
         win.avg='standard',class='affinity.fixed', q.fixed=0.2,add.spectrograms=TRUE,min.freq=500,max.freq=1500,
         spec.ratio=30)

 source('R/UMAPBiplotAddSpectrograms.R')

 UMAPBiplotAddSpectrograms(input.dir.Focal='/Users/denaclink/Desktop/RStudio Projects/gibbonID/DanumDetections/UpdatedDanumDetectionsHQ99/',
                           output.dir.Focal = '/Users/denaclink/Desktop/RStudio Projects/gibbonID/DanumDetections/UpdatedDanumDetectionsHQ99/Thumbnails/',
                           min.freq = 500, max.freq = 1500,main='Female Gibbon Calls',class='fixed')

# source('R/AffinityBiplotAddSpectrograms.R')
# source('R/UMAPBiplotAddSpectrograms.R')
#
# set.seed(13)
# AffinityBiplotAddSpectrograms(input.dir.Focal='/Users/denaclink/Desktop/RStudio Projects/gibbonID/TrueFalsePositives',
#                               output.dir.Focal = '/Users/denaclink/Desktop/RStudio Projects/gibbonID/TrueFalsePositives/Thumbnails/',
#                               min.freq = 400, max.freq = 1500,main='gibbonR detections',class='fixed')
#
#
# UMAPBiplotAddSpectrograms(input.dir.Focal='/Users/denaclink/Desktop/RStudio Projects/gibbonID/TrueFalsePositives',
#                               output.dir.Focal = '/Users/denaclink/Desktop/RStudio Projects/gibbonID/TrueFalsePositives/ThumbnailsUMAP/',
#                               min.freq = 400, max.freq = 1500,main='gibbonR detections')
#
#
