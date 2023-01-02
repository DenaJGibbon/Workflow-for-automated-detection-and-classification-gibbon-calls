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
subset.directory <- "/Users/denaclink/Desktop/RStudio Projects/gibbonID/DanumDetections/TrueFalsePositives/"

trainingdata <- MFCCFunctionMeanSD(input.dir=subset.directory , min.freq = 400, max.freq = 1600)

traningdatanames <- list.files(subset.directory,
                               full.names = F,pattern = '.wav')

trainingdata$Class <- str_split_fixed(traningdatanames,pattern = '_',n=2)[,1]


trainingdata$Class <- as.factor(trainingdata$Class)


AcousticSignalsMFCC.umap <-
  umap::umap(trainingdata[,-c(1,51)],
             #labels=as.factor(trainingdata$Class),
             controlscale=TRUE,scale=3)

plot.for.AcousticSignalsMFCC <-
  cbind.data.frame(AcousticSignalsMFCC.umap$layout[,1:2],
                   trainingdata$Class)

colnames(plot.for.AcousticSignalsMFCC) <-
  c("Dim.1", "Dim.2", "Class")

plot.for.AcousticSignalsMFCC$Class <- as.factor(plot.for.AcousticSignalsMFCC$Class)

plot.for.AcousticSignalsMFCC$Class <- revalue(plot.for.AcousticSignalsMFCC$Class,
                                              c(o = 'y',oo='y'))

plot.for.AcousticSignalsMFCC$Class <- revalue(plot.for.AcousticSignalsMFCC$Class,
                                              c(y = 'T', n = 'F'))

plot.for.AcousticSignalsMFCC <-
  subset(plot.for.AcousticSignalsMFCC,Class=='T' | Class=='F')

plot.for.AcousticSignalsMFCC <- droplevels(plot.for.AcousticSignalsMFCC)

Plot1 <- ggpubr::ggscatter(data = plot.for.AcousticSignalsMFCC,x = "Dim.1",
                  y = "Dim.2",
                  color  = "Class", shape='Class', alpha=0.1)+ggtitle('Validated detections')+
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()  #remove y axis ticks
  )+scale_color_manual(values=c('orange','blue') )

Plot1



# Supervised clustering -------------------------------------------------
trainingdata$Class <- revalue(trainingdata$Class,
                                              c(o = 'y',oo='y'))

ml.model.svm <- e1071::svm(trainingdata[,-c(1,51)], trainingdata$Class, kernel = "radial", #gamma = tune.rad$best.parameters$gamma, cost = tune.rad$best.parameters$cost,
                           cross = 25,
                           probability = TRUE)

print(paste('SVM accuracy',ml.model.svm$tot.accuracy))

# Unsupervised clustering -------------------------------------------------


AcousticSignalsAP <-
  apcluster::apcluster(negDistMat(r=2),q= 0.1,
                       trainingdata[,-c(1,51)],
                       maxits=100000,convits=10000)


aricode::NMI(as.factor(AcousticSignalsAP@idx),trainingdata$class)

AcousticSignals.umap <-
  umap::umap(trainingdata[,-c(1,51)],
             #labels=as.factor( as.numeric(AcousticSignalsAP@idx)),
             controlscale=TRUE,scale=3)

plot.for.AcousticSignals <-
  cbind.data.frame(AcousticSignals.umap$layout[,1:2],#VGGishDF$PercentClass,
                   as.factor( as.numeric(AcousticSignalsAP@idx)))

colnames(plot.for.AcousticSignals) <-
  c("Dim.1", "Dim.2","Cluster")

plot.for.AcousticSignals$Cluster <- as.factor(plot.for.AcousticSignals$Cluster)
plot.for.AcousticSignals$Class <- trainingdata$Class

Plot2 <- ggpubr::ggscatter(data = plot.for.AcousticSignals,x = "Dim.1",
                  y = "Dim.2",
                  color='Cluster', alpha=0.4) + guides(color='none')+ggtitle('Affinity propagation')+
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()  #remove y axis ticks
  )+scale_color_manual(values= matlab::jet.colors (  34) )



cowplot::plot_grid(Plot1,Plot2)

table(plot.for.AcousticSignals$Validation,plot.for.AcousticSignals$Cluster)


# Female individuals ------------------------------------------------------
source('R/MFCCFunction.R')
subset.directory <- '/Users/denaclink/Desktop/RStudio Projects/gibbonID/data/FemaleGibbonsSwiftHQ/'
subset.directory <- '/Users/denaclink/Desktop/RStudio Projects/gibbonID/TrueFalsePositives'

trainingdataFemales <- gibbonR::MFCCFunction(input.dir=subset.directory , min.freq = 600, max.freq = 1400,win.avg = 'standard')

trainingdataFemalesnames <- list.files(subset.directory,
                               full.names = F,pattern = '.wav')

trainingdataFemales$Class <- str_split_fixed(trainingdataFemalesnames,pattern = '_',n=2)[,1]

which(trainingdataFemales$Class=='Y')

trainingdataFemales <-
  subset(trainingdataFemales,Class=='Y')

trainingdataFemales$Class <- str_split_fixed(trainingdataFemalesnames,pattern = '_',n=3)[,2][which(trainingdataFemales$Class=='Y')]

trainingdataFemales$Class <- as.factor(trainingdataFemales$Class)

MetaData <- read.csv("/Users/denaclink/Desktop/RStudio Projects/T0010_SEAsia2018_2019.csv")

# Save as new object
trainingdataFemalesUpdate <- data.frame()

UniqueClass <- unique(trainingdataFemales$Class)

for(b in 1:length(UniqueClass)){
 TempClass <-  UniqueClass[b]
 TempMeta <- subset(MetaData,Deployment.Comments==TempClass)
 trainingdataFemalessub <- subset(trainingdataFemales,Class==TempClass)
 trainingdataFemalessub$lat <- TempMeta$LAT..decimal.degrees.
 trainingdataFemalessub$lon <- TempMeta$LON..decimal.degrees.

 trainingdataFemalesUpdate <- rbind.data.frame(trainingdataFemalesUpdate, trainingdataFemalessub)
}


head(trainingdataFemalesUpdate)

AcousticSignalsMFCC.umap.F <-
  umap::umap(trainingdataFemalesUpdate[,-c(1,179)],
             #labels=as.factor(trainingdataFemales$Class),
             controlscale=TRUE,scale=3)

plot.for.AcousticSignalsMFCC.F <-
  cbind.data.frame(AcousticSignalsMFCC.umap.F$layout[,1:2],
                   trainingdataFemalesUpdate$Class)

colnames(plot.for.AcousticSignalsMFCC.F) <-
  c("Dim.1", "Dim.2", "Class")

plot.for.AcousticSignalsMFCC.F$Class <- as.factor(plot.for.AcousticSignalsMFCC.F$Class)


Plot1Females <- ggpubr::ggscatter(data = plot.for.AcousticSignalsMFCC.F,x = "Dim.1",
                           y = "Dim.2",
                           color  = "Class", alpha=0.4)+ggtitle('Recording units')+
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()  #remove y axis ticks
  )

Plot1Females



# Unsupervised clustering -------------------------------------------------
library(apcluster)
#trainingdataFemalesUpdate <- trainingdataFemales

# Adaptive returns q=0.1
q.val.seq <- seq(from=0.1,to=0.9,by=0.1)

AcousticSignal.sil.df <- data.frame()
for(a in 1:length(q.val.seq)){
  print(a)
  AcousticSignalsAP <-
    apcluster::apcluster(negDistMat(r=2),q=q.val.seq[a],
                         trainingdataFemalesUpdate[,-c(1,179)],
                         maxits=100000,convits=10000)


  sil <-
    cluster::silhouette(x = AcousticSignalsAP@idx,
                        dist = dist( trainingdataFemalesUpdate[,-c(1,179)]))

  sil.val <- (summary(sil)$avg.width)
  temp.sil.df <-  cbind.data.frame(sil.val,q.val.seq[a])
  AcousticSignal.sil.df <- rbind.data.frame(AcousticSignal.sil.df,temp.sil.df)
}


MaxSil <- which.max(AcousticSignal.sil.df$sil.val)
max(AcousticSignal.sil.df$sil.val)

AcousticSignalsAPFemales <-
  apcluster::apcluster(negDistMat(r=2),q=q.val.seq[MaxSil],
                       trainingdataFemalesUpdate[,-c(1,179)],
                       maxits=100000,convits=10000)

length(AcousticSignalsAPFemales@exemplars)

NewDataFrame <- cbind.data.frame(trainingdataFemalesUpdate$Class,AcousticSignalsAPFemales@idx)
colnames(NewDataFrame) <- c('Recorder','Cluster')
NewDataFrame$Cluster <- as.factor(NewDataFrame$Cluster)

gghistogram(data=NewDataFrame,x='Cluster',fill='Recorder',stat="count",
            facet.by = 'Recorder')+guides(fill="none")




AcousticSignals.umap.F <-
  umap::umap(trainingdataFemalesUpdate[,-c(1,179)],
             #labels=as.factor( as.numeric(AcousticSignalsAPFemales@idx)),
             controlscale=TRUE,scale=3)

length(AcousticSignalsAPFemales@exemplars)

plot.for.AcousticSignals.F <-
  cbind.data.frame(AcousticSignals.umap.F$layout[,1:2],
                   as.factor( as.numeric(AcousticSignalsAPFemales@idx)),
                   trainingdataFemalesUpdate$Class)

colnames(plot.for.AcousticSignals.F) <-
  c("Dim.1", "Dim.2","Cluster","Recorder")

plot.for.AcousticSignals.F$Cluster <- as.factor(plot.for.AcousticSignals.F$Cluster)
plot.for.AcousticSignals.F$Class <- trainingdataFemales$Class

Plot2Females <- ggpubr::ggscatter(data = plot.for.AcousticSignals.F,x = "Dim.1",
                           y = "Dim.2",
                           color='Cluster', alpha=0.4) + guides(color='none')+
  ggtitle('Affinity propagation')+
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()  #remove y axis ticks
  ) +scale_color_manual(values=matlab::jet.colors(length(AcousticSignalsAPFemales@exemplars)
) )

Plot2Females

cowplot::plot_grid(Plot1Females,Plot2Females)

table(plot.for.AcousticSignals$Validation,plot.for.AcousticSignals$Cluster)



source('R/AffinityBiplotAddSpectrogramsSpatial.R')
set.seed(13)
AffinityBiplotAddSpectrogramsSpatial(input.dir.Focal='data/FemaleGibbonsSwiftHQ',
                              output.dir.Focal = 'data/FemaleGibbonsSwiftHQ/Thumbnails/',
                              min.freq = 400, max.freq = 1600,main='Female Gibbon Calls',class='unsupervised')


source('R/UMAPBiplotAddSpectrograms.R')

UMAPBiplotAddSpectrograms(input.dir.Focal='data/FemaleGibbonsSwiftHQ',
                              output.dir.Focal = 'data/FemaleGibbonsSwiftHQ/Thumbnails/',
                              min.freq = 400, max.freq = 1500,main='Female Gibbon Calls',class='fixed')


source('R/AffinityBiplotAddSpectrograms.R')
source('R/UMAPBiplotAddSpectrograms.R')

set.seed(13)
AffinityBiplotAddSpectrograms(input.dir.Focal='/Users/denaclink/Desktop/RStudio Projects/gibbonID/TrueFalsePositives',
                              output.dir.Focal = '/Users/denaclink/Desktop/RStudio Projects/gibbonID/TrueFalsePositives/Thumbnails/',
                              min.freq = 400, max.freq = 1500,main='gibbonR detections',class='fixed')


UMAPBiplotAddSpectrograms(input.dir.Focal='/Users/denaclink/Desktop/RStudio Projects/gibbonID/TrueFalsePositives',
                              output.dir.Focal = '/Users/denaclink/Desktop/RStudio Projects/gibbonID/TrueFalsePositives/ThumbnailsUMAP/',
                              min.freq = 400, max.freq = 1500,main='gibbonR detections')


Recall <- 49.43
Precision <- 97.58

(2 * Precision * Recall)/(Precision + Recall)







