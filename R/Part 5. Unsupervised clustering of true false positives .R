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
# Get input directory for data and sound files
# NOTE: You must change this to the location where you have stored the downloaded data
input.dir <- '/Volumes/DJC Files/Clink et al Zenodo Data/'

source('R/MFCCFunctionMeanSD.R')
subset.directory <- paste(input.dir, 'TrueFalsePositives', sep='')

trainingdata <- MFCCFunctionMeanSD(input.dir=subset.directory , min.freq = 500, max.freq = 1600)

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

ggpubr::ggscatter(data = plot.for.AcousticSignalsMFCC,x = "Dim.1",
                  y = "Dim.2",
                  color  = "Class", shape='Class', alpha=0.1,facet.by = 'Class')
Plot1



# Supervised classification -------------------------------------------------
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
  )+scale_color_manual(values= viridis::turbo ( length(AcousticSignalsAP@exemplars)  ) )



cowplot::plot_grid(Plot1,Plot2)



