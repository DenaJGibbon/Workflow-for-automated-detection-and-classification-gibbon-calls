library(gibbonR)
library(ROCR)
library(stringr)
library(ggpubr)
library(reshape2)
library(dplyr)

# # Link .wav files to selection tables -----------------------------------

AnnotatedFiles <- list.files("Data/AnnotatedFilesTest/")
AnnotatedFilesFull <- list.files("Data/AnnotatedFilesTest/",full.names = T)

Filename <- str_split_fixed(AnnotatedFiles,pattern = '.Table',n=2)[,1]
ListofWavs <- list.files('/Volumes/Dena Clink Toshiba 3 TB/SWIFT_sparse_array_Danum/',recursive = T,full.names = T)

ListIndex <- lapply(1:length(Filename),
                    function(x)
                      which(str_detect(ListofWavs,Filename[x])))

WavFileNames <- ListofWavs[unlist(ListIndex)]


# # Add in all training data and female added
# subset.directory <- "/Volumes/Dena Clink Toshiba 3 TB/gibbonRandomDetections/TrainingSVMRF_all/Subset400_8"
# 
# trainingdata <- MFCCFunction(input.dir= subset.directory, min.freq = 500, max.freq = 1600)
# 
# trainingdata$class <- as.factor(trainingdata$class)
# 
# trainingdata$class <- plyr::revalue(trainingdata$class,
#                                     c('duet'='female.gibbon',
#                                       'hornbill.helmeted'='noise',
#                                       'hornbill.rhino'='noise',
#                                       'long.argus'='noise',
#                                       'short.argus'='noise'))
#

#write.csv(trainingdata,'MFCCTrainingSamples_400_8.csv',row.names = F)

trainingdata <- read.csv('MFCCTrainingAllSamples.csv')
#trainingdata <- read.csv('MFCCTrainingSamples_400_8.csv')
trainingdata$class <- as.factor(trainingdata$class)
# 
# # Run detector/classifier over list of wav files --------------------------
# output.dir <- 'Data/TestOutput5s400Samples/'
# 
# feature.df <- trainingdata
# 
# gibbonR(input=WavFileNames,
#         feature.df=feature.df,
#         model.type.list=c('RF','SVM'),
#         tune = TRUE,
#         short.wav.duration=300,
#         target.signal = c("female.gibbon"),
#         min.freq = 500, max.freq = 1600,
#         noise.quantile.val=0.15,
#         minimum.separation =3,
#         n.windows = 9, num.cep = 12,
#         spectrogram.window =160,
#         pattern.split = ".wav",
#         min.signal.dur = 5,
#         max.sound.event.dur = 36,
#         maximum.separation =1,
#         probability.thresh.svm =0,
#         probability.thresh.rf = 0,
#         wav.output = "FALSE",
#         output.dir =output.dir,
#         swift.time=FALSE,time.start=5,time.stop=10,
#         write.table.output=TRUE,verbose=TRUE,
#         random.sample='NA')
# 



# Version 7: Use only ROCR
# Version 6: Use updated annotations tables

# Data Preparation --------------------------------------------------------

# Link to gibbonR output selection tables
RandomIterFolders <-list.files("Data/TestOutput5s/",
                               full.names = T)

RandomIterFoldersShort <-list.files("Data/TestOutput5s/",
                               full.names = F)

# Link to annotated sound files
AnnotatedFiles <- list.files("Data/AnnotatedFilesTest/")
AnnotatedFilesFull <- list.files("Data/AnnotatedFilesTest/",full.names = T)

# Determine file names from annotated file names
nslash <- str_count(AnnotatedFilesFull,pattern = '/')[1]+1
snames <- str_split_fixed(AnnotatedFilesFull,pattern = '/',n=nslash)[,nslash]

# Combine all annotations
humanannotations <- data.frame()
for(x in 1:length(AnnotatedFilesFull)){
  print(x)
  temp.table <- read.delim2(AnnotatedFilesFull[x],fill = T,header =T)
  file.name <- str_split_fixed(snames[x],pattern = '[.]',n=2)[,1]
  recorder <- str_split_fixed(file.name,pattern='_',n=3)[,1]
  date <- str_split_fixed(file.name,pattern='_',n=3)[,2]
  time <- str_split_fixed(file.name,pattern='_',n=3)[,3]
  
  if(nrow(temp.table >0)){
    temp.table.updated <- cbind.data.frame(file.name,recorder,date,time,temp.table)
  } else {
    temp.row <- as.data.frame(t(rep('NA',ncol(temp.table))))
    colnames(temp.row) <- colnames(temp.table)
    temp.table.updated <- cbind.data.frame(file.name,recorder,date,time,temp.row)
    
  }
  
  temp.table.updated <- temp.table.updated[,c("file.name", "recorder", "date", "time", "Selection", "View", 
    "Channel", "Begin.Time..s.", "End.Time..s.", "Low.Freq..Hz.", 
    "High.Freq..Hz.", "Dur.90...s.", "Freq.95...Hz.", "Call.type"
  )]
  humanannotations <- rbind.data.frame(humanannotations,temp.table.updated)
}

# Convert all non-female gibbon calls to 'noise' category
humanannotations$Call.type <- 
  ifelse(humanannotations$Call.type=='female.gibbon','female.gibbon','noise')

humanannotations <- droplevels(subset(humanannotations,Call.type=='female.gibbon'))

RandomFiles <- str_split_fixed(RandomIterFoldersShort,pattern = 'gibbonR',n=2)[,1]
AnnotatedFiles.Notxt <- str_split_fixed(AnnotatedFiles,pattern = '.Table',n=2)[,1]

RandomIterFolders <- 
  RandomIterFolders[(RandomFiles %in% AnnotatedFiles.Notxt)]

RandomIterFoldersShort <- 
  RandomIterFoldersShort[(RandomFiles %in% AnnotatedFiles.Notxt)]


# Match automated detections --------------------------------------------------------------

RandomDetectionsDF  <- data.frame()

for(k in 1:length(RandomIterFolders)){ 
  print(k)
  # Select the folder
  output.dir <- RandomIterFolders[k]
  range.secs.start <- 4
  range.secs.end <- 2
  
  # List short and long file names
  RandomSelectionTables <-  RandomIterFolders[k]
  RandomSelectionTablesShort <- RandomIterFoldersShort[k]
  
  for(j in 1:length(RandomSelectionTables)){
    
    # Read in temporary selection table
    gibbonRoutput <- 
      read.delim(RandomSelectionTables[j])
    
    # Identify the recording ID
    recordingID <- str_split_fixed(RandomSelectionTablesShort[j],pattern = 'gibbonR',n=2)[,1]
    
    # Subset based on the recording ID
    humanannotationsSub <- subset(humanannotations,file.name==recordingID)
    
    # Subset female.gibbon annotations
    humanannotationsSub <- subset(humanannotationsSub,Call.type=='female.gibbon')
    
    # In some cases the recordings had zero gibbon females so we need to determine how many rows
    if(nrow(humanannotationsSub)>0){
      
      # Subset based on model type
      uniqueML <- unique(gibbonRoutput$model.type)
      
      # Loop over each ML algorithm and annotation row to match calls
      matched.annotations.df <- data.frame()
      dataframe.to.find.false.positives <- data.frame()
      
      for(a in 1:length(uniqueML)){
        # Prepare gibbonR output data 
        gibbonRoutputsub <- subset(gibbonRoutput,model.type==uniqueML[a])
        gibbonRoutputsub <- subset(gibbonRoutputsub,File.Name==recordingID)
        signal <- unique(gibbonRoutputsub$signal)
        File.Name <- unique(gibbonRoutputsub$File.Name)
        
        # For each row in the annotated file match the corresponding detections
        for(b in 1:nrow(humanannotationsSub)){
          humanannotationsSub.subset <-  humanannotationsSub[b,]
          
          min.start.time <- as.numeric(humanannotationsSub.subset$Begin.Time..s.)-range.secs.start
          max.start.time <- as.numeric(humanannotationsSub.subset$End.Time..s.)+range.secs.end
          
          detections.ml <- subset(gibbonRoutputsub, Begin.Time..s. > min.start.time & End.Time..s.< max.start.time)
          
          # If there is a detection
          if(nrow(detections.ml) > 0){
            
            DetectionsOver50 <- subset(detections.ml,probability > 0.5)
            if(nrow(DetectionsOver50)>0){    
              class.label <- c('1')
              model.type <- uniqueML[a]
              probability <- median(DetectionsOver50$probability)
              actual.label <- c('1')
              temprow <- cbind.data.frame(humanannotationsSub.subset,model.type,probability,class.label,actual.label,signal,File.Name)
              
              dataframe.to.find.false.positives <- rbind.data.frame(dataframe.to.find.false.positives,DetectionsOver50)
            } else{
              class.label <- c('0')
              model.type <- uniqueML[a]
              probability <- c('0.0')
              actual.label <- c('1')
              temprow <- cbind.data.frame(humanannotationsSub.subset,model.type,probability,class.label,actual.label,signal,File.Name)
            }
            # If there was not a detection assign the annotation a false negative value
          } else {
            class.label <- c('0')
            model.type <- uniqueML[a]
            probability <- c('0.0')
            actual.label <- c('1')
            temprow <- cbind.data.frame(humanannotationsSub.subset,model.type,probability,class.label,actual.label,signal,File.Name)
          }
          matched.annotations.df <- rbind.data.frame(matched.annotations.df,temprow)
        }
      }
      
      # Create ID of true positives 
      dataframe.to.find.false.positives.id <- paste(dataframe.to.find.false.positives$File.Name,dataframe.to.find.false.positives$model.type,
                                                    dataframe.to.find.false.positives$Begin.Time..s., dataframe.to.find.false.positives$probability,sep='_')
      # Create matching variable for gibbonR output
      gibbonRoutput.id <- paste(gibbonRoutput$File.Name,gibbonRoutput$model.type,
                                gibbonRoutput$Begin.Time..s., gibbonRoutput$probability,sep='_')
      
      # Find which of the detections were true positives
      FalsePositiveRemove <- which(gibbonRoutput.id %in% dataframe.to.find.false.positives.id)
      
      # Remove the true positives from all the detection
      false.positive.detections  <-
        gibbonRoutput[ -FalsePositiveRemove,]
      
      # Need to add true negatives; this takes all the remaining detections and assigns them either 
      # a 0 or 1 based on the 50% threshold - if the probability is over 50% then the model classified as a gibbon
      false.positive.detections$class.label <- ifelse(false.positive.detections$probability> 0.5, '1', '0')
      
      # Since we removed the true positives all the remaining are not gibbons
      false.positive.detections$actual.label <- rep('0',nrow(false.positive.detections))
      
      # Subset the matched.annotation df so it has the same column name
      matched.annotations.df.columnsub <-
        matched.annotations.df[,dput(colnames(false.positive.detections))]
      
      # Combine annotations with detections
      CombinedDetectionDF <- 
        rbind.data.frame(matched.annotations.df.columnsub,false.positive.detections)
      
    } else {
      # If there were no gibbons in the annotated recording do as above using the 50% cut off
      false.positive.detections <- gibbonRoutput
      false.positive.detections$class.label <- ifelse(false.positive.detections$probability> 0.5, '1', '0')
      false.positive.detections$actual.label <- rep('0',nrow(false.positive.detections))
      CombinedDetectionDF <- false.positive.detections
      
    }
    
   # CombinedDetectionDF$trainingdata <- ListTrainingDirectories[k]
    
    RandomDetectionsDF <- rbind.data.frame(CombinedDetectionDF,RandomDetectionsDF)
  }
}

RandomDetectionsDFAUC <- subset(RandomDetectionsDF,model.type=='SVM'& probability >0)#

RandomDetectionsDF <- subset(RandomDetectionsDF,model.type=='SVM'& probability >0.97)#

FalseNegative <- nrow(subset(RandomDetectionsDF,class.label==0 & actual.label ==1))
TruePositive  <-  nrow(subset(RandomDetectionsDF,class.label==1 & actual.label ==1))
FalsePositive  <-  nrow(subset(RandomDetectionsDF,class.label==1 & actual.label ==0 ))

Precision <- TruePositive/ (TruePositive+ FalsePositive)
Precision

Recall <- TruePositive/ nrow(humanannotations)
Recall

F1 <-2 * (Precision * Recall) / (Precision + Recall)
F1

# RF Precision: 0.77; Recall: 0.79
# SVM Precision: 0.77; Recall: 0.79

#RandomDetectionsDF <- subset(RandomDetectionsDF,model.type=='RF')

# Create a vector of probabilities
predictions <- as.numeric(RandomDetectionsDFAUC$probability)

# Create a vector of actual labels
# From ROCR label  labels should be supplied as ordered factor(s), the lower level corresponding to the negative class
labels <- as.factor(RandomDetectionsDFAUC$actual.label)

# Create a prediction object using ROCR
pred <- prediction(predictions, labels)

# Calculate the area under the curve
perfauc <- performance(pred, "auc")
auc <- perfauc@y.values[[1]]
auc

# Calculate F1; harmonic mean of precision and recall
perfF1 <- performance(pred, "f")

F1 <-  perfF1@y.values[[1]]
maxindex <- which.max(na.omit(F1))
max(na.omit(F1))
Probability <- perfF1@x.values[[1]]
Probability[maxindex]

perf <- performance(pred, "prec","rec")
perf@x.values[[1]][maxindex]
perf@y.values[[1]][maxindex]

