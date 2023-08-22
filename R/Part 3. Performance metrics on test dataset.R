library(gibbonR)
library(ROCR)
library(stringr)
library(ggpubr)
library(reshape2)
library(dplyr)

# # Link .wav files to selection tables -----------------------------------
# Get input directory for data and sound files
# NOTE: You must change this to the location where you have stored the downloaded data
input.dir <- '/Volumes/DJC Files/Clink et al Zenodo Data/'

# Get directory to location of .wav files
wavfile.dir <- paste(input.dir,'ValidationSoundFiles',sep='')

# Link to annotated selection tables 
AnnotatedFiles <- list.files( 'Data/AnnotatedFilesTest')

# Find full file path of annotated selection tables
AnnotatedFilesFull <- list.files('Data/AnnotatedFilesTest',
                                 full.names = T)

# List wave files
WavFileNames <- list.files(wavfile.dir,recursive = T,full.names = T)

# Read in .csv
trainingdata <- read.csv('Data/MFCCTrainingAllSamples.csv')

# Convert class to a factor
trainingdata$class <- as.factor(trainingdata$class)
 
# # Run detector/classifier over list of wav files --------------------------
# Below is the code to run the detector/classifier over the test data
# output.dir <- 'Data/'
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


# Data Preparation --------------------------------------------------------

# Link to gibbonR output selection tables
RandomIterFolders <-list.files('Data/TestOutput5s',
                               full.names = T)

RandomIterFoldersShort <-list.files('Data/TestOutput5s',
                               full.names = F)

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

# Remove all non gibbon female annotations
humanannotations <- droplevels(subset(humanannotations,Call.type=='female.gibbon'))

# Isolate file names
RandomFiles <- str_split_fixed(RandomIterFoldersShort,pattern = 'gibbonR',n=2)[,1]
AnnotatedFiles.Notxt <- str_split_fixed(AnnotatedFiles,pattern = '.Table',n=2)[,1]

# Remove files contained in training data
AnnotatedFiles.Notxt <- AnnotatedFiles.Notxt[- which(AnnotatedFiles.Notxt %in% c("S10_20180425_060002", "S10_20180702_060002", "S10_20180704_060002","S11_20180314_080003", "S11_20180322_060002", "S17_20180408_060002"))]

AnnotatedFiles.Notxt <- AnnotatedFiles.Notxt[-c(all_combinations[,z])]

# Find matching text and sound files
RandomIterFolders <- 
  RandomIterFolders[(RandomFiles %in% AnnotatedFiles.Notxt)]

# Find matching text and sound files
RandomIterFoldersShort <- 
  RandomIterFoldersShort[(RandomFiles %in% AnnotatedFiles.Notxt)]


# Match automated detections --------------------------------------------------------------
# Set start and stop times to be considered detections
range.secs.start <- 4
range.secs.end <- 2


RandomDetectionsDF  <- data.frame()
for(k in 1:length(RandomIterFolders)){ 
  print(k)
  # Select the folder
  output.dir <- RandomIterFolders[k]
  
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
    
    RandomDetectionsDF <- rbind.data.frame(CombinedDetectionDF,RandomDetectionsDF)
  }
}

# Subset so only focus on SVM
RandomDetectionsDFAUC <- subset(RandomDetectionsDF,model.type=='SVM')

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

# isolate F1 
F1 <-  perfF1@y.values[[1]]

# What is the max F1
maxindex <- which.max(na.omit(F1))
max(na.omit(F1)) # 0.77

# Find which probability has highest F1
Probability <- perfF1@x.values[[1]]
Probability[maxindex] # 0.98

perfrec <- performance(pred, "rec")
perfrec@x.values[[1]][maxindex]
perfrec@y.values[[1]][maxindex]

perfprec <- performance(pred, "prec")
perfprec@x.values[[1]][maxindex]
perfprec@y.values[[1]][maxindex]

max(na.omit(F1)) # 0.75

