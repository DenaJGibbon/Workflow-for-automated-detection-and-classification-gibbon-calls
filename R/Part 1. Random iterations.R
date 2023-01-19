library(gibbonR)

# Set input directory for data and sound files
# NOTE: You must change this to the location where you have stored the downloaded data
input.dir <- '/Volumes/DJC Files/Clink et al Zenodo Data/'

# Set directory to location of .wav files
wavfile.dir <- paste(input.dir,'ValidationSoundFiles',sep='')

# Set directory to location of training files
training.file.directory <- paste(input.dir,'TrainingFilesValidated',sep='')

# Set output directory for detection files
detection.output.dir <- paste(input.dir,'RandomizationDetections',sep='')

# Link to annotated selection tables 
AnnotatedFiles <- list.files( paste(input.dir,'AnnotatedFilesValidation',sep=''))

# Find full file path of annotated selection tables
AnnotatedFilesFull <- list.files(paste(input.dir,'AnnotatedFilesValidation',sep=''),
                                 full.names = T)

# Isolate the file name
Filename <- str_split_fixed(AnnotatedFiles,pattern = '.Table',n=2)[,1]

# List wave files
WavFileNames <- list.files(wavfile.dir,recursive = T,full.names = T)

# Divide training dataset into smaller increments -------------------------
# Determine number of training samples
n.files <- 
  length(list.files(training.file.directory))

# List training .wav files
wav.file.names <- list.files(training.file.directory)

# Isolate label
trainingdataID <- str_split_fixed(wav.file.names,pattern = '_',n=2)[,1]

# Determine which recording it came from
recordingID <- paste(str_split_fixed(wav.file.names,pattern = '_',n=5)[,2],
                     str_split_fixed(wav.file.names,pattern = '_',n=5)[,3],
                     str_split_fixed(wav.file.names,pattern = '_',n=5)[,4],sep='_')


# Create table with training sample labels
table(trainingdataID)

# Isolate gibbon female samples
duet.training <- which(trainingdataID=='duet')

# Isolate the rest of the samples
noise.training <- which(trainingdataID!='duet')

# Create vector for number of samples to include in training
subset.vals <- c(10,20,40,80,160,320,400)

# Automated detection and classification over random iterations ----------------------------------
# Loop to randomly sample training data over 10 iterations 
for(z in 1:10){
for(a in 1:length(subset.vals)){
  print(paste('processing',z, 'out of 10 for subset',subset.vals[a] ))
  subset.val <- subset.vals[a]
  duet.subset <- duet.training[sample(1:length(duet.training),subset.val,replace = F)]
  noise.subset <- noise.training[sample(1:length(noise.training),subset.val,replace = F)]
  combined.subset <- c(duet.subset,noise.subset)
  
  subset.directory <- paste('Data/Subset',subset.val,'_',z,sep='')
  
  if (!dir.exists(subset.directory)){
    dir.create(subset.directory)
    print(paste('Created output dir',subset.directory))
  } else {
    print(paste(subset.directory,'already exists'))
  }
  
  file.copy(file.path(training.file.directory,wav.file.names[combined.subset]), subset.directory)

  output.dir <- paste(detection.output.dir,'Subset',subset.val,'_',z,sep='')
  
  if (!dir.exists(output.dir)){
    dir.create(output.dir)
    print(paste('Created output dir',output.dir))
  } else {
    print(paste(output.dir,'already exists'))
  }
  

  trainingdata <- MFCCFunction(input.dir= subset.directory, min.freq = 500, max.freq = 1600)
  
  trainingdata$class <- as.factor(trainingdata$class)
  
  trainingdata$class <- plyr::revalue(trainingdata$class,
                                      c('duet'='female.gibbon',
                                        'hornbill.helmeted'='noise',
                                        'hornbill.rhino'='noise',
                                        'long.argus'='noise',
                                        'short.argus'='noise',
                                        'solo'='noise'))

  trainingdata$class <- as.factor(trainingdata$class)
  
  feature.df <- trainingdata
  
  gibbonR(input=WavFileNames,
          feature.df=feature.df,
          model.type.list=c('SVM','RF'),
          tune = TRUE,
          short.wav.duration=300,
          target.signal = c("female.gibbon"),
          min.freq = 500, max.freq = 1600,
          noise.quantile.val=0.15,
          minimum.separation =3,
          n.windows = 9, num.cep = 12,
          spectrogram.window =160,
          pattern.split = ".wav",
          min.signal.dur = 5,
          max.sound.event.dur = 24,
          maximum.separation =1,
          probability.thresh.svm =0,
          probability.thresh.rf = 0,
          wav.output = "FALSE",
          output.dir =output.dir,
          swift.time=FALSE,time.start=5,time.stop=10,
          write.table.output=TRUE,verbose=TRUE,
          random.sample='NA')
  
  
}
}


# Automated detection and classification all training data ----------------------------------
trainingdata <- MFCCFunction(input.dir= paste(input.dir,'TrainingFilesValidated',sep=''), min.freq = 500, max.freq = 1600)

trainingdata$class <- as.factor(trainingdata$class)

trainingdata$class <- plyr::revalue(trainingdata$class,
                                    c('duet'='female.gibbon',
                                      'hornbill.helmeted'='noise',
                                      'hornbill.rhino'='noise',
                                      'long.argus'='noise',
                                      'short.argus'='noise',
                                      'solo'='noise'))
# 
trainingdata$class <- as.factor(trainingdata$class)

output.dir <- paste(input.dir,'RandomizationDetections/TrainingDataAll_1',sep='')
  
feature.df <- trainingdata

gibbonR(input=WavFileNames,
        feature.df=feature.df,
        model.type.list=c('SVM','RF'),
        tune = TRUE,
        short.wav.duration=300,
        target.signal = c("female.gibbon"),
        min.freq = 500, max.freq = 1600,
        noise.quantile.val=0.15,
        minimum.separation =3,
        n.windows = 9, num.cep = 12,
        spectrogram.window =160,
        pattern.split = ".wav",
        min.signal.dur = 5,
        max.sound.event.dur = 24,
        maximum.separation =1,
        probability.thresh.svm =0,
        probability.thresh.rf = 0,
        wav.output = "FALSE",
        output.dir =output.dir,
        swift.time=FALSE,time.start=5,time.stop=10,
        write.table.output=TRUE,verbose=TRUE,
        random.sample='NA')


# Automated detection and classification all training data + females added ----------------------------------
trainingdata <- MFCCFunction(input.dir= paste(input.dir,'TrainingFilesValidatedAddFemales',sep=''), min.freq = 500, max.freq = 1600)

trainingdata$class <- as.factor(trainingdata$class)

trainingdata$class <- plyr::revalue(trainingdata$class,
                                    c('duet'='female.gibbon',
                                      'hornbill.helmeted'='noise',
                                      'hornbill.rhino'='noise',
                                      'long.argus'='noise',
                                      'short.argus'='noise'))
# 
trainingdata$class <- as.factor(trainingdata$class)

output.dir <- output.dir <- paste(input.dir,'RandomizationDetections/TrainingDataFemalesAdded_1',sep='')

feature.df <- trainingdata

gibbonR(input=WavFileNames,
        feature.df=feature.df,
        model.type.list=c('SVM','RF'),
        tune = TRUE,
        short.wav.duration=300,
        target.signal = c("female.gibbon"),
        min.freq = 500, max.freq = 1600,
        noise.quantile.val=0.15,
        minimum.separation =3,
        n.windows = 9, num.cep = 12,
        spectrogram.window =160,
        pattern.split = ".wav",
        min.signal.dur = 5,
        max.sound.event.dur = 24,
        maximum.separation =1,
        probability.thresh.svm =0,
        probability.thresh.rf = 0,
        wav.output = "FALSE",
        output.dir =output.dir,
        swift.time=FALSE,time.start=5,time.stop=10,
        write.table.output=TRUE,verbose=TRUE,
        random.sample='NA')




