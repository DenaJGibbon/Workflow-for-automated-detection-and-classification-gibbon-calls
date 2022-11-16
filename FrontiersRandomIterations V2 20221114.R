library(gibbonR)

# Link files on Box to selection tables -----------------------------------

AnnotatedFiles <- list.files("/Users/denaclink/Desktop/RStudio Projects/Multi-species-detector/Data/MultiSpecies/")
AnnotatedFilesFull <- list.files("/Users/denaclink/Desktop/RStudio Projects/Multi-species-detector/Data/MultiSpecies/",full.names = T)

Filename <- str_split_fixed(AnnotatedFiles,pattern = '.Table',n=2)[,1]
ListofWavs <- list.files('/Volumes/Dena Clink Toshiba 3 TB/SWIFT_sparse_array_Danum/',recursive = T,full.names = T)

ListIndex <- lapply(1:length(Filename),
                    function(x)
                      which(str_detect(ListofWavs,Filename[x])))

WavFileNames <- ListofWavs[unlist(ListIndex)]


# Divide training dataset into smaller increments -------------------------
n.files <- 
  length(list.files("/Users/denaclink/Desktop/RStudio Projects/Gibbon-automated-detection/TrainingFilesValidated"))

wav.file.names <- list.files("/Users/denaclink/Desktop/RStudio Projects/Gibbon-automated-detection/TrainingFilesValidated")

trainingdataID <- str_split_fixed(wav.file.names,pattern = '_',n=2)[,1]

recordingID <- paste(str_split_fixed(wav.file.names,pattern = '_',n=5)[,2],
                     str_split_fixed(wav.file.names,pattern = '_',n=5)[,3],
                     str_split_fixed(wav.file.names,pattern = '_',n=5)[,4],sep='_')

length(unique(recordingID))

table(trainingdataID)

source("/Users/denaclink/Desktop/RStudio Projects/gibbonR/R/gibbonR.R")

duet.training <- which(trainingdataID=='duet')
noise.training <- which(trainingdataID!='duet')
subset.vals <- c(10,20,40,80,160,320,400)

for(z in 1:10){
for(a in 1:length(subset.vals)){
  print(paste('processing',z, 'out of 10 for subset',subset.vals[a] ))
  subset.val <- subset.vals[a]
  duet.subset <- duet.training[sample(1:length(duet.training),subset.val,replace = F)]
  noise.subset <- noise.training[sample(1:length(noise.training),subset.val,replace = F)]
  combined.subset <- c(duet.subset,noise.subset)
  
  subset.directory <- paste("/Volumes/Dena Clink Toshiba 3 TB/gibbonRandomDetections/TrainingSVMRF_all/",'Subset',subset.val,'_',z,sep='')
  
  if (!dir.exists(subset.directory)){
    dir.create(subset.directory)
    print(paste('Created output dir',subset.directory))
  } else {
    print(paste(subset.directory,'already exists'))
  }
  
  file.copy(file.path('TrainingFilesValidated',wav.file.names[combined.subset]), subset.directory)

  output.dir <- paste('/Users/denaclink/Desktop/RStudio Projects/Gibbon-automated-detection/gibbonRoutputRandomIterationsUpdated/','Subset',subset.val,'_',z,sep='')
  
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
                                        'short.argus'='noise'))
  # 
  trainingdata$class <- as.factor(trainingdata$class)
  
  
  # Automated detection and classification ----------------------------------
  

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
          min.signal.dur = 3,
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


# Add in all training data and female added
trainingdata <- MFCCFunction(input.dir= subset.directory, min.freq = 500, max.freq = 1600)

trainingdata$class <- as.factor(trainingdata$class)

trainingdata$class <- plyr::revalue(trainingdata$class,
                                    c('duet'='female.gibbon',
                                      'hornbill.helmeted'='noise',
                                      'hornbill.rhino'='noise',
                                      'long.argus'='noise',
                                      'short.argus'='noise'))
# 
trainingdata$class <- as.factor(trainingdata$class)


# Automated detection and classification all training data ----------------------------------
trainingdata <- MFCCFunction(input.dir= "/Users/denaclink/Desktop/RStudio Projects/Gibbon-automated-detection/TrainingFilesValidated/", min.freq = 500, max.freq = 1600)

trainingdata$class <- as.factor(trainingdata$class)

trainingdata$class <- plyr::revalue(trainingdata$class,
                                    c('duet'='female.gibbon',
                                      'hornbill.helmeted'='noise',
                                      'hornbill.rhino'='noise',
                                      'long.argus'='noise',
                                      'short.argus'='noise'))
# 
trainingdata$class <- as.factor(trainingdata$class)

output.dir <- '/Users/denaclink/Desktop/RStudio Projects/Gibbon-automated-detection/gibbonRoutputRandomIterationsUpdated/TrainingDataAll_1'

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
        min.signal.dur = 3,
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
trainingdata <- MFCCFunction(input.dir= "/Users/denaclink/Desktop/RStudio Projects/Gibbon-automated-detection/TrainingFilesValidatedAddFemales/", min.freq = 500, max.freq = 1600)

trainingdata$class <- as.factor(trainingdata$class)

trainingdata$class <- plyr::revalue(trainingdata$class,
                                    c('duet'='female.gibbon',
                                      'hornbill.helmeted'='noise',
                                      'hornbill.rhino'='noise',
                                      'long.argus'='noise',
                                      'short.argus'='noise'))
# 
trainingdata$class <- as.factor(trainingdata$class)

output.dir <- '/Users/denaclink/Desktop/RStudio Projects/Gibbon-automated-detection/gibbonRoutputRandomIterationsUpdated/TrainingDataFemalesAdded_1'

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
        min.signal.dur = 3,
        max.sound.event.dur = 24,
        maximum.separation =1,
        probability.thresh.svm =0,
        probability.thresh.rf = 0,
        wav.output = "FALSE",
        output.dir =output.dir,
        swift.time=FALSE,time.start=5,time.stop=10,
        write.table.output=TRUE,verbose=TRUE,
        random.sample='NA')




