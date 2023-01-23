library(stringr)
library(lubridate)
library(ggpubr)

# Prepare data ------------------------------------------------------------
# Get input directory for data and sound files
# NOTE: You must change this to the location where you have stored the downloaded data
input.dir <- '/Volumes/DJC Files/Clink et al Zenodo Data/'

# Get full file paths for LTSA annotations
gibbon.files <- list.files(paste(input.dir,'DataSheets/GibbonAnnotationsLTSA',sep=''),full.names = T)
gibbon.files.short <- list.files(paste(input.dir,'DataSheets/GibbonAnnotationsLTSA',sep=''),full.names = F)

# Get full file paths for detection tables
ValidationTables <- 
  list.files(paste(input.dir,'DataSheets/TrueFalsePositiveTables',sep=''),
             full.names = T)

# Combine tables into a dataframe
ValidationDF <- data.frame()
for(a in 1:length(ValidationTables)){
  print(a)
  Tempdf <- read.csv(ValidationTables[a], header = T)
  ValidationDF <- rbind.data.frame(ValidationDF,Tempdf )
}

# How many rows of detections?
nrow(ValidationDF) # 4771

# Subset yes or overlap
ValidationDFCorrect <-
  subset(ValidationDF,target.signal=='y'|target.signal=='o'|target.signal=='oo')

# How many correct?
nrow(ValidationDFCorrect) # 3662

# Calculate precision
nrow(ValidationDFCorrect)/nrow(ValidationDF)


# Convert detections to standardized format -------------------------------
# Isolate start hour
ValidationDFCorrect$start.time.mins <-  as.numeric(substr(ValidationDFCorrect$time,start=1,stop=1))

# Loop to convert time to HH:MM:SS format; date is converted to today's date
ValidationDFupdated <- data.frame()
for(z in 1:nrow(ValidationDFCorrect)){
  print(z)
  TempRow <- ValidationDFCorrect[z,]
  TempStart <- round(TempRow$start.time/60,2)
  
  hour <-  hour(seconds_to_period(TempRow$start.time)) +TempRow$start.time.mins
  minute <-  minute(seconds_to_period(TempRow$start.time))
  second<- second(seconds_to_period(TempRow$start.time))
  
  if(hour==1){
    hour <- 10
  }
  
  if(hour==2){
    hour <- 11
  }
  
  TempStartTime <- paste( hour,':',minute,':',second,sep='')
  
  TempStartTime <- strptime(TempStartTime,format = '%H:%M:%S')
  
  hour <-  hour(seconds_to_period(TempRow$end.time)) +TempRow$start.time.mins
  minute <-  minute(seconds_to_period(TempRow$end.time))
  second<- second(seconds_to_period(TempRow$end.time))
  
  if(hour==1){
    hour <- 10
  }
  
  if(hour==2){
    hour <- 11
  }
  TempendTime <- paste( hour,':',minute,':',second,sep='')
  
  TempendTime <- strptime(TempendTime,format = '%H:%M:%S')
  
  TempRow <- cbind.data.frame(TempRow,TempStartTime,TempendTime)
  ValidationDFupdated <- rbind.data.frame(ValidationDFupdated,TempRow)
}

# Check output
head(ValidationDFupdated)

# Separate start time
ValidationDFupdated$TempStartTime <- str_split_fixed(ValidationDFupdated$TempStartTime,pattern = ' ',n=2)[,2]

# Convert to hour only
ValidationDFupdated$TempStartHour <- str_split_fixed(ValidationDFupdated$TempStartTime,pattern = ':',n=2)[,1]


# Convert LTSA data to standardized format --------------------------------
Recorder.gibbon <- str_split_fixed(gibbon.files.short,pattern = '_',n=2)[,1]
gibbon.annotations <- data.frame()
for(a in 1:length(gibbon.files)){
  temp.gibbon <- read.csv(gibbon.files[a])
  
  temp.gibbon <- temp.gibbon[,c("Date", "Call.type", "Time.start", "Time.end","Quality")]
  temp.gibbon$Recorder <- rep(Recorder.gibbon[a],nrow(temp.gibbon))
  
  colnames(temp.gibbon) <- c( "Date", "Call.type", "Start.time", "End.time","Quality","Recorder")
  temp.gibbon <- temp.gibbon[,c("Recorder", "Date", "Call.type", "Start.time", "End.time","Quality")]
  
  if(a==11){
    temp.gibbon <- temp.gibbon[1:25,]
  }
  gibbon.annotations <- rbind.data.frame(gibbon.annotations,temp.gibbon)
}

# Focus only on duet
gibbon.annotations <- 
  droplevels(subset(gibbon.annotations,Call.type=='duet' ))

# Check output
head(gibbon.annotations)

# Convert both LTSA and detection data to standardized time 
gibbon.annotations$Start.time <- strptime(gibbon.annotations$Start.time,format = '%H:%M:%S' )
gibbon.annotations <- na.omit(gibbon.annotations)
gibbon.annotations$Start.time <-as.POSIXct(gibbon.annotations$Start.time)


ValidationDFupdated$Start.time <- strptime(ValidationDFupdated$TempStartTime,format = '%H:%M:%S' )
ValidationDFupdated <- na.omit(ValidationDFupdated)
ValidationDFupdated$Start.time <-as.POSIXct(ValidationDFupdated$Start.time)

ValidationDFhistdf <- ValidationDFupdated[,c("recorder",'Start.time')]
ValidationDFhistdf$DataSet <- rep('Automated',nrow(ValidationDFupdated))

gibbon.annotationshistdf <- gibbon.annotations[,c("Recorder",'Start.time')]
gibbon.annotationshistdf$DataSet <- rep('Manual',nrow(gibbon.annotationshistdf))

# Combine into a dataframe for plotting
AnnotationsCombinedDF <- 
  rbind.data.frame(ValidationDFhistdf[,c("DataSet",'Start.time')],gibbon.annotationshistdf[,c("DataSet",'Start.time')]  )


# Histogram of automated versus manual ------------------------------------
# Note: need to change to today's date
gghistogram(data=AnnotationsCombinedDF,x='Start.time',fill='DataSet',facet.by = 'DataSet',scales='free')+
  
  scale_x_datetime(limits = ymd_h(c("2023-01-23 11", "2023-01-23 16"))) + theme(legend.position = "none")+
  scale_fill_manual(values=c('blue','yellow') )+xlab('Time')+ylab('Count')


# KS test on Unix values --------------------------------------------------
AutomatedSub<- subset(AnnotationsCombinedDF,DataSet=='Automated')
ManualSub<- subset(AnnotationsCombinedDF,DataSet=='Manual')

# Convert times to Unix time by using as.numeric, then do ks test 
ks.test(as.numeric(AutomatedSub$Start.time),
        as.numeric(ManualSub$Start.time))


