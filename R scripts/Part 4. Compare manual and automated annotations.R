library(stringr)
library(lubridate)
library(ggpubr)

ValidationTables <- 
  list.files('Data/TrueFalsePositiveTables',
             full.names = T)

ValidationDF <- data.frame()
for(a in 1:length(ValidationTables)){
  print(a)
  Tempdf <- read.csv(ValidationTables[a], header = T)
  ValidationDF <- rbind.data.frame(ValidationDF,Tempdf )
}

nrow(ValidationDF) # 4771

ValidationDFCorrect <-
  subset(ValidationDF,target.signal=='y'|target.signal=='o'|target.signal=='oo')

nrow(ValidationDFCorrect) # 4372; still need to decide what to do with '?'

nrow(ValidationDFCorrect)/nrow(ValidationDF)

table(ValidationDFCorrect$target.signal)

tail(table(ValidationDFCorrect$recorder,ValidationDFCorrect$date))

head(ValidationDFCorrect)

start.time.mins <- ValidationDFCorrect$start.time/60
ValidationDFCorrect$start.time.mins <-  as.numeric(substr(ValidationDFCorrect$time,start=1,stop=1))

ValidationDFupdated <- data.frame()
for(z in 1:nrow(ValidationDF)){
  
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

head(ValidationDFupdated)

ValidationDFupdated$TempStartTime <- str_split_fixed(ValidationDFupdated$TempStartTime,pattern = ' ',n=2)[,2]
ValidationDFupdated$TempStartHour <- str_split_fixed(ValidationDFupdated$TempStartTime,pattern = ':',n=2)[,1]


gibbon.files <- list.files("/Users/denaclink/Desktop/LTSA_detections/gibbons",full.names = T)
gibbon.files.short <- list.files("/Users/denaclink/Desktop/LTSA_detections/gibbons",full.names = F)
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

gibbon.annotations <- 
  droplevels(subset(gibbon.annotations,Call.type=='duet' ))

head(gibbon.annotations)

gibbon.annotations$Start.time <- strptime(gibbon.annotations$Start.time,format = '%H:%M:%S' )
gibbon.annotations <- na.omit(gibbon.annotations)
gibbon.annotations$Start.time <-as.POSIXct(gibbon.annotations$Start.time)
gghistogram(data=gibbon.annotations, x='Start.time')


ValidationDFupdated$Start.time <- strptime(ValidationDFupdated$TempStartTime,format = '%H:%M:%S' )
ValidationDFupdated <- na.omit(ValidationDFupdated)
ValidationDFupdated$Start.time <-as.POSIXct(ValidationDFupdated$Start.time)

gghistogram(data=ValidationDFupdated,x='Start.time')


ValidationDFhistdf <- ValidationDFupdated[,c("recorder",'Start.time')]
ValidationDFhistdf$DataSet <- rep('Automated',nrow(ValidationDFupdated))

gibbon.annotationshistdf <- gibbon.annotations[,c("Recorder",'Start.time')]
gibbon.annotationshistdf$DataSet <- rep('Manual',nrow(gibbon.annotationshistdf))

AnnotationsCombinedDF <- 
  rbind.data.frame(ValidationDFhistdf[,c("DataSet",'Start.time')],gibbon.annotationshistdf[,c("DataSet",'Start.time')]  )

# Note: need to change to today's date
gghistogram(data=AnnotationsCombinedDF,x='Start.time',fill='DataSet',facet.by = 'DataSet',scales='free')+
  
  scale_x_datetime(limits = ymd_h(c("2023-01-02 11", "2023-01-02 16"))) + theme(legend.position = "none")+
  scale_fill_manual(values=c('blue','yellow') )+xlab('Time')+ylab('Count')

AutomatedSub<- subset(AnnotationsCombinedDF,DataSet=='Automated')
ManualSub<- subset(AnnotationsCombinedDF,DataSet=='Manual')

plot(density(as.numeric(AutomatedSub$Start.time)))
plot(density(as.numeric(ManualSub$Start.time)))

ks.test(as.numeric(AutomatedSub$Start.time),
        as.numeric(ManualSub$Start.time))

