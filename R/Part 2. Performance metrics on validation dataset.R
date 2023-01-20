library(ROCR)
library(stringr)
library(ggpubr)
library(reshape2)
library(dplyr)
library(gibbonR)
library(bbmle)


# Data Preparation --------------------------------------------------------
# Set input directory for data and sound files
# NOTE: You must change this to the location where you have stored the downloaded data
input.dir <- '/Volumes/DJC Files/Clink et al Zenodo Data/'

# Link to gibbonR output selection tables
RandomIterFolders <-list.files(paste(input.dir,'gibbonRoutputRandomIterations5s',sep=''),
                               full.names = T)

# Get just the names of the folders
ListTrainingDirectories <-list.files(paste(input.dir,'gibbonRoutputRandomIterations5s',sep=''),
                                     full.names = F)

# Link to annotated sound files
AnnotatedFiles <- list.files( paste(input.dir,'AnnotatedFilesValidation',sep=''))
AnnotatedFilesFull <- list.files( paste(input.dir,'AnnotatedFilesValidation',sep=''),
                                  full.names = T)

# Determine file names from annotated file names
nslash <- str_count(AnnotatedFilesFull,pattern = '/')[1]+1
snames <- str_split_fixed(AnnotatedFilesFull,pattern = '/',n=nslash)[,nslash]

# Combine all annotations
humanannotations <- data.frame()
for(x in 1:length(AnnotatedFilesFull)){
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
  humanannotations <- rbind.data.frame(humanannotations,temp.table.updated)
}

# Convert all non-female gibbon calls to 'noise' category
humanannotations$Call.type <- 
  ifelse(humanannotations$Call.type=='female.gibbon','female.gibbon','noise')

# Drop all non gibbon female annotations
humanannotations <- droplevels(subset(humanannotations,Call.type=='female.gibbon'))

# Match automated detections --------------------------------------------------------------
# Set number of seconds before/after to be considered a detection
range.secs.start <- 4
range.secs.end <- 2

# Loop over randomized detection folders
RandomDetectionsDF  <- data.frame()

for(k in 1:length(RandomIterFolders)){ 
  print(k)
  # Select the folder
  output.dir <- RandomIterFolders[k]
  
  # List short and long file names
  RandomSelectionTables <- list.files(output.dir,full.names = T)
  RandomSelectionTablesShort <- list.files(output.dir,full.names = F)
  
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
    
  # Subset based on model type (SVM or RF)
  uniqueML <- unique(gibbonRoutput$model.type)
  
  # Create empty dataframes to fill in the loop
  matched.annotations.df <- data.frame()
  dataframe.to.find.false.positives <- data.frame()
 
  # Loop over each ML algorithm and annotation row to match calls
  
  for(a in 1:length(uniqueML)){
    # Prepare detector output data 
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
    if(nrow(detections.ml) > 1){
      print(detections.ml)
      print(a)
      print(b)
    }
    
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
#  0 or 1 based on the 50% threshold - if the probability is over 50% then the model classified as a gibbon
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

CombinedDetectionDF$trainingdata <- ListTrainingDirectories[k]

RandomDetectionsDF <- rbind.data.frame(CombinedDetectionDF,RandomDetectionsDF)
  }
}
  

# Calculate model performance  ----------------------------------------------------------
# Create sequence of probabilities
ProbthreshSeq <- seq(0,1,0.1)

# Convert ML probability to numeric
RandomDetectionsDF$probability <- as.numeric(RandomDetectionsDF$probability)

# Isolate training data folders
TrainingData <- unique(RandomDetectionsDF$trainingdata)

ROCRPerformanceDFCombined <- data.frame()
PerformanceDF <- data.frame()
AUCDfCombined <- data.frame()
for(a in 1:length(TrainingData)){
  # Subset based on training folders
  RandomDetectionsDFTemp <-   subset(RandomDetectionsDF,trainingdata==TrainingData[a])
  RandomDetectionsDFTemp <- RandomDetectionsDFTemp[which(as.numeric(RandomDetectionsDFTemp$End.Time..s.) - as.numeric(RandomDetectionsDFTemp$Begin.Time..s.) >=6),]
  # Index based on ML algorithm
  UniqueML <- unique(RandomDetectionsDFTemp$model.type)
  
  if(nrow(RandomDetectionsDFTemp)>0){
  # Loop by algorithm
  for(b in 1: length(UniqueML)){ 
    
    # Subset based on ML algorithm
    MLDataSub <- subset(RandomDetectionsDFTemp,model.type==UniqueML[b])
    
    # Create a variable for the MLAlgo
    MLAlgo <- UniqueML[b]
    
    # Create a vector of probabilities
    predictions <- as.numeric(MLDataSub$probability)
    
    # Create a vector of actual labels
    # From ROCR label  labels should be supplied as ordered factor(s), the lower level corresponding to the negative class
    labels <- as.factor(MLDataSub$actual.label)
    if(length(unique(labels)) ==2){
    # Create a prediction object using ROCR
    pred <- prediction(predictions, labels)
    
    # Calculate precision and recall
    perf <- performance(pred, "prec", "rec")
    rec <- perf@x.values[[1]]
    prec <- perf@y.values[[1]]
    
    # Calculate the area under the curve
    perfauc <- performance(pred, "auc")
    auc <- perfauc@y.values[[1]]
    
    # Calculate F1; harmonic mean of precision and recall
    perfF1 <- performance(pred, "f")
    F1 <-  perfF1@y.values[[1]]
    Probability <- perfF1@x.values[[1]]
    
    # Combine AUC into dataframe
    AUCDf <- cbind.data.frame(auc,TrainingData[a],MLAlgo)
    colnames(AUCDf) <- c('AUC','TrainingData','MLAlgo')
    AUCDfCombined <- rbind.data.frame(AUCDfCombined,AUCDf)
    
    # Combine other metrics from ROCR into dataframe
    ROCRPerformanceDF <- cbind.data.frame(prec,rec,F1,Probability,MLAlgo)
   
    # Combine values into probability bins
    ROCRPerformanceDF <-
      ROCRPerformanceDF %>% mutate(Probability = cut(Probability, breaks=ProbthreshSeq))
    
    # Add training data info
    ROCRPerformanceDF$TrainingData <- TrainingData[a]
    
    # Combine everything into a data frame
    ROCRPerformanceDFCombined <- rbind.data.frame(ROCRPerformanceDF,ROCRPerformanceDFCombined)
    
    # Combine with larger dataframe
    PerformanceDF <- rbind.data.frame(PerformanceDF,ROCRPerformanceDFCombined)
    
  }
  }
  }
}

# remove any NA values
ROCRPerformanceDFCombined <- na.omit(ROCRPerformanceDFCombined)

# Create histogram of recall
hist(ROCRPerformanceDFCombined$rec)

# Create column indicating training data samples
ROCRPerformanceDFCombined$TrainingDataSplit <-
  str_split_fixed(ROCRPerformanceDFCombined$TrainingData,pattern = '_',n=2)[,1]

# Create column indicating training data samples
AUCDfCombined$TrainingDataSplit <-
  str_split_fixed(AUCDfCombined$TrainingData,pattern = '_',n=2)[,1]


# Model selection for AUC -----------------------------------------------------
# Assign to a new object
auc.df <- AUCDfCombined

# Convert algorithm to a factor
auc.df$Algorithm <- as.factor(auc.df$MLAlgo)

# Create column indicating training data samples
auc.df$TrainingData <- str_split_fixed(auc.df$TrainingDataSplit,
                                       pattern = '_',n=2)[,1]

# Convert to a factor
auc.df$TrainingData <- 
  as.factor(auc.df$TrainingData)

# Relabel factor levels for plotting 
auc.df$TrainingData <- factor(auc.df$TrainingData,levels=c('Subset10', "Subset20","Subset40",
                                                           "Subset80","Subset160","Subset320","Subset400",
                                                           "TrainingDataAll","TrainingDataFemalesAdded"))
# Reorder factor levels for plotting 
levels(auc.df$TrainingData) <-  c("n=10","n=20","n=40",
                                  "n=80","n=160","n=320", "n=400","All","All + F")#

# Convert n=160 to reference level for modeling
auc.df <- within(auc.df, TrainingData <- relevel(TrainingData, ref = "n=160"))

# Remove any NA
auc.df <- na.omit(auc.df)

# Create full linear model
lm1 <- lm(AUC ~ TrainingData+Algorithm , data=auc.df)

# Create null linear model
lmnull <- lm(AUC ~ 1 , data=auc.df)

# Compare models using AIC
bbmle::AICctab(lmnull,lm1,weights=T)

# Create coefficient plot for manuscript
AUCcoef <- sjPlot::plot_model(lm1,sort.est =T)+ylim(-0.25,0.25)+ geom_hline(yintercept = 0)+theme_bw()+scale_color_manual(values= c("#0080FF", "#FF8000") )
AUCcoef

# Summary of full model
summary(lm1)
anova(lm1)


# Model selection for F1 score --------------------------------------------

performance.df <- ROCRPerformanceDFCombined

performance.df$TrainingDataSplit <- 
  factor(performance.df$TrainingDataSplit,levels=c('Subset10', "Subset20","Subset40",
                                                                           "Subset80","Subset160","Subset320","Subset400",
                                                                           "TrainingDataAll","TrainingDataFemalesAdded"))


levels(performance.df$TrainingDataSplit) <-  c("n=10","n=20","n=40",
                                          "n=80","n=160","n=320", "n=400","All","All + F")#


performance.df <- within(performance.df, TrainingDataSplit <- relevel(TrainingDataSplit, ref = "n=160"))
performance.df$MLAlgo <- as.factor(performance.df$MLAlgo)

lm1 <- lm(F1 ~ TrainingDataSplit*MLAlgo , data=performance.df)
#lm1inter <- lm(AUC ~ TrainingData*Algorithm , data=auc.df)
lmnull <- lm(F1 ~ 1 , data=performance.df)
bbmle::AICctab(lmnull,lm1,weights=T)

F1coef<- sjPlot::plot_model(lm1,sort.est =T)+ylim(-0.25,0.25)+ geom_hline(yintercept = 0)+theme_bw()+scale_color_manual(values= c("#0080FF", "#FF8000") )

cowplot::plot_grid(AUCcoef,F1coef)

performance.df$TrainingData <- str_split_fixed(performance.df$TrainingData,
                                               pattern = '_',n=2)[,1]

performance.df$TrainingData <- as.factor(performance.df$TrainingData)



TempProb <-  str_split_fixed(performance.df$Probability, pattern = ',',n=2)[,2]
TempProb <-  str_split_fixed(TempProb, pattern = ']',n=2)[,1]

performance.df$Threshold <- as.numeric( TempProb )

performance.df <- na.omit( performance.df )

performance.df$Algorithm <-performance.df$MLAlgo

performance.dfF1 <- performance.df %>% 
  group_by(Algorithm,TrainingData,Threshold) %>% 
  summarize(mean=mean(F1),sd=sd(F1)
  )

performance.dfF1$TrainingData <- as.factor(performance.dfF1$TrainingData )



levels(performance.dfF1$TrainingData) <-  c("n=10","n=20","n=40",
                                            "n=80","n=160","n=320", "n=400","All","All + F")#


performance.dfF1$Threshold <- as.numeric(performance.dfF1$Threshold)


ggline(data=performance.dfF1,
       x='Threshold',y='mean',group  = 'TrainingData',color='TrainingData',shape='TrainingData',
       facet.by = 'Algorithm')+
  scale_color_manual(values= matlab::jet.colors(length(unique(performance.dfF1$TrainingData))) )+
  ylab('F1 score')+ylim(0,1)+geom_hline(yintercept = max(performance.dfF1$mean),lty='dashed')

performance.dfF1[which.max(performance.dfF1$mean),]

subset(performance.dfF1,mean > 0.79)
# Summarize output --------------------------------------------------------

library(dplyr)


PerformanceDFSummary <- performance.df %>%
  group_by(TrainingData,Algorithm,Probability = cut(Threshold, breaks = seq(0, max(Threshold), 0.25))) %>%
  summarise(PrecisionMed = median(prec),
            PrecisionSD = sd(prec),
            RecallMed =  median(rec),
            RecallSD = sd(rec),
            F1Med=median(F1),
            F1SD = sd(F1)
  )

auc.dfSummary <- auc.df %>%
  group_by(TrainingData,MLAlgo) %>%
  summarise(AUCMed = median(AUC),
            AUCSD = sd(AUC)
  )

auc.dfSummary[,c('AUCMed','AUCSD')] <- round(auc.dfSummary[,c('AUCMed','AUCSD')],2)

auc.dfSummary$AUCMSD <- paste(auc.dfSummary$AUCMed,
                              "±",auc.dfSummary$AUCSD)


AucTable <- flextable::flextable(auc.dfSummary)

AucTable_merge <- flextable::merge_v(AucTable, j = c("TrainingData", "MLAlgo"))



PerformanceDFSummary[,c("PrecisionMed", 
                        "PrecisionSD", "RecallMed", "RecallSD", "F1Med", "F1SD")] <- 
  
  round(PerformanceDFSummary[,c("PrecisionMed", 
                                "PrecisionSD", "RecallMed", "RecallSD", "F1Med", "F1SD")],2)


PerformanceDFSummary$PrecisionMSD <- paste(PerformanceDFSummary$PrecisionMed,
                                           "±",PerformanceDFSummary$PrecisionSD)

PerformanceDFSummary$RecallMSD <- paste(PerformanceDFSummary$RecallMed,
                                        "±",PerformanceDFSummary$RecallSD)

PerformanceDFSummary$F1MSD <- paste(PerformanceDFSummary$F1Med,
                                    "±",PerformanceDFSummary$F1SD)




PerformanceDFSummary <- 
  subset(PerformanceDFSummary,Probability== '(0.5,0.75] ' | Probability== '(0.75,1]' )


levels(PerformanceDFSummary$TrainingData) <-  c("n=10","n=20","n=40",
                                                "n=80","n=160","n=320", "n=400","All","All + F")#


PerformanceDFSummary <- PerformanceDFSummary[order(PerformanceDFSummary$TrainingData),]
order(PerformanceDFSummary$TrainingData)

PerformanceDFSummary <- cbind.data.frame(PerformanceDFSummary,auc.dfSummary$AUCMSD)

library(flextable)

PerformanceTable <- flextable::flextable(PerformanceDFSummary[,c("TrainingData", "Algorithm",'PrecisionMSD','RecallMSD','F1MSD','auc.dfSummary$AUCMSD')])
PerformanceTable <- set_header_labels(PerformanceTable,
                        TrainingData = "Training Data",
                        Algorithm = "Algorithm", 
                        PrecisionMSD = "Precision (mean ± sd)",
                        RecallMSD = "Recall (mean ± sd)",
                        F1MSD = "F1 (mean ± sd)",
                        `auc.dfSummary$AUCMSD` = "AUC (mean ± sd)"
)

ft_merge <- merge_v(PerformanceTable, j = c("TrainingData", "Algorithm"))
ft_merge
#save_as_docx(ft_merge,path='PerformanceSummaryTableA_20221208.docx')



