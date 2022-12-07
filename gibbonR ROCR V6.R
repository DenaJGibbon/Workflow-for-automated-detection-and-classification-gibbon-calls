library(ROCR)
library(stringr)
library(ggpubr)
library(reshape2)
library(dplyr)
library(gibbonR)

# Version 6: Use updated annotations tables

# Data Preparation --------------------------------------------------------

# Link to gibbonR output selection tables
RandomIterFolders <-list.files("gibbonRoutputRandomIterationsUpdated/",
                               full.names = T)

# Get just the names of the folders
ListTrainingDirectories <-list.files("gibbonRoutputRandomIterationsUpdated/",
                                     full.names = F)

# Link to annotated sound files
AnnotatedFiles <- list.files("Data/MultiSpecies/")
AnnotatedFilesFull <- list.files("Data/MultiSpecies/",full.names = T)

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


# Match automated detections --------------------------------------------------------------
output.dir <- "gibbonRoutputRandomIterationsUpdated/"

RandomDetectionsDF  <- data.frame()

for(k in 1:length(RandomIterFolders)){ 
  print(k)
  # Select the folder
  output.dir <- RandomIterFolders[k]
  range.secs.start <- 3
  range.secs.end <- 3
  
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
  
  if(nrow(detections.ml) > 0){
    class.label <- c('1')
    model.type <- uniqueML[a]
    probability <- median(detections.ml$probability)
    actual.label <- c('1')
    temprow <- cbind.data.frame(humanannotationsSub.subset,model.type,probability,class.label,actual.label,signal,File.Name)
    dataframe.to.find.false.positives <- rbind.data.frame(dataframe.to.find.false.positives,detections.ml)
  } else {
    class.label <- c('0')
    model.type <- uniqueML[a]
    probability <- c('0.15')
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

CombinedDetectionDF$trainingdata <- ListTrainingDirectories[k]

RandomDetectionsDF <- rbind.data.frame(CombinedDetectionDF,RandomDetectionsDF)
  }
}
  

# Calculate model performance  ----------------------------------------------------------
# Create sequence of probabilities
ProbthreshSeq <- seq(0,1,0.1)

# Convert ML probability to numeric
CombinedDetectionDF$RandomDetectionsDF <- as.numeric(CombinedDetectionDF$probability)

# Isolate training data folders
UniqueTraining <- unique(RandomDetectionsDF$trainingdata)

ROCRPerformanceDFCombined <- data.frame()
PerformanceDF <- data.frame()
AUCDfCombined <- data.frame()
for(a in 1:length(UniqueTraining)){
  
  # Subset based on training folders
  RandomDetectionsDFTemp <-   subset(RandomDetectionsDF,trainingdata==UniqueTraining[a])
  
  # Index based on ML algorithm
  UniqueML <- unique(RandomDetectionsDFTemp$model.type)
  
  # Loop by algorithm
  for(b in 1: length(UniqueML)){ 
    
    # Subset based on ML algorithm
    MLDataSub <- subset(RandomDetectionsDFTemp,model.type==UniqueML[b])
    
    # Create a variable for the MLAlgo
    MLAlgo <- UniqueML[b]
    
    # Loop over 
    PerformanceSeqDF <- data.frame()
    for(c in 1:length(ProbthreshSeq)){
      ThreshSub <- subset(MLDataSub,probability >= ProbthreshSeq[c])
      
      # FALSENeg is when the system missed calls that were there
      FALSENeg <- ThreshSub[ThreshSub$class.label ==0 & ThreshSub$actual.label ==1 ,]
      FALSENegN <- nrow(FALSENeg) 
      
      # TRUEPos is when the system got it right
      TRUEPos <- ThreshSub[ThreshSub$class.label ==1 & ThreshSub$actual.label ==1,]
      TRUEPosN <- nrow(TRUEPos)
      
      # False Positive
      FalsePos <- ThreshSub[ThreshSub$class.label ==1 & ThreshSub$actual.label ==0 ,]
      FalsePosN <- nrow(FalsePos)
      
      # Precision
      Precision <- TRUEPosN / (TRUEPosN +FalsePosN) 
      
      # Recall
      Recall <-  TRUEPosN / length( which(humanannotations$Call.type=='female.gibbon'))
      
      # F1 score
      F1 <- 2*(Precision*Recall)/(Precision + Recall)
      F1
      
      
      
      Probability <- ProbthreshSeq[c]
      
      TrainingData <- UniqueTraining[a]
      PerformanceRow <- cbind.data.frame(Precision, Recall, F1,Probability,MLAlgo,TrainingData)
      PerformanceSeqDF <- rbind.data.frame(PerformanceSeqDF,PerformanceRow)
    }
    
    predictions <- as.numeric(MLDataSub$probability)
    labels <- MLDataSub$actual.label
    pred <- prediction(predictions, labels)
    
    perf <- performance(pred, "prec", "rec")
    rec <- perf@x.values[[1]]
    prec<- perf@y.values[[1]]
    
    perfauc <- performance(pred, "aucpr")
    auc <- perfauc@y.values[[1]]
    
    perfF1 <- performance(pred, "f")
    F1 <-  perfF1@y.values[[1]]
    Probability <- perfF1@x.values[[1]]
    
    obs <- as.factor(MLDataSub$actual.label)
    Class1 <- as.numeric(MLDataSub$probability)
    Class2 <- 1-as.numeric(MLDataSub$probability)
    pred <- factor(ifelse(MLDataSub$probability >= .5, "1", "0"))
    
    test_set <- cbind.data.frame(obs,Class1,Class2,pred)
    ProbVals <- prSummary(test_set, lev = levels(test_set$obs))
    ProbVals <- t(as.data.frame(ProbVals[2:4]))
    
    AUCDf <- cbind.data.frame(auc,UniqueTraining[a],MLAlgo,ProbVals)
    colnames(AUCDf) <- c('AUC','TrainingData','MLAlgo',"Prec","Rec","F")
    AUCDfCombined <- rbind.data.frame(AUCDfCombined,AUCDf)
    
    ROCRPerformanceDF <- cbind.data.frame(prec,rec,F1,Probability,MLAlgo)
   
    ROCRPerformanceDF <-
      ROCRPerformanceDF %>% mutate(Probability = cut(Probability, breaks=ProbthreshSeq))
    
    ROCRPerformanceDF$TrainingData <- UniqueTraining[a]
    
    ROCRPerformanceDFCombined <- rbind.data.frame(ROCRPerformanceDF,ROCRPerformanceDFCombined)
    PerformanceDF <- rbind.data.frame(PerformanceDF,PerformanceSeqDF)
    
  }

}

which(ROCRPerformanceDFCombined$rec > 1)


hist(ROCRPerformanceDFCombined$rec)

ROCRPerformanceDFCombined$TrainingDataSplit <-
  str_split_fixed(ROCRPerformanceDFCombined$TrainingData,pattern = '_',n=2)[,1]

PerformanceDF$TrainingDataSplit <-
  str_split_fixed(PerformanceDF$TrainingData,pattern = '_',n=2)[,1]

AUCDfCombined$TrainingDataSplit <-
  str_split_fixed(AUCDfCombined$TrainingData,pattern = '_',n=2)[,1]


ggboxplot(data=PerformanceDF,x='Probability',y='F1',color='MLAlgo',facet.by = 'TrainingDataSplit')#+xlim(0,1)

ggscatter(data=PerformanceDF,x='Recall',y='Precision',color='MLAlgo',facet.by = 'TrainingDataSplit')+xlim(0,1)


ggboxplot(data=ROCRPerformanceDFCombined,x='Probability',y='F1',fill='MLAlgo',facet.by = 'TrainingDataSplit')
ggboxplot(data=ROCRPerformanceDFCombined,x='Probability',y='rec',facet.by = 'TrainingDataSplit',fill='MLAlgo')
#ggscatter(data=ROCRPerformanceDFCombined,x='rec',y='prec',facet.by = 'TrainingDataSplit')

ggboxplot(data=AUCDfCombined,x='TrainingDataSplit',y='AUC',color = 'TrainingDataSplit',facet.by ='MLAlgo' )
ggboxplot(data=AUCDfCombined,x='TrainingDataSplit',y='AUC',color = 'TrainingDataSplit',facet.by ='MLAlgo' )



# Noise quant val = 0.15; max recall = 0.75
# Noise quant val = 0.3; max recall = 0.55

max(na.omit(PerformanceDF$F1))

library(ROCR)
MLDataSub$probability <- as.numeric(MLDataSub$probability)
predictions <- MLDataSub$probability
labels <- MLDataSub$actual.label
pred <- prediction(predictions, labels)
pred

perf <- performance(pred, "tpr", "fpr")
perf
plot(perf,
     avg="threshold",
     spread.estimate="boxplot")
     
perf <- performance(pred, "prec", "rec")
perf@x.values[[1]]
perf@y.values[[1]]

plot(perf,
     avg= "threshold",
     colorize=TRUE,
     lwd= 3,
     main= "... Precision/Recall graphs ...")



perfauc <- performance(pred, "aucpr")
perfauc@y.values

perfF1 <- performance(pred, "f")
F1 <-  perfF1@y.values[[1]]
max(na.omit(F1))   


library(caret)
table(MLDataSub$actual.label)
table(MLDataSub$class.label,MLDataSub$actual.label)

tbl_2_1_pred <- factor(ThreshSub$class.label)
tbl_2_1_truth <- factor(ThreshSub$actual.label)
tbl_2_1 <- table(tbl_2_1_pred, tbl_2_1_truth)

precision(tbl_2_1)
precision(data = tbl_2_1_pred, reference = tbl_2_1_truth, relevant = "1")
recall(data = tbl_2_1_pred, reference = tbl_2_1_truth, relevant = "1")



ROCRSubset80_2 <- subset(ROCRPerformanceDFCombined,TrainingData=='Subset80_2')

library(dplyr)
ROCRSubset80_2 %>% group_by(Probability) %>% summarise_each(funs(mean, sd))
(PerformanceDF[1:11,])

performance.df <- ROCRPerformanceDFCombined
performance.df$TrainingData <- str_split_fixed(performance.df$TrainingData,
                                               pattern = '_',n=2)[,1]
auc.df <- AUCDfCombined
auc.df$Algorithm <- as.factor(auc.df$MLAlgo)
auc.df$TrainingData <- str_split_fixed(auc.df$TrainingDataSplit,
                                       pattern = '_',n=2)[,1]

auc.df$TrainingData <- 
  as.factor(auc.df$TrainingData)



auc.df$TrainingData <- factor(auc.df$TrainingData,levels=c('Subset10', "Subset20","Subset40",
                                                           "Subset80","Subset160","Subset320","Subset400",
                                                           "TrainingDataAll","TrainingDataFemalesAdded"))

levels(auc.df$TrainingData) <-  c("n=10","n=20","n=40",
                                  "n=80","n=160","n=320", "n=400","All","All + F")#

AUCbox <- ggboxplot(data=auc.df,
                    x='TrainingData',y='AUC',fill='TrainingData',facet.by = 'Algorithm')+ theme(axis.text.x = element_text(angle = 45,  hjust=1))+
  theme(legend.position="none")+scale_fill_manual(values= matlab::jet.colors(9) )

auc.df <- within(auc.df, TrainingData <- relevel(TrainingData, ref = "n=160"))

auc.df <- na.omit(auc.df)
lm1 <- lm(AUC ~ TrainingData+Algorithm , data=auc.df)
#lm1inter <- lm(AUC ~ TrainingData*Algorithm , data=auc.df)
lmnull <- lm(AUC ~ 1 , data=auc.df)
bbmle::AICctab(lmnull,lm1,weights=T)

AUCcoef <- sjPlot::plot_model(lm1,sort.est =T)+ylim(-0.25,0.25)+ geom_hline(yintercept = 0)+theme_bw()+scale_color_manual(values= c("#0080FF", "#FF8000") )
AUCcoef
summary(lm1)
anova(lm1)

cowplot::plot_grid(AUCbox,AUCcoef)

performance.df$TrainingData <- as.factor(performance.df$TrainingData)


performance.df$TrainingData <- factor(performance.df$TrainingData,levels=c('Subset10', "Subset20","Subset40",
                                                                           "Subset80","Subset160","Subset320","Subset400",
                                                                           "TrainingDataAll","TrainingDataFemalesAdded"))


levels(performance.df$TrainingData) <-  c("n=10","n=20","n=40",
                                          "n=80","n=160","n=320", "n=400","All","All + F")#

Precisionbox <- ggboxplot(data=performance.df,
                          x='TrainingData',y='Precision',fill='TrainingData',facet.by = 'ml.algorithm')+ theme(axis.text.x = element_text(angle = 45,  hjust=1))+
  theme(legend.position="none")


lm2 <- lm(Precision ~ TrainingData * ml.algorithm, data=performance.df)
Precisioncoef <- sjPlot::plot_model(lm2,sort.est =T) + geom_hline(yintercept = 0)+theme_bw()
summary(lm2)

cowplot::plot_grid(Precisionbox,Precisioncoef)

Recallbox <- ggboxplot(data=performance.df,
                       x='TrainingData',y='Recall',fill='TrainingData',facet.by = 'ml.algorithm')+ theme(axis.text.x = element_text(angle = 45,  hjust=1))+
  theme(legend.position="none")


lm3 <- lm(Recall ~ TrainingData *ml.algorithm, data=performance.df)
Recallcoef <- sjPlot::plot_model(lm3,sort.est =T)+ylim(-0.2,0.4) + geom_hline(yintercept = 0)+theme_bw()
summary(lm3)

cowplot::plot_grid(Recallbox,Recallcoef)

performance.df <- within(performance.df, TrainingData <- relevel(TrainingData, ref = "n=160"))
performance.df$Algorithm <- performance.df$MLAlgo

lm3 <- lm(F1 ~ TrainingData+Algorithm, data=performance.df)
lm3algo <- lm(F1 ~ Algorithm, data=performance.df)
lm3training <- lm(F1 ~ TrainingData, data=performance.df)
lm3null <- lm(F1 ~ 1, data=performance.df)
bbmle::AICctab(lm3,lm3null,weights=T)

F1coef <- sjPlot::plot_model(lm3,sort.est =T)+ylim(-0.2,0.2) + geom_hline(yintercept = 0)+theme_bw()
F1coef
summary(lm3)

cowplot::plot_grid(Recallbox,Recallcoef)

subset(performance.df,TrainingData=='TrainingDataAll')

library(dplyr)
TempProb <-  str_split_fixed(performance.df$Probability, pattern = ',',n=2)[,2]
TempProb <-  str_split_fixed(TempProb, pattern = ']',n=2)[,1]
performance.df$Threshold <- as.numeric( TempProb )

performance.df <- na.omit( performance.df )


performance.dfF1 <- performance.df %>% 
  group_by(Algorithm,TrainingData,Threshold) %>% 
  summarize(  median=median(F1),sd=sd(F1)
  )

performance.dfF1$TrainingData <- as.factor(performance.dfF1$TrainingData )



levels(performance.dfF1$TrainingData) <-  c("n=10","n=20","n=40",
                                            "n=80","n=160","n=320", "n=400","All","All + F")#


performance.dfF1$Threshold <- as.numeric(performance.dfF1$Threshold)


ggline(data=performance.dfF1,
       x='Threshold',y='median',group  = 'TrainingData',color='TrainingData',shape='TrainingData',
       facet.by = 'Algorithm')+
  scale_color_manual(values= matlab::jet.colors(length(unique(performance.dfF1$TrainingData))) )+
  ylab('F1 score')+ylim(0,1)


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


PerformanceDFSummary$TrainingData <- factor(PerformanceDFSummary$TrainingData,levels=c('Subset10', "Subset20","Subset40",
                                                                                       "Subset80","Subset160","Subset320","Subset400",
                                                                                       "TrainingDataAll","TrainingDataFemalesAdded"))



PerformanceDFSummary <- 
  subset(PerformanceDFSummary,Probability== '(0.75,1]')


levels(PerformanceDFSummary$TrainingData) <-  c("n=10","n=20","n=40",
                                                "n=80","n=160","n=320", "n=400","All","All + F")#


PerformanceDFSummary <- PerformanceDFSummary[order(PerformanceDFSummary$TrainingData),]
order(PerformanceDFSummary$TrainingData)

PerformanceDFSummary <- cbind.data.frame(PerformanceDFSummary,auc.dfSummary$AUCMSD)

library(flextable)

PerformanceTable <- flextable::flextable(PerformanceDFSummary[,c("TrainingData", "Algorithm",'PrecisionMSD','RecallMSD','F1MSD','auc.dfSummary$AUCMSD')])

ft_merge <- merge_v(PerformanceTable, j = c("TrainingData", "Algorithm"))

save_as_docx(ft_merge,path='PerformanceSummaryTableA_20221116.docx')
