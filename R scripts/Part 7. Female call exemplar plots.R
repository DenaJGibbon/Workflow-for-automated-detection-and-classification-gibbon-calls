library(ggpubr)
library(png)
DFForLevels <- cbind.data.frame(c("S10", "S11", "S12", "S14", "S15", "S16", "S17", "S18", "S19",
                                  "S20"),
                                rep(NA,10))

colnames(DFForLevels) <- c("Recorder", "Cluster")

NewDataFrame$Cluster <- as.factor(NewDataFrame$Cluster)
DFForLevels$Cluster <- as.factor(DFForLevels$Cluster)

jet.color.vals <- matlab::jet.colors(10)

# Cluster 1
NewDataFrame1 <- subset(NewDataFrame,Cluster=='1')
NewDataFrame1 <- rbind.data.frame(NewDataFrame1,DFForLevels)
levels(NewDataFrame1$Cluster)

img1 <-( png::readPNG("/Users/denaclink/Desktop/RStudio Projects/gibbonID/exemplars/exemplar 1 .png"))
img1 <-grid::rasterGrob(img1, interpolate=TRUE)

NewDataFrame1 <- as.data.frame(table(NewDataFrame1$Recorder))
NewDataFrame1$Freq[NewDataFrame1$Freq == '1'] <- 'NA'

colnames(NewDataFrame1) <- c('Recorder','Number of Calls')
NewDataFrame1$`Number of Calls` <- as.numeric(NewDataFrame1$`Number of Calls`)

Barplot1 <- ggbarplot(data=NewDataFrame1,x='Recorder',y='Number of Calls',
                      fill = jet.color.vals[1] )+
  ylab('Number of calls')+ylim(0,50) +
  annotation_custom(img1,xmin=3, xmax=13, ymin=12, ymax=60)+ggtitle('Cluster 1')



# Cluster 2
NewDataFrame2 <- subset(NewDataFrame,Cluster=='2')
NewDataFrame2 <- rbind.data.frame(NewDataFrame2,DFForLevels)
levels(NewDataFrame2$Cluster)

img2 <-( png::readPNG("/Users/denaclink/Desktop/RStudio Projects/gibbonID/exemplars/exemplar 2 .png"))
img2 <-grid::rasterGrob(img2, interpolate=TRUE)

NewDataFrame2 <- as.data.frame(table(NewDataFrame2$Recorder))
NewDataFrame2$Freq[NewDataFrame2$Freq == '1'] <- 'NA'

colnames(NewDataFrame2) <- c('Recorder','Number of Calls')
NewDataFrame2$`Number of Calls` <- as.numeric(NewDataFrame2$`Number of Calls`)

Barplot2 <- ggbarplot(data=NewDataFrame2,x='Recorder',y='Number of Calls',
                      fill = jet.color.vals[2] )+
  ylab('Number of calls')+ylim(0,50) +
  annotation_custom(img2,xmin=3, xmax=13, ymin=12, ymax=60)+ggtitle('Cluster 2')


# Cluster 3
NewDataFrame3 <- subset(NewDataFrame,Cluster=='3')
NewDataFrame3 <- rbind.data.frame(NewDataFrame3,DFForLevels)
levels(NewDataFrame3$Cluster)

img3 <-( png::readPNG("/Users/denaclink/Desktop/RStudio Projects/gibbonID/exemplars/exemplar 3 .png"))
img3 <-grid::rasterGrob(img3, interpolate=TRUE)

NewDataFrame3 <- as.data.frame(table(NewDataFrame3$Recorder))
NewDataFrame3$Freq[NewDataFrame3$Freq == '1'] <- 'NA'

colnames(NewDataFrame3) <- c('Recorder','Number of Calls')
NewDataFrame3$`Number of Calls` <- as.numeric(NewDataFrame3$`Number of Calls`)

Barplot3 <- ggbarplot(data=NewDataFrame3,x='Recorder',y='Number of Calls',
                      fill = jet.color.vals[4] )+
  ylab('Number of calls')+ylim(0,50) +
  annotation_custom(img3,xmin=3, xmax=13, ymin=12, ymax=60)+ggtitle('Cluster 3')


# Cluster 4
NewDataFrame4 <- subset(NewDataFrame,Cluster=='4')
NewDataFrame4 <- rbind.data.frame(NewDataFrame4,DFForLevels)
levels(NewDataFrame4$Cluster)

img4 <-( png::readPNG("/Users/denaclink/Desktop/RStudio Projects/gibbonID/exemplars/exemplar 4 .png"))
img4 <-grid::rasterGrob(img4, interpolate=TRUE)

NewDataFrame4 <- as.data.frame(table(NewDataFrame4$Recorder))
NewDataFrame4$Freq[NewDataFrame4$Freq == '1'] <- 'NA'

colnames(NewDataFrame4) <- c('Recorder','Number of Calls')
NewDataFrame4$`Number of Calls` <- as.numeric(NewDataFrame4$`Number of Calls`)

Barplot4 <- ggbarplot(data=NewDataFrame4,x='Recorder',y='Number of Calls',
                      fill = jet.color.vals[4] )+
  ylab('Number of calls')+ylim(0,50) +
  annotation_custom(img4,xmin=3, xmax=13, ymin=12, ymax=60)+ggtitle('Cluster 4')


# Cluster 5
NewDataFrame5 <- subset(NewDataFrame,Cluster=='5')
NewDataFrame5 <- rbind.data.frame(NewDataFrame5,DFForLevels)
levels(NewDataFrame5$Cluster)

img5 <-( png::readPNG("/Users/denaclink/Desktop/RStudio Projects/gibbonID/exemplars/exemplar 5 .png"))
img5 <-grid::rasterGrob(img5, interpolate=TRUE)

NewDataFrame5 <- as.data.frame(table(NewDataFrame5$Recorder))
NewDataFrame5$Freq[NewDataFrame5$Freq == '1'] <- 'NA'

colnames(NewDataFrame5) <- c('Recorder','Number of Calls')
NewDataFrame5$`Number of Calls` <- as.numeric(NewDataFrame5$`Number of Calls`)

Barplot5 <- ggbarplot(data=NewDataFrame5,x='Recorder',y='Number of Calls',
                      fill = jet.color.vals[5] )+
  ylab('Number of calls')+ylim(0,50) +
  annotation_custom(img5,xmin=3, xmax=13, ymin=12, ymax=60)+ggtitle('Cluster 5')


# Cluster 6
NewDataFrame6 <- subset(NewDataFrame,Cluster=='6')
NewDataFrame6 <- rbind.data.frame(NewDataFrame6,DFForLevels)
levels(NewDataFrame6$Cluster)

img6 <-( png::readPNG("/Users/denaclink/Desktop/RStudio Projects/gibbonID/exemplars/exemplar 6 .png"))
img6 <-grid::rasterGrob(img6, interpolate=TRUE)

NewDataFrame6 <- as.data.frame(table(NewDataFrame6$Recorder))
NewDataFrame6$Freq[NewDataFrame6$Freq == '1'] <- 'NA'

colnames(NewDataFrame6) <- c('Recorder','Number of Calls')
NewDataFrame6$`Number of Calls` <- as.numeric(NewDataFrame6$`Number of Calls`)

Barplot6 <- ggbarplot(data=NewDataFrame6,x='Recorder',y='Number of Calls',
                      fill = jet.color.vals[6] )+
  ylab('Number of calls')+ylim(0,50) +
  annotation_custom(img6,xmin=3, xmax=13, ymin=12, ymax=60)+ggtitle('Cluster 6')


# Cluster 7
NewDataFrame7 <- subset(NewDataFrame,Cluster=='7')
NewDataFrame7 <- rbind.data.frame(NewDataFrame7,DFForLevels)
levels(NewDataFrame7$Cluster)

img7 <-( png::readPNG("/Users/denaclink/Desktop/RStudio Projects/gibbonID/exemplars/exemplar 7 .png"))
img7 <-grid::rasterGrob(img7, interpolate=TRUE)

NewDataFrame7 <- as.data.frame(table(NewDataFrame7$Recorder))
NewDataFrame7$Freq[NewDataFrame7$Freq == '1'] <- 'NA'

colnames(NewDataFrame7) <- c('Recorder','Number of Calls')
NewDataFrame7$`Number of Calls` <- as.numeric(NewDataFrame7$`Number of Calls`)

Barplot7 <- ggbarplot(data=NewDataFrame7,x='Recorder',y='Number of Calls',
                      fill = jet.color.vals[7] )+
  ylab('Number of calls')+ylim(0,50) +
  annotation_custom(img7,xmin=-2, xmax=8, ymin=12, ymax=60)+ggtitle('Cluster 7')

# Cluster 8
NewDataFrame8 <- subset(NewDataFrame,Cluster=='8')
NewDataFrame8 <- rbind.data.frame(NewDataFrame8,DFForLevels)
levels(NewDataFrame8$Cluster)

img8 <-( png::readPNG("/Users/denaclink/Desktop/RStudio Projects/gibbonID/exemplars/exemplar 8 .png"))
img8 <-grid::rasterGrob(img8, interpolate=TRUE)

NewDataFrame8 <- as.data.frame(table(NewDataFrame8$Recorder))
NewDataFrame8$Freq[NewDataFrame8$Freq == '1'] <- 'NA'

colnames(NewDataFrame8) <- c('Recorder','Number of Calls')
NewDataFrame8$`Number of Calls` <- as.numeric(NewDataFrame8$`Number of Calls`)

Barplot8 <- ggbarplot(data=NewDataFrame8,x='Recorder',y='Number of Calls',
                      fill = jet.color.vals[8] )+
  ylab('Number of calls')+ylim(0,50) +
  annotation_custom(img8,xmin=3, xmax=13, ymin=12, ymax=60)+ggtitle('Cluster 8')

# Cluster 9
NewDataFrame9 <- subset(NewDataFrame,Cluster=='9')
NewDataFrame9 <- rbind.data.frame(NewDataFrame9,DFForLevels)
levels(NewDataFrame9$Cluster)

img9 <-( png::readPNG("/Users/denaclink/Desktop/RStudio Projects/gibbonID/exemplars/exemplar 9 .png"))
img9 <-grid::rasterGrob(img9, interpolate=TRUE)

NewDataFrame9 <- as.data.frame(table(NewDataFrame9$Recorder))
NewDataFrame9$Freq[NewDataFrame9$Freq == '1'] <- 'NA'

colnames(NewDataFrame9) <- c('Recorder','Number of Calls')
NewDataFrame9$`Number of Calls` <- as.numeric(NewDataFrame9$`Number of Calls`)

Barplot9 <- ggbarplot(data=NewDataFrame9,x='Recorder',y='Number of Calls',
                      fill = jet.color.vals[9] )+
  ylab('Number of calls')+ylim(0,50) +
  annotation_custom(img9,xmin=3, xmax=13, ymin=12, ymax=60)+ggtitle('Cluster 9')

# Cluster 10
NewDataFrame10 <- subset(NewDataFrame,Cluster=='10')
NewDataFrame10 <- rbind.data.frame(NewDataFrame10,DFForLevels)
levels(NewDataFrame10$Cluster)

img10 <-( png::readPNG("/Users/denaclink/Desktop/RStudio Projects/gibbonID/exemplars/exemplar 10 .png"))
img10 <-grid::rasterGrob(img10, interpolate=TRUE)

NewDataFrame10 <- as.data.frame(table(NewDataFrame10$Recorder))
NewDataFrame10$Freq[NewDataFrame10$Freq == '1'] <- 'NA'

colnames(NewDataFrame10) <- c('Recorder','Number of Calls')
NewDataFrame10$`Number of Calls` <- as.numeric(NewDataFrame10$`Number of Calls`)

Barplot10 <- ggbarplot(data=NewDataFrame10,x='Recorder',y='Number of Calls',
                       fill = jet.color.vals[10] )+
  ylab('Number of calls')+ylim(0,50) +
  annotation_custom(img10,xmin=3, xmax=13, ymin=16, ymax=60)+ggtitle('Cluster 10')



png('exemplar.png',width = 2200*7, height = 1800*10,res=1500)

cowplot::plot_grid(Barplot1, Barplot2,Barplot3,Barplot4,Barplot5,
                    Barplot6,Barplot7,Barplot8,Barplot9,Barplot10,
                   ncol = 2)
graphics.off()

