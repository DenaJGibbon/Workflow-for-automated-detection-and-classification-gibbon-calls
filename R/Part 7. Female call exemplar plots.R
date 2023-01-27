library(ggpubr)
library(png)
library(matlab)
library(grid)

# Prepare data ------------------------------------------------------------
# Set file path to exemplars
Exemplar.file.paths <- list.files('Data/Exemplars',full.names = T)

# Read in datasheet with cluster and recorder assignment
ClusterByRecorderDF <- read.csv('Data/ClusterByRecorderDF.csv')

# Create data frame with 'NA' place holders for plots later
DFForLevels <- cbind.data.frame(c("S10", "S11", "S12", "S14", "S15", "S16", "S17", "S18", "S19",
                                  "S20"),
                                rep(NA,10))

# Fix column names
colnames(DFForLevels) <- c("Recorder", "Cluster")

# Make column a factor
ClusterByRecorderDF$Cluster <- as.factor(ClusterByRecorderDF$Cluster)

# Make column a factor
DFForLevels$Cluster <- as.factor(DFForLevels$Cluster)

# Create vector with colors 
jet.color.vals <- matlab::jet.colors(10)

# Create histograms with exemplar spectrogram embedded --------------------
# Cluster 1
ClusterByRecorderDF1 <- subset(ClusterByRecorderDF,Cluster=='1')
ClusterByRecorderDF1 <- rbind.data.frame(ClusterByRecorderDF1,DFForLevels)
levels(ClusterByRecorderDF1$Cluster)

img1 <- png::readPNG(Exemplar.file.paths[1])
img1 <-grid::rasterGrob(img1, interpolate=TRUE)

ClusterByRecorderDF1 <- as.data.frame(table(ClusterByRecorderDF1$Recorder))
ClusterByRecorderDF1$Freq[ClusterByRecorderDF1$Freq == '1'] <- 'NA'

colnames(ClusterByRecorderDF1) <- c('Recorder','Number of Calls')
ClusterByRecorderDF1$`Number of Calls` <- as.numeric(ClusterByRecorderDF1$`Number of Calls`)

Barplot1 <- ggbarplot(data=ClusterByRecorderDF1,x='Recorder',y='Number of Calls',
                      fill = jet.color.vals[1] )+
  ylab('Number of calls')+ylim(0,50) +
  annotation_custom(img1,xmin=3, xmax=13, ymin=12, ymax=60)+ggtitle('Cluster 1')


# Cluster 2
ClusterByRecorderDF2 <- subset(ClusterByRecorderDF,Cluster=='2')
ClusterByRecorderDF2 <- rbind.data.frame(ClusterByRecorderDF2,DFForLevels)
levels(ClusterByRecorderDF2$Cluster)

img2 <-png::readPNG(Exemplar.file.paths[2])
img2 <-grid::rasterGrob(img2, interpolate=TRUE)

ClusterByRecorderDF2 <- as.data.frame(table(ClusterByRecorderDF2$Recorder))
ClusterByRecorderDF2$Freq[ClusterByRecorderDF2$Freq == '1'] <- 'NA'

colnames(ClusterByRecorderDF2) <- c('Recorder','Number of Calls')
ClusterByRecorderDF2$`Number of Calls` <- as.numeric(ClusterByRecorderDF2$`Number of Calls`)

Barplot2 <- ggbarplot(data=ClusterByRecorderDF2,x='Recorder',y='Number of Calls',
                      fill = jet.color.vals[2] )+
  ylab('Number of calls')+ylim(0,50) +
  annotation_custom(img2,xmin=3, xmax=13, ymin=12, ymax=60)+ggtitle('Cluster 2')


# Cluster 3
ClusterByRecorderDF3 <- subset(ClusterByRecorderDF,Cluster=='3')
ClusterByRecorderDF3 <- rbind.data.frame(ClusterByRecorderDF3,DFForLevels)
levels(ClusterByRecorderDF3$Cluster)

img3 <- png::readPNG(Exemplar.file.paths[3])
img3 <-grid::rasterGrob(img3, interpolate=TRUE)

ClusterByRecorderDF3 <- as.data.frame(table(ClusterByRecorderDF3$Recorder))
ClusterByRecorderDF3$Freq[ClusterByRecorderDF3$Freq == '1'] <- 'NA'

colnames(ClusterByRecorderDF3) <- c('Recorder','Number of Calls')
ClusterByRecorderDF3$`Number of Calls` <- as.numeric(ClusterByRecorderDF3$`Number of Calls`)

Barplot3 <- ggbarplot(data=ClusterByRecorderDF3,x='Recorder',y='Number of Calls',
                      fill = jet.color.vals[4] )+
  ylab('Number of calls')+ylim(0,50) +
  annotation_custom(img3,xmin=3, xmax=13, ymin=12, ymax=60)+ggtitle('Cluster 3')


# Cluster 4
ClusterByRecorderDF4 <- subset(ClusterByRecorderDF,Cluster=='4')
ClusterByRecorderDF4 <- rbind.data.frame(ClusterByRecorderDF4,DFForLevels)
levels(ClusterByRecorderDF4$Cluster)

img4 <- png::readPNG(Exemplar.file.paths[4])
img4 <-grid::rasterGrob(img4, interpolate=TRUE)

ClusterByRecorderDF4 <- as.data.frame(table(ClusterByRecorderDF4$Recorder))
ClusterByRecorderDF4$Freq[ClusterByRecorderDF4$Freq == '1'] <- 'NA'

colnames(ClusterByRecorderDF4) <- c('Recorder','Number of Calls')
ClusterByRecorderDF4$`Number of Calls` <- as.numeric(ClusterByRecorderDF4$`Number of Calls`)

Barplot4 <- ggbarplot(data=ClusterByRecorderDF4,x='Recorder',y='Number of Calls',
                      fill = jet.color.vals[4] )+
  ylab('Number of calls')+ylim(0,50) +
  annotation_custom(img4,xmin=3, xmax=13, ymin=12, ymax=60)+ggtitle('Cluster 4')


# Cluster 5
ClusterByRecorderDF5 <- subset(ClusterByRecorderDF,Cluster=='5')
ClusterByRecorderDF5 <- rbind.data.frame(ClusterByRecorderDF5,DFForLevels)
levels(ClusterByRecorderDF5$Cluster)

img5 <- png::readPNG(Exemplar.file.paths[5])
img5 <-grid::rasterGrob(img5, interpolate=TRUE)

ClusterByRecorderDF5 <- as.data.frame(table(ClusterByRecorderDF5$Recorder))
ClusterByRecorderDF5$Freq[ClusterByRecorderDF5$Freq == '1'] <- 'NA'

colnames(ClusterByRecorderDF5) <- c('Recorder','Number of Calls')
ClusterByRecorderDF5$`Number of Calls` <- as.numeric(ClusterByRecorderDF5$`Number of Calls`)

Barplot5 <- ggbarplot(data=ClusterByRecorderDF5,x='Recorder',y='Number of Calls',
                      fill = jet.color.vals[5] )+
  ylab('Number of calls')+ylim(0,50) +
  annotation_custom(img5,xmin=3, xmax=13, ymin=12, ymax=60)+ggtitle('Cluster 5')


# Cluster 6
ClusterByRecorderDF6 <- subset(ClusterByRecorderDF,Cluster=='6')
ClusterByRecorderDF6 <- rbind.data.frame(ClusterByRecorderDF6,DFForLevels)
levels(ClusterByRecorderDF6$Cluster)

img6 <- png::readPNG(Exemplar.file.paths[6])
img6 <-grid::rasterGrob(img6, interpolate=TRUE)

ClusterByRecorderDF6 <- as.data.frame(table(ClusterByRecorderDF6$Recorder))
ClusterByRecorderDF6$Freq[ClusterByRecorderDF6$Freq == '1'] <- 'NA'

colnames(ClusterByRecorderDF6) <- c('Recorder','Number of Calls')
ClusterByRecorderDF6$`Number of Calls` <- as.numeric(ClusterByRecorderDF6$`Number of Calls`)

Barplot6 <- ggbarplot(data=ClusterByRecorderDF6,x='Recorder',y='Number of Calls',
                      fill = jet.color.vals[6] )+
  ylab('Number of calls')+ylim(0,50) +
  annotation_custom(img6,xmin=3, xmax=13, ymin=12, ymax=60)+ggtitle('Cluster 6')


# Cluster 7
ClusterByRecorderDF7 <- subset(ClusterByRecorderDF,Cluster=='7')
ClusterByRecorderDF7 <- rbind.data.frame(ClusterByRecorderDF7,DFForLevels)
levels(ClusterByRecorderDF7$Cluster)

img7 <- png::readPNG(Exemplar.file.paths[7])
img7 <-grid::rasterGrob(img7, interpolate=TRUE)

ClusterByRecorderDF7 <- as.data.frame(table(ClusterByRecorderDF7$Recorder))
ClusterByRecorderDF7$Freq[ClusterByRecorderDF7$Freq == '1'] <- 'NA'

colnames(ClusterByRecorderDF7) <- c('Recorder','Number of Calls')
ClusterByRecorderDF7$`Number of Calls` <- as.numeric(ClusterByRecorderDF7$`Number of Calls`)

Barplot7 <- ggbarplot(data=ClusterByRecorderDF7,x='Recorder',y='Number of Calls',
                      fill = jet.color.vals[7] )+
  ylab('Number of calls')+ylim(0,50) +
  annotation_custom(img7,xmin=-2, xmax=8, ymin=12, ymax=60)+ggtitle('Cluster 7')

# Cluster 8
ClusterByRecorderDF8 <- subset(ClusterByRecorderDF,Cluster=='8')
ClusterByRecorderDF8 <- rbind.data.frame(ClusterByRecorderDF8,DFForLevels)
levels(ClusterByRecorderDF8$Cluster)

img8 <- png::readPNG(Exemplar.file.paths[8])
img8 <-grid::rasterGrob(img8, interpolate=TRUE)

ClusterByRecorderDF8 <- as.data.frame(table(ClusterByRecorderDF8$Recorder))
ClusterByRecorderDF8$Freq[ClusterByRecorderDF8$Freq == '1'] <- 'NA'

colnames(ClusterByRecorderDF8) <- c('Recorder','Number of Calls')
ClusterByRecorderDF8$`Number of Calls` <- as.numeric(ClusterByRecorderDF8$`Number of Calls`)

Barplot8 <- ggbarplot(data=ClusterByRecorderDF8,x='Recorder',y='Number of Calls',
                      fill = jet.color.vals[8] )+
  ylab('Number of calls')+ylim(0,50) +
  annotation_custom(img8,xmin=3, xmax=13, ymin=12, ymax=60)+ggtitle('Cluster 8')

# Cluster 9
ClusterByRecorderDF9 <- subset(ClusterByRecorderDF,Cluster=='9')
ClusterByRecorderDF9 <- rbind.data.frame(ClusterByRecorderDF9,DFForLevels)
levels(ClusterByRecorderDF9$Cluster)

img9 <- png::readPNG(Exemplar.file.paths[9])
img9 <-grid::rasterGrob(img9, interpolate=TRUE)

ClusterByRecorderDF9 <- as.data.frame(table(ClusterByRecorderDF9$Recorder))
ClusterByRecorderDF9$Freq[ClusterByRecorderDF9$Freq == '1'] <- 'NA'

colnames(ClusterByRecorderDF9) <- c('Recorder','Number of Calls')
ClusterByRecorderDF9$`Number of Calls` <- as.numeric(ClusterByRecorderDF9$`Number of Calls`)

Barplot9 <- ggbarplot(data=ClusterByRecorderDF9,x='Recorder',y='Number of Calls',
                      fill = jet.color.vals[9] )+
  ylab('Number of calls')+ylim(0,50) +
  annotation_custom(img9,xmin=3, xmax=13, ymin=12, ymax=60)+ggtitle('Cluster 9')

# Cluster 10
ClusterByRecorderDF10 <- subset(ClusterByRecorderDF,Cluster=='10')
ClusterByRecorderDF10 <- rbind.data.frame(ClusterByRecorderDF10,DFForLevels)
levels(ClusterByRecorderDF10$Cluster)

img10 <- png::readPNG(Exemplar.file.paths[10])
img10 <-grid::rasterGrob(img10, interpolate=TRUE)

ClusterByRecorderDF10 <- as.data.frame(table(ClusterByRecorderDF10$Recorder))
ClusterByRecorderDF10$Freq[ClusterByRecorderDF10$Freq == '1'] <- 'NA'

colnames(ClusterByRecorderDF10) <- c('Recorder','Number of Calls')
ClusterByRecorderDF10$`Number of Calls` <- as.numeric(ClusterByRecorderDF10$`Number of Calls`)

Barplot10 <- ggbarplot(data=ClusterByRecorderDF10,x='Recorder',y='Number of Calls',
                       fill = jet.color.vals[10] )+
  ylab('Number of calls')+ylim(0,50) +
  annotation_custom(img10,xmin=3, xmax=13, ymin=16, ymax=60)+ggtitle('Cluster 10')



# Save plot as .png -------------------------------------------------------
png('exemplar.png',width = 2200*7, height = 1800*10,res=1500)

cowplot::plot_grid(Barplot1, Barplot2,Barplot3,Barplot4,Barplot5,
                    Barplot6,Barplot7,Barplot8,Barplot9,Barplot10,
                   ncol = 2)
graphics.off()

