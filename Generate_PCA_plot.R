#Clean environment
rm(list=ls()) 

#install.packages("ggplot2")
library(ggplot2)

#Set working directory
setwd(Users)
#Read in CPM normalized counts table (can also be FPKM)
data <- read.delim("Example.txt", row.names = "Ensembl")  
#Sanity check that your data file has the right amount of rows and columns
nrow(data)
ncol(data)
#Sanity check if variables are all read in as numeric
sapply(data,class)

#Trim low counts. Remove genes that only contain zeros across entire row
trimmed_x <- data [which(rowSums(data) > 0),]
#Sanity check that data has been trimmed (list should be shorter now than line 12)
nrow(trimmed_x)
ncol(trimmed_x)

#Transpose data table
x.transposed <-t(trimmed_x)
#Sanity check that data has been transposed (rows and columns should now be opposite of lines 12 and 13)
nrow(x.transposed)
ncol(x.transposed)
sapply(x.transposed, class)
#log transform data
x <- log2(x.transposed+1)

#Load targets file containing annotation of data points
targets <- read.delim("Example2.txt") 
summary(targets$Sample)

#Set color-coding for Virus 
#Use same format to annotate/color-code for any other category or parameter
type <-targets$Virus
type <- factor(type,levels= c("Healthy_Child","Healthy_Adult","Child_SARS_CoV_2","Adult_SARS_CoV_2","IV","RSV"))
type
colType <-c("#C6DBEF","#3082BD","#FDCC65","#756BB1","#FB6949","#30A354")[type]
colType


######Run analysis and generate PCA plot. Scale and Center should be TRUE#########
pca <-prcomp(x,scale.= TRUE, center=TRUE)

#Variances of principal components
variance <- pca$sdev^2/sum(pca$sdev^2)
head(variance)
#Variance as percentage (use to label PC1 and PC2 axes)
variance_percentage <-variance*100
head(variance_percentage)

#Plot PCA plot
plot(pca$x,col=colType,cex=3, pch=19, xlab="PC1 (19.6%)", ylab="PC2 (6.3%)", lwd=3)

#Add text labels of sample names
text(pca$x,labels=targets$Sample,cex=2,font=2)

#Save PCA plot as a png file
png(file="PCA_plot.png",width=400, height=467)
plot(pca$x,col=colType,cex=2.5, cex.lab=1.2, cex.axis=1.2, pch=19, lwd=3, xlab="PC1 (19.6%)", ylab="PC2 (6.3%)")
#text(pca$x,labels=targets$Sample,cex=1,font=2)
dev.off()

