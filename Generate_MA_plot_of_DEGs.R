#Clean environment
rm(list=ls()) 
#Load packages
library(ggplot2)
library(ggrepel)
#Set your working directory
setwd("/Users/x/Desktop/") 
x <- read.delim(insert_file_here, row.names = "Ensembl") 
head(x)
ma <- x[,c("logFC","logCPM")]

#Optional: add targets file with gene names to be labeled in plot
#genenames <-read.delim("x",header=TRUE)
#head(genenames)

#Color code data points/DEGs based on logFC and p-value cut-offs
x$color="black"
x$color[x$logFC>1 & x$PValue<(0.05)]="red"
x$color[x$logFC<(-1) & x$PValue<(0.05)]="blue"

#Quantify number of DEGs up/down regulated
sum(x$logFC>1 & x$PValue<(0.05))
sum(x$logFC<(-1) & x$PValue<(0.05))

#Save MA plot as png image file
#Hash geom_label_repel if you do not wish to add gene name labels
png(file="Example.png",width=500, height=500)
ggplot(x,aes(logCPM,logFC)) + geom_point(size=2,color=x$color,shape=16) + ylim(-15,15) + ylab("log2(Adults/Children)") +
  geom_label_repel(data=subset(x,Name %in% genenames$Name),size=7, point.padding=1,label.padding=0.3,segment.color="white", segment.size = 1, direction="both") +
theme_minimal(base_size=20) 

dev.off()






