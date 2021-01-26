#Clean environment
rm(list=ls())
 
#Install package
install.packages("BiocManager")
BiocManager::install("WGCNA")

#Set your working directory
setwd("Users/")
# Load the WGCNA package
library(WGCNA)
# The following setting is important, do not omit.
options(stringsAsFactors = FALSE)

#### DATA INPUT AND CLEANING ####
#Read in the data set
x <- read.csv("[insert CSV file]")
#Take a quick look at what is in the data set
dim(x)

#Remove excess data and transpose (this will depend on your initial csv import)
datExpr0 <- as.data.frame(t(x[, -c(1)]))
names(datExpr0) <- x$Ensembl
rownames(datExpr0) <- names(x)[-c(1)]
#sanity check
nrow(datExpr0)
ncol(datExpr0)
head(datExpr0)

#Check for genes and samples with too many missing values
gsg <- goodSamplesGenes(datExpr0, verbose = 3)
gsg$allOK

#IF the last statement returns TRUE, all genes have passed the cuts. If not, we remove the offending genes and samples
###
if (!gsg$allOK)
{
  # Optionally, print the gene and sample names that were removed:
  if (sum(!gsg$goodGenes)>0) 
    printFlush(paste("Removing genes:", paste(names(datExpr0)[!gsg$goodGenes], collapse = ", ")));
  if (sum(!gsg$goodSamples)>0) 
    printFlush(paste("Removing samples:", paste(rownames(datExpr0)[!gsg$goodSamples], collapse = ", ")));
  # Remove the offending genes and samples from the data:
  datExpr0 = datExpr0[gsg$goodSamples, gsg$goodGenes]
}
####
dim(datExpr0)
#Cluster the samples (in contrast to clustering genes that will come later) to see if there are any obvious outliers
sampleTree <- hclust(dist(datExpr0), method = "average")
# Plot the sample tree: Open a graphic output window of size 12 by 9 inches
# The user should change the dimensions if the window is too large or too small.
sizeGrWindow(10,10)
#pdf(file = "Plots/sampleClustering.pdf", width = 12, height = 9);
par(cex = 0.6);
par(mar = c(0,4,2,0))
plot(sampleTree, main = "Sample clustering to detect outliers", sub="", xlab="", cex.lab = 1.5, cex.axis = 1.5, cex.main = 2)  

#Removing outliers (this is optional - depends on the nature of your input data/analysis)
#Choose a height cut that will remove the offending sample, and use a branch cut at that height

# SKIPPED #
# Plot a line to show the cut
#abline(h = 90000, col = "red")
# Determine cluster under the line
#clust <- cutreeStatic(sampleTree, cutHeight = 90000, minSize = 10)
#table(clust)
# clust 1 contains the samples we want to keep.
#keepSamples <- (clust==1)
#datExpr <- datExpr0[keepSamples, ]
#nGenes <- ncol(datExpr)
#nSamples <- nrow(datExpr)

#The variable datExpr now contains the expression data ready for network analysis
datExpr <- datExpr0
nGenes <- ncol(datExpr)
nSamples <- nrow(datExpr)

dim(datExpr)
#Load in the trait data and match the samples for which they were measured to the expression samples
traitData <- read.csv("[name of csv file containing trait data]")
dim(traitData)
names(traitData)

# remove columns that hold information we do not need.
#allTraits <- traitData[, -c(31, 16)];
#allTraits <- allTraits[, c(2, 11:36) ];
#dim(allTraits)
#names(allTraits)

# Form a data frame analogous to expression data that will hold the clinical traits.

MySamples <- rownames(datExpr)
traitRows <- match(MySamples, traitData$Sample)
datTraits <- traitData[traitRows, -1]
head(datTraits)
rownames(datTraits) <- traitData[traitRows, 1]
head(traitData)


collectGarbage()

#We now have the expression data in the variable datExpr, and the corresponding clinical traits in the variable datTraits
#Before we continue with network construction and module detection, visualize how clinical traits relate to sample dendrogram

# Re-cluster samples
sampleTree2 <- hclust(dist(datExpr), method = "average")
# Convert traits to a color representation: white means low, red means high, grey means missing entry
traitColors <- numbers2colors(datTraits, signed = FALSE)     
# Plot the sample dendrogram and the colors underneath.
plotDendroAndColors(sampleTree2, traitColors, groupLabels = names(datTraits), main = "Sample dendrogram and trait heatmap")

#In the plot, shown in Fig. 2, white means a low value, red a high value, and grey a missing entry.
#The last step is to save the relevant expression and trait data for use in the next steps of the tutorial.
save(datExpr, datTraits, file = "Example_dataInput.RData")

##### NETWORK CONSTRUCTION AND MODULE DETECTION #####
### Using a convenient 1-step network construction and module detection function ###

# The following setting is important, do not omit.
options(stringsAsFactors = FALSE)
# Allow multi-threading within WGCNA. This helps speed up certain calculations.
# At present this call is necessary for the code to work.
# Any error here may be ignored but you may want to update WGCNA if you see one.
# Caution: skip this line if you run RStudio or other third-party R environments. 
# See note above.

#enableWGCNAThreads()
# Load the data saved in the first part
lnames <- load(file = "Example_dataInput.RData")
#The variable lnames contains the names of loaded variables.
lnames


# Choose a set of soft-thresholding powers
powers <- c(1:20)
# Call the network topology analysis function
sft <- pickSoftThreshold(datExpr, powerVector = powers, verbose = 5)
# Plot the results:
sizeGrWindow(9, 5)
par(mfrow = c(1,2))
cex1 = 0.9
# Scale-free topology fit index as a function of the soft-thresholding power
plot(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],xlab="Soft Threshold (power)",ylab="Scale Free Topology Model Fit,signed R^2",type="n",main = paste("Scale independence"))
text(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],labels=powers,cex=cex1,col="red")
# this line corresponds to using an R^2 cut-off of h
abline(h=0.81,col="red")
# Mean connectivity as a function of the soft-thresholding power
plot(sft$fitIndices[,1], sft$fitIndices[,5],xlab="Soft Threshold (power)",ylab="Mean Connectivity", type="n",main = paste("Mean connectivity"))
text(sft$fitIndices[,1], sft$fitIndices[,5], labels=powers, cex=cex1,col="red")

#Choose power that is lowest power for which the fit index curve flattens out upon reaching high value (red line)
#power = 4
net = blockwiseModules(datExpr, power = 4,TOMType = "unsigned", minModuleSize = 30,
                       reassignThreshold = 0, mergeCutHeight = 0.25,
                       numericLabels = TRUE, pamRespectsDendro = FALSE,
                       saveTOMs = TRUE,
                       saveTOMFileBase = "Example_TOM",
                       verbose = 3)

#Module size of 30 is large, medium sensitivity = deepSplit=2
#mergeCutHeight = threshold for merging Modules
#net$colors contains module assignment
#net$MEs contains module eigengenes
#parameter maxBlockSize tells function how many genes the largest block can contain. Default is 5000


# Table shows how many modules were identified. Top row is modules, ordered in descending size. 
#Module 0 is reserved for genes outside of all modules - the null module
table(net$colors)

# open a graphics window
sizeGrWindow(12, 9)
# Convert labels to colors for plotting
mergedColors <- labels2colors(net$colors)
# Plot the dendrogram and the module colors underneath
plotDendroAndColors(net$dendrograms[[1]], mergedColors[net$blockGenes[[1]]],
                    "Module colors",
                    dendroLabels = FALSE, hang = 0.03,
                    addGuide = TRUE, guideHang = 0.05)

#Save module assignment and eigengene info 
moduleLabels <- net$colors
moduleColors <- labels2colors(net$colors)
MEs <- net$MEs
geneTree = net$dendrograms[[1]]
save(MEs, moduleLabels, moduleColors, geneTree, 
     file = "Example_networkConstruction_auto.RData")

#### Relating modules to external info and identifying important genes ####
# Load the expression and trait data saved in the first part
lnames = load(file = "Example_dataInput.RData");
#The variable lnames contains the names of loaded variables.
lnames
# Load network data saved in the second part.
lnames = load(file = "Example_networkConstruction_auto.RData");
lnames

#goal: to ID modules that are significantly associated with measured clinical traits
#eigengene = summary profile for each module
#So, simply correlate eigengenes with external traits and look for most significant associations

# Define numbers of genes and samples
nGenes <- ncol(datExpr)
nSamples <- nrow(datExpr)
# Recalculate MEs with color labels
MEs0 <- moduleEigengenes(datExpr, moduleColors)$eigengenes
MEs <- orderMEs(MEs0)
moduleTraitCor <- cor(MEs, datTraits, use = "p")
moduleTraitPvalue <- corPvalueStudent(moduleTraitCor, nSamples)

#Visualize. Columns are traits, rows are module eigengenes
sizeGrWindow(10,6)
# Will display correlations and their p-values
textMatrix <-  paste(signif(moduleTraitCor, 2), "\n(", signif(moduleTraitPvalue, 1), ")", sep = "");
dim(textMatrix) <- dim(moduleTraitCor)
par(mar = c(6, 8.5, 3, 3))
# Display the correlation values within a heatmap plot
labeledHeatmap(Matrix = moduleTraitCor,
               xLabels = names(datTraits),
               yLabels = names(MEs),
               ySymbols = names(MEs),
               colorLabels = FALSE,
               colors = blueWhiteRed(50),
               textMatrix = textMatrix,
               setStdMargins = FALSE,
               cex.text = 0.7,
               zlim = c(-1,1),
               main = paste("Module-trait relationships"))

#Gene relationship to specific trait
#To quantify associations of individual genes with (one) trait of interest, define Gene Significance (GS) as correlation between gene and the trait
#For each module, also define a quantitative measure of module membership (MM) as correlation of eigengene and gene expression profile
#This allows to quantify similarity of ALL genes on the array to every module

# Define variables 
SARS_reads <- as.data.frame(datTraits$SARS_CoV_2reads)
names(SARS_reads) <- "SARS_reads"

# names (colors) of the modules
modNames <- substring(names(MEs), 3)

geneModuleMembership <- as.data.frame(cor(datExpr, MEs, use = "p"))
MMPvalue <- as.data.frame(corPvalueStudent(as.matrix(geneModuleMembership), nSamples))

names(geneModuleMembership) <- paste("MM", modNames, sep="")
names(MMPvalue) <- paste("p.MM", modNames, sep="")

geneTraitSignificance <- as.data.frame(cor(datExpr, SARS_reads, use = "p"))
GSPvalue <- as.data.frame(corPvalueStudent(as.matrix(geneTraitSignificance), nSamples))

names(geneTraitSignificance) <- paste("GS.", names(SARS_reads), sep="")
names(GSPvalue) <- paste("p.GS.", names(SARS_reads), sep="")

#identifying genes with high GS and MM
#If you see strong correlation between GS and MM< this suggests genes highly significantly associated with a trait are often also the most 
#central elements of modeuls associated with the trait
#Choose a module with highest association (r-value) with the trait of interest
module = "turquoise"
column = match(module, modNames)
moduleGenes = moduleColors==module

sizeGrWindow(7, 7);
par(mfrow = c(1,1));
verboseScatterplot(abs(geneModuleMembership[moduleGenes, column]),
                   abs(geneTraitSignificance[moduleGenes, 1]),
                   xlab = paste("Module Membership in", module, "module"),
                   ylab = "Gene significance for SARS-CoV-2 avg reads",
                   main = paste("Module membership vs. gene significance\n"),
                   cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, col = module)

### summary output 
names(datExpr)

### only for one module
names(datExpr)[moduleColors=="turquoise"]

### annotate probe IDs
#load csv file with gene name annotations for ENSG IDs
annot <- read.csv(file = "Gene_name_annotations_noURNA.csv")
dim(annot)
names(annot)
probes <- names(datExpr)
probes2annot <- match(probes, annot$Ensembl)
# The following is the number or probes without annotation:
sum(is.na(probes2annot))
# Should return 0.

#Create data frame holding following information: probe ID, gene symbol module color, gene significance for weight, module membership and p-values for all modules
#Modules will be ordered by significance for weight, most significant on left

# Create the starting data frame
geneInfo0 <- data.frame(Ensembl = probes, geneSymbol = annot$Name[probes2annot],moduleColor = moduleColors,geneTraitSignificance,GSPvalue)
head(geneInfo0)
# Order modules by their significance for weight
modOrder <- order(-abs(cor(MEs, SARS_reads, use = "p")))
# Add module membership information in the chosen order
for (mod in 1:ncol(geneModuleMembership))
{
  oldNames = names(geneInfo0)
  geneInfo0 = data.frame(geneInfo0, geneModuleMembership[, modOrder[mod]], 
                         MMPvalue[, modOrder[mod]]);
  names(geneInfo0) = c(oldNames, paste("MM.", modNames[modOrder[mod]], sep=""),
                       paste("p.MM.", modNames[modOrder[mod]], sep=""))
}
# Order the genes in the geneInfo variable first by module color, then by geneTraitSignificance
geneOrder <- order(geneInfo0$moduleColor, -abs(geneInfo0$GS.SARS_reads))
geneInfo <- geneInfo0[geneOrder, ]
head(geneInfo)

#Write file
write.csv(geneInfo, file = "Example_MODULES.csv")


