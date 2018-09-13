library(factoextra)
library(readxl)
BreastTissue <- read_excel("Portfolio/BreastTissue.xlsx",   #reading in our data 
                           sheet = "Data")                  #publicly provided by the machine learning library @ UCI

summary(BreastTissue)                                       #a quick summary of all our variables

#Our classification groups! Expanding them for clarity sake when it comes to our visual
BreastTissue$Class[BreastTissue$Class == "car"] <- "Carcinoma"
BreastTissue$Class[BreastTissue$Class == "fad"] <- "Fibro-adenoma"
BreastTissue$Class[BreastTissue$Class == "mas"] <- "Mastopathy"
BreastTissue$Class[BreastTissue$Class == "gla"] <- "Glandular"
BreastTissue$Class[BreastTissue$Class == "con"] <- "Connective"
BreastTissue$Class[BreastTissue$Class == "adi"] <- "Adipose"

#code to conduct PCA. important to scale your variables.
PCanalysis <- prcomp(BreastTissue[,3:11], scale = TRUE)

#screeplot of out PCA analysis. This was a purposely chosen example to show desirable traits of PCA
#most times it is rare to have so much of variation (80%) explained in jsut first two PCAs
fviz_eig(PCanalysis)

#this is a plot of how correlated our indiviudal variables are the the 1st and 2nd component.
#the farther along the axis a variable is, the more that variable contributes to the PC.
#For example PA500 and HFS contribute a lot to PC2 but not as much to PC1
fviz_pca_var(PCanalysis,
             col.var = "contrib", # Color by contributions to the PC
             repel = TRUE)

#This is probably the most exciting and important plot in PCA. It can show us if our PCA reveals structural differences
#in the classes of our data (given our goal is classification). Here four distinct groups can be seen.
#Adipose, carcinoma, and connective tissue are most definitely structurally different and identifiable. 
#More analysis and thorough classification techniques will need to be done to seperate Fibro, Glandular, and Mastopathy.
#Since this data is actually quite small, a clustering technique could have produced similar, if not better results!
fviz_pca_ind(PCanalysis, 
             col.ind = BreastTissue$Class,
             addEllipses = TRUE,
             ellipse.type = "confidence",
             legend.title = "Breast Tissue Type",
             labels = FALSE)
