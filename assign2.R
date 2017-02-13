library(tidyr, dplyr)
library(fpc)
library(psych)
library(corrplot)
library(Matrix)

skills <- read.table("Assn2_skill sets.csv",sep=",", header=TRUE)
skillset1<-skills[,2:5]
skillset2<-skills[,7:16]
skillset3<-skills[,19:24]
skillset<-cbind(skillset1, skillset2, skillset3)
skillset.cor <- cor(skillset,use="complete.obs")
#skillset <-skills[,2:25]
skillset.cor <- cor(skillset,use="complete.obs")
#skillset.cor <- scale(skillset.cor, center = TRUE)
corrplot(skillset.cor, order="AOE", method="circle", tl.pos="lt", type="upper",        
         tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         sig.level=0.01, insig = "blank")

# since there are missing values in cor, we can replace these values with 0, then we can compute the ranks using built in functions 

rankMatrix(skillset.cor)
# The rank of matrix skillset.cor is 19, not equal to the row number 20. Therefore the rows/variables are not independent from each other. This correlation matrix is not of full rank.

# the 'prcomp' command is asked to read a COV or CORR matrix ("covmat="),
#    which overrides any data matrix named in 1st argument:
ncomp <- 3
skillset.cr <- prcomp(skillset, cor = TRUE, scores = TRUE, covmat = skillset.cor)
skillset.cr$sdev
skillset.cr$sdev^2
skillset.cr$rotation
loadings <- abs(skillset.cr$rotation)
loadings2 <- sweep(loadings, 2, colSums(loadings), "/")

rawLoadings     <- skillset.cr$rotation[,1:ncomp] %*% diag(skillset.cr$sdev, ncomp, ncomp)
rotatedLoadings <- varimax(rawLoadings)$loadings

rotatedLoadings <- as.matrix.data.frame(rotatedLoadings)
rownames(rotatedLoadings)<-rownames(skillset.cor)




print(skillset.cr)    ## prints out eigenvalues
plot(skillset.cr,type='l')   ## scree plot

#from this plot, it indicates there are 4 principle components for this data set
X2<-skillset.cr$rotation[,1:3]  ## prepare to print first two components
plot(X2,pch=rownames(X2))  # this only uses the first letter of each rowname to label points

# R code to read in wechsler data as a correlation matrix, run a PCA
##########################################################################
# the 'prcomp' command is asked to read a COV or CORR matrix ("covmat="),
#    which overrides any data matrix named in 1st argument
# NOTE: "prcomp" uses SVD as estimation method; BUT seems to print varimax rotated solution
########################################################################
# NOTE: "princomp" uses "eigen" as estimation method; seems to print unrotated components
skillset <- na.omit(skillset)
skillset.cr2 <- princomp(skillset,cor = TRUE, scores = TRUE, covmat = skillset.cor)
#skillset.cr2 <- princomp(skillset)
print(skillset.cr2)    ## prints out eigenvalues
plot(skillset.cr2,type='l')   ## scree plot
X2b<-skillset.cr2$loadings[,1:2]  ## prepare to print first two components
plot(X2b,pch=rownames(X2b))  # this only uses the first letter of each rowname to label points

########################################################################
# maybe it's best just to use "eigen" directly, and define the principal components in terms of 
# the eigendecomposition..
# accomplish a PCA via the eigendecomposition routines in R
# function eigen returns eigenvalues (in $values) and eigenvectors (in $vectors)
skillset.eigen<-eigen(skillset.cor)
skillset.eigen
skillsetvalue<-skillset.eigen$values
skillsetvectors<-skillset.eigen$vectors
# create 2x2 diagonal identity matrix
I4<-matrix(c(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1),nrow=4,byrow=TRUE)
# plug sqrt(eigenvalues) into diagonal of identity matrix to create weighting matrix "lamsqrt"
Lamsqrt<-I4
for (i in 1:4) {Lamsqrt[i,i]<-(skillset.eigen$values[i])^.5}
# weight the first two eigenvectors to obtain (the first 2) principal components
skillsetvectors1<-skillsetvectors[,1:4]
P2<-skillsetvectors1%*%Lamsqrt #The operator %*% is used for matrix multiplication. An n by 1 or 1 by n matrix may of course be used as an n-vector if in the context such is appropriate
# now plot the first four principal components
rownames(P2)<-rownames(skillset.cor)
plot(P2,pch=rownames(P2))  # this only uses the first letter of each rowname to label points

I7<-matrix(c(1,0,0,0,0,0,0,
             0,1,0,0,0,0,0,
             0,0,1,0,0,0,0,
             0,0,0,1,0,0,0,
             0,0,0,0,1,0,0,
             0,0,0,0,0,1,0,
             0,0,0,0,0,0,1),nrow=7,byrow=TRUE)
# plug sqrt(eigenvalues) into diagonal of identity matrix to create weighting matrix "lamsqrt"
Lamsqrt<-I7
for (i in 1:7) {Lamsqrt[i,i]<-(skillset.eigen$values[i])^.5}
# weight the first two eigenvectors to obtain (the first 2) principal components
skillsetvectors2<-skillsetvectors[,1:7]
P2<-skillsetvectors2%*%Lamsqrt #The operator %*% is used for matrix multiplication. An n by 1 or 1 by n matrix may of course be used as an n-vector if in the context such is appropriate
# now plot the first four principal components
rownames(P2)<-rownames(skillset.cor)
plot(P2,pch=rownames(P2))  # this only uses the first letter of each rowname to label points

I8<-matrix(c(1,0,0,0,0,0,0,0,
             0,1,0,0,0,0,0,0,
             0,0,1,0,0,0,0,0,
             0,0,0,1,0,0,0,0,
             0,0,0,0,1,0,0,0,
             0,0,0,0,0,1,0,0,
             0,0,0,0,0,0,1,0,
             0,0,0,0,0,0,0,1),nrow=8,byrow=TRUE)
# plug sqrt(eigenvalues) into diagonal of identity matrix to create weighting matrix "lamsqrt"
Lamsqrt<-I8
for (i in 1:8) {Lamsqrt[i,i]<-(skillset.eigen$values[i])^.5}
# weight the first two eigenvectors to obtain (the first 2) principal components
skillsetvectors3<-skillsetvectors[,1:8]
P2<-skillsetvectors3%*%Lamsqrt #The operator %*% is used for matrix multiplication. An n by 1 or 1 by n matrix may of course be used as an n-vector if in the context such is appropriate
# now plot the first four principal components
rownames(P2)<-rownames(skillset.cor)
plot(P2,pch=rownames(P2))  # this only uses the first letter of each rowname to label points

P2 <- as.data.frame(P2)
C1 <-  dplyr::select (P2, 1:1)
C2 <-  dplyr::select (P2, 2:2)
C3 <-  dplyr::select (P2, 3:3)
