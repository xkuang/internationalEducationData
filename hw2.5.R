library(cluster)
library(Matrix)

P <- matrix(c(97,4,4,7,2,
    9,87,8,37,9,
    8,16,93,12,12,
    11,59,17,96,12,
    9,15,26,12,86),nrow=5,byrow=TRUE)

# the following code puts elements of the lowerhalf vector P into the full matrix D
#symmetry the matrix
#method 1
S <- forceSymmetric(P)
#method 2
P[lower.tri(P)] = t(P)[lower.tri(P)]

DS <- as.matrix(daisy(P, metric = "euclidean", stand = FALSE))
# Now that we can program in R, let's wite code to check the triangle inequality (TI) for all triples
# of points. Then fix any violations of TI by adding a constant c to all entries.
# m = size of matrix (# of stimuli).
m<-4; minc<-0  # note you can put two R statements on one line - separate them with semicolon
for (k in 3:m)
{ for (j in 2:(k-1)) 
{ for (i in 1:(j-1)) 
{ i;j;k
  c1<-D[i,j]+D[j,k]-D[i,k]
  c2<-D[j,k]+D[i,k]-D[i,j] 
  c3<-D[i,j]+D[i,k]-D[j,k]
  c <- min(c1,c2,c3,c4,c5)  
  if (c<0) minc<-min(c,minc) }}}
# if minc<0, then the TI is violated, by abs(c)
C<-matrix(numeric(16),4,4)
C<-C+abs(minc)   # (matrix + scalar) adds the scalar elementwise to the matrix
D
C
Delta<-D+C
for (i in 1:m) Delta[i,i]=0   # put 0s on diagonal of Delta
Delta
DeltaSq<-Delta^2
DeltaSq

# now compute the row / col means and the grand mean
aveDsq<-c(1:4)
for (i in 1:m) aveDsq[i]<-mean(DeltaSq[i,])
m<-4
aveDsq
grmean<-mean(aveDsq[])
grmean

# now we can define matrix B*, the quasi-scalar products matrix
B<-matrix(numeric(16),4,4)
for (i in 1:m)
{ for (j in 1:m) 
{ B[i,j] <- -0.5*(DeltaSq[i,j]-aveDsq[i]-aveDsq[j]+grmean)
}}
B

# now factor matrix B*; start with eigendecomposition
# Function "eigen" puts eigenvalues into object "values", eigenvectors into "vectors"
Bcomp<-eigen(B)
Bcomp

#define principal components (use first two only)
wts<-matrix(numeric(4),2,2)
for (i in 1:2) wts[i,i]<-sqrt(Bcomp$values[i])  
evec<-Bcomp$vectors[,1:2]
wts
evec
P<-evec%*%wts
P

# plot the final 2-dim configuration
plot(P)
points<-c("a","b","c","d")  # prepare to label points
text(P,points)

# note that the obtained configuration matrix (X) is close to the one we used 
# to generate the proximity "data".

# however, the large negative eigenvalue (#4) is perhaps problematic.  Using a larger additive 
# constant will generally increase all the eigenvalue, so it might help.  But figuring out the 
# best constant to use is the "additive constant problem". There has been recent work on this.