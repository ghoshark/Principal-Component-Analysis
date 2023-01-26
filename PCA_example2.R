#iris
#http://rstudio-pubs-static.s3.amazonaws.com/374116_4d85d22a4e3742ca91a9daaf63a548da.html
# cov(x,y)=???ni=1(xi???x¯)(yi???y¯)n???1

df <- iris
cv <- cov(df[,-5])
cv
#Manually calculate covariance between variables
var1 <- 1; var2 <- 2
sum(apply(apply(df[,var1:var2], 2, function(x) x - mean(x)),1,prod))/(nrow(df)-1) 

OR
#R calculated covariance between variables
var1 <- 1; var2 <- 2
cv[var1,var2] #R function

Eigenvalues <- eigen(cv)$values
Eigenvectors <- eigen(cv)$vectors
#Manual component loadings
round(Eigenvectors,5)

#prcomp component loadings
round(prcomp(df[,-5])$rotation,5)

Calculation of the PCA data transformation

#Transforming the original data by the principal components rotates the data to make the principal components the new axes
#This is done by multiplying the raw data by the principal components
#PCA-transformed data is often normalized to the mean
#However, this will not change the shape of the data, only the scale of the axes
#PCA-transformation
PC <- as.matrix(df[,-5]) %*% Eigenvectors 
#Normalization
PC <- apply(PC, 2, function(x) x - mean(x))

head(PC)
#prcomp PCA transformation
head(prcomp(df[,-5])$x)


#The variances of the resulting transformation should be equivalent to the eigenvalues of the original covariance matrix
#The covariances of the resulting transformation should all be zero
#This is because the principal components are orthogonal and, thus, uncorrelated

round(cov(PC), 3)
round(Eigenvalues, 3)
