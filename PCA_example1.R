# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/
# Here, we'll use the two packages FactoMineR (for the analysis) 
# and factoextra (for ggplot2-based visualization).
install.packages("factoextra")
install.packages("FactoMineR")
install.packages("corrplot")

#Load them in R:
library("FactoMineR")
library("factoextra")
library("corrplot")

# We'll use the demo data sets decathlon2 from the factoextra package:
decathlon2
data(decathlon2)
head(decathlon2,n=3)

#We start by subsetting active individuals and active variables for the principal component analysis
decathlon2.active <- decathlon2[1:23, 1:10]
head(decathlon2.active[, 1:6], 4)

#Data standardization
#In principal component analysis, variables are often scaled (i.e. standardized). 
#This is particularly recommended when variables are measured in different scales (e.g: kilograms, kilometers, centimeters, .); 
#otherwise, the PCA outputs obtained will be severely affected.
#When scaling variables, the data can be transformed as follow:

# \[ \frac{x_i - mean(x)}{sd(x)} \]

#Note that, by default, the function PCA() [in FactoMineR], standardizes the data automatically during the PCA; so you don't need do this transformation before the PCA.

# The R code below, computes principal component analysis on the active individuals/variables:
library("FactoMineR")
res.pca <- PCA(decathlon2.active, scale.unit=TRUE, ncp = 5, graph = FALSE)
print(res.pca)
#X: a data frame. Rows are individuals and columns are numeric variables
#scale.unit: a logical value. If TRUE, the data are scaled to unit variance before the analysis. This standardization to the same scale avoids some variables to become dominant just because of their large measurement units. It makes variable comparable.
#ncp: number of dimensions kept in the final results.
#graph: a logical value. If TRUE a graph is displayed.

#Eigenvalues / Variances
#Eigenvalues are large for the first PCs and small for the subsequent PCs. 
#That is, the first PCs corresponds to the directions with the maximum amount of variation in the dataset.
#The eigenvalues and the proportion of variances by the principal components (PCs) can be extracted using the function get_eigenvalue()
library("factoextra")
eig.val <- get_eigenvalue(res.pca)
print(eig.val)

#The cumulative percentage explained is obtained by adding the successive proportions of variation explained to obtain the running total. For instance, 41.242% plus 18.385% equals 59.627%, and so forth
#Therefore, about 59.627% of the variation is explained by the first two eigenvalues together.
#Unfortunately, there is no well-accepted objective way to decide how many principal components are enough. This will depend on the specific field of application and the specific data set. In practice, we tend to look at the first few principal components in order to find interesting patterns in the data.

#Scree Plot
#It is the plot of eigenvalues ordered from largest to the smallest. 
#The number of component is determined at the point, beyond which the remaining eigenvalues are all relatively small and of comparable size 
#From the plot , we might want to stop at the fifth principal component. 
#87% of the information (variances) contained in the data are retained by the first five principal components
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

#Variables in PCA
#A simple method to extract the results, for variables, from a PCA output is to use the function get_pca_var()
var <- get_pca_var(res.pca)
print(var)

# Coordinates
head(var$coord)
# Cos2: quality on the factore map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)

#Visualize variables and draw conclusions about their correlations.
#Correlation Circle : The correlation between a variable and a principal component (PC) is used as the coordinates of the variable on the PC. 
#Positively correlated variables are grouped together.
#Negatively correlated variables are positioned on opposite sides of the plot origin (opposed quadrants).
#The distance between variables and the origin measures the quality of the variables on the factor map.

head(var$coord)
fviz_pca_var(res.pca, col.var = "black")

#Quality of representation
#The quality of representation of the variables on factor map is called cos2 (square cosine, squared coordinates).
#A high cos2 indicates a good representation of the variable on the principal component. In this case the variable is positioned close to the circumference of the correlation circle.
#A low cos2 indicates that the variable is not perfectly represented by the PCs. In this case the variable is close to the center of the circle.
#For a given variable, the sum of the cos2 on all the principal components is equal to one.

head(var$cos2)
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)

#Contributions of variables to PCs
#The contributions of variables in accounting for the variability in a given principal component are expressed in percentage.
#Variables that are correlated with PC1 (i.e., Dim.1) and PC2 (i.e., Dim.2) are the most important in explaining the variability in the data set.
#Variables that do not correlated with any PC or correlated with the last dimensions are variables with low contribution and might be removed to simplify the overall analysis.
#The larger the value of the contribution, the more the variable contributes to the component.

head(var$contrib)
library("corrplot")
corrplot(var$contrib, is.corr=FALSE)   

#The function fviz_contrib() [factoextra package] can be used to draw a bar plot of variable contributions. 
# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

#The total contribution to PC1 and PC2 is obtained with the following R code:
#It can be seen that the variables - X100m, Long.jump and Pole.vault - contribute the most to the dimensions 1 and 2.

fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)

#Colour by groups: We start by classifying the variables into 3 groups using the kmeans clustering algorithm
res.km <- kmeans(var$coord, centers = 3, nstart = 25)
grp <- as.factor(res.km$cluster)

# Color variables by groups
fviz_pca_var(res.pca, col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster")

#Dimension description
#The function dimdesc() [in FactoMineR], for dimension description, can be used to identify the most significantly associated variables with a given principal component 
res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
# Description of dimension 1
res.desc$Dim.1
res.desc$Dim.2
