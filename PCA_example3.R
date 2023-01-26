#https://dataaspirant.com/2017/09/01/perform-principal-component-analysis-r/

#The iris dataset having 150 observations (rows) with 4 features.
summary(iris)

# Taking the numeric part of the IRIS data
data_iris <- iris[1:4]

#Let's use the cov() function to calculate the covariance matrix of the loaded iris data set.
# Calculating the covariance matrix
Cov_data <- cov(data_iris )
print(Cov_data)

# Calculate eigenvectors and eigenvalues using the covariance matrix
#We can use the eigen() function to do this automatically for us.
Eigen_data <- eigen(Cov_data)
print(Eigen_data$values)
print(Eigen_data$vectors)

#Alternatively, we can look at the PCA function princomp() which automatically calculates these values.
# Using the inbuilt function princomp()
#Let's now compare the Eigen value and Eigen vectors from the 2 methods used

PCA_data <- princomp(data_iris ,cor="False")
PCA_data$sdev^2
PCA_data$loadings[,1:4]


# us now understand our model. We transformed our 4 features into 4 new orthogonal components. 
# To know the importance of the first component, we can view the summary of the model.
#From the Proportion of Variance, we see that the first component has an importance of 92.5% in predicting the class while the second principal component has an importance of 5.3% and so on. This means that using just the first component instead of all the 4 features will make our model accuracy to be about 92.5% while we use only one-fourth of the entire set of features.
#If we want the higher accuracy, we can take the first two components together and obtain a cumulative accuracy of up to 97.7%.

summary(PCA_data)
biplot(PCA_data)

#The X-Axis represents the first principal component. 
#We see that the Petal Width and Petal Length vectors are parallel to the X-Axis. 
#Hence they are combined and completely transformed into the first principal component. 
#The first component also contains some part of Sepal Length and Sepal Width.
#The vertical part of the Sepal Length and Sepal Width Vectors are explained by the second principal component.

#The screeplot() function in R plots the components joined by a line. We look at the plot and find the point of 'arm-bend'. 
#This is the point where the cumulative contribution starts decreasing and becomes parallel to the x-axis.
#This plot shows the bend at the second principal component.

screeplot(PCA_data, type="lines")

#Let us now fit two naive Bayes models.

  #one over the entire data.
  #The second on the first principal component.

model2 = PCA_data$loadings[,1]
#For the second model, we need to calculate scores by multiplying our loadings with the data
model2_scores <- as.matrix(data_iris) %*% model2

#Loading libraries for naiveBayes model
library(class)
library(e1071)

#Fitting the first model over the entire data
mod1<-naiveBayes(iris[,1:4], iris[,5])
#Fitting the second model using the first principal component
mod2<-naiveBayes(model2_scores, iris[,5])

# Accuracy for the first model
table(predict(mod1, iris[,1:4]), iris[,5])

# Accuracy for the second model
table(predict(mod2, model2_scores), iris[,5])
  
#We can see that there is a difference of 3 predictions between the two models. In return for reducing our data to one-fourth of the original, we lose on the accuracy only slightly. 
#Hence it is a great tradeoff.