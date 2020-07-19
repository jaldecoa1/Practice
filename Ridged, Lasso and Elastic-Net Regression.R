## Ridged, Lasso and Elastic-Net Regression
library(glmnet)

# Set seed for the random number generator
set.seed(42)

# Generate two separate datasets
# Dataset x will be a matrix of random values
# Dataset y will be dependent on the first 15 columns in x,
# and will also include 1000 random values to create some noise
n <- 1000
p <- 5000
real_p <- 15

x <- matrix(rnorm(n*p), nrow = n, ncol = p)
y <- apply(x[,1:real_p], 1, sum) + rnorm(n)

# Divide data into training and testing sets
# Create a vector of indexes (train_rows) that contains the row
# numbers of the rows that will be in the training set
# and create new matrix (x.train) that just contains Training data.
# Finally, create a training set (x.test) that contains the remaining
# rows.
train_rows <- sample(1:n, .66*n)
x.train <- x[train_rows, ]
x.test <- x[-train_rows, ]

# Select training values in y and save them in "y.train".
# Then select testing values in y and save in "y.test"
y.train <- y[train_rows]
y.test <- y[-train_rows]

######################
## Ridge Regression ##
######################

# Fit model to training data, using cross validation to obtain
# optimal values for lambda.
alpha0.fit <- cv.glmnet(x.train, y.train, type.measure = "mse",
                        alpha = 0, family = "gaussian")

# Use predict() function to apply alpha0.fit to the testing data
alpha0.predicted <- predict(alpha0.fit, 
                            s = alpha0.fit$lambda.1se, 
                            newx = x.test)

# Calculate the mean squared error of the difference between the 
# true values, stored in y.test, and the predicted values, stored
# in alpha0.predicted
mean((y.test - alpha0.predicted)^2)

######################
## Lasso Regression ##
######################

# Call cv.glmnet() to fit a linear regression using 10-fold cross
# validation to determine optimal values for lambda
alpha1.fit <- cv.glmnet(x.train, y.train, type.measure = "mse",
                        alpha = 1, family = "gaussian")

# Call predict() function like in the ridge regression model
alpha1.predicted <- predict(alpha1.fit,
                            s = alpha1.fit$lambda.1se, newx = x.test)

# Calculate the mean squared error
mean((y.test - alpha1.predicted)^2)

## The mean squared error of 1.19 we get with the Lasso Regression is
## smaller than the 14.47 MSE we get with Ridge Regression model. So
## the Lasso Regression is much better with this data than Ridge Regression

############################
## Elastic-Net Regression ##
############################

# Call cv.glmnet() to determine optimal values for lambda, but set alpha
# to 0.5.
alpha0.5.fit <- cv.glmnet(x.train, y.train, type.measure = "mse",
                          alpha = 0.5, family = "gaussian")

# Call predict() funtion 
alpha0.5.predicted <- 
        predict(alpha0.5.fit, s = alpha0.5.fit$lambda.1se, newx = x.test)

# Calculate MSE
mean((y.test - alpha0.5.predicted)^2)

## So far, the Lasso Regression model has the smallest MSE. But to truly know
## if Lasso Regression is the best fit model, we must try different alpha values

# Start by making an empty list called list.of.fits that will store a bunch
# of Elastic-Net Regression fits.
list.of.fits <- list()

# Use a for-loop to try different values for alpha
for (i in 0:10) {
        fit.name <- paste0("alpha", i/10)
        
        list.of.fits[[fit.name]] <-
                cv.glmnet(x.train, y.train, type.measure="mse", alpha=i/10, 
                          family="gaussian")
}

# Create an empty data frame
results <- data.frame()

# Create another for-loop to predict values
for (i in 0:10) {
        fit.name <- paste0("alpha", i/10)
        
        predicted <- 
                predict(list.of.fits[[fit.name]], 
                        s=list.of.fits[[fit.name]]$lambda.1se, newx=x.test)
        
        mse <- mean((y.test - predicted)^2)
        
        temp <- data.frame(alpha=i/10, mse=mse, fit.name=fit.name)
        results <- rbind(results, temp)
}

# Print results
results

## Based on the results, the fit where alpha = 1 is the best result, which
## suggests that Lasso Regression is the best method to use with this data







