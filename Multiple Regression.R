# Comparing Simple and Multiple Regression

# Import raw data
mouse.data <- data.frame(
        size = c(1.4, 2.6, 1.0, 3.7, 5.5, 3.2, 3.0, 4.9, 6.3),
        weight = c(0.9, 1.8, 2.4, 3.5, 3.9, 4.4, 5.1, 5.6, 6.3),
        tail = c(0.7, 1.3, 0.7, 2.0, 3.6, 3.0, 2.9, 3.9, 4.0))

## Simple regression ##

# Plot the data to determine whether there is a relationship in
# the data. If yes, then linear regression makes sense
plot(mouse.data$weight, mouse.data$size)

# Use lm function to fit simple linear regression to data
simple.regression <- lm(size ~ weight, data = mouse.data)
summary(simple.regression)

# Plot simple regression line
abline(simple.regression, col = "red", lwd = 2)

## Multiple Regression ## 

# Plot data to see if multilpe regression is appropriate
plot(mouse.data)

# Use lm() function to fit a plane to the data
multiple.regression <- lm(size ~ weight + tail, data = mouse.data)
summary(multiple.regression)








