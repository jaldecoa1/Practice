## Drawing ROC graphs and calculate AUC 
library(pROC)
library(randomForest)

# Set seed for the random number generator for reproducibility
set.seed(420)

# Create dataset
num.samples <- 100
weight <- sort(rnorm(n = num.samples, mean = 172, sd = 29))

obese <- ifelse(test = (runif(n = num.samples) < (rank(weight)/100)),
                yes = 1, no = 0)

# Plot data
plot(x = weight, y = obese)

# Use glm function to fit logistic regression curve to data
glm.fit = glm(obese ~ weight, family = binomial)

# Pass weight and the fitted.values stored in glm.fit into the 
# lines() function to draw a curve that tells us the predicted
# probability that an individual is obese or not obese
lines(weight, glm.fit$fitted.values)

# Draw ROC graph and remove padding on side of graph
par(pty = "s")
roc(obese, glm.fit$fitted.values, 
    plot = TRUE, 
    legacy.axes = TRUE,
    percent = TRUE,
    xlab = "False Positive Percentage", 
    ylab = "True Positive Percentage",
    col = "#377eb8", 
    lwd = 4,
    print.auc = TRUE)

# Focus on a specific threshold by saving the calculations that the 
# roc() function does in a variable and then make a data.frame that 
# contains all of the True Positive Percentages, by multiplying the
# Sensitivities by 100 and the False Positive Percentages, by
# multiplying 1 - Specificities by 100, and designate the threshold
roc.info <- roc(obese, glm.fit$fitted.values, legacy.axes = TRUE)

roc.df <- data.frame(
        tpp = roc.info$sensitivities * 100,
        fpp = (1 - roc.info$specificities) * 100,
        thresholds = roc.info$thresholds
)

# Isolate the TPP, the FPP and the thresholds used when the True 
# Positive Rate is between 60 and 80
roc.df[roc.df$tpp > 60 & roc.df$tpp < 80,]

## Highlight area under the curve
roc(obese, glm.fit$fitted.values, 
    plot = TRUE, 
    legacy.axes = TRUE,
    percent = TRUE,
    xlab = "False Positive Percentage", 
    ylab = "True Positive Percentage",
    col = "#377eb8", 
    lwd = 4,
    print.auc = TRUE,
    print.auc.x = 60,
    partial.auc = c(100, 90),
    auc.polygon = TRUE,
    auc.polygon.col = "#377eb822")

## Overlap two ROC curves for comparison 

# Create random forest 
rf.model <- randomForest(factor(obese) ~ weight)

# Draw original ROC curve for the logistic regression
roc(obese, glm.fit$fitted.values, 
    plot = TRUE, 
    legacy.axes = TRUE,
    percent = TRUE,
    xlab = "False Positive Percentage",
    ylab = "True Positive Percentage",
    col = "#377eb8",
    lwd = 4,
    print.auc = TRUE)

# Add ROC curve for Random Forest
plot.roc(obese, rf.model$votes[,1],
         percent = TRUE,
         col = "#4daf4a",
         lwd = 4,
         print.auc = TRUE,
         add = TRUE,
         print.auc.y = 40)

# Draw a legend in the graph
legend("bottomright", 
       legend = c("Logistic Regression", "Random Forest"),
       col = c("#337eb8", "#4daf4a"), lwd = 4)


