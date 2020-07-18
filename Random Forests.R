# Random forests
library(ggplot2)
library(randomForest)

# Get data
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
data <- read.csv(url, header = FALSE)

# Clean up data
colnames(data) <- c(
        "age",
        "sex",
        "cp",
        "trestbps",
        "chol",
        "fbs",
        "restecg",
        "thalach",
        "exang",
        "oldpeak",
        "slope",
        "ca",
        "thal",
        "hd"
)

data[data == "?"] <- NA

data[data$sex == 0,]$sex <- "F"
data[data$sex == 1,]$sex <- "M"
data$sex <- as.factor(data$sex)

data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)

data$ca <- as.integer(data$ca)
data$ca <- as.factor(data$ca)

data$thal <- as.integer(data$thal)
data$thal <- as.factor(data$thal)

data$hd <- ifelse(test=data$hd == 0, yes="Healthy", no="Unhealthy")
data$hd <- as.factor(data$hd)

# Since we're going to be randomly sampling, set the 
# set the seed for the random number generator so that
# we can reporduce our results.
set.seed(42)

# Impute values for the NAs in the dataset with rfImput()
# This number should get smaller if our estimates are improving.
# Since they don't, we can assume that it's as good as it's
# going to get.
data.imputed <- rfImpute(hd ~ ., data = data, iter = 6)
        
# Build random forest with randomForest() function
model <- randomForest(hd ~ ., data=data.imputed, proximity = TRUE)
        
# To see if 500 trees is enough for optimal classification
# we can plot the error rate
oob.error.data <- data.frame(
        Trees=rep(1:nrow(model$err.rate), times=3),
        Type=rep(c("OOB", "Healthy", "Unhealthy"), each=nrow(model$err.rate)),
        Error=c(model$err.rate[,"OOB"], 
                model$err.rate[,"Healthy"], 
                model$err.rate[,"Unhealthy"]))
        
ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
        geom_line(aes(color=Type))        
        
## Will adding more trees reduce the error rate? To test this
## we will create a random forest with more trees.

model <- randomForest(hd ~ ., data = data.imputed, ntree = 1000, proximity = TRUE)

# Plot error rates
ob.error.data <- data.frame(
        Trees=rep(1:nrow(model$err.rate), times=3),
        Type=rep(c("OOB", "Healthy", "Unhealthy"), each=nrow(model$err.rate)),
        Error=c(model$err.rate[,"OOB"], 
                model$err.rate[,"Healthy"], 
                model$err.rate[,"Unhealthy"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
        geom_line(aes(color=Type))

## Now we need to make sure we are considering the optimal
## number of variables at each internal node in the tree

# Create an empty vector that can hold 10 values
oob.values <- vector(length = 10)

# Create a loop that tests different numbers of variables 
# at each step.
for(i in 1:10) {
        temp.model <- randomForest(hd ~ ., data=data.imputed, mtry=i, ntree=1000)
        oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}


## Create an MDS plot
distance.matrix <- dist(1-model$proximity)

mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)

mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

# Format MDS data for ggplot2 and plot graph
mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X = mds.values[,1],
                       Y = mds.values[,2],
                       Status = data.imputed$hd)

ggplot(data = mds.data, aes(x = X, y = Y, label = Sample)) +
        geom_text(aes(color = Status)) +
        theme_bw() +
        xlab(paste("MDS1 - ", mds.var.per[1], "%", sep = "")) +
        ylab(paste("MDS2 - ", mds.var.per[2], "%", sep = "")) +
        ggtitle("MDS plot using (1 - Random Forest Proximities)")






