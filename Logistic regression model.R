##Regression analysis
library(ggplot2)

#Import data
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"

data <- read.csv(url, header = FALSE)

#Add column labels
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

#Change question marks to NA
data[data == "?"] <- NA

#Recode sex to 0 = "F", 1 = "M" and change to factor
data[data$sex == 0,]$sex <- "F"
data[data$sex == 1,]$sex <- "M"
data$sex <- as.factor(data$sex)

#Change variables to factors
data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)

#Change variables to intergers, then to factor
data$ca <- as.integer(data$ca)
data$ca <- as.factor(data$ca)

data$thal <- as.integer(data$thal)
data$thal <- as.factor(data$thal)

#Convert hd to factor
data$hd <- ifelse(test = data$hd == 0, yes = "Healthy", no = "Unhealthy")
data$hd <- as.factor(data$hd)

#Check how many rows have NA values
nrow(data[is.na(data$ca) | is.na(data$thal),])
data[is.na(data$ca) | is.na(data$thal),]

#Determine whether to remove the NA samples. Becuase we  have 303 rows of data
#we can remove the six NA samples
nrow(data)
data <- data[!(is.na(data$ca) | is.na(data$thal)),]

#Make sure that healthy and disease samples come from each gender.
#If only males have heart disease, then we should remove all females from model.
xtabs(~ hd + sex, data = data)

#Make sure that all four levels of chest pain (cp) were reported by a bunch
#of patients.
xtabs(~ hd + cp, data = data)

#Do the same thing as above for all the boolean and categorical variables
#we are using to predict heart disease.
xtabs(~ hd + fbs, data = data)
xtabs(~ hd + restecg, data = data)
xtabs(~ hd + exang, data = data)
xtabs(~ hd + slope, data = data)
xtabs(~ hd + ca, data = data)
xtabs(~ hd + thal, data = data)

#Start by building a simple model.
#Try to predict heart disease using only the gender of each patient.
logistic <- glm(hd ~ sex, data = data, family = "binomial")
summary(logistic)

#Build a model including all remaining variables in data
logistic2 <- glm(hd ~ ., data = data, family = "binomial")
summary(logistic2)

#Calculate McFadden's Pseudo R-squared by pulling the log-likelihood of the 
#null model out of the logistic variable by getting the value of the null
#deviance and dividing it by -2
ll.null <- logistic2$null.deviance/-2

#We can pull the log-likelihood for the fancy model out of the logistic variable by
#getting the value for the residual deviance and dividing by -2
ll.proposed <- logistic2$deviance/-2

#Divide the two values to get a Pseudo R-squared, which can be interpreted
#as the effect size
(ll.null - ll.proposed) / ll.null

#We can use those same log-likelihoods to calculate a p-value for that 
#R-squared using a chi-square distribution. Because the p-value is small
#we can assume the R-squared value isn't due to chance
1 - pchisq(2*(ll.proposed - ll.null), df = (length(logistic2$coefficients) - 1))

##Create graph of regression line
#Begin by creating new data frame that contains the probabilities of having
#heart disease along with the actual heart disease status
predicted.data <- data.frame(
        probability.of.hd = logistic2$fitted.values,
        hd = data$hd)

#Sort the data frame from low probabilities to high probablities
predicted.data <- predicted.data[
        order(predicted.data$probability.of.hd, decreasing = FALSE),
]

#Add new column to data frame that has the rank of each sample, from low
#probability to high probability
predicted.data$rank <- 1:nrow(predicted.data)

#Program graph in ggplot2
ggplot(data = predicted.data, aes(x = rank, y = probability.of.hd)) +
        geom_point(aes(color = hd), alpha = 1, shape = 4, stroke = 2) +
        xlab("Index") +
        ylab("Predicted probability of getting heart disease")

ggplot(data = predicted.data, aes(x = rank, y = probability.of.hd)) +
        geom_point(aes(color = hd), alpha = 1, shape = 4, stroke = 2) +
        xlab("Index") + 
        ylab("Predicted probability of getting heart disease")

#Save graph as pdf file
ggsave("heart_disease_probabilities.pdf")


