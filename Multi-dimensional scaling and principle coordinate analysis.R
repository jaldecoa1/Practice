## Multi-dimensional scaling and Principal coordinate analysis
library(ggplot2)

# Generate fake data 
data.matrix <- matrix(nrow = 100, ncol = 10)

colnames(data.matrix) <- c(
        paste("wt", 1:5, sep = ""),
        paste("ko", 1:5, sep = "")
)

rownames(data.matrix) <- paste("gene", 1:100, sep="")

for (i in 1:100) {
        wt.values <- rpois(5, lambda=sample(x=10:1000, size=1))
        ko.values <- rpois(5, lambda=sample(x=10:1000, size=1))
        
        data.matrix[i,] <- c(wt.values, ko.values)
}

head(data.matrix)
dim(data.matrix)

# Perform PCA on dataset
pca <- prcomp(t(data.matrix), scale = TRUE, center = TRUE)

pca.var <- pca$sdev^2

pca.var.per <- round(pca.var/sum(pca.var)*100, 1)

pca.data <- data.frame(Sample = rownames(pca$x),
                       X = pca$x[,1],
                       Y = pca$x[,2])

# Create PCA plot using ggplot2
ggplot(data = pca.data, aes(x = X, y = Y, label = Sample)) +
        geom_text() +
        xlab(paste("PC1 - ", pca.var.per[1], "%", sep = "")) +
        ylab(paste("PC2 - ", pca.var.per[2], "%", sep = "")) +
        theme_bw() +
        ggtitle("PCA Graph")

## Create an MDS/PCoA plot to compare with the one above

# Create a distance matrix with dist() function
distance.matrix <- dist(scale(t(data.matrix),
                              center = TRUE,
                              scale = TRUE),
                        method = "euclidean")

# Perform multi-dimensional scaling on the distance matrix
# and return eigenvalues
mds.stuff <- cmdscale(distance.matrix,
                      eig = TRUE,
                      x.ret = TRUE)

# Calculate the amount of variation each axis in the MDS plot
# accounts for using the eigenvalues
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)
mds.var.per

# Format data for ggplot2
mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2])
mds.data

# Use ggplot2 to make a graph
ggplot(data = mds.data, aes(x = X, y = Y, label = Sample)) +
        geom_text() +
        theme_bw() +
        xlab(paste("MDS1 - ", mds.var.per[1], "%", sep = "")) +
        ylab(paste("MDS2 - ", mds.var.per[2], "%", sep = "")) +
        ggtitle("MDS plot using Euclidean distance")

## The two graphs look identical because we used the euclidean
## distance metric. Now, let's try using a different metric - 
## i.e., the average of the absolute value of the log fold change

# Calculate the log2 values of the measurements for each gene
log2.data.matrix <- log2(data.matrix)

# Since the average of absolute values of the log-fold change
# isn't one of the distance metrics built into the dist() function<
# we'll create our own distance matrix by hand. First create an 
# empty matrix.
log2.distance.matrix <- matrix(0,
                               nrow=ncol(log2.data.matrix),
                               ncol=ncol(log2.data.matrix),
                               dimnames=list(colnames(log2.data.matrix),
                                             colnames(log2.data.matrix)))

# Fill the matrix with the average of the absolute values of the
# log fold changes.

for(i in 1:ncol(log2.distance.matrix)) {
        for(j in 1:i) {
                log2.distance.matrix[i, j] <-
                        mean(abs(log2.data.matrix[,i] - log2.data.matrix[,j]))
        }
}

# Perform multi-dimensional scaling on our new distance matrix
mds.stuff <- cmdscale(as.dist(log2.distance.matrix),
                      eig = TRUE,
                      x.ret = TRUE)

# Calculate the amount of variation each axis in the MDS plot
# accounts for using the eigenvalues
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

# Format data for ggplot2
mds.values <- mds.stuff$points
mds.data <- data.frame(Sample = rownames(mds.values),
                       X = mds.values[,1],
                       Y = mds.values[,2])

# Create graph using ggplot2
ggplot(data = mds.data, aes(x = X, y = Y, label = Sample)) +
        geom_text() +
        theme_bw() +
        xlab(paste("MDS1 - ", mds.var.per[1], "%", sep = "")) +
        ylab(paste("MDS2 - ", mds.var.per[2], "%", sep = "")) +
        ggtitle("MDS plot using av(logFC) as the distance")


