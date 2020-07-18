## Principal component analysis
library(ggplot2)

# Create fake dataset
data.matrix <- matrix(nrow = 100, ncol = 10)

# Name columns
colnames(data.matrix) <- c(
        paste("wt", 1:5, sep=""),
        paste("ko", 1:5, sep="")
)

# Name rows
rownames(data.matrix) <- paste("gene", 1:100, sep="")

# Generate values for columns
for (i in 1:100) {
        wt.values <- rpois(5, lambda=sample(x=10:1000, size=1))
        ko.values <- rpois(5, lambda=sample(x=10:1000, size=1))
        
        data.matrix[i,] <- c(wt.values, ko.values)
}

# Show data frame
head(data.matrix)

# Show dimensions of data frame
dim(data.matrix)

# Call prcomp() to do PCA on the data
pca <- prcomp(t(data.matrix), scale = TRUE)

# Plot the principal component graph
plot(pca$x[,1], pca$x[,2])

# Calculate percent of variation in the original data PC1 accounts for
# and plot findings
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
barplot(pca.var.per, main = "Scree Plot", xlab = "Principal Component",
        ylab = "Percent Variation")

# Use ggplot2 to create more visually appealing graph of data
pca.data <- data.frame(Sample = rownames(pca$x),
                       X = pca$x[,1],
                       Y = pca$x[,2])
ggplot(data = pca.data, aes(x = X, y = Y, label = Sample)) +
        geom_text() +
        xlab(paste("PC1 - ", pca.var.per[1], "%", sep = "")) +
        ylab(paste("PC2 - ", pca.var.per[2], "%", sep = "")) +
        theme_bw() +
        ggtitle("My PCA Graph")

# Use loading scores to see which genes have largest effect on 
# where samples are plotted in the PCA plot

# The prcomp() function calls the loading scores "rotation"
loading_scores <- pca$rotation[,1]

# Use ab() to calculate absolute value and sort values based on 
# magnitude rather than high or low values
gene_scores <- abs(loading_scores)

# Call names of top 10 genes with largest loading score magnitudes
gene_score_ranked <- sort(gene_scores, decreasing = TRUE)
top_10_genes <- names(gene_score_ranked[1:10])

top_10_genes

