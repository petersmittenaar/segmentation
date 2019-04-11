# Simulate data and run k-means and hierarchical clustering
# 
# This code serves to provide some simple examples of k-means in an idealized and 
# more realistic setting.

# install packages if not available on system
install.packages2 = function(package.name) {
  if (!require(package.name, character.only = TRUE)) install.packages(package.name)
  library(package.name, character.only = TRUE)
}
install.packages2('tibble')
install.packages2('dplyr')
install.packages2('mclust')
install.packages2('ggplot2')

# Function definitions

GenerateClusteredData = function(
  cluster.centres.x = c(1, 2),
  cluster.centres.y = c(1, 2),
  cluster.n         = c(150, 150),
  cluster.sds.x     = c(0.2, 0.2),
  cluster.sds.y     = c(0.2, 0.2)
) {
  df = tibble::tibble(x=numeric(0), y=numeric(0), true.clust=numeric(0))
  for (cl.ix in seq_len(length(cluster.centres.x))) {
    df = dplyr::bind_rows(df, tibble::tibble(
      x=rnorm(cluster.n[cl.ix], cluster.centres.x[cl.ix], cluster.sds.x[cl.ix]),
      y=rnorm(cluster.n[cl.ix], cluster.centres.y[cl.ix], cluster.sds.y[cl.ix]),
      true.clust=rep(cl.ix, cluster.n[cl.ix])
    ))
  }

  return(df)
}

PrettyScatter = function(g) {
  # Take a ggplot object and make it look better for segmentation purposes
  return(
    g + 
      theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 16)
            ) +
      guides(color=FALSE)
  )
}

GenerateAndPlotClusters = function(df, kClusters=length(unique(df$true.clust)), plot.label='', do.dendrogram=TRUE) {
  # Assumes to receive a df with x and y and true.clusters
  
  # Convert to factor just in case
  true.clust = factor(df$true.clust)
  dfcl = dplyr::select(df, x, y)

  # Fit models
  km = kmeans(dfcl, centers = kClusters, nstart = 100)
  htree = hclust(dist(dfcl))
  hcl = cutree(htree, k = kClusters)
  
  print(PrettyScatter(
    ggplot(cbind(dfcl, cluster=factor(km$cluster)), aes(x, y, color=cluster)) + 
      geom_point(alpha=0.5) +
      # add cluster mean
      geom_point(
        data=tibble::rownames_to_column(tibble::as_tibble(km$centers)), 
        aes(x, y, color=rowname), 
        size=15,
        shape='+') +
      labs(title=paste('K-means', plot.label))))
  
  # hcplot = mclust::clPairs(dfcl, hcl, main = paste('Hierarchical clustering', plot.label))
  if (do.dendrogram) {
    plot(htree, labels = FALSE, xlab = '', sub = '', main = paste('Dendrogram', plot.label))
    # abline(h=kClusters, col="red", lty=2)
  }
  
  
  
}

