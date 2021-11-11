library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(sqldf) # SQL library


data <- read.csv("/Users/nehakhatri/Documents/Hackathon_2021/sample_sales_data.csv")


create_cluster<-function(df,cluster){
  df1 <- df[-c(1)]
  df2 <- scale(df1) # Scaling the data
  # Number of clusters
  # Elbow method
  fviz_nbclust(df2, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

  # Compute k-means with k = 3 as a base line
  set.seed(1234)
  km.res <- kmeans(df2, cluster, nstart = 25)
  aggregate(df1, by=list(cluster=km.res$cluster), mean)
  dd <- cbind(df, cluster = km.res$cluster)
  
  #Save plot to user's computer
  pdf(file = "/Users/nehakhatri/Documents/Hackathon_2021/clustering_plot3.pdf",   # The directory you want to save the file in
      width = 4, # The width of the plot in inches
      height = 4) # The height of the plot in inches
  
  fviz_cluster(km.res, data = df1,
               geom="point",
               palette = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07", "#F4A582"),
               ggtheme = theme_minimal(),
               main = "Partitioning Clustering Plot"
  )
  
  dev.off()
  
  return(head(dd))
}
