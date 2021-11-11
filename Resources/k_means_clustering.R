library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(sqldf) # SQL library


data <- read.csv("/Users/nehakhatri/Documents/Hackathon_2021/sample_sales_data.csv")

create_elbow_plot<-function(df){
  df1 <- df[-c(1)]
  df2 <- scale(df1) # Scaling the data
  # Number of clusters
  # Elbow method
  elbow_cluster_plot<- fviz_nbclust(df2, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")
  return(elbow_cluster_plot)
}

create_cluster<-function(df,cluster){
  df1 <- df[-c(1)]
  df2 <- scale(df1) # Scaling the data
  # Compute k-means with k = 3 as a base line
  set.seed(1234)
  km.res <- kmeans(df2, cluster, nstart = 25)
  
  aggregate(df1, by=list(cluster=km.res$cluster), mean)
  
  dd <- cbind(df, cluster = km.res$cluster)
  
  
  cluster_plot <- fviz_cluster(km.res, data = df1,
               geom="point",
               palette = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07", "#F4A582"),
               ggtheme = theme_minimal(),
               main = "Partitioning Clustering Plot"
  )

  return(list(cluster_plot, dd))
}
