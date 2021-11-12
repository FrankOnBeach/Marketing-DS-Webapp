library(shiny)
library("stlplus")
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization



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
  
  return(list(cluster_plot=cluster_plot, sample_output=head(dd), final_output=dd))
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  inFile <- eventReactive(input$submit_cl,{input$file1_cl})
  
  model_cl <- eventReactive(input$submit_cl, {
    
    req(input$file1_cl)
    
    cluster_df <- read.csv(inFile()$datapath,
                      header = input$header_cl,
                      sep = input$sep_cl,
                      quote = input$quote_cl)
    
    
    elbow_plot = create_elbow_plot(cluster_df)
    
    cluster = create_cluster(cluster_df,input$cluster_number)
    
    plot_cl <- cluster$cluster_plot
    sample_output <- cluster$sample_output
    final_output <- cluster$final_output
    
    list(elbow_plot = elbow_plot, plot_cl = plot_cl, sample_output =sample_output, final_output=final_output )
    #list(elbow_plot = elbow_plot, plot_cl = plot_cl,  final_output=final_output )
    #list(plot = plot,model_accuracy = model_accuracy ,render_df = render_df,forecast_df = forecast_df)
  })
  
  output$elbow_plot <- renderPlot({
    model_cl()$elbow_plot
  },height = 300, width = 800 )
  
  output$title_elbow_plot<-renderUI({
    HTML("Number of Clusters")
  })
  output$sample_output <- renderDataTable({
    model_cl()$sample_output
  })
  
  output$title_sample_output<-renderUI({
    HTML("Sample Cluster Table")
  })
  
  output$plot_cl <- renderPlot({
    model_cl()$plot_cl
  },height = 475, width = 800 )
  
  output$title_plot_cl<-renderUI({
    HTML("Cluster Plot")
  })
  output$downloadData_cl <- downloadHandler(
    filename = function() {
      "cluster_table.csv"
    },
    content = function(fname) {
      write.csv(model_cl()$final_output, fname)
    }
  )
  
})
