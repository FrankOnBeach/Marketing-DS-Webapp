library(shiny)
library("stlplus")
library(xts)
library(forecast)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(anomalize)

fix_date_format<-function(date_col,date_format){
  # This method is used to correctly format the date colomn
  if(date_format=="mm/dd/yy"){
    fixed_date_col =format(as.Date(date_col,"%m/%d/%Y"),"20%y-%m-%d")
    fixed_date_col = as.Date(fixed_date_col)
  }
  else if(date_format=="mm/dd/yyyy"){
    fixed_date_col = as.Date(date_col,"%m/%d/%Y")
  }
  else if(date_format=="yyyy-mm-dd"){
    fixed_date_col = as.Date(date_col)
  }
  return(fixed_date_col)
}


anomaly_detection_funct<-function(df,value_col,max_anoms){
  
  df <- as_tibble(df)
  plot_ad = df %>% 
    time_decompose(value_col, method = "stl", frequency = "auto", trend = "auto") %>%
    anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = max_anoms) %>%
    time_recompose() %>%
    plot_anomalies(time_recomposed = TRUE) +
    scale_x_date(labels = date_format("%m-%d-%Y"),date_breaks = "1 week")+
    geom_line()+labs(title=title)
  
  anomaly_tibble <- df %>% time_decompose(value_col, method = "stl", frequency = "auto", trend = "auto") %>% anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = max_anoms)
  anomaly_df<-as.data.frame(anomaly_tibble)
  
  return (list(plot_ad, anomaly_df))
}


shinyServer(function(input, output) {
  inFile <- eventReactive(input$submit_ad,{input$file_ad})
  
  model_ad <- eventReactive(input$submit_ad, {
    
    req(input$file_ad)
    
    df_ad <- read.csv(inFile()$datapath,
                      header = input$header,
                      sep = input$sep,
                      quote = input$quote)
   
    
    date_col_ad = df_ad[[input$date_field_ad]]
    value_col_ad = as.numeric(gsub(",", "", df_ad[[input$value_field_ad]]))
    
    result_ad = anomaly_detection_funct(df_ad,input$value_col_ad, input$max_anoms)
    plot_ad <- plot(result[1])
    anomaly_df <- result[2]
    
  
  output$main_plot_ad <- renderPlot({
    model_ad()$plot_ad
  },height = 675, width = 1150 )
  
  output$title_ad<-renderUI({
    HTML("Anomaly Detection Results")
  })
  
  output$main_table_ad <- DT::renderDataTable({
    model()$anomaly_df
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      "anomaly_detection_result.csv"
    },
    content = function(fname_ad) {
      write.csv(model_ad()$forecast_df_ad, fname_ad)
    })
  
  })
})
 


    
    
    
    