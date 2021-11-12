library(shiny)
library("stlplus")
library(xts)
library(forecast)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(tibbletime)
library(scales)
library(dplyr)
library(anomalize)


detect_plot_anomalies_funct<-function(ts,value_col,alpha,max_anoms){
  print("start")
  
  anomaly_ts = ts %>% 
    time_decompose(value_col, method = "stl", frequency = "auto", trend = "auto") %>%
    anomalize(remainder, method = "gesd", alpha = alpha, max_anoms = max_anoms)
  anomaly_df<-as.data.frame(anomaly_ts)
  print("---------------")
  anomaly_plot = anomaly_ts%>% 
    time_recompose() %>%
    plot_anomalies(time_recomposed = TRUE) +
    scale_x_date(labels = date_format("%m-%d-%Y"),date_breaks = "1 month")+
    geom_line()
  # ggsave(temp_plot,path = file_path,filename=plot_file_name,device = "png",width=20, height=4, dpi=100)
  return(list(plot_ad = anomaly_plot, anomaly_df = anomaly_df))
}


shinyServer(function(input, output) {
  inFile_ad <- eventReactive(input$submit_ad,{input$file_ad})
  
  model_ad <- eventReactive(input$submit_ad, {
    
    req(input$file_ad)
    
    df_ad <- read.csv(inFile_ad()$datapath,fileEncoding = "UTF-8-BOM")

# cols = sub("", "", names(df_ad))
#     colnames(df_ad)<-cols

    date_col = input$date_field_ad
    value_col = input$value_field_ad
    alpha = as.numeric(input$alpha)
    max_anoms = as.numeric(input$max_anoms_ad)
    
    # df_ad$value_col = as.numeric(gsub(",", "", df_ad[[value_col]]))

    
    df_ad[[value_col]] = as.numeric(gsub(",", "", df_ad[[value_col]]))
    df_ad[[date_col]] = mdy(df_ad[[date_col]])
    df_ad = df_ad[c(date_col,value_col)]
    ts = as_tibble(df_ad)
    print(head(df_ad))
    result_ad = detect_plot_anomalies_funct(ts,value_col,alpha,max_anoms)
    anomaly_df = result_ad$anomaly_df
    just_anomaly_df = anomaly_df[anomaly_df[["anomaly"]] == 'Yes',][c(date_col,"observed","remainder","anomaly")]
    list(plot_ad=result_ad$plot_ad,anomaly_df=anomaly_df,just_anomaly_df=just_anomaly_df)

  })
  output$main_plot_ad <- renderPlot({
    model_ad()$plot_ad
  },height = 675, width = 1400 )
  
  output$title_ad<-renderUI({
    HTML("Anomaly Detection Results")
  })
  
  output$main_table_ad <- DT::renderDataTable({
    model_ad()$just_anomaly_df
  })
  
  output$downloadData_ad <- downloadHandler(
    filename = function() {
      "anomaly_detection_result.csv"
    },
    content = function(fname) {
      write.csv(model_ad()$anomaly_df, fname)
    })
  
  })



    
    
    
    