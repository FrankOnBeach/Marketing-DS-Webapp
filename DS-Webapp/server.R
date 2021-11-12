library(shiny)
library("stlplus")
library(shinymaterial)
library(forecast)
library(xts)
library(forecast)
library(dplyr)
library(CausalImpact)
library(ggplot2)
library(lubridate)
library(stringr)

# Time Series Forecasting Helper Functions
not_sel <- "Not Selected"
fix_date_format<-function(date_col,date_format){
  # This method is used to correctly format the date column
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

convert_start_date<-function(start,format){
  d <- try(as.Date(start, format="%Y-%m-%d"))
  if("try-error" %in% class(d) || is.na(d)) {
    start_stamp = c(as.numeric(unlist(str_split(start, " "))[1]),as.numeric(unlist(str_split(start, " "))[2]))
    return(start_stamp)
  }
  else{
    if(format %in% c("%m","%V")){
      start_stamp = c(as.numeric(strftime(start, format = "%Y")),as.numeric(strftime(start, format = format)))
    }
    else{
      start_stamp = c(as.numeric(strftime(start, format = "%Y")),substr(quarters(as.Date(start)), 2, 2))
    }
    return(start_stamp)
  }
}


transform_to_time_series<-function(value_col,date_col,frequency,start,end){

  
  # This function transforms data column to time series
  if(frequency=="day"){
    start_date=as.integer(strsplit(as.character(min(date_col, na.rm = TRUE)),"-")[[1]])
    end_date=as.integer(strsplit(as.character(max(date_col, na.rm = TRUE)),"-")[[1]])
    myts <- ts(value_col, start=start_date, end=end_date, frequency=365)
  }
  else if(frequency=="week"){
    start_stamp=convert_start_date(start,"%V")
    print(start_stamp)
    myts <- ts(value_col, start=start_stamp, frequency=52)
  }
  else if(frequency=="month"){
    start_stamp=convert_start_date(start,"%w")
    myts <- ts(value_col, start=start_stamp, frequency=12)
    # myts <- ts(value_col, frequency=12)
  }
  else if(frequency=="quarter"){
    start_stamp=convert_start_date(start,"q")
    myts <- ts(value_col, start=start_stamp, frequency=4)
  }
  return(myts)
  
}

make_forecast_decomposition<-function(myts,period,method){
  # TODO High-Fix the packages used for model#### 
  # TODO Delete models we don't use #### 
  # This function picks the method user selected then perform the action
  # change the models accordingly
  if(method=="ARIMA1"){
    arima_fit<-auto.arima(myts)
    result<-forecast(arima_fit,h=period)
  }
  if(method=="ARIMA2"){
    # With forced seasonality
    arima_fit<-auto.arima(myts,D=1)
    result<-forecast(arima_fit,h=period)
  }
  else if(method=="STL"){
    result <- stl(myts, s.window = "periodic", t.window = 13, robust = TRUE)
    result <- mstl(myts)
  }
  else if(method=="STL+ARIMA"){
    fit <- stl(myts, s.window = "periodic", t.window = 13, robust = TRUE)
    result <-forecast(fit,method="arima",h=period)
  }
  return(result)
}

calculate_end_date <- function(start,freq,col_len){
  d <- try(as.Date(start, format="%Y-%m-%d"))
  start_date = as.Date("1899-01-01")
  if("try-error" %in% class(d) || is.na(d)) {
    if(freq=="week"){
      start_date =as.Date(paste(as.numeric(unlist(str_split(start, " "))[1]),as.numeric(unlist(str_split(start, " "))[2]), 1, sep="-"), "%Y-%U-%u")
    }
    if(freq=="month"){
      start_date = as.Date(paste(as.numeric(unlist(str_split(start, " "))[1]),as.numeric(unlist(str_split(start, " "))[2]), 1, sep="-"), "%Y-%m-%d")
    }
    if(freq=="quarter"){
      quarter_date="Q{quarter}/{year}"
      year=unlist(str_split(start, " "))[1]
      quarter=unlist(str_split(start, " "))[2]
      quarter_date=glue(quarter_date)
      start_date = as.Date(as.yearqtr(quarter_date, format = "Q%q/%y"))
    }
  }
  else{
    start_date=start
  }
  if(freq=="week"){
    start_stamp = ymd(as.Date(start_date)) + weeks(col_len)
    # start_stamp = ymd(as.Date(start_date)) + weeks(2)
    return(start_stamp)
  }
  if(freq=="month"){
    start_stamp = ymd(as.Date(start_date)) %m+% months(col_len)
    return(start_stamp)
  }
  if(freq=="quarter"){
    start_stamp = ymd(as.Date(start_date)) %m+% months(3*(col_len-1))
    return(start_stamp)
  }
}

return_forecasted_value <- function(result,freq,date_col,period,start_date){
  if(freq=="day"){
    start_date=max(as.Date(date_col), na.rm = TRUE) + 1
  }
  else{
    col_len = length(date_col)
    start_date=calculate_end_date(start_date,freq,col_len)
  }
  forecasted_date = seq(start_date,by = freq,length.out = period)
  result_df = as.data.frame(result)
  colnames(result_df)[1] <-"Date"
  result_df$Date = as.character(forecasted_date)
  row.names(result_df) <- NULL
  return(result_df)
}


col_name_fix<-function(df,date_col){
  i=1
  for(col in colnames(df)){
    if(grepl(date_col, col, fixed=TRUE)){
      colnames(df)[i]<-date_col
      return(df)
    }
  }
}

# Causal Impact Helper Functions
run_causal_impact <- function(ci_df,impact_point){
  pre.period = c(1,impact_point)
  post.period <- c(impact_point+1,nrow(ci_df))
  impact_result <- CausalImpact(ci_df, pre.period, post.period)
  return(impact_result)
}


shinyServer(function(input, output) {
  # Forecasting
  inFile_forecasting<- eventReactive(input$submit_forecasting,{input$file_forecasting})
  model_forecasting <- eventReactive(input$submit_forecasting, {
    
    req(input$file_forecasting)
    
    ts_df <- read.csv(inFile_forecasting()$datapath,
                          header = input$header_forecasting,
                          sep = input$sep_forecasting,
                          quote = input$quote_forecasting)
    ts_df = col_name_fix(ts_df,input$date_field_forecasting)

    # If col name has space, replace the space with period

    date_col = ts_df[[input$date_field_forecasting]]
    value_col = as.numeric(gsub(",", "", ts_df[[input$value_field_forecasting]]))

    if(input$frequency_forecasting=="day"){
      fixed_date_col = fix_date_format(date_col,input$date_format_forecasting)
      myts = transform_to_time_series(value_col,fixed_date_col,input$frequency_forecasting,input$start_date_forecasting)
    } 
    else{
      fixed_date_col=date_col
      myts = transform_to_time_series(value_col,date_col,input$frequency_forecasting,input$start_date_forecasting)
    }
    result = make_forecast_decomposition(myts,input$period_forecasting,input$method_forecasting)
    plot <- plot(result)
    
    
    render_df <- data.frame(as.character(date_col),value_col)
    # names(render_df) <- c(input$date_field,input$no_space_value_field)
    names(render_df) <- c(input$date_field_forecasting,input$value_field_forecasting)
    
    # Return accuracy
    
    if(input$method_forecasting %in% c("ARIMA1","ARIMA2","STL+ARIMA")){
      model_accuracy <- accuracy(result)
      forecast_df <- return_forecasted_value(result,input$frequency_forecasting,fixed_date_col,input$period_forecasting,input$start_date_forecasting)
    } 
    else{
      model_accuracy <- "Not a forcasting model"
      forecast_df <-ts_df[FALSE,]
    }
    list(plot = plot,model_accuracy = model_accuracy ,render_df = render_df,forecast_df = forecast_df)
  })
  
  output$main_plot_forecasting <- renderPlot({
    model_forecasting()$plot
  },height = 675, width = 1150 )
  
  output$model_accuracy_forecasting <- renderTable({
    model_forecasting()$model_accuracy
  })
  
  output$title_forecasting<-renderUI({
    HTML("Model Accuracy")
  })
  
  output$main_table_forecasting <- DT::renderDataTable({
    model_forecasting()$render_df
  })
  
  output$downloadData_forecasting <- downloadHandler(
    filename = function() {
      "forecast_result.csv"
    },
    content = function(fname) {
      write.csv(model_forecasting()$forecast_df, fname)
    }
  )
  # Causal Impact
  inFile_ci<- eventReactive(input$submit_ci,{input$file_ci})
  model_ci <- eventReactive(input$submit_ci, {
    
    req(input$file_ci)
    
    ci_df <- read.csv(inFile_ci()$datapath,
                      header = input$header_ci,
                      sep = input$sep_ci,
                      quote = input$quote_ci)
    impact_field_col = input$impact_field_ci
    impact_point = as.numeric(input$impact_point_ci)

    
    # Reordering the columns since the causal col has to be in the front
    cols = names(ci_df)
    print(impact_field_col)
    # cols = c(impact_field_col,cols[cols!=impact_field_col])
    # print(cols)
    # print(names(ci_df))
    # ci_df = ci_df[cols]
    
    # ci_df[] <- lapply(ci_df, function(x) as.numeric(as.character(x)))
    ci_df <- zoo(ci_df)
    ci_result = run_causal_impact(ci_df,impact_point)
    ci_plot <-plot(ci_result)
    list(ci_plot = ci_plot,ci_result = ci_result$report)
  })
  
  output$main_plot_ci<- renderPlot({
    model_ci()$ci_plot
  },height = 400, width = 1150 )
  
  # output$results_ci <- renderTable({
  #   model_ci()$ci_result
  # })
  
  output$results_ci<-renderUI({
    HTML(model_ci()$ci_result)
  })
  
})




