library(shiny)
library("stlplus")
library(xts)
library(forecast)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

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
    # fit <- mstl(myts)
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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  inFile <- eventReactive(input$submit,{input$file1})
  
  model <- eventReactive(input$submit, {
    
    req(input$file1)
    
    ts_df <- read.csv(inFile()$datapath,
                          header = input$header,
                          sep = input$sep,
                          quote = input$quote)
    ts_df = col_name_fix(ts_df,input$date_field)

    # If col name has space, replace the space with period

    date_col = ts_df[[input$date_field]]
    value_col = as.numeric(gsub(",", "", ts_df[[input$value_field]]))

    if(input$frequency=="day"){
      fixed_date_col = fix_date_format(date_col,input$date_format)
      myts = transform_to_time_series(value_col,fixed_date_col,input$frequency,input$start_date)
    } 
    else{
      fixed_date_col=date_col
      myts = transform_to_time_series(value_col,date_col,input$frequency,input$start_date)
    }
    print(myts)
    result = make_forecast_decomposition(myts,input$period,input$method)
    plot <- plot(result)
    
    
    render_df <- data.frame(as.character(date_col),value_col)
    # names(render_df) <- c(input$date_field,input$no_space_value_field)
    names(render_df) <- c(input$date_field,input$value_field)
    
    # Return accuracy
    
    if(input$method %in% c("ARIMA1","ARIMA2","STL+ARIMA")){
      model_accuracy <- accuracy(result)
      forecast_df <- return_forecasted_value(result,input$frequency,fixed_date_col,input$period,input$start_date)
    } 
    else{
      model_accuracy <- "Not a forcasting model"
      forecast_df <-ts_df[FALSE,]
    }
    list(plot = plot,model_accuracy = model_accuracy ,render_df = render_df,forecast_df = forecast_df)
  })
  
  output$main_plot <- renderPlot({
    model()$plot
  },height = 675, width = 1150 )
  
  output$model_accuracy <- renderTable({
    model()$model_accuracy
  })
  
  output$title<-renderUI({
    HTML("Model Accuracy")
  })
  
  output$main_table <- DT::renderDataTable({
    model()$render_df
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      "forecast_result.csv"
    },
    content = function(fname) {
      write.csv(model()$forecast_df, fname)
    }
  )
  
})




