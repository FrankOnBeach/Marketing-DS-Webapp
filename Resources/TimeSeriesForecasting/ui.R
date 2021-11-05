#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # Application title
    titlePanel("Time Series Forecasting"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Choose CSV File",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            textInput("date_field", "Write down the column name of date field", "Date"),
            textInput("value_field", "Write down the column name of value field. If col name has space, replace the space with period(e.g. XX XX -> XX.XX)", "Sessions"),
            
            
            # Input: Select Data's Date Format ----
            radioButtons("date_format", "Select Data's Date Format",
                         choices = c("Not a date column"= "not_date",
                                     "mm/dd/yy" = "mm/dd/yy",
                                     "mm/dd/yyyy" = "mm/dd/yyyy",
                                     "yyyy-mm-dd" = "yyyy-mm-dd"),
                         selected = "mm/dd/yy"),
            
            # Input: Select Data Format ----
            radioButtons("frequency", "Select Data's Frequency: daily or weekly or monthly?",
                         choices = c("Daily" = "day",
                                     "Weekly" = "week",
                                     "Monthly" = "month",
                                     "Quarterly" = "quarter"),
                         selected = "Daily"),
            
            textInput("start_date", "If a non-daily frequency is chosen, write down the year and the number of week/month/quarter it is of the year. For example: Dec 2020 would be 2020 12 or simply write down the date in yyyy-mm-dd format","1999-09-09"),
            # textInput("end_date", "Write down the end date of your data if a non-daily frequency is chosen. Make sure the date is the last day of the week/month/quarter","1999-09-09"),
            
            
            # # Input: Select Time Series Seasonality ----
            # radioButtons("seasonality", "Select Time Series Seasonality",
            #              choices = c("Weekly" = 53,
            #                          "Monthly" = 13,
            #                          "Quarterly" = 5),
            #              selected = "Weekly"),
            
            # Input: Select Time Series Method ----
            radioButtons("method", "Select Time Series Method",
                         choices = c("Time Series Decomposition (STL)" = "STL",
                                     "ARIMA(No Seasonality)" = "ARIMA1",
                                     "ARIMA(With Seasonality)" = "ARIMA2",
                                     "STL and ARIMA" = "STL+ARIMA"
                                    ),
                         selected = "ARIMA"),
           
            sliderInput("period",
                        "If forecast method is chosen please put a forecast period",
                        min = 5,
                        max = 365,
                        value = 5),
            actionButton("submit", "Run analysis"),
            downloadButton("downloadData", "Download")
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput(outputId = "main_plot",  width = "100%"),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            htmlOutput("title"),
            tableOutput("model_accuracy"),
            br(),
            br(),
            dataTableOutput(outputId = "main_table"),
        )
    )
))