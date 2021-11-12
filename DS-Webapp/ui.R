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
library(shinythemes)

not_sel <- "Not Selected"
about_page <- tabPanel(
    title = "About",
    titlePanel("About"),
    "This is an R shiny app for our Hackathon Project at Bounteous.",
    br()
    # TODO Low-Add a suggestion box here ####
    # TODO Paste description for each model ####
)

# Define UI for application that draws a histogram
time_series_forecasting_page <- tabPanel(
    # TODO Get rid off forecasting models that make no sense #### 
    # Application title
    title = "Time Series Decomposition and Forecasting",
    titlePanel("Time Series Forecasting"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            fileInput("file_forecasting", "Choose CSV File",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            checkboxInput("header_forecasting", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep_forecasting", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote_forecasting", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            # selectInput("date_field_forecasting", "Choose the column name of date field", choices = c(not_sel)),
            # selectInput("value_field_forecasting", "Choose the column name of value field", choices = c(not_sel)),
            textInput("date_field_forecasting", "Write down the column name of date field", "Date"),
            textInput("value_field_forecasting", "Write down the column name of value field. If col name has space, replace the space with period(e.g. XX XX -> XX.XX)", "Sessions"),
            
            
            # Input: Select Data's Date Format ----
            radioButtons("date_format_forecasting", "Select Data's Date Format",
                         choices = c("Not a date column"= "not_date",
                                     "mm/dd/yy" = "mm/dd/yy",
                                     "mm/dd/yyyy" = "mm/dd/yyyy",
                                     "yyyy-mm-dd" = "yyyy-mm-dd"),
                         selected = "mm/dd/yy"),
            
            # Input: Select Data Format ----
            radioButtons("frequency_forecasting", "Select Data's Frequency: daily or weekly or monthly?",
                         choices = c("Daily" = "day",
                                     "Weekly" = "week",
                                     "Monthly" = "month",
                                     "Quarterly" = "quarter"),
                         selected = "Daily"),
            
            textInput("start_date_forecasting", "If a non-daily frequency is chosen, write down the year and the number of week/month/quarter it is of the year. For example: Dec 2020 would be 2020 12 or simply write down the date in yyyy-mm-dd format","1999-09-09"),
            # textInput("end_date", "Write down the end date of your data if a non-daily frequency is chosen. Make sure the date is the last day of the week/month/quarter","1999-09-09"),
            
         
            radioButtons("method_forecasting", "Select Time Series Method",
                         choices = c("Time Series Decomposition (STL)" = "STL",
                                     "ARIMA(No Seasonality)" = "ARIMA1",
                                     "ARIMA(With Seasonality)" = "ARIMA2",
                                     "STL and ARIMA" = "STL+ARIMA"
                         ),
                         selected = "ARIMA"),
            
            sliderInput("period_forecasting",
                        "If forecast method is chosen please put a forecast period",
                        min = 5,
                        max = 365,
                        value = 5),
            actionButton("submit_forecasting", "Run analysis"),
            downloadButton("downloadData_forecasting", "Download")
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
                plotOutput(outputId = "main_plot_forecasting",  width = "100%"),
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
                br(),
                htmlOutput("title_forecasting"),
                tableOutput("model_accuracy_forecasting"),
                br(),
                br(),
                dataTableOutput(outputId = "main_table_forecasting")
                
            )

        
    )
)
causal_impact_page <- tabPanel(
    # TODO Get rid off forecasting models that make no sense #### 
    # Application title
    title = "Causal Impact",
    titlePanel("Causal Impact"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            fileInput("file_ci", "Choose CSV File",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            checkboxInput("header_ci", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep_ci", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote_ci", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            # selectInput("date_field_forecasting", "Choose the column name of date field", choices = c(not_sel)),
            # selectInput("value_field_forecasting", "Choose the column name of value field", choices = c(not_sel)),
            textInput("impact_field_ci", "Write down the column name of you want to run causal impact on", "2021"),
            textInput("impact_point_ci", "Write down when the impact happened.", "10"),
            # sliderInput("period_ci",
            #             "Select Impact point",
            #             min = 5,
            #             max = 365,
            #             value = 5),
            actionButton("submit_ci", "Run Causal Impact")
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput(outputId = "main_plot_ci",  width = "100%"),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            htmlOutput("results_ci"),
            br(),
            br()
        )
        
        
    )
)

ui <- navbarPage(
    title = "Bounteous Marketing Data Science App",
    theme = shinytheme('flatly'),
    about_page,
    time_series_forecasting_page,
    causal_impact_page
)