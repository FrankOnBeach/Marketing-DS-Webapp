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
    "This is an R shiny app for our Hackathon Project at Bounteous By Manasa Chitiprolu,Neha Khatr,Rebecca Manson, Frank Xu",
    h1("Time Series Decomposition and Forecasting"),
    h1("Causal Impact"),
    h1("Anomaly Detection"),
    h1("Clustering (K-Means)"),
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
            # selectInput("date_field_forecasting", "Choose the column name of date field", choices = c(not_sel)),
            # selectInput("value_field_forecasting", "Choose the column name of value field", choices = c(not_sel)),
            textInput("date_field_forecasting", "Write down the column name of date field", "Date"),
            textInput("value_field_forecasting", "Write down the column name of value field. If col name has space, replace the space with period(e.g. XX XX -> XX.XX)", "Sessions"),
            
            
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
                                     "Forecasting(STL and ARIMA)" = "STL+ARIMA"
                         ),
                         selected = "STL"),
            
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

anomaly_detection_page <- tabPanel(
    title = "Anomaly Detection",

    # Application title
    titlePanel("Anomaly Detection"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            fileInput("file_ad", "Choose CSV File",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            textInput("date_field_ad", "Write down the column name of date field", "Date"),
            textInput("value_field_ad", "Write down the column name of value field. If col name has space, replace the space with period(e.g. XX XX -> XX.XX)", "Sessions"),
            
            sliderInput("alpha",
                        "Choose how sensitive you want the anomaly detection to be.The higher the value is the more sensitive it is",
                        min = 0.02,
                        max = 0.15,
                        value = 0.05),
            
            sliderInput("max_anoms_ad",
                        "Choose the max % of anomalies you would like to see. For example, 20% would be 0.2",
                        min = 0.01,
                        max = 0.3,
                        value = 0.05),
            # textInput("max_anoms_ad", "Write down the max % of anomalies you would like to see. For example, 20% would be 0.2", "0.08"),
            #textInput("frequency_ad", "Write down the frequency you would like to use. For example, 2 days", "365"),
            
            # Input: Select Data's Date Format ----
            radioButtons("date_format_ad", "Select Data's Date Format",
                         choices = c("Not a date column"= "not_date",
                                     "mm/dd/yy" = "mm/dd/yy",
                                     "mm/dd/yyyy" = "mm/dd/yyyy",
                                     "yyyy-mm-dd" = "yyyy-mm-dd"),
                         selected = "mm/dd/yy"),
            
            
            actionButton("submit_ad", "Run analysis"),
            downloadButton("downloadData_ad", "Download")
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput(outputId = "main_plot_ad",  width = "100%"),
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
            htmlOutput("title_ad"),
            tableOutput("Anomaly Chart"),
            br(),
            br(),
            dataTableOutput(outputId = "main_table_ad"),
        )
    )    

)

clustering_page <- tabPanel(
    title = "Clustering",
    # Application title
    titlePanel("Clustering"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            fileInput("file1_cl", "Choose CSV File, file must only contain your data IDs, all other metrics for the cluster must be numeric",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),

            
            #textInput("k_means_model_description", "Write down the column name of date field", "DATE"),
            #textInput("cluster_count", "Write down the column name of value field. If col name has space, replace the space with period(e.g. XX XX -> XX.XX)", "Sessions"),
            
            
            # Input: Select Data's Date Format ----
            radioButtons("cluster_number", "Select your number of clusters, from the first chart, where the series turns",
                         choices = c("3"= 3,
                                     "4" = 4,
                                     "5" = 5
                         ),
                         selected = "3"),
            
            actionButton("submit_cl", "Run analysis"),
            downloadButton("downloadData_cl", "Download")
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
            htmlOutput("title_elbow_plot"),
            plotOutput(outputId = "elbow_plot",  width = "80%"),
            br(),
            htmlOutput("title_sample_output"),
            dataTableOutput(outputId = "sample_output"),
            br(),
            br(),
            htmlOutput("title_plot_cl"),
            plotOutput(outputId = "plot_cl",  width = "80%"),
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
    causal_impact_page,
    anomaly_detection_page,
    clustering_page
)