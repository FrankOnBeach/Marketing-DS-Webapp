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
  titlePanel("Anomaly Detection"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput("file_ad", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      checkboxInput("header_ad", "Header", TRUE),
      
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
))