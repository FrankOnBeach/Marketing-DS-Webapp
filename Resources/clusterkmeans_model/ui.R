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
  titlePanel("Clustering"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput("file1_cl", "Choose CSV File, file must only contain your data IDs, all other metrics for the cluster must be numeric",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      checkboxInput("header_cl", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep_cl", "Separator: Specify how your data is split",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote_cl", "Quoted data: Specify if your data has quotes, otherwise select None",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '""'),
      
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
))