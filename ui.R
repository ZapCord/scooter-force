library(shiny)
library(shinyjs)
jsCode <- "shinyjs.refresh = function() { location.reload(); }"

ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = jsCode, functions=c("refresh")),
  titlePanel("Scooter Leg Forces"),
  fluidRow(
    column(2,id="fileinputpanel",
           fileInput("existing_data_input", 
                     label=h5("Browse existing data in .csv format")),
           helpText("Instructions: choose the 1D .csv file from the
                    snowforce3.0 device connected to the 16x10 board")
    ),
    column(3, id="subjectpanel", h3("Subject"),
           hr(),
           helpText("Fill in subject
                and test information if new data input is chosen"),
           textInput("subject", label = h5("Enter Subject Name:"),
                     value = Sys.info()["user"]),
           hr(),
           textInput("test",label = h5("Enter Test Name:"),
                     value = "Scoot 60s"),
           hr()
    ),
    column(7, h3("Visualization"),
           tabsetPanel(
             tabPanel("Plot",plotOutput("plot")),
             tabPanel("Table",DT::dataTableOutput("table")),
             tabPanel("Average Plot",plotOutput("avgplot")),
             tabPanel("Average Table",DT::dataTableOutput("avgtable")),
             helpText("SBC orientation ->")
           )
    )
    
  ),
  fluidRow(
    column(2,helpText("File Upload Status:"),
           verbatimTextOutput("fileinputpanel")),
    hr(),
    column(3,hr(),
           downloadButton("saveplot", label = "Save Plot"),
           downloadButton("savetable", label = "Save Table"),
           downloadButton("saveaverageplot", label = "Save Avg Plot"),
           downloadButton("saveaveragetable", label = "Save Avg Table"),
           actionButton("refresh",label = "Refresh Page"),
           hr()
    ),
    column(7,
           sliderInput("slider_time", label = h3("Time Index"), min = 1, 
                       max = 100, value = 1, width='100%',
                       animate = animationOptions(interval=250)),
           hr(),
           sliderInput("slider_range", label = h3("Times to calculate average from"),
                       min = 1, max = 100, value=c(1,100),width='100%'),
           checkboxGroupInput("checkGroup", label = h3("Plot Options"),
                              choices = list("Interpolate" = 1))
           
    ),
  )
  
)