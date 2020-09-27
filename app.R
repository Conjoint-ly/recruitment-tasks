
#### Load data ####
Sys.setlocale(locale = "UTF-8")
library(shiny)
library(DT)
library(readr)

options(stringsAsFactors = FALSE)


uploadDF <- data.frame()

dataSources = list(
 "Example 1: Vegetable juice flavour preferences" = read_csv("example1.csv"),
 "Example 2: Soft Drinks Product Variants" = read_csv("example2.csv")
)

defaultOptions = list(
  paging = TRUE,
  searching = TRUE,
  ordering = TRUE,
  fixedColumns = TRUE,
  dom = 'lfrtipB',
  buttons = c('copy', 'csv', 'excel'),
  scrollX = TRUE
)


#### Define UI for application ####
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("dataSource", "Select source of data: ", 
                  names(dataSources),
                  selected = dataSources[1],
                  multiple = FALSE,
                  selectize = TRUE),
      actionButton("uploadButton", "Upload a new dataset"),
      hr()
    ),
    mainPanel(
      tabPanel("Data preview", 
               DTOutput('dataOutput')
               )
    )
  )
)

server <- function(input, output, session) {
  #### Uploading data for analysis ####
  observeEvent(input$uploadButton, {
    showModal(modalDialog(
      title = "Upload data",
      fade = TRUE,
      fileInput("uploadFile", "Choose a .csv file (compatible with Excel)",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      textOutput("uploadMsg"),
      footer =  tagList(
        modalButton("Cancel"),
        actionButton("uploadOK1", "OK")
      )
    ))
  })
  
  observeEvent(input$uploadOK1, {
    if(!is.null(input$uploadFile)){
      # Error check
      
      output$caption <- renderText(input$uploadFile$datapath)
      
      uploadDF <<- read_csv(input$uploadFile$datapath)
      
      if(ncol(uploadDF) > 50){
        output$uploadMsg <- renderText("File must have no more than 50 columns.")
        return(FALSE)
      }
      
      if(nrow(uploadDF) > 20000){
        output$uploadMsg <- renderText("File must have no more than 20,000 rows.")
        return(FALSE)
      }
      
      if(min(dim(uploadDF)) < 1){
        output$uploadMsg <- renderText("File seems to be empty.")
        return(FALSE)
      }
      
      if(!all(apply(uploadDF, 2, is.numeric))){
        output$uploadMsg <- renderText("File seems to have non-numeric columns.")
        return(FALSE)
      }
      
      if(any(colnames(uploadDF) %in% c("Reach","Frequency"))){
        output$uploadMsg <- renderText("Columns cannot be called `Reach` or `Frequency`.")
        return(FALSE)
      }
      
      dataSources[[input$uploadFile$name]] <<- uploadDF
      
      updateSelectInput(session, "dataSource",
                        choices = names(dataSources),
                        selected = input$uploadFile$name
      )
      removeModal()

    } else {
      output$uploadMsg <- renderText("Please supply a valid .csv file")
    }
  })
  
  #### Update outputs ####
  
  output$caption <- renderText({
    paste("Analysis is based on", nrow(dataSources[[input$dataSource]]),"responses.")
  })
  
  output$dataOutput = renderDataTable({
    return(datatable(
      dataSources[[input$dataSource]],
      extensions = 'Buttons',
      options = defaultOptions,
      class = "display")
    )
  })

}

#### Run the application ####
shinyApp(ui = ui, server = server)
