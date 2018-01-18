library(shiny)
#library(datasets)
library("bipartite")

ui <- shinyUI(fluidPage(
  titlePanel("Bipartite Network analysis"),
  tabsetPanel(
    tabPanel("Upload File",
             titlePanel("Uploading Files"),
             sidebarLayout(
               sidebarPanel(
                 fileInput('file1', 'Choose CSV File',
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                 
                 # added interface for uploading data from
                 # http://shiny.rstudio.com/gallery/file-upload.html
                 tags$br(),
                 checkboxInput('header', 'Header', TRUE),
                 radioButtons('sep', 'Separator',
                              c(Comma=',',
                                Semicolon=';',
                                Tab='\t'),
                              ','),
                 radioButtons('quote', 'Quote',
                              c(None='',
                                'Double Quote'='"',
                                'Single Quote'="'"),
                              '"')
                 
               ),
               mainPanel(
                 tableOutput('contents')
               )
             )
    ),
    tabPanel("Network plot",
             #pageWithSidebar(
               headerPanel('Bipartite Network plot') ,
               # sidebarPanel(
               #   
               #   # "Empty inputs" - they will be updated after the data is uploaded
               #   selectInput('xcol', 'X Variable', ""),
               #   selectInput('ycol', 'Y Variable', "", selected = "")
                 
               #),
               mainPanel(
                 plotOutput('MyPlot')
               )
             ),
    
    tabPanel("Indices",
             #pageWithSidebar(
             headerPanel('Network indices') ,
             # sidebarPanel(
             #   
             #   # "Empty inputs" - they will be updated after the data is uploaded
             #   selectInput('xcol', 'X Variable', ""),
             #   selectInput('ycol', 'Y Variable', "", selected = "")
             
             #),
             mainPanel(
               verbatimTextOutput('indices')
             )
    )
    
    
    
    )
    
  )
)
#)

server <- shinyServer(function(input, output) { #session
  # added "session" because updateSelectInput requires it
  
  
  data <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    
    inFile <- input$file1 
    
    # tested with a following dataset: write.csv(mtcars, "mtcars.csv")
    # and                              write.csv(iris, "iris.csv")
    df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                   quote = input$quote)
    
    
    # Update inputs (you could create an observer with both updateSel...)
    # You can also constraint your choices. If you wanted select only numeric
    # variables you could set "choices = sapply(df, is.numeric)"
    # It depends on what do you want to do later on.
    
    # updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
    #                   choices = names(df), selected = names(df))
    # updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
    #                   choices = names(df), selected = names(df)[2])
    
    return(df)
  })
  
  output$contents <- renderTable({
    data()
  })
  
  output$MyPlot <- renderPlot({
    # for a histogram: remove the second variable (it has to be numeric as well):
    # x    <- data()[, c(input$xcol, input$ycol)]
    # bins <- nrow(data())
    # hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
    # Correct way:
    # x    <- data()[, input$xcol]
    # bins <- nrow(data())
    # hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
    
    # I Since you have two inputs I decided to make a scatterplot
    # x <- data()[, c(input$xcol, input$ycol)]
    # plot(x)
    
    df2<-data()
    row.names(df2) <-df2$X
    df2<-df2[,-1]
    df2 <- as.matrix(df2)
    
    plotweb(df2, labsize=1.5, text.rot=90)
    
  }, width=1500, height = 500)
  
  output$indices <- renderPrint({
    df2<-data()
    row.names(df2) <-df2$X
    df2<-df2[,-1]
    df2 <- as.matrix(df2)
    
    networklevel(df2)
    
  }, width=100)
})

shinyApp(ui, server)