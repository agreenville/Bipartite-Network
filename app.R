library(shiny)
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
                              '"'),
                 radioButtons("abund", "Abundance data", c(Yes="yes", No="no"))
                 
               ),
               mainPanel(
                 tableOutput('contents')
               )
             )
    ),
    tabPanel("Network plot",
             #pageWithSidebar(
               headerPanel('Bipartite Network plot') ,
                #sidebarPanel(
               #   
               #   # "Empty inputs" - they will be updated after the data is uploaded
               #   selectInput('xcol', 'X Variable', ""),
               #   selectInput('ycol', 'Y Variable', "", selected = "")
                 
                  
                #),
               mainPanel(
                 downloadButton('downloadPlot', 'Download plot'),
                 plotOutput('MyPlot')
                          )
             ),
    
    tabPanel("Indices",
             headerPanel('Network indices') ,
            
             mainPanel(
               verbatimTextOutput('indices')
             )
      )
      
    )
  )
)

 # define function for data manipulaion
 # for dataset with abund data in last coln
  net.abund.fn <- function(df){
    df2 <- df[-length(df)]
    ab <-  unlist(df[length(df)])
    
    row.names(df2) <-df2$X
    df2<-df2[,-1]
    df2 <- as.matrix(df2)
    names(ab) <- rownames(df2)
    dd <- list(network = df2,abund = ab)
    return(dd)
  }
 
  # for dataset with no abundance data
  net.fn <- function(df){
    row.names(df) <-df$X
    df<-df[,-1]
    df <- as.matrix(df)
    return(df)
  }
  
# to do: define plotting functions with and without abundance
  # replace duplication below with functions
  
  
server <- shinyServer(function(input, output) { #session
  # added "session" because updateSelectInput requires it

  data <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    
    inFile <- input$file1 
    
    df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                   quote = input$quote)
    
    return(df)
  })
  
  output$contents <- renderTable({
    data()
  })
  
  output$MyPlot <- renderPlot({

        if(input$abund=='yes'){
      # df2 <- data()[-length(data())]
      # ab <-  unlist(data()[length(data())])
      # 
      # row.names(df2) <-df2$X
      # df2<-df2[,-1]
      # df2 <- as.matrix(df2)
      # names(ab) <- rownames(df2)
      
    dd<-net.abund.fn(data())    
      
     plotweb(dd$network, low.abun=dd$abund, high.y=1.25,low.y = 0.7, high.abun.col='lightblue',
             low.abun.col='lightgreen', col.interaction="grey90",
             abuns.type='independent', labsize=1.5, text.rot=90)
     
     }
    
    if(input$abund=='no'){
    #   df2<-data()
    # 
    # row.names(df2) <-df2$X
    # df2<-df2[,-1]
    # df2 <- as.matrix(df2)
    
    df2 <- net.fn(data())
    
    plotweb(df2, high.y=1.25,low.y = 0.7, col.high='lightblue', col.low = 'lightgreen',
            col.interaction="grey90", labsize=1.5, text.rot=90)
    }
    
  }, width=1500, height = 500)
  
  output$indices <- renderPrint({
  
    if(input$abund=='yes'){  
      # df2<- data()[-length(data())]
      # row.names(df2) <-df2$X
      # df2<-df2[,-1]
      # df2 <- as.matrix(df2)
      
      dd<-net.abund.fn(data())
      
      networklevel(dd$network)
    } 
    
    if(input$abund=='no'){  
    # df2<-data()
    # row.names(df2) <-df2$X
    # df2<-df2[,-1]
    # df2 <- as.matrix(df2)
    df2 <- net.fn(data())  
    
    networklevel(df2)
  }
    }, width=100)
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("network_plot", 'png', sep='.')
      },
    
    content = function(file) {
      
      if(input$abund=='yes'){
        # df2 <- data()[-length(data())]
        # ab <-  unlist(data()[length(data())])
        # 
        # row.names(df2) <-df2$X
        # df2<-df2[,-1]
        # df2 <- as.matrix(df2)
        # names(ab) <- rownames(df2)
        
        dd<-net.abund.fn(data())
        
        
        png(file, width = 300, height = 190, units = 'mm', res = 300)
        #par(mfrow=c(3,1), mar=c(5.1, 4.1, 4.1, 9.5))
        plotweb(dd$network, low.abun=dd$abund, high.abun.col='lightblue' ,low.abun.col='lightgreen' ,
                col.interaction="grey90", abuns.type='independent', labsize=1.5,
                high.y=1.25,low.y = 0.7, text.rot=90)
        dev.off()
      }
      
      if(input$abund=='no'){
        # df2<-data()
        # 
        # row.names(df2) <-df2$X
        # df2<-df2[,-1]
        # df2 <- as.matrix(df2)
        
        df2 <- net.fn(data())
        
        png(file, width = 300, height = 190, units = 'mm', res = 300)
        plotweb(df2, col.interaction="grey90", col.high='lightblue', col.low = 'lightgreen',
                labsize=1.5,high.y=1.25,low.y = 0.7, text.rot=90)
        dev.off()
        }
    })
})

shinyApp(ui, server)