library(shiny)
library(bipartite)

ui <- shinyUI(fluidPage(
  titlePanel("Bipartite Network Analysis"),
  tabsetPanel(
    tabPanel("Upload file",
             titlePanel("Uploading Files"),
             sidebarLayout(
               sidebarPanel(
                 h4("Instructions:"),
                 p("Save data file as a csv, with rows as plant species and columns
                   as invertebrate species. If you have the abundance of each plant species,
                   add abundance data as the last column."),
                 
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
                 radioButtons("abund", "Abundance data", c(Yes="yes", No="no")),
                 
                 h4("Credits:"),
                 p("The Bipartite Network analysis app was created with", em("Shiny"), 
                    "and", em("Bipartite"), "package for",
                 span(em("R.")),
                  "By Aaron Greenville."),
                 p("Source code can be found on", a("GitHub.",
                    href="https://github.com/agreenville/Bipartite-Network")),
                 a("MIT License",
                   href="https://github.com/agreenville/Bipartite-Network/blob/master/LICENSE"),
                 p(""),
                 p("Version 0.7") 
                 
               ),
               
               mainPanel(
                 tableOutput('contents')
                
               )
             )
    ),
    tabPanel("Network graph",
             #pageWithSidebar(
               headerPanel('Bipartite Network Graph') ,
               
               mainPanel(
                 downloadButton('downloadPlot', 'Download graph'),
                 plotOutput('MyPlot')
                 )
             ),
    
    tabPanel("Nestedness",
             headerPanel('Nestedness') ,
            
             mainPanel(
               downloadButton('downloadMatrix', 'Download matrix'),
               splitLayout(cellWidths = c("10%", "60%", "60%"), verbatimTextOutput('indices'),
                           plotOutput("nest.matrix"), plotOutput("net.sort.matrix")),
               
               hr(),
               print("If you cannot see the entire matrix, hover mouse over it to reveal scroll bars.")
                           
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

server <- shinyServer(function(input, output) { #session
  
  # Read data in
  data <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    
    inFile <- input$file1 
    
    df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                   quote = input$quote)
    
    return(df)
  })

  # Display data    
  output$contents <- renderTable({
    data()
  })

  # Display plot    
  output$MyPlot <- renderPlot({

    if(input$abund=='yes'){
      
      dd<-net.abund.fn(data())    
      
      plotweb(dd$network, low.abun=dd$abund, high.y=1.25,low.y = 0.7, high.abun.col='lightblue',
             low.abun.col='lightgreen', col.interaction="grey90",
             abuns.type='independent', labsize=1.5, text.rot=90)
          }
    
    if(input$abund=='no'){
    
      df2 <- net.fn(data())
    
      plotweb(df2, high.y=1.25,low.y = 0.7, col.high='lightblue', col.low = 'lightgreen',
            col.interaction="grey90", labsize=1.5, text.rot=90)
    }
    
  }, width=1500, height = 500)

  # Display indicies
  # had to use an else function instead of if, as above for it to work 
  output$indices <- renderPrint({
     
    if(input$abund=='yes'){  
      
      dd<-net.abund.fn(data())
      
      #networklevel(dd$network) # calc different network indices
      
      nested(dd$network, "NODF2") # calc different nestedness indices
      
    } 
    
    else{ #(input$#abund=='no'){
      
      df2 <- net.fn(data())
      #networklevel(df2) # calc different network indices
      nested(df2, "NODF2") # calc different nestedness indices NODF2 = NODF on NeD site
    }

  }, width=100)
 
  # Display matrix
  # need a better way to add title
  output$nest.matrix <- renderPlot({
    
    if(input$abund=='yes'){
      
      dd<-net.abund.fn(data())    
      
      
      visweb(dd$network, labsize=1.5, type = "none") # matrix as data is entered.
      title("Original matrix", line =-20.5)
    }
    
    if(input$abund=='no'){
      
      df2 <- net.fn(data())
      
       
      visweb(df2, labsize=1.5,  type = "none") # matrix as data is entered.
     title("Original matrix", line =-20.5)
    }
    
  }, width=600, height = 300)
  
  output$net.sort.matrix<- renderPlot({
    
    if(input$abund=='yes'){
      
      dd<-net.abund.fn(data())    
      
      
      visweb(dd$network, labsize=1.5, type = "nested") # sorted by row/colSums
      title("Ordered matrix", line=-20.5)
    }
    
    if(input$abund=='no'){
      
      df2 <- net.fn(data())
      
      
      visweb(df2, labsize=1.5,  type = "nested") # sorted by row/colSums.
      title("Ordered matrix", line=-20.5)
    }
    
  }, width=600, height = 300)
  
  
  # Handle the plot download  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("network_plot", 'png', sep='.')
      },
    
    content = function(file) {
      
    if(input$abund=='yes'){
        
      dd<-net.abund.fn(data())
      
      png(file, width = 300, height = 190, units = 'mm', res = 300)
      plotweb(dd$network, low.abun=dd$abund, high.abun.col='lightblue' ,low.abun.col='lightgreen' ,
                col.interaction="grey90", abuns.type='independent', labsize=1.5,
                high.y=1.25,low.y = 0.7, text.rot=90)
      dev.off()
      }
      
    if(input$abund=='no'){
        
      df2 <- net.fn(data())
        
      png(file, width = 300, height = 190, units = 'mm', res = 300)
      plotweb(df2, col.interaction="grey90", col.high='lightblue', col.low = 'lightgreen',
                labsize=1.5,high.y=1.25,low.y = 0.7, text.rot=90)
      dev.off()
        }
    })
  
  # Handle the matrix download  
  output$downloadMatrix <- downloadHandler(
    filename = function() {
      paste("network_matrix", 'png', sep='.')
    },
    
    content = function(file) {
      
      if(input$abund=='yes'){
        
        dd<-net.abund.fn(data())
        
        png(file, width = 150, height = 190, units = 'mm', res = 300)
        par(mfrow=c(2,1))
        visweb(dd$network, labsize=1.25, type = "none") # matrix as data is entered.
        title("Original matrix", line =-1)
        
        visweb(dd$network, labsize=1.25, type = "nested") # sorted by row/colSums
        title("Ordered matrix", line=-1)
        par(mfrow=c(1,1))
        
        dev.off()
      
      }
      
      if(input$abund=='no'){
        
        df2 <- net.fn(data())
        
        png(file, width = 150, height = 190, units = 'mm', res = 300)
        par(mfrow=c(2,1))
        visweb(df2, labsize=1.25, type = "none") # matrix as data is entered.
        title("Original matrix", line =-1)
        
        visweb(df2, labsize=1.25, type = "nested") # sorted by row/colSums
        title("Ordered matrix", line=-1)
        par(mfrow=c(1,1))
        dev.off()
      }
    })
})

shinyApp(ui, server)
