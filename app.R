##########################################################################
# shiny app to generate bipartite graph and nestedness index
#
# Aaron Greenville
#########################################################################


library(shiny)
library(bipartite)
source("R/Network_functions.R")

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
                  "The app was created for the subject Biodiversity Conservation,
                 coordinated by Dr Brad Murray at the University of Technology Sydney.
                 The app was implemented in practicals and assessments,
                 investigating pollination and conservation,
                 and designed by the team of Dr Yvonne Davila,
                 Dr Aaron Greenville, and Dr Brad Murray."),
                 p("Source code can be found on", a("GitHub.",
                    href="https://github.com/agreenville/Bipartite-Network")),
                 a("MIT License",
                   href="https://github.com/agreenville/Bipartite-Network/blob/master/LICENSE"),
                 p(""),
                 p("Version 1.0.3"),
                 p(""),
                 h4("Citation:"),
                 p(span("Greenville, A.C. (2018). "), a("Bipartite Network Analysis vs1.0.2.",
                      href="https://zenodo.org/badge/latestdoi/117910523"),
                   a(tags$img(src="https://zenodo.org/badge/117910523.svg"), href="https://zenodo.org/badge/latestdoi/117910523")),
                 h4("References:"),
                 p("Chang, W., Cheng, J., Allaire, J.J., Xie, Y. & McPherson, J. (2017). ", em("Shiny: web application framework for R."), "R
                        package version 1.0.5. https://CRAN.R-project.org/package=shiny"),
                 p("Dormann, C.F., Gruber, B., & Fruend, J. (2008). Introducing the bipartite package:
                      analysing ecological networks. ", em("R news"), "8/2:8-11."),
                 p("R Core Team. (2017). ", em("R: a language and environment for statistical
                                               computing."), "R Foundation for Statistical Computing, Vienna, Austria.")
                   
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
               downloadButton('downloadMatrix', 'Download matrices'),
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
      
      plotweb(dd$network, low.abun=dd$abund, high.y=1.25,low.y = 0.7, high.abun.col='darkblue',
             low.abun.col='lightgreen', col.interaction="grey90",
             abuns.type='independent', labsize=1.5, text.rot=90)
          }
    
    if(input$abund=='no'){
    
      df2 <- net.fn(data())
    
      plotweb(df2, high.y=1.25,low.y = 0.7, col.high='darkblue', col.low = 'lightgreen',
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
  output$nest.matrix <- renderPlot({
    
    if(input$abund=='yes'){
      
      dd<-net.abund.fn(data())    
      
      
      visweb2(dd$network, main="Original matrix", labsize=1, type = "none") # matrix as data is entered.
      #title("Original matrix", line =-20.5)
    }
    
    if(input$abund=='no'){
      
      df2 <- net.fn(data())
      
       
      visweb2(df2,main="Original matrix", labsize=1,  type = "none") # matrix as data is entered.
     #title("Original matrix", line =-20.5)
    }
    
  } ) #,width=550, height = 300
  
  output$net.sort.matrix<- renderPlot({
    
    if(input$abund=='yes'){
      
      dd<-net.abund.fn(data())    
      
      
      visweb2(dd$network, main="Ordered matrix", labsize=1, type = "nested") # sorted by row/colSums
      #title("Ordered matrix", line=-20.5)
    }
    
    if(input$abund=='no'){
      
      df2 <- net.fn(data())
      
      
      visweb2(df2, main="Ordered matrix", labsize=1,  type = "nested") # sorted by row/colSums.
      #title("Ordered matrix", line=-20.5)
    }
    
  } ) #, width=550, height = 300
  
  
  # Handle the plot download  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("network_graph", 'png', sep='.')
      },
    
    content = function(file) {
      
    if(input$abund=='yes'){
        
      dd<-net.abund.fn(data())
      
      png(file, width = 300, height = 190, units = 'mm', res = 300)
      plotweb(dd$network, low.abun=dd$abund, high.abun.col='darkblue' ,low.abun.col='lightgreen' ,
                col.interaction="grey90", abuns.type='independent', labsize=1.5,
                high.y=1.25,low.y = 0.7, text.rot=90)
      dev.off()
      }
      
    if(input$abund=='no'){
        
      df2 <- net.fn(data())
        
      png(file, width = 300, height = 190, units = 'mm', res = 300)
      plotweb(df2, col.interaction="grey90", col.high='darkblue', col.low = 'lightgreen',
                labsize=1.5,high.y=1.25,low.y = 0.7, text.rot=90)
      dev.off()
        }
    })
  
  # Handle the matrix download  
  output$downloadMatrix <- downloadHandler(
    filename = function() {
      paste("network_matrices", 'png', sep='.')
    },
    
    content = function(file) {
      
      if(input$abund=='yes'){
        
        dd<-net.abund.fn(data())
        
        png(file, width = 190, height = 230, units = 'mm',res = 300  ) #
        par(mfrow=c(2,1))
        visweb2(dd$network, main="Original matrix",  labsize=1, type = "none") # matrix as data is entered.
        #title("Original matrix", line =-1)
        
        visweb2(dd$network, main="Ordered matrix", labsize=1, type = "nested") # sorted by row/colSums
        #title("Ordered matrix", line=-1)
        par(mfrow=c(1,1))
        
        dev.off()
      
      }
      
      if(input$abund=='no'){
        
        df2 <- net.fn(data())
        
        png(file, width = 190, height = 230, units = 'mm',res = 300 ) #
        par(mfrow=c(2,1))
        visweb2(df2, main="Original matrix",labsize=1, type = "none") # matrix as data is entered.
        #title("Original matrix", line =-1)
        
        visweb2(df2,main="Ordered matrix", labsize=1, type = "nested") # sorted by row/colSums
        #title("Ordered matrix", line=-1)
        par(mfrow=c(1,1))
        dev.off()
      }
    })
})

shinyApp(ui, server)
