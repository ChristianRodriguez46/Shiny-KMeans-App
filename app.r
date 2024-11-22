# Load the Shiny package
 library (shiny)
 # User Interface
ui <- fluidPage (
   titlePanel ( "K-Means Clustering" ) ,
   sidebarLayout (
     sidebarPanel (
       numericInput("clusters" , " Number of Clusters " , 3 , min = 1 , max = 10) 
       ) ,
     mainPanel(
       plotOutput( "clusterPlot" )
       )
     )
   )

 # Server Logic
 server <- function ( input,output ) {
   output$clusterPlot <- renderPlot ({
     set.seed (123)
     
     data <- mtcars[, c( "mpg" ,  "wt" ) ]
     
     kmeans_result <- kmeans( data , centers = input$clusters )
     
     plot ( data , col = kmeans_result$cluster , pch = 20 ,cex = 2)
     
     points ( kmeans_result$centers , col = 1:nrow(kmeans_result$centers),pch = 4 , cex = 4 , lwd = 4)
    })
 }

# Combine UI and Server
shinyApp ( ui = ui , server = server )