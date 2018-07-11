library(shiny)
library(tm)
library(SnowballC)
library(wordcloud)
library(corpus)
library(syuzhet)
data("iris")

shinyUI(
  fluidPage(
    
    titlePanel(title = "Product Review Analysis"),
    
    sidebarLayout(
      
      sidebarPanel(h3(""),
                   fileInput("file","Upload CSV file")
                #   actionButton("update", "Create word Cloud")
                   ),
      
      mainPanel(h2("PRA"),

                
                tabsetPanel(type="tab",
                            tabPanel("Home",tableOutput("pra")),
                            tabPanel("Rankings"),
                            tabPanel("Word Counter", plotOutput("wordcloud")),
                            tabPanel("Data set", tableOutput("filedisplay")),
                            tabPanel("Grammar", tableOutput("grammar")),
                            tabPanel("Plot", plotOutput("plot"),downloadButton("downloadplot","Download the plot"))
                            )
                )
    )
  )
)

