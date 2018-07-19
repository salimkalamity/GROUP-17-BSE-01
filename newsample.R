library(shiny)
library(shinydashboard)
ui <- dashboardPage(
  dashboardHeader(title="PRODUCT REVIEW ANALYSIS SYSTEM"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "dash1", icon = icon("tachometer")),
      menuItem("Topics", tabName = "dash2", icon = icon("tachometer")),
      menuItem("Help", tabName = "dash3", icon = icon("tachometer")),
      menuItem("Contacts", tabName = "dash4", icon = icon("tachometer"))
      
  )),
  dashboardBody()
    
)

server <- function(input, output) { }
shinyApp(ui, server)
