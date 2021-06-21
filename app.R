library(shiny)
library(readxl)
library(meta)
library(metafor)
library(Matrix)
library(readxl)
library(plotrix)
library(PerformanceAnalytics)
library(tidyverse)
library(MuMIn)
library(ggplot2)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)

    
ui <-   navbarPage(fileInput("file", NULL),
        tabPanel(
            "LR Plot",
        fluidPage(
        
        tableOutput("tab"),
        verbatimTextOutput('lmSummary'))),
        tabPanel(
          "Correlation matrix",
          fluidPage(
          plotOutput("plot"),
          )))
    
server <- function(input, output, session) {
    df <- reactive({
    req(input$file)
    read_excel(input$file$datapath)})
      
    random_eff <- reactive({
    req(df())
    model<- rma.mv(Fat_mean,Fat_var, mods= ~ factor(Level),  random = ~ 1 | Studies, data=df())
          return(model)
        })
    
    output$lmSummary <- renderPrint({
    req(random_eff())
    summary(random_eff())
        })
    
    output$plot <- renderPlot(funnel(random_eff(), level=c(90, 95, 99),shade=c("white", "gray", "darkgray"), refline=0))
}

shinyApp(ui, server)
