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
library(plotly)


ui <- fluidPage(
  titlePanel("Shiny meta regression"),
  fluidRow(
    column(
      12,
      wellPanel(
        navbarPage(
          "Meta regression", fileInput("file", NULL),
          tabPanel(
            "Results",
            fluidPage(
              tableOutput("tab"),
              verbatimTextOutput("lmSummary")
            )
          ),
          tabPanel(
            "Funnel plot",
            fluidPage(
              plotOutput("plot", height = "820px"),
            )
          ),
          tabPanel(
            "Forest plot",
            fluidPage(
              plotOutput("forest", height = "820px")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  df <- reactive({
    req(input$file)
    read_excel(input$file$datapath)
  })

  random_eff <- reactive({
    req(df())
    model <- rma.mv(Fat_mean, Fat_var, mods = ~ factor(Level), random = ~ 1 | Author, data = df(), slab = paste(Author))
    return(model)
  })

  output$lmSummary <- renderPrint({
    req(random_eff())
    summary(random_eff())
  })

  output$plot <- renderPlot({
    funnel(random_eff(), level = c(90, 95, 99), shade = c("white", "gray", "darkgray"), refline = 0)
  })

  output$forest <- renderPlot({
    forest(random_eff(),
      addfit = FALSE, level = 95, header = TRUE, xlab = "Fat %", ilab = cbind(df()$n, df()$Gender, paste(df()$Level), paste(df()$Method)),
      ilab.xpos = c(-25, -20, -13, -3)
    )
    op <- par(cex = 1.25, font = 4)
    text(c(-25, -20, -13, -3), 75, c("Sample", "Gender", "Level", "Method"))
  })
}
shinyApp(ui, server)
