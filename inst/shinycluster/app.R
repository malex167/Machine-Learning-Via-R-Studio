library(shiny)
library(ggplot2)
library(mclust)

ui <- fluidPage(

  titlePanel("Cluster Analysis: Iris Data Frame"),

  sidebarLayout(
    sidebarPanel(
      numericInput("k",
                  "K Value:",
                  min = 1,
                  max = 10,
                  value = 1,
                  step = 1),

      numericInput("G",
                   "G Value:",
                   min = 1,
                   max = 10,
                   value = 1,
                   step = 1),

      selectInput("what",
                  "Plot Specification:",
                  c("Classification" = "classification",
                    "Uncertainty" = "uncertainty",
                    "BIC" = "BIC",
                    "Density" = "density")),

      actionButton("null", "G = NULL")
    ),

    mainPanel(
      plotOutput("plot1"),
      plotOutput("plot2")
    )
  )
)

server <- function(input, output) {

  k <- reactive(input$k)
  G <- reactive(input$G)
  what <- reactive(input$what)

  pc <- princomp(x = iris[,1:4],
                 cor = TRUE,
                 scores = TRUE)

  sc <- pc$scores

  clus <- reactive({
    kmeans(x = sc[,1:2],
           centers = k())
  })

  km <- reactive({
             plot(sc[,1:2],
             pch = 21,
             cex = 1.5,
             bg = clus()$cluster,
             main = paste0("K = ", k()))
  })

  mc <- reactive({
    Mclust(iris[,1:4], G = G())
  })

  observeEvent(input$null, {

    mc <- reactive({
      Mclust(iris[,1:4], G = NULL)
  })

    mcplot <- reactive({
      plot(mc(), what = what())
    })

    output$plot2 <- renderPlot({

      mcplot()

    })
  })
  mcplot <- reactive({
    plot(mc(), what = what())
  })

  output$plot1 <- renderPlot({

    km()

  })

  output$plot2 <- renderPlot({

    mcplot()

  })

}

shinyApp(ui = ui, server = server)
