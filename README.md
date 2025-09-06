---
title: "Project2"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Project2}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
```

```{r setup}
library(PROJECT2AFORCEMATH5793)
library(ggplot2)
library(dplyr)
library(mclust)
```

# Normality:

The initial function, DistCalc, provides the calculation for statistical distance required in the second function. The specified example already provided it but in the event this isn’t the case, a function is provided.

In the context of assessing normality, examples 4.14 and 4.15 provide an approach towards multivariate problems and identifying outliers through table 4.3 where different techniques were implemented to analyze the stiffness of boards. Below is the “NormCheck” function which takes the statistical distance to create a a chi squared plot. Outliers are then identified based on the z-scaled criteria and these outliers are removed at which point a new plot is produced.

Overall, the function is excellent at replicating the worked example where, for the majority of the data, it appears multivariate normal although there is an evident outlier present.

```{r}
tbl <- PROJECT2AFORCEMATH5793::T4_3
head(tbl)
```

```{r}
PROJECT2AFORCEMATH5793::DistCalc(tbl[,-5])
```

```{r}
PROJECT2AFORCEMATH5793::NormCheck(tbl)
```

# Principle Component Analysis

Using the worked example 8.3, 5 socioeconomic variables for Madison, Wisconsin: total population, profession, employment, government employment, medium home value were analyzed using the function “myPrinComp” which takes in a data frame along with an option for “S” or “R” to utilize the variance-covariance matrix or correlation matrix in determining the principle components. This are then visually displayed as a scree plot.

The function replicates the calculations shown in the worked example while also adding the scree plot visualization. In the context of the example problem, the majority of the variability is explained by the first two principle components which account for approximately 92%. 

```{r}
df <- PROJECT2AFORCEMATH5793::T8_5
head(df)
```

```{r}
PROJECT2AFORCEMATH5793::myPrinComp(df = df,
                                   mat = "S")
```

```{r}
PROJECT2AFORCEMATH5793::myPrinComp(df = df,
                                   mat = "R")
```

# Factor Analysis

An analysis of example 9.4 is shown below through the use of the function “myFA” which takes in a data frame, the desired number of factors along with a specified rotation and calculates the correlation matrix “R” which is used to determine loadings, specified variance, factors, residual matrix, and cumulative proportion.

Specifically from the loadings calculated for factors 1 and 2, the first three variables (JP Morgan, Citibank, and Wells Fargo) are loaded heavily on the first factor while 4 and 5 (Royal Dutch Shell and ExxonMobil) are loaded heavily on the second factor indicating these groupings share some commonality.

```{r}
df <- PROJECT2AFORCEMATH5793::T8_4
head(df)
```

```{r}
PROJECT2AFORCEMATH5793::myFA(df = df,
                             factors = 2,
                             rotation = "none")
```

# Statistical Cluster Analysis

The “Clust” package demonstrated below utilizes example 12.13 with some additional information provided. The kmean function is utilized to show a general clustering using principle components and then Mclust is utilized with various parameters allowing a user to modify the optimal number of mixture components and change plotting type which is tested to produce the figures in example 12.13. The function allows for variability in the parameters depending on what a user would like to do although G being null resembles that of the textbook. 

```{r}
iris <- PROJECT2AFORCEMATH5793::iris
head(iris)
```

```{r}
PROJECT2AFORCEMATH5793::Clust(df = iris[,1:4], k = 2, G = NULL, what = "classification")
```

# Shiny

```
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
```

![](C:/Users\tcaal\Desktop\Advanced_Stats\Module14\shiny.png){}
