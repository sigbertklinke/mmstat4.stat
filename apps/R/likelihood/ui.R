library("shiny")

shinyUI(fluidPage(
  div(class="navbar navbar-static-top",
      div(class = "navbar-inner", 
          fluidRow(column(12, div(class = "brand pull-left", "Statistik I&II - Likelihood und Loglikelihood"))
          ))),
  sidebarLayout(
    sidebarPanel(
      radioButtons("dist", "Verteilungstyp:",
                   c("Bernoulli", "Poisson", "Exponential")),
      uiOutput("sliderUI"),
      helpText(HTML("gr&uuml;n = letzter Wert<br>rot = bisheriges Maximum")),
      hr(),
      numericInput("obs1", "Beobachtungswerte", value = runif(1)),
      numericInput("obs2", "", value = runif(1)),
      numericInput("obs3", "", value = runif(1)),
      numericInput("obs4", "", value = runif(1)),
      numericInput("obs5", "", value = runif(1))
    ),
    mainPanel(plotOutput("outputPlot", height="600px"))
  )
))
