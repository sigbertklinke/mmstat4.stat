library("shiny")

shinyUI(fluidPage(
  div(class="navbar navbar-static-top",
      div(class = "navbar-inner", 
          fluidRow(column(12, div(class = "brand pull-left", "Statistik I&II - Schwankungs- und Konfidenzintervall"))
          ))),
  sidebarLayout(
    sidebarPanel(
#      uiOutput("typeUI"),
      uiOutput("alphaUI"),
      hr(),
      uiOutput("sizeUI"),
      br(),
      uiOutput("goUI"),
      uiOutput("speedUI"),
      uiOutput("sampleUI"),
      uiOutput("populationUI"),
      hr()
      ),
        
  mainPanel(plotOutput("outputPlot", height="600px"))
    #,
    #            HTML('<hr>'),
    #            htmlOutput("distText"))
  )
))
