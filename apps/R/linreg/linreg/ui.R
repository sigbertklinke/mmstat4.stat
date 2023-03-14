library("shiny")
library("shinydashboard")

dashboardPage(
  dashboardHeader(title="MM*Stat"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text=gettext("Visualize"), startExpanded=TRUE,
               uiOutput("showUI")),
      menuItem(text=gettext("Data choice"), startExpanded=TRUE,
               uiOutput("datasetUI"),
               uiOutput("variableYSelectUI"),
               uiOutput("variableXSelectUI")), 
      menuItem(text=gettext("Options"), startExpanded=FALSE,
               uiOutput("cexUI"))
    )
  ),
  dashboardBody(
    fluidRow(
      column(width = 12, 
             box(title=gettext("Simple linear regression"), status="primary",
                 plotOutput("scatterPlot"),
                 verbatimTextOutput("outputR"))
      )
    )
  )
)

#shinyUI(fluidPage(
#  div(class="navbar navbar-static-top",
#      div(class = "navbar-inner", 
#          fluidRow(column(6, div(class = "brand pull-left", gettext("Simple linear regression"))),
#                   column(2, checkboxInput("showcoeff", gettext("Visualize"), TRUE)),
#                   column(2, checkboxInput("showdata", gettext("Data choice"), TRUE)),
#                   column(2, checkboxInput("showoptions", gettext("Options"), FALSE))))),
#    
#  sidebarLayout(
#    sidebarPanel(
#      conditionalPanel(
#        condition = 'input.showcoeff',
#        uiOutput("showUI")
#      ),
#      conditionalPanel(
#        condition = 'input.showdata',
#        hr(),
#        uiOutput("datasetUI"),
#        uiOutput("variableYSelectUI"),
#        uiOutput("variableXSelectUI")
#      ),
#      conditionalPanel(
#        condition = 'input.showoptions',
#        hr(),
#        uiOutput("cexUI")
#      )
#    ),
#    mainPanel(plotOutput("scatterPlot"),
#              verbatimTextOutput("outputR")
#              )),
#
#  htmlOutput("logText")  
#))