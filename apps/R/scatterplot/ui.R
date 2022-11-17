library("shiny")
library("shinydashboard")

dashboardPage(
  dashboardHeader(title="MM*Stat"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text=gettext("Graphics"), startExpanded=TRUE,
               uiOutput("graphUI"),
               uiOutput("permuteUI")),
      menuItem(text=gettext("Coefficients"), startExpanded=TRUE,
               uiOutput("coeffUI")),
      menuItem(text=gettext("Data choice"), startExpanded=TRUE,
               uiOutput("datasetUI"),
               uiOutput("variableSelectUI")), 
      menuItem(text=gettext("Options"), startExpanded=FALSE,
               uiOutput("cexUI"))
    )
  ),
  dashboardBody(
    fluidRow(
      column(width = 12, 
             box(title=gettext("Scatterplots and correlation"), status="primary",
                 plotOutput("scatterPlot"))
      )
    )
  )
)

#shinyUI(fluidPage(
#  div(class="navbar navbar-static-top",
#      div(class = "navbar-inner", 
#          fluidRow(column(4, div(class = "brand pull-left", gettext("Scatterplots and correlation"))),
#                   column(2, checkboxInput("showgraph", gettext("Graphics"), TRUE)),
#                   column(2, checkboxInput("showcoeff", gettext("Coefficients"), FALSE)),
#                   column(2, checkboxInput("showdata", gettext("Data choice"), TRUE)),
#                   column(2, checkboxInput("showoptions", gettext("Options"), FALSE))))),
#    
#  sidebarLayout(
#    sidebarPanel(
#      conditionalPanel(
#        condition = 'input.showgraph',
#        uiOutput("graphUI"),
#        uiOutput("permuteUI")
#      ),
#      conditionalPanel(
#        condition = 'input.showcoeff',
#        hr(),
#        uiOutput("coeffUI")
#      ),
#      conditionalPanel(
#        condition = 'input.showdata',
#        hr(),
#        uiOutput("datasetUI"),
#        uiOutput("variableSelectUI")
#      ),
#      conditionalPanel(
#        condition = 'input.showoptions',
#        hr(),
#        uiOutput("cexUI")
#      )
#    ),
#    mainPanel(plotOutput("scatterPlot"))),
#
#  htmlOutput("logText")  
#))