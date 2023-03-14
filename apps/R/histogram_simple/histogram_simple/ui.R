library("shiny")
library("shinydashboard")

dashboardPage(
  dashboardHeader(title="MM*Stat"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text=gettext("Histogram parameter"), startExpanded=TRUE,
               uiOutput("binsUI"),
               uiOutput("obsUI")),
      menuItem(text=gettext("Data choice"), startExpanded=TRUE,
               uiOutput("datasetUI"),
               uiOutput("variableUI")), 
      menuItem(text=gettext("Options"), startExpanded=FALSE,
               uiOutput("cexUI"))
    )
  ),
  dashboardBody(
    fluidRow(
      column(width = 12, 
             box(title=gettext("Simple Histogram"), status="primary",
                 plotOutput("distPlot"))
      )
    )
  )
)

#shinyUI(fluidPage(
#  div(class="navbar navbar-static-top",
#      div(class = "navbar-inner", 
#          fluidRow(column(6, div(class = "brand pull-left", gettext("Simple Histogram"))),
#                   column(2, checkboxInput("showbins", gettext("Histogram parameter"), TRUE)),
#                   #column(2, checkboxInput("showobs", gettext("Show observations"), FALSE)),
#                   column(2, checkboxInput("showdata", gettext("Data choice"), FALSE)),
#                   column(2, checkboxInput("showoptions", gettext("Options"), FALSE))))),
#    
#  sidebarLayout(
#    sidebarPanel(
#      conditionalPanel(
#        condition = 'input.showbins',
#        uiOutput("binsUI"),
#        br(),
#        uiOutput("obsUI")
#      ),
#      conditionalPanel(
#        condition = 'input.showdata',
#        hr(),
#        uiOutput("datasetUI"),
#        uiOutput("variableUI")
#      ),
#      conditionalPanel(
#        condition = 'input.showoptions',
#        hr(),
#        uiOutput("cexUI")
#      )
#    ),
#    mainPanel(plotOutput("distPlot"))),
#
#  htmlOutput("logText")  
#))