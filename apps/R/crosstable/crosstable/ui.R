library("shiny")
library("shinydashboard")

dashboardPage(
  dashboardHeader(title="MM*Stat"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text=gettext("Coefficients"), startExpanded=TRUE,
               uiOutput("coeffUI")),
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
             box(title=gettext("Crosstable"), status="primary",
                 htmlOutput("contingencyTable"))
      )
    )
  )
)
#shinyUI(fluidPage(
#  div(class="navbar navbar-static-top",
#      div(class = "navbar-inner", 
#          fluidRow(column(6, div(class = "brand pull-left", gettext("Crosstable"))),
#                   column(2, checkboxInput("showcoeff", gettext("Coefficients"), FALSE)),
#                   column(2, checkboxInput("showdata", gettext("Data choice"), TRUE)),
#                   column(2, checkboxInput("showoptions", gettext("Options"), FALSE))))),
#    
#  sidebarLayout(
#    sidebarPanel(
#      conditionalPanel(
#        condition = 'input.showcoeff',
#        uiOutput("coeffUI")
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
#    mainPanel(htmlOutput("contingencyTable"))),
#
#  htmlOutput("logText")  
#))