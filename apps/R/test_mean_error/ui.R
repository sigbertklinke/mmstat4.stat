library("shiny")
library("shinydashboard")

dashboardPage(
  dashboardHeader(title="MM*Stat"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text=gettext("Test parameter"), startExpanded=TRUE,
               uiOutput("testUI"),
               uiOutput("mu0UI"),
               uiOutput("alphaUI")),
      menuItem(text=gettext("Sample parameter"), startExpanded=TRUE,
               uiOutput("sizeUI"),
               uiOutput("goUI")),
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
             box(title=gettext("Test of mean with type I and II error"), status="primary",
                 plotOutput("outputPlot"),
                 htmlOutput("distText"))
      )
    )
  )
)

#shinyUI(fluidPage(
#  
#  div(class="navbar navbar-static-top",
#      div(class = "navbar-inner", 
#          fluidRow(column(4, div(class = "brand pull-left", gettext("Test of mean with type I and II error"))),
#                   column(2, checkboxInput("showtest", gettext("Test parameter"), TRUE)),
#                   column(2, checkboxInput("showsample", gettext("Sample parameter"), FALSE)),
#                   column(2, checkboxInput("showdata", gettext("Data choice"), FALSE)),
#                   column(2, checkboxInput("showoptions", gettext("Options"), FALSE))))),
# sidebarLayout(
#    sidebarPanel(
#      conditionalPanel(
#        condition = 'input.showtest',
#        uiOutput("testUI"),
#        br(),
#        uiOutput("mu0UI"),
#        uiOutput("alphaUI")
#      ),
#      conditionalPanel(
#        condition = 'input.showsample',
#        hr(),
#        uiOutput("sizeUI"),
#        br(),
#        uiOutput("goUI")
##        br(),
##        uiOutput("sampleUI"),
##        uiOutput("populationUI")
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
#      )), 
#    
#      mainPanel(plotOutput("outputPlot"),
#                HTML('<hr>'),
#                htmlOutput("distText"))),
#
#      htmlOutput("logText")
#  ))
  