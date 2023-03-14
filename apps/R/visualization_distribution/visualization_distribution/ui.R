library("shiny")
library("shinydashboard")

dashboardPage(
  dashboardHeader(title="MM*Stat"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text=gettext("Plot parameter"), startExpanded=TRUE,
               uiOutput("methodUI"),
               uiOutput("binsUI")),
      menuItem(text=gettext("Descriptives"), startExpanded=TRUE,
               uiOutput("addmeanUI"),
               uiOutput("addmedianUI")),
      menuItem(text=gettext("Data choice"), startExpanded=TRUE,
               uiOutput("datasetUI"),
               uiOutput("variableUI")), 
      menuItem(text=gettext("Options"), startExpanded=FALSE,
               uiOutput("cexUI"))
    )
  ),
  dashboardBody(
    fillPage(
      fluidRow(
        box(title=gettext("Variable visualizations"), status="primary", width=6, 
            plotOutput("plotStrip", height = "300px")),
        box(width=6, title='', plotOutput("plotHist", height = "300px"))
      ),
      fluidRow(
        box(width=6, plotOutput("plotBox", height = "300px")),
        box(width=6, plotOutput("plotEcdf", height = "300px"))
      )
    )
  )
)

#shinyUI(fluidPage(
#  div(class="navbar navbar-static-top",
#      div(class = "navbar-inner", 
#          fluidRow(column(4, div(class = "brand pull-left", gettext("Variable visualizations"))),
#                   column(2, checkboxInput("showplot", gettext("Plot parameter"), TRUE)),
#                   column(2, checkboxInput("showparam", gettext("Descriptives"), TRUE)),
#                   column(2, checkboxInput("showdata", gettext("Data choice"), FALSE)),
#                   column(2, checkboxInput("showoptions", gettext("Options"), FALSE))))),
#
#  sidebarLayout(
#    sidebarPanel(
#      conditionalPanel(
#        condition = 'input.showplot',
#        uiOutput("methodUI"),
#        uiOutput("binsUI")
#      ),
#      conditionalPanel(
#        condition = 'input.showparam',
#        hr(),
#        uiOutput("addmeanUI"),
#        uiOutput("addmedianUI")
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
#    
#    mainPanel(fluidRow(
#      column(width=6, height=3, plotOutput("plotStrip", height = "300px")),
#      column(width=6, height=3, plotOutput("plotHist", height = "300px"))),
#    fluidRow(
#      column(width=6, height=3,  plotOutput("plotBox", height = "300px")),
#      column(width=6, height=3,  plotOutput("plotEcdf", height = "300px"))),
#    HTML('<hr>'),
#    htmlOutput("distText"))),
#  
#    htmlOutput("logText")
#))