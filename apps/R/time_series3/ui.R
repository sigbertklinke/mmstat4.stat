library("shiny")
library("shinydashboard")

dashboardPage(
  dashboardHeader(title="MM*Stat"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text=gettext("Trend"), startExpanded=TRUE,
               uiOutput("trendUI")),
      menuItem(text=gettext("Seasonality"), startExpanded=TRUE,
               uiOutput("seasonUI")),
      menuItem(text= gettext("Model quality"), startExpanded=TRUE,
               uiOutput("qualityUI")),
      menuItem(text=gettext("Data choice"), startExpanded=TRUE,
               uiOutput("datasetUI"),
               uiOutput("variableUI")), 
      menuItem(text=gettext("Options"), startExpanded=FALSE,
               uiOutput("cexUI"))
    )
  ),
  dashboardBody(
    fillPage(
      column(width = 12, 
             box(title=gettext("Time series analysis"), status="primary",
                 plotOutput("outputPlot"),
                 verbatimTextOutput("outputTest"))
      )
    )
  )
)

#shinyUI(fluidPage(
#  div(class="navbar navbar-static-top",
#      div(class = "navbar-inner", 
#          fluidRow(column(4, div(class = "brand pull-left", gettext("Time series analysis"))),
#                   column(2, checkboxInput("showtrend", gettext("Trend"), TRUE)),
#                   column(2, checkboxInput("showseason", gettext("Seasonality"), TRUE)),
#                   column(2, checkboxInput("showquality", gettext("Model quality"), TRUE)),
#                   column(2, checkboxInput("showdata", gettext("Data choice"), FALSE)),
#                   column(2, checkboxInput("showoptions", gettext("Options"), FALSE))))),
#    
#    sidebarLayout(
#      sidebarPanel(
#        conditionalPanel(
#          condition = 'input.showtrend',
#          uiOutput("trendUI"),
#          hr()
#        ),
#        conditionalPanel(
#          condition = 'input.showseason',
#          uiOutput("seasonUI"),
#          hr()
#        ),
#        conditionalPanel(
#          condition = 'input.showquality',
#          uiOutput("qualityUI"),
#          hr()
#        ),
#        conditionalPanel(
#          condition = 'input.showdata',
#          uiOutput("datasetUI"),
#          uiOutput("variableUI"),
#          hr()
#        ),
#        conditionalPanel(
#          condition = 'input.showoptions',
#          uiOutput("cexUI"),
#          hr()
#        )
#      ),
#    
#      mainPanel(plotOutput("outputPlot"),
#                br(),
#                verbatimTextOutput("outputTest"))),
#
#      htmlOutput("logText")
#  ))