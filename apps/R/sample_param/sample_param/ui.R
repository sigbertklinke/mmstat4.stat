library("shiny")
library("shinydashboard")

dashboardPage(
  dashboardHeader(title="MM*Stat"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text=gettext("Sample parameter"), startExpanded=TRUE,
               uiOutput("paramUI"),
               uiOutput("sizeUI")),
      menuItem(text=gettext("Specify speed"), startExpanded=TRUE,
               uiOutput("goUI"),
               uiOutput("speedUI")),
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
             box(title=gettext("Distribution of sample parameters"), status="primary",
                 plotOutput("samplePlot"),
                 plotOutput("outputSamplePlot", height = "200px"))
      )
    )
  )
)

#shinyUI(fluidPage(
#  
#  div(class="navbar navbar-static-top",
#      div(class = "navbar-inner", 
#          fluidRow(column(4, div(class = "brand pull-left", gettext("Distribution of sample parameters"))),
#                   column(2, checkboxInput("showsample", gettext("Sample parameter"), TRUE)),
#                   column(2, checkboxInput("showspeed", gettext("Specify speed"), FALSE)),
#                   column(2, checkboxInput("showdata", gettext("Data choice"), FALSE)),
#                   column(2, checkboxInput("showoptions", gettext("Options"), FALSE))))),
#    
#  sidebarLayout(
#    sidebarPanel(
#      conditionalPanel(
#        condition = 'input.showsample',
#        uiOutput("paramUI"),
#        br(),
#        uiOutput("sizeUI"),
#        br(),
#        uiOutput("goUI")
#      ),
#      conditionalPanel(
#        condition = 'input.showspeed',
#        br(),
#        uiOutput("speedUI")
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
#      ),
#    
#      mainPanel(plotOutput("samplePlot"),
#                plotOutput("outputSamplePlot", height = "200px"))),
#
#      htmlOutput("logText")
#  ))
  