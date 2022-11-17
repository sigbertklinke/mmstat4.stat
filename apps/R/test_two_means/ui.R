library("shiny")
library("shinydashboard")

dashboardPage(
  dashboardHeader(title="MM*Stat"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text=gettext("Test parameter"), startExpanded=TRUE,
               uiOutput("alphaUI")),
      menuItem(text=gettext("Sample parameter"), startExpanded=TRUE,
               uiOutput("size1UI"),
               uiOutput("size2UI"),
               uiOutput("goUI")),
      menuItem(text=gettext("Data choice"), startExpanded=TRUE,
               uiOutput("datasetUI"),
               uiOutput("variableUI"),
               uiOutput("groupUI")), 
      menuItem(text=gettext("Options"), startExpanded=FALSE,
               uiOutput("cexUI"))
    )
  ),
  dashboardBody(
    fillPage(
      column(width = 12, 
             box(title=gettext("Test of two means"), status="primary",
                 plotOutput("outputTestPlot"),
                 plotOutput("outputSamplePlot", height = "200px"))
      )
    )
  )
)

#shinyUI(fluidPage(
#  div(class="navbar navbar-static-top",
#      div(class = "navbar-inner", 
#          fluidRow(column(4, div(class = "brand pull-left", gettext("Test of two means"))),
#                   column(2, checkboxInput("showtest", gettext("Test parameter"), TRUE)),
#                   column(2, checkboxInput("showsample", gettext("Sample parameter"), TRUE)),
#                   column(2, checkboxInput("showdata", gettext("Data choice"), FALSE)),
#                   column(2, checkboxInput("showoptions", gettext("Options"), FALSE))))),
#    
#    sidebarLayout(
#      sidebarPanel(
#        conditionalPanel(
#          condition = 'input.showtest',
#          uiOutput("alphaUI")
#        ),
#        conditionalPanel(
#          condition = 'input.showsample',
#          hr(),
#          uiOutput("size1UI"),
#          br(),
#          uiOutput("size2UI"),
#          br(),
#          uiOutput("goUI")
#        ),
#        conditionalPanel(
#          condition = 'input.showdata',
#          hr(),
#          uiOutput("datasetUI"),
#          uiOutput("variableUI"),
#          uiOutput("groupUI")
#        ),
#        conditionalPanel(
#          condition = 'input.showoptions',
#          hr(),
#          uiOutput("cexUI")
#        )
#      ),
#    
#      mainPanel(plotOutput("outputTestPlot"),
#                plotOutput("outputSamplePlot", height = "200px"))),
#
#      htmlOutput("logText")
#  ))