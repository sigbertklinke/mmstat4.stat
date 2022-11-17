library("shiny")
library("shinydashboard")

dashboardPage(
  dashboardHeader(title="MM*Stat"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text=gettext("Choose dotplot type"), startExpanded=TRUE,
               uiOutput("methodUI")),
      menuItem(text=gettext("Location parameters"), startExpanded=TRUE,
               uiOutput("addmeanUI"),
               uiOutput("addmedianUI")),
      menuItem(text=gettext("Data choice"), startExpanded=FALSE,
               uiOutput("datasetUI"),
               uiOutput("variableUI")), 
      menuItem(text=gettext("Options"), startExpanded=FALSE,
               hr(),
               uiOutput("cexUI"))
    )
  ),
  dashboardBody(
    fluidRow(
      column(width = 12, 
             box(title=gettext("Dotplot with location parameters"), status="primary",
                 plotOutput("distPlot"))
      )
    )
  )
)

#shinyUI(fluidPage(
#  div(class="navbar navbar-static-top",
#      div(class = "navbar-inner", 
#          fluidRow(column(4, div(class = "brand pull-left", gettext("Dotplot with location parameters"))),
#                   column(2, checkboxInput("showtype", gettext("Choose dotplot type"), TRUE)),
#                   column(2, checkboxInput("showparameters", gettext("Location parameters"), TRUE)),
#                   column(2, checkboxInput("showdata", gettext("Data choice"), FALSE)),
#                   column(2, checkboxInput("showoptions", gettext("Options"), FALSE))))),
#    
#  sidebarLayout(
#    sidebarPanel(
#      conditionalPanel(
#        condition = 'input.showtype',
#        uiOutput("methodUI")
#      ),
#      conditionalPanel(
#        condition = 'input.showparameters',
#        br(),
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
#    mainPanel(plotOutput("distPlot"))),
#
#    htmlOutput("logText")
#  ))
  