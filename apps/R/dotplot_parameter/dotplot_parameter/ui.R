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
               uiOutput("addmedianUI"),
               uiOutput("addrangeUI"),
               uiOutput("addiqrUI")),
      menuItem(text=gettext("Data choice"), startExpanded=FALSE,
               uiOutput("datasetUI"),
               uiOutput("variableUI")), 
      menuItem(text=gettext("Options"), startExpanded=FALSE,
               uiOutput("cexUI"))
    )
  ),
  dashboardBody(
    fluidRow(
      column(width = 12, 
             box(title=gettext("Dotplot with parameters"), status="primary",
                 plotOutput("distPlot"))
      )
    )
  )
)

#shinyUI(fluidPage(
#  div(class="navbar navbar-static-top",
#      div(class = "navbar-inner", 
#          fluidRow(column(4, div(class = "brand pull-left", gettext("Dotplot with parameters"))),
#                   column(2, checkboxInput("showtype", gettext("Choose dotplot type"), TRUE)),
#                   column(2, checkboxInput("showparameters", gettext("Parameters"), TRUE)),
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
#        uiOutput("addmedianUI"),
#        uiOutput("addrangeUI"),
#        uiOutput("addiqrUI")
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
#      mainPanel(plotOutput("distPlot"))),
#
#    htmlOutput("logText")
#  ))
  