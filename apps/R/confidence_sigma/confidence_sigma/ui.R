library("shiny")
library("shinydashboard")

dashboardPage(
  dashboardHeader(title="MM*Stat"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text=gettext("Confidence interval parameter"), startExpanded=TRUE,
               uiOutput("conflevelUI"),
               uiOutput("sizeUI")),
      menuItem(text=gettext("Sample drawing"), startExpanded=TRUE,
               uiOutput("goUI"),
               uiOutput("resetUI"),
               uiOutput("speedUI")),
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
             box(title=gettext("Confidence intervals for the variance"), status="primary",
                 plotOutput("outputConfPlot"),
                 plotOutput("outputSamplePlot", height = "200px")
             )
      )
    )
  )
)

#shinyUI(fluidPage(
#    
#    div(class="navbar navbar-static-top",
#        div(class = "navbar-inner", 
#            fluidRow(column(4, div(class = "brand pull-left", gettext("Confidence intervals for the variance"))),
#                     column(2, checkboxInput("showtest", gettext("Confidence interval parameter"), TRUE)),
#                     column(2, checkboxInput("showsample", gettext("Sample drawing"), TRUE)),
#                     column(2, checkboxInput("showdata", gettext("Data choice"), FALSE)),
#                     column(2, checkboxInput("showoptions", gettext("Options"), FALSE))))),
#    
#    sidebarLayout(
#      sidebarPanel(
#        conditionalPanel(
#          condition = 'input.showtest',
#          uiOutput("conflevelUI"),
#          br(),
#          uiOutput("sizeUI")
#        ),
#        conditionalPanel(
#          condition = 'input.showsample',
#          hr(),
#          uiOutput("goUI"),
#          uiOutput("resetUI"),
#          uiOutput("speedUI")
#        ),
#        conditionalPanel(
#          condition = 'input.showdata',
#          hr(),
#          uiOutput("datasetUI"),
#         uiOutput("variableUI")
#        ),
#        conditionalPanel(
#          condition = 'input.showoptions',
#          hr(),
#          uiOutput("cexUI")
#        )
#      ),
#    
#      mainPanel(plotOutput("outputConfPlot"),
#                plotOutput("outputSamplePlot", height = "200px"))),
#
#      htmlOutput("logText")
#  ))
