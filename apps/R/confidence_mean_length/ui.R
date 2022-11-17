library("shiny")

library("shiny")
library("shinydashboard")

dashboardPage(
  dashboardHeader(title="MM*Stat"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text=gettext("Confidence interval parameter"), startExpanded=TRUE,
               uiOutput("conflevelUI"),
               uiOutput("sizeUI"),
               uiOutput("sigmavarUI"),
               uiOutput("lengthUI")),
      menuItem(text=gettext("Options"), startExpanded=FALSE,
               uiOutput("cexUI"))
    )
  ),
  dashboardBody(
    fluidRow(
      column(width = 12, 
             box(title=gettext("Confidence interval length for the mean"), status="primary",
                 plotOutput("outputConfPlot"))
      )
    )
  )
)

#shinyUI(fluidPage(
#    
#    div(class="navbar navbar-static-top",
#        div(class = "navbar-inner", 
#            fluidRow(column(8, div(class = "brand pull-left", gettext("Confidence interval length for the mean"))),
#                     column(2, checkboxInput("showtest", gettext("Confidence interval parameter"), TRUE)),
#                     column(2, checkboxInput("showoptions", gettext("Options"), FALSE))))),
#    
#    sidebarLayout(
#      sidebarPanel(
#        conditionalPanel(
#          condition = 'input.showtest',
#          uiOutput("conflevelUI"),
#          br(),
#          uiOutput("sizeUI"),
#          br(),
#          uiOutput("sigmavarUI"),
#          uiOutput("lengthUI")
#          ),
#        conditionalPanel(
#          condition = 'input.showoptions',
#          hr(),
#          uiOutput("cexUI")
#        )
#      ),
#    
#      mainPanel(plotOutput("outputConfPlot"))),
#      htmlOutput("logText")
#  ))
  