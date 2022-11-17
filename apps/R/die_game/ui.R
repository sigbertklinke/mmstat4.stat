library("shiny")
library("shinydashboard")

dashboardPage(
  dashboardHeader(title="MM*Stat"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text=gettext("Game parameter"), startExpanded=TRUE,
               uiOutput("rollsUI"),
               uiOutput("sixesUI")),
      menuItem(text=gettext("Game parameter"), startExpanded=TRUE,
               uiOutput("probUI")),
      menuItem(text=gettext("Options"), startExpanded=FALSE,
               uiOutput("cexUI"))
    )
  ),
  dashboardBody(
    fluidRow(
      column(width = 12, 
             box(title=gettext("Die rolling sisters"), status="primary",
                 htmlOutput("formula"),
                 plotOutput("distPlot"))
      )
    )
  )
)
#shinyUI(fluidPage(
#  div(class="navbar navbar-static-top",
#      div(class = "navbar-inner", 
#          fluidRow(column(4, div(class = "brand pull-left", gettext("Die rolling sisters"))),
#                   column(2, checkboxInput("showgame", gettext("Game parameter"), TRUE)),
#                   column(2, checkboxInput("showprob", gettext("Game parameter"), TRUE)),
#                   column(2, checkboxInput("showoptions", gettext("Options"), FALSE))))),
#    
#    sidebarLayout(
#      sidebarPanel(
#        conditionalPanel(
#          condition = 'input.showgame',
#          uiOutput("rollsUI"),
#          br(),
#          uiOutput("sixesUI")
#          ),
#        conditionalPanel(
#          condition = 'input.showprob',
#          br(),
#          uiOutput("probUI")
#          ),
#        conditionalPanel(
#          condition = 'input.showoptions',
#          hr(),
#          uiOutput("cexUI")
#          )
#        ),
#    
#      mainPanel(htmlOutput("formula"),
#                plotOutput("distPlot"))),
#
#    htmlOutput("logText")
#))