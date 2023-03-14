library("shiny")
library("shinydashboard")

dashboardPage(
  dashboardHeader(title="MM*Stat"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text=gettext("Deal parameter"), startExpanded=TRUE,
               uiOutput("pointdoorUI"),
               htmlOutput("hostopensUI"),
               uiOutput("afteropenUI")),
      menuItem(text=gettext("Make a deal"), startExpanded=TRUE,
               uiOutput("guestUI", inline = TRUE), 
               uiOutput("hostUI", inline = TRUE), 
               uiOutput("speedUI")), 
      menuItem(text=gettext("Options"), startExpanded=FALSE,
               uiOutput("cexUI"))
    )
  ),
  dashboardBody(
    fluidRow(
      column(width = 12, 
             box(title=gettext("Let's Make a Deal"), status="primary",
                 plotOutput("distPlot"),
                 htmlOutput('imagePlot'))
      )
    )
  )
)

#shinyUI(fluidPage(
#  div(class="navbar navbar-static-top",
#      div(class = "navbar-inner", 
#          fluidRow(column(4, div(class = "brand pull-left", gettext("Let's Make a Deal"))),
#                   column(2, checkboxInput("showdeal", gettext("Make a deal"), TRUE)),
#                   column(2, checkboxInput("showspeed", gettext("Make a deal"), FALSE)),
#                   column(2, checkboxInput("showoptions", gettext("Options"), FALSE))))),  
#    
#  sidebarLayout(
#    sidebarPanel(
#      conditionalPanel(
#        condition = 'input.showdeal',
#        uiOutput("pointdoorUI"),
#        br(),
#        htmlOutput("hostopensUI"),
#        #textOutput("hostopensUI"),
#        br(),
#        uiOutput("afteropenUI"), 
#        br(),
#        uiOutput("goUI")
#      ),
#      conditionalPanel(
#        condition = 'input.showspeed',
#        hr(),
#        uiOutput("speedUI")
#      ),
#      conditionalPanel(
#        condition = 'input.showoptions',
#        hr(),
#        uiOutput("cexUI")
#      )
#      ),
#
#      mainPanel(
#        plotOutput("distPlot"),
#        htmlOutput('imagePlot')
#      )),
#
#    htmlOutput("logText")
#    
#))  