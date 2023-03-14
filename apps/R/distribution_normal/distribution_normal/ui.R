library("shiny")
library("shinydashboard")

dashboardPage(
  dashboardHeader(title="MM*Stat"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text=gettext("Distribution parameter"), startExpanded=TRUE,
               uiOutput("norm.muUI"),
               uiOutput("norm.sigma2UI")),
      menuItem(text=gettext("Choose function"), startExpanded=TRUE,
               uiOutput("pdforcdfUI"),
               uiOutput("refitUI")),
      menuItem(text=gettext("Options"), startExpanded=FALSE,
               uiOutput("cexUI"))
    )
  ),
  dashboardBody(
    fluidRow(
      column(width = 12, 
             box(title=gettext("Normal distributions"), status="primary",
                 plotOutput("distPlot"))
      )
    )
  )
)

#shinyUI(fluidPage(
#  
#   div(class="navbar navbar-static-top",
#        div(class = "navbar-inner", 
#            fluidRow(column(4, div(class = "brand pull-left", gettext("Continuous probability distributions"))),
#                     column(2, checkboxInput("showdist", gettext("Choose distribution"), TRUE)),
#                     column(2, checkboxInput("showparameter", gettext("Distribution parameter"), TRUE)),
#                     column(2, checkboxInput("showfunction", gettext("Choose function"), TRUE)),
#                     column(2, checkboxInput("showoptions", gettext("Options"), FALSE))))),
#    
##    conditionalPanel(condition = "input.distribution=='EXP'",
##                     uiOutput("EXPUI")),
##    conditionalPanel(condition = "input.distribution=='NORM'",
##                     uiOutput("NORMUI")),
##    br(),
#    sidebarLayout(
#      sidebarPanel(
#        conditionalPanel(
#          condition = 'input.showdist',
#          uiOutput("distributionUI")
#        ),
#        conditionalPanel(
#          condition = "input.distribution=='EXP'",
#          conditionalPanel(
#            condition = 'input.showparameter',
#            uiOutput("exp.lambdaUI")
#          )
#        ),
#        conditionalPanel(
#          condition = "input.distribution=='NORM'",
#          conditionalPanel(
#            condition = 'input.showparameter',
#            uiOutput("norm.muUI"),
#            uiOutput("norm.sigma2UI")
#          )
#        ),
#        conditionalPanel(
#          condition = 'input.showfunction',
#          br(),
#          uiOutput("pdforcdfUI"),
#          uiOutput("refitUI")
#        ),
#        conditionalPanel(
#          condition = 'input.showoptions',
#          hr(),
#          uiOutput("cexUI")
#        )        
#      ),
# 
#      mainPanel(plotOutput("distPlot"))
#      ),
#
#      htmlOutput("logText")
#  ))
 