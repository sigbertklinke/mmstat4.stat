library("shiny")
library("shinydashboard")

dashboardPage(
  dashboardHeader(title="MM*Stat"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text=gettext("Distribution parameter"), startExpanded=TRUE,
               uiOutput("hyper.NUI"),
               uiOutput("hyper.MUI"),
               uiOutput("hyper.nUI")),
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
             box(title=gettext("Hypergeometric distributions"), status="primary",
                 plotOutput("distPlot"))
      )
    )
  )
)

#shinyUI(fluidPage(   
#    
#  div(class="navbar navbar-static-top",
#        div(class = "navbar-inner", 
#            fluidRow(column(4, div(class = "brand pull-left", gettext("Discrete probability distributions"))),
#                     column(2, checkboxInput("showdist", gettext("Choose distribution"), TRUE)),
#                     column(2, checkboxInput("showparameter", gettext("Distribution parameter"), TRUE)),
#                     column(2, checkboxInput("showfunction", gettext("Choose function"), TRUE)),
#                     column(2, checkboxInput("showoptions", gettext("Options"), FALSE))))),
#    
##    conditionalPanel(condition = "input.distribution=='BINOM'",
##                     uiOutput("BINOMUI")),
##    conditionalPanel(condition = "input.distribution=='HYPER'",
##                     uiOutput("HYPERUI")),
##    conditionalPanel(condition = "input.distribution=='POIS'",
##                     uiOutput("POISUI")),
##    br(),
#    sidebarLayout(
#      sidebarPanel(
#        conditionalPanel(
#          condition = 'input.showdist',
#          uiOutput("distributionUI")
#          ),
#        conditionalPanel(
#          condition = "input.distribution=='BINOM'",
#          conditionalPanel(
#            condition = 'input.showparameter',
#            uiOutput("binom.nUI"),
#            uiOutput("binom.pUI")
#            )
#          ),
#        conditionalPanel(
#          condition = "input.distribution=='HYPER'",
#          conditionalPanel(
#            condition = 'input.showparameter',
#            uiOutput("hyper.NUI"),
#            uiOutput("hyper.MUI"),
#            uiOutput("hyper.nUI")
#            )
#          ),
#        conditionalPanel(
#          condition = "input.distribution=='POIS'",
#          conditionalPanel(
#            condition = 'input.showparameter',
#            uiOutput("pois.lambdaUI")
#            )
#          ),
#        conditionalPanel(
#          condition = 'input.showfunction',
#          br(),
#          uiOutput("pdforcdfUI"),
#          uiOutput("refitUI")
#          ),
#        conditionalPanel(
#          condition = 'input.showoptions',
#          hr(),
#          uiOutput("cexUI")
#          )       
#        ),
# 
#        mainPanel(plotOutput("distPlot"))
#      ),
#
#      htmlOutput("logText")
#  ))
  