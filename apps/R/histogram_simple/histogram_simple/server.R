library("shiny")
source("helper2.R")

mmstat$vartype <<- 'numvars'

mmstat.ui.elem("bins", 'sliderInput',
               label=gettext("Number of bins:"),
               min = 1, max = 50, value = 30)
mmstat.ui.elem("obs",  'checkboxInput',
               label = gettext("Show observations"), value = FALSE)
mmstat.ui.elem("dataset",    "dataSet",     
               choices=mmstat.getDataNames("USCRIME", "CARS", "DECATHLON"))
mmstat.ui.elem("variable",   "variable1",   
               vartype="numeric")
mmstat.ui.elem("cex",        "fontSize")

shinyServer(function(input, output, session) {
  
  output$binsUI    <- renderUI({ mmstat.ui.call('bins') })
  output$obsUI     <- renderUI({ mmstat.ui.call('obs') })
  output$datasetUI <- renderUI({ mmstat.ui.call('dataset') })
  output$cexUI     <- renderUI({ mmstat.ui.call('cex')  })
  
  
  output$variableUI <- renderUI({
    inp  <- mmstat.getValues(NULL, dataset=input$dataset)
    mmstat.ui.call('variable',
                    choices = mmstat.getVarNames(inp$dataset, 'numeric'))
  })   
  
  getVar <- reactive({
    mmstat.log(paste('getVar'))
    var             <- mmstat.getVar(isolate(input$dataset), input$variable)
    dec             <- mmstat.dec(c(var$mean, var$median))
    var$decimal     <- dec$decimal
    var[['pos']]    <- 2*(var$mean<var$median)
    var
  })
  
  output$distPlot <- renderPlot({
    
    var    <- getVar()
    input  <- mmstat.getValues(NULL, bins=input$bins, cex=input$cex, obs =input$obs)
    bins   <- seq(var$min, var$max, length.out = as.numeric(input$bins) + 1)
    hist(var$values,
         breaks=bins,
         col="grey", 
         xlab=var$xlab,
         main=gettext("Histogram"), 
         sub=var$sub,
         ylab=gettext("Absolute frequency"),
         cex.axis=input$cex,
         cex.lab=input$cex,
         cex.main=1.2*input$cex,
         cex.sub=input$cex,
         axes=F)
    usr <- par("usr")
    mmstat.axis(1, usr[1:2], cex.axis=input$cex)
    mmstat.axis(2, usr[3:4], cex.axis=input$cex)    
    if (input$obs) rug(var$values, lwd=1)
    box()
  })
  
  output$logText <- renderText({
    mmstat.getLog(session)
  })
})
