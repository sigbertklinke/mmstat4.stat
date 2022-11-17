library("shiny")
source("helper2.R")

mmstat$UI <<- list(dataset         = list(inputId = "dataset", 
                                          label   = gettext("Select a data set"),
                                          value   = mmstat.getDatasets("CARS", "USCRIME"),
                                          choices = mmstat.getDatasetNames()),
                   variable        = list(inputId = "variable", 
                                          label   = gettext("Select a variable"),
                                          # choices depends on var
                                          value   = mmstat.getVariableNames(1)),
                   method          = list(inputId ="method", 
                                          label   = gettext("Select a Dotplot type"),
                                          choices = gettext(c("overplot", "jitter", "stack"), "name"),
                                          value   = "jitter"),
                   addmean         = list(inputId = "addmean",
                                          label = gettext("Add mean (aquamarine, dotted)"),
                                          value = FALSE),
                   addmedian       = list(inputId = "addmedian",
                                          label = gettext("Add median (blue, dashed)"),
                                          value = FALSE),
                   cex             = list(inputId   = "cex", 
                                          label     = gettext("Font size"),
                                          min       = 1,
                                          max       = 1.5,
                                          step      = 0.05,
                                          value     = 1)
                   )

mmstat$vartype <<- 'numvars'

shinyServer(function(input, output, session) {
  
  output$methodUI <- renderUI({
    mmstat.log('methodUI')  
    args <- mmstat$UI$method
    args$value <- NULL
    do.call('selectInput', args)
  })
  
  output$addmedianUI <- renderUI({
    mmstat.log('addmedianUI')
    args <- mmstat$UI$addmedian
    do.call('checkboxInput', args)
  })
  
  output$addmeanUI <- renderUI({
    mmstat.log('addmeanUI')
    args <- mmstat$UI$addmean
    do.call('checkboxInput', args)
  })

  output$variableUI <- renderUI({
    mmstat.log('variableUI')
    inp  <- mmstat.getValues(NULL, dataset=input$dataset)
    args <- mmstat$UI$variable
    args$value <- NULL
    args$choices <- gettext(mmstat$dataset[[inp$dataset]]$numvars, "name")
    do.call('selectInput', args)
  })
  
  output$datasetUI <- renderUI({
    mmstat.log('datasetUI')
    args <- mmstat$UI$dataset
    args$value <- NULL
    do.call('selectInput', args)
  })
  
  output$cexUI <- renderUI({
    mmstat.log('cexUI')
    args <- mmstat$UI$cex
    do.call('sliderInput', args)
  })

  getVar <- reactive({
    mmstat.log(paste('getVar'))
    var         <- mmstat.getVar(isolate(input$dataset), input$variable)
    var$ticks   <- mmstat.ticks(var$n, nmin=30)   
    dec         <- mmstat.dec(0.1*c(0, var$sd/sqrt(max(var$ticks))))
    var$decimal <- dec$decimal
    var
  })
   
  output$distPlot <- renderPlot({
    mmstat.log(sprintf('distPlot %s', input$method))
    inp   <- mmstat.getValues(NULL, method    = input$method,
                                    cex       = input$cex,
                                    addmean   = input$addmean,
                                    addmedian = input$addmedian
               )
    var <- getVar()
    stripchart(var$values, 
               method=inp$method, 
               main=sprintf(gettext("Dotplot (%s) of %s"), tolower(gettext(inp$method)), var$name), 
               xlab=var$xlab, 
               sub=var$sub, 
               cex.axis=inp$cex,
               cex.lab=inp$cex,
               cex.main=1.2*inp$cex,
               cex.sub=inp$cex,
               axes=F)
    usr <- par("usr")
    mmstat.axis(1, usr[1:2], cex.axis=inp$cex)
    box()
    if (inp$addmean || inp$addmedian) {
      pos <- mmstat.pos(usr[3:4], 0.05)
      if (inp$addmean) {
        abline(v=var$mean, lwd=3, lty="dotted", col=mmstat$col[[1]])
        text(var$mean, pos, sprintf("%.*f", var$decimal, var$mean), col=mmstat$col[[1]], pos=4-var$pos, cex=inp$cex)
      }
      if (inp$addmedian) {
        abline(v=var$median, lwd=3, lty="dashed", col=mmstat$col[[3]])
        text(var$median, pos, sprintf("%.*f", var$decimal, var$median), col=mmstat$col[[3]], pos=2+var$pos, cex=inp$cex)
      }
    }
  })
  
  output$logText <- renderText({
    mmstat.getLog(session)
  })
})
