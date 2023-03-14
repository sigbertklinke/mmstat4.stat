library("shiny")
source("helper2.R")

dpc <-  gettext(c("overplot", "jitter", "stack"), "name")

mmstat.ui.elem("method",  "selectInput",
               label    = gettext("Select a Dotplot type"),
               choices  = dpc,
               selected = "overplot")
mmstat.ui.elem("bins",       "sliderInput",
               label   = gettext("Histogram: choose number of bins"),
               min     = 1,
               max     = 50,
               value   = 30)
mmstat.ui.elem("addmean",    "checkboxInput",
               label = gettext("Add mean (aquamarine, dotted)"),
               value = FALSE)
mmstat.ui.elem("addmedian",  "checkboxInput",
               label = gettext("Add median (blue, dashed)"),
               value = FALSE)               
mmstat.ui.elem("obs",        "checkboxInput",
               label = gettext("Show observations"),
               value = FALSE)               
mmstat.ui.elem("dataset",    "dataSet",     choices=mmstat.getDataNames("USCRIME", "CARS", "DECATHLON"))
mmstat.ui.elem("variable",   "variable1",   vartype="numeric")
mmstat.ui.elem("cex",        "fontSize")

shinyServer(function(input, output, session) {

  output$methodUI    <- renderUI({ mmstat.ui.call('method') })
  output$binsUI      <- renderUI({ mmstat.ui.call('bins') })
  output$addmeanUI   <- renderUI({ mmstat.ui.call('addmean') })
  output$addmedianUI <- renderUI({ mmstat.ui.call('addmedian') })  
  output$obsUI       <- renderUI({ mmstat.ui.call('obs') })  
  output$datasetUI   <- renderUI({ mmstat.ui.call('dataset') })
  output$cexUI       <- renderUI({ mmstat.ui.call('cex')  })

  output$variableUI <- renderUI({
    inp  <- mmstat.getValues(NULL, dataset=input$dataset)
    mmstat.ui.call('variable',
                   choices = mmstat.getVarNames(inp$dataset, 'numeric')
    )
  })  
  
  readHTML <- reactive({
    var <- getVar()
    html<- mmstat.html(gettext("distText.html"), 
                       100*input$cex,
                       var$xlab, var$dataname,
                       var$decimal, var$mean,
                       var$decimal, var$median,
                       var$decimal, var$min,
                       var$decimal, var$quart[1],
                       var$decimal, var$max,
                       var$decimal, var$quart[2],
                       var$decimal, var$sd,
                       var$decimal, var$iqr,
                       var$decimal, var$var,
                       var$decimal, diff(var$range)
                      )
    html
  })
  
  getVar <- reactive({
    var <- mmstat.getVar(isolate(input$dataset), input$variable, 'numeric')
    dec <- mmstat.dec(c(var$mean, var$median))
    var[['decimal']] <- dec$decimal
    var[['pos']]     <- 2*(var$mean<var$median)
    var
  })
  
  output$plotStrip <- renderPlot({
    inp <- mmstat.getValues(NULL,
                            method    = input$method,
                            addmean   = input$addmean,
                            addmedian = input$addmedian,
                            cex       = input$cex)
    var <- getVar()
    par(mar=c(3.1, 4.1, 4.1, 2.1))
    stripchart(var$values, 
              inp$method, 
              xlim=var$range, 
              axes=F, 
              main=sprintf(gettext("Dotplot (%s)"), tolower(gettext(inp$method))), 
              cex.axis=inp$cex, cex.lab=inp$cex, cex.main=1.2*inp$cex, cex.sub=inp$cex,
              xlab=var$xlab)
    usr <- par("usr")
    mmstat.axis(1, usr[1:2], cex.axis=inp$cex)  
    box()
    if (inp$addmean) abline(v=var$mean, col=mmstat$col[[1]], lwd=3, lty="dotted")
    if (inp$addmedian) abline(v=var$median, col=mmstat$col[[3]], lwd=3, lty="dashed")
  })

  output$plotHist <- renderPlot({
    var  <- getVar()
    inp <- mmstat.getValues(NULL,
                            bins      = input$bins,
                            addmean   = input$addmean,
                            addmedian = input$addmedian,
                            obs       = input$obs,
                            cex       = input$cex)
    bins <- seq(var$min, var$max, length.out = as.numeric(inp$bins) + 1)
    par(mar=c(3.1, 4.1, 4.1, 2.1))
    hist(var$values,
         xlim=var$range, 
         breaks = bins, 
         col="grey", 
         xlab=var$xlab,
         main=gettext("Histogram"),
         ylab=gettext("Absolute frequency"),
         cex.axis=inp$cex,
         cex.lab=inp$cex,
         cex.main=1.2*inp$cex,
         cex.sub=inp$cex,
         axes=F)
    usr <- par("usr")
    mmstat.axis(1, usr[1:2], cex.axis=inp$cex)   
    mmstat.axis(2, usr[3:4], cex.axis=inp$cex)
    box()
    if (inp$addmean) abline(v=var$mean, col=mmstat$col[[1]], lwd=3, lty="dotted")
    if (inp$addmedian) abline(v=var$median, col=mmstat$col[[3]], lwd=3, lty="dashed")
    if (inp$obs) rug(var$values, lwd=1)
  })
   
  output$plotBox <- renderPlot({
    inp <- mmstat.getValues(NULL,
                            bins      = input$bins,
                            addmean   = input$addmean,
                            addmedian = input$addmedian,
                            obs       = input$obs,
                            cex       = input$cex)
    var  <- getVar()
    par(mar=c(3.1, 4.1, 4.1, 2.1))
    boxplot(var$values, 
            horizontal=T, 
            ylim=var$range, 
            axes=F, 
            xlab=var$xlab,
            cex.axis=inp$cex,
            cex.lab=inp$cex,
            cex.main=1.2*inp$cex,
            cex.sub=inp$cex,
            main=gettext("Boxplot"))
    usr <- par("usr")
    mmstat.axis(1, usr[1:2], cex.axis=inp$cex)
    box()
    if (inp$addmean) abline(v=var$mean, col=mmstat$col[[1]], lwd=3, lty="dotted")
    if (inp$addmedian) abline(v=var$median, col=mmstat$col[[3]], lwd=3, lty="dashed")
    if (inp$obs) rug(var$values, lwd=1)
  })
     
  output$plotEcdf <- renderPlot({
    inp <- mmstat.getValues(NULL,
                            bins      = input$bins,
                            addmean   = input$addmean,
                            addmedian = input$addmedian,
                            obs       = input$obs,
                            cex       = input$cex)
    var  <- getVar()
    par(mar=c(3.1, 4.1, 4.1, 2.1))
    plot(ecdf(var$values), 
         xlim=var$range, 
         main=gettext(" Ecdf "),
         xlab=var$xlab,
         cex.axis=inp$cex,
         cex.lab=inp$cex,
         cex.main=1.2*inp$cex,
         cex.sub=inp$cex,
         axes=F)
    usr <- par("usr")
    mmstat.axis(1, usr[1:2], cex.axis=inp$cex)
    mmstat.axis(2, usr[3:4], cex.axis=inp$cex)
    box()
    if (inp$addmean) abline(v=var$mean, col=mmstat$col[[1]], lwd=3, lty="dotted")
    if (inp$addmedian) abline(v=var$median, col=mmstat$col[[3]], lwd=3, lty="dashed")
  })
     
  output$distText <- renderText({
    mmstat.log("called 'distText'")
    html <- readHTML()
    html
  })
  
  output$logText <- renderText({
    mmstat.getLog(session)
  })
  
})