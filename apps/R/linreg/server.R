library("shiny")
library("scatterplot3d")
library("lattice")
source("helper2.R")

mmstat$vartype <<- 'numvars'
mmstat.ui.elem("show", 'checkboxGroupInput',
               label    = gettext("Show"),
               choices  = gettext(c("SHOW.REGRESSION", "SHOW.VALUES", "SHOW.CONFIDENCE"), "name"),
               value    = character())
mmstat.ui.elem("dataset",    "dataSet",     
               choices = mmstat.getDataNames("USCRIME", "CARS", "DECATHLON"))
mmstat.ui.elem("variableYSelect",   "variable1",   
               vartype = "numeric", label = gettext("Select dependent variable (Y)"))
mmstat.ui.elem("variableXSelect",   "variable1",   
               vartype = "numeric", label = gettext("Select independent variable (X)"),
               value = mmstat.getVarNames(1, 'numeric', 2))
mmstat.ui.elem("cex",  "fontSize")

shinyServer(function(input, output, session) {
  
  output$showUI <- renderUI({ mmstat.ui.call('show') })
  output$datasetUI   <- renderUI({ mmstat.ui.call('dataset') })
  output$cexUI       <- renderUI({ mmstat.ui.call('cex')  })

  output$variableXSelectUI <- renderUI({
    inp  <- mmstat.getValues(NULL, dataset=input$dataset)
    mmstat.ui.call('variableXSelect', choices= mmstat.getVarNames(inp$dataset, 'numeric'))
  })

  output$variableYSelectUI <- renderUI({
    inp  <- mmstat.getValues(NULL, dataset=input$dataset)
    mmstat.ui.call('variableYSelect', choices=mmstat.getVarNames(inp$dataset, 'numeric'))
  })
  
  observe({
    inp <- mmstat.getValues(NULL, dataset=input$dataset)
    updateSelectInput(session, "variableYSelect",
                      choices  = mmstat.getVarNames(inp$dataset, 'numeric'),
                      selected = mmstat$dataset[[inp$dataset]]$numvars[2])
    updateSelectInput(session, "variableXSelect",
                      choices  = mmstat.getVarNames(inp$dataset, 'numeric'),
                      selected = mmstat$dataset[[inp$dataset]]$numvars[1])
  })
  
  output$scatterPlot <- renderPlot({
    inp <- mmstat.getValues(NULL, 
                            dataset=input$dataset,
                            variableXSelect=input$variableXSelect,
                            variableYSelect=input$variableYSelect,
                            show=input$show,
                            cex=input$cex)
    varx <- mmstat.getVar(isolate(inp$dataset), varname=inp$variableXSelect, na.action=na.pass)
    vary <- mmstat.getVar(isolate(inp$dataset), varname=inp$variableYSelect, na.action=na.pass)
    if (is.na(pmatch('SHOW.CONFIDENCE', inp$show))) {
      plot (varx$values, vary$values, xlab=gettext(inp$variableXSelect), ylab=gettext(inp$variableYSelect),
            cex.axis=inp$cex, cex.lab=inp$cex, cex.main=inp$cex, cex.sub=inp$cex, pch=19,
            sub=gettext(varx$sub))
      if (!is.na(pmatch('SHOW.REGRESSION', inp$show))) {
        lreg <- lm (vary$values~varx$values)
        if (inp$variableXSelect!=inp$variableYSelect) {
          abline(lreg, col=mmstat$col[[1]], lwd=2)
        } else {
          abline(h=mean(vary$values, na.rm=T), col=mmstat$col[[1]], lwd=2)
        }
      }
    } else {
      rx   <- range(varx$values, na.rm=T)
      df   <- data.frame(y=vary$values, x=varx$values)
      lreg <- lm(y~x, data=df)
      x    <- rx[1]+diff(rx)*(0:200)/200
      df   <- data.frame(x=x)
      y    <- predict(lreg, df, interval="pred")
      if (inp$variableXSelect!=inp$variableYSelect) ylim <- range(c(range(y), vary$values)) else ylim <- range(vary$values)
      plot (varx$values, vary$values, xlab=gettext(inp$variableXSelect), ylab=gettext(inp$variableYSelect),
            cex.axis=inp$cex, cex.lab=inp$cex, cex.main=inp$cex, cex.sub=inp$cex, pch=19,
            xlim=rx, ylim=ylim)
      if (inp$variableXSelect!=inp$variableYSelect) {
        abline(lreg, col=mmstat$col[[1]], lwd=2)
      } else {
        abline(h=mean(vary$values, na.rm=T), col=mmstat$col[[1]], lwd=2)
      }
      if (inp$variableXSelect!=inp$variableYSelect) {
        lines(x, y[,'lwr'], lwd=1, col=mmstat$col[[2]])
        lines(x, y[,'upr'], lwd=1, col=mmstat$col[[2]])
        y <- predict(lreg, df, interval="conf")
        lines(x, y[,'lwr'], lwd=1, col=mmstat$col[[1]])
        lines(x, y[,'upr'], lwd=1, col=mmstat$col[[1]])
        pos <- "topright"
        if (lreg$coefficient[2]>0) pos <- "topleft"
        legend(pos, legend=c(mmstat.math(" &hat(Y)[h]; "), mmstat.math(" &Y[h]; ")), 
               lwd=1, col=c(mmstat$col[[1]], mmstat$col[[2]]),
               title=gettext("C.I. for"))
      }
    }
  })
  

  output$outputR <- renderPrint({
    inp <- mmstat.getValues(NULL, 
                            dataset=input$dataset,
                            variableXSelect=input$variableXSelect,
                            variableYSelect=input$variableYSelect,
                            show=input$show)
    varx <- mmstat.getVar(isolate(inp$dataset), varname=inp$variableXSelect, na.action=na.pass)
    vary <- mmstat.getVar(isolate(inp$dataset), varname=inp$variableYSelect, na.action=na.pass)
    xn   <- gsub('[ ()]+', ".", gettext(inp$variableXSelect)) 
    yn   <- gsub('[ ()]+', ".", gettext(inp$variableYSelect)) 
    assign(xn, varx$values)
    assign(yn, vary$values)
    lreg <- eval(parse(text=sprintf("lm(%s~%s)", yn, xn)))
    if (!is.na(pmatch('SHOW.VALUE', inp$show))) {
      print(summary(lreg))
    } else  if (!is.na(pmatch('SHOW.REGRESSION', inp$show))) {
      print(lreg)
    }
  })


  output$logText <- renderText({
    mmstat.getLog(session)
  })
})
