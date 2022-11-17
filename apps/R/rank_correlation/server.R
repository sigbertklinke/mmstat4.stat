library("shiny")
source("helper2.R")

mmstat$vartype <<- 'ordvars'
mmstat.ui.elem('coeff', 'checkboxGroupInput',
               label    = gettext("Show coefficient(s)"),
               choices  = gettext(c("SHOW.SPEARMAN", "SHOW.KENDALL"), "name"),
               value    = character())
mmstat.ui.elem("dataset",    "dataSet",     
               choices=mmstat.getDataNames("ALLBUS1994-TRUST", "ALLBUS2002-TRUST", "ALLBUS2012-TRUST", "ALLBUS1992-ECON", "ALLBUS2002-ECON", "ALLBUS2012-ECON"))
mmstat.ui.elem("variableYSelect",   "variable1",   vartype="ordered", 
               label    = gettext("Select column variable"))
mmstat.ui.elem("variableXSelect",   "variable1",   vartype="ordered",
               label    = gettext("Select row variable"))
mmstat.ui.elem("cex",        "fontSize")

shinyServer(function(input, output, session) {
  
  output$coeffUI           <- renderUI({ mmstat.ui.call('coeff') })
  output$datasetUI         <- renderUI({ mmstat.ui.call('dataset') })
  output$variableXSelectUI <- renderUI({ mmstat.ui.call('variableXSelect') })
  output$variableYSelectUI <- renderUI({ mmstat.ui.call('variableYSelect') })
  output$cexUI             <- renderUI({ mmstat.ui.call('cex') })

  observe({
    inp = mmstat.getValues(NULL, dataset=input$dataset)
    updateSelectInput(session, "variableYSelect",
                      choices = mmstat.getVarNames(inp$dataset, 'ordered'),
                      selected = mmstat$dataset[[inp$dataset]]$ordvars[1])
    updateSelectInput(session, "variableXSelect",
                      choices = mmstat.getVarNames(inp$dataset, 'ordered'),
                      selected = mmstat$dataset[[inp$dataset]]$ordvars[2])
  })
  
  output$contingencyTable <- renderText({
    inp <- mmstat.getValues(NULL, 
                            dataset=input$dataset,
                            coeff=input$coeff,
                            variableXSelect=input$variableXSelect,
                            variableYSelect=input$variableYSelect,
                            cex=input$cex)
    varx  <- mmstat.getVar(isolate(inp$dataset), varname=inp$variableXSelect)
    vary  <- mmstat.getVar(isolate(inp$dataset), varname=inp$variableYSelect)
    tab   <- table(varx$values, vary$values)
    vars  <- c(gettext(vary$name), gettext(varx$name))
    lines <- NULL
    for (i in seq(inp$coeff)) {
      if (inp$coeff[i]=="SHOW.SPEARMAN") {
        lines <- c(lines, sprintf(gettext("Spearman rank correlation r<sub>s</sub>=%.3f"), 
                                  cor(as.numeric(varx$values), as.numeric (vary$values), method="s")))
      }
      if (inp$coeff[i]=="SHOW.KENDALL") {        
        lines <- c(lines, sprintf(gettext("Kendalls rank correlation &tau;=%.3f"), 
                                cor(as.numeric(varx$values), as.numeric(vary$values), method="k")))
      }
    } 
    HTML(htmlTable(tab, vars=vars, lines=lines, cex=inp$cex, title=gettext(input$dataset)))
  })
  
  output$logText <- renderText({
    mmstat.getLog(session)
  })
})
