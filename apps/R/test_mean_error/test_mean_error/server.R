library("shiny")
source("helper2.R")
mmstat.ui.elem("test", 'radioButtons', 
                label   = gettext("Choose test type"), 
                choices = gettext(c('two.sided', 'less', 'greater'), "name"),
                selected   = 'two.sided')
mmstat.ui.elem('mu0', 'sliderInput',
               label     = HTML(gettext("Hypothetical mean (&mu;<sub>0</sub>)")))
mmstat.ui.elem('alpha', 'significance')
mmstat.ui.elem('size',  'sampleSize')
mmstat.ui.elem("go",         "drawSample")
mmstat.ui.elem("dataset",    "dataSet",     choices=mmstat.getDataNames("BOSTONHOUSING", "USCRIME"))
mmstat.ui.elem("variable",   "variable1",   vartype="numeric")
mmstat.ui.elem("cex",        "fontSize")

mmstat$vartype <<- 'numvars'

shinyServer(function(input, output, session) {

  output$testUI    <- renderUI({ mmstat.ui.call('test') })      
  output$alphaUI   <- renderUI({ mmstat.ui.call('alpha') })
  output$goUI      <- renderUI({ mmstat.ui.call('go') })
#  output$populationUI      <- renderUI({ mmstat.ui.call('population') }) 
  output$datasetUI <- renderUI({ mmstat.ui.call('dataset') })
  output$cexUI     <- renderUI({ mmstat.ui.call('cex')  })
  
  output$mu0UI <- renderUI({
    var   <- getVar()
    alpha <- mmstat$UI$alpha$ticks
    mmstat.ui.call('mu0', 
                    min = mmstat.round.down(max(var$min, qnorm(min(alpha)/200, var$mean, var$sd/sqrt(30))), var$decimal),
                    max = mmstat.round.up(min(var$max, qnorm(1-min(alpha)/200, var$mean, var$sd/sqrt(30))), var$decimal),
                    value = round(var$mean, var$decimal),
                    step  = 10^(-var$decimal))
  })

  output$sizeUI <- renderUI({
    var  <- getVar()
    mmstat.ui.call('size', ticks=var$ticks, max=length(var$ticks))
  })
  
  output$variableUI <- renderUI({
    inp  <- mmstat.getValues(NULL, dataset=input$dataset)
    mmstat.ui.call('variable',
                   choices = mmstat.getVarNames(inp$dataset, 'numeric')
    )
  })  

  getVar <- reactive({
    mmstat.log(paste('getVar'))
    var         <- mmstat.getVar(input$dataset, input$variable)
    var$ticks   <- mmstat.ticks(var$n, nmin=30)   
    dec         <- mmstat.dec(0.1*c(0, var$sd/sqrt(max(var$ticks))))
    var$decimal <- dec$decimal
    var
  })

  drawSample <- reactive ({
    mmstat.log('drawSample')
    inp <- mmstat.getValues(NULL, go=input$go, size=input$size)
    var  <- getVar()
    index  <- sample(var$n, var$ticks[inp$size], replace=T)
    values <- var$values[index]
    list(values = values,
         jitter = runif(length(values)),
         index  = index,
         range  = range(values),
         mean   = mean(values),
         sd     = sd(values),
         n      = length(values)
    ) 
  })   

  doTest <- reactive({
    var    <- getVar()
    sample <- drawSample()
    inp    <- mmstat.getValues(list(mu0=var$mean), 
                               test=input$test, alpha=input$alpha, mu0=input$mu0,
                               cex=input$cex)    
    test <- list(xq.sd = var$sd/sqrt(sample$n))
    if (inp$test=='two.sided') {
      test$h0   <- '&mu;=&mu;<sub>0</sub>'
      test$h1   <- '&mu;&ne;&mu;<sub>0</sub>'
      test$c    <- qnorm(1-mmstat$UI$alpha$ticks[inp$alpha]/200)
      test$cu   <- inp$mu0-test$c*test$xq.sd
      test$co   <- inp$mu0+test$c*test$xq.sd
      test$dec  <- (sample$mean<test$cu)||(sample$mean>test$co)
      test$beta <- 100*diff(pnorm(c(test$cu, test$co), var$mean, test$xq.sd))
    }
    if (inp$test=='less') {
      test$h0   <- '&mu;&ge;&mu;<sub>0</sub>'
      test$h1   <- '&mu;&lt;&mu;<sub>0</sub>'
      test$c    <- qnorm(1-mmstat$UI$alpha$ticks[inp$alpha]/100)
      test$cu   <- inp$mu0-test$c*test$xq.sd
      test$co   <- +Inf
      test$dec  <- sample$mean<test$cu
      test$beta <- 100*(1-pnorm(test$cu, var$mean, test$xq.sd))
    }
    if (inp$test=='greater') {
      test$h0   <- '&mu;&le;&mu;<sub>0</sub>'
      test$h1   <- '&mu;&gt;&mu;<sub>0</sub>'
      test$c    <- qnorm(1-mmstat$UI$alpha$ticks[inp$alpha]/100)
      test$cu   <- -Inf
      test$co   <- inp$mu0+test$c*test$xq.sd
      test$dec  <- sample$mean>test$co
      test$beta <- 100*pnorm(test$co, var$mean, test$xq.sd)
    }
    test
  })

  readHTML <- reactive({
    var    <- getVar()
    sample <- drawSample()
    inp    <- mmstat.getValues(list(mu0=var$mean), 
                               test=input$test, alpha=input$alpha, mu0=input$mu0,
                               cex=input$cex)
    test   <- doTest()
    html <- mmstat.html(gettext("distText.html"), 
                        100*inp$cex,
#                       var$xlab, var$dataname,
                        sample$n,
                        var$decimal, sample$mean,              # xbar
                        var$decimal, var$mean,                 # mu
                        var$decimal, sample$sd,                # s
                        var$decimal, var$sd,                   # sigma
                        var$decimal, sample$sd/sqrt(sample$n), # est std.dev of Xbar sigma unknown
                        var$decimal, var$sd/sqrt(sample$n),    # est std.dev of Xbar sigma known
                        test$h0, test$h1,                      # hypotheses
                        ifelse(test$dec, gettext("Reject H<sub>0</sub>"), gettext("Can not reject H<sub>0</sub>")),
                        mmstat$alpha[inp$alpha],
                        test$beta
    )
    html
  })  

  output$outputPlot <- renderPlot({
    mmstat.log('outputPlot')  
    var    <- getVar()
    sample <- drawSample()
    test   <- doTest()
    inp    <- mmstat.getValues(list(mu0=var$mean), 
                               test=input$test, alpha=input$alpha, mu0=input$mu0,
                               cex=input$cex)
    if (is.list(sample)) {
      xlim  <- mmstat.range( qnorm(c(0.00001, 0.99999), var$mean, var$sd/sqrt(30)),
                             qnorm(c(0.00001, 0.99999), inp$mu0, var$sd/sqrt(30)))
      xq.sd <- test$xq.sd
      
#      if (inp$population) {
#        xlim <- mmstat.range(xlim, var$range)
#      } else if (inp$sample) {
#        xlim <- mmstat.range(xlim, sample$range)
#      }
      # xlim <- c(max(var$min, xlim[1]), min(var$max, xlim[2]))
      x   <- mmstat.pos(xlim, (0:300)/300)
      # true distribution
      yt  <- dnorm(x, var$mean, xq.sd) 
      # distribution under H0
      y0  <- dnorm(x, inp$mu0, xq.sd) 
      
      my   <- max(yt, y0)
      ylim <- my*c(-0.25, 1)

      if (inp$test=='two.sided') {
        main = mmstat.math("&H[0];: &mu==mu[0]; vs. &H[1];: &mu!=mu[0]; ")
      }
      if (inp$test=='less') {
        main = mmstat.math("&H[0];: &mu>=mu[0]; vs. &H[1];: &mu<mu[0]; ")
      }      
      if (inp$test=='greater') {
        main = mmstat.math("&H[0];: &mu<=mu[0]; vs. &H[1];: &mu>mu[0]; ")
      }     
      par (mar=c(5,0,2,0))
      plot (x, yt, type='l', xlab=var$xlab, sub=var$sub, xlim=xlim, ylim=ylim, axes=F,
            main=main, ylab="", col=mmstat$col[[1]],
            cex.axis=inp$cex, cex.lab=inp$cex, cex.main=1.2*inp$cex, cex.sub=inp$cex)
      lines(x, y0, col=mmstat$col[[8]])
      
      abline(h=0)
      mmstat.axis(1, xlim, cex.axis=inp$cex)
      usr <- par('usr')
      pos <- 2+2*(inp$mu0>var$mean)
      xp <- qnorm(0.25*(pos-1), inp$mu0, xq.sd)
      yp <- dnorm(xp, inp$mu0, xq.sd)
      text(xp, yp, mmstat.math(" &bar(X);~&N(mu[0], sigma^2/n); "), pos=pos, cex=inp$cex, col=mmstat$col[[8]])

      pos <- 2+2*(inp$mu0<var$mean)
      xp <- qnorm(0.25*(pos-1), var$mean, xq.sd)
      yp <- dnorm(xp, var$mean, xq.sd)
      text(xp, yp, mmstat.math(" &bar(X);~&N(mu[1], sigma^2/n); "), pos=pos, cex=inp$cex, col=mmstat$col[[1]])      

      mmstat.plotTestRegions(c(test$cu, test$co), xlim=usr[1:2], ylim=-my*c(0.05, 0.2), cex=inp$cex,
                             label=mmstat.math(gettext(" &sigma; known")), pos=2+2*(var$mean>inp$mu0))
      
      if (inp$test=='two.sided') {
        xp <- c(x[x<test$cu], test$cu)
        yp <- dnorm(xp, inp$mu0, xq.sd)
        xp <- c(xp, test$cu)
        yp <- c(yp, 0)
        polygon(xp, yp, col=mmstat$col[[16]], border=NA)

        xp <- c(test$cu, x[(x>test$cu) & (x<test$co)], test$co)
        yp <- dnorm(xp, var$mean, xq.sd)
        xp <- c(test$cu, xp, test$co)
        yp <- c(0, yp, 0)
        polygon(xp, yp, col=mmstat$col[[9]], border=NA)
        
        xp <- c(test$co, x[x>test$co])
        yp <- dnorm(xp, inp$mu0, xq.sd)
        xp <- c(test$co, xp)
        yp <- c(0, yp)
        polygon(xp, yp, col=mmstat$col[[16]], border=NA)
      }
      if (inp$test=='less') {        
        xp <- c(x[x<test$cu], test$cu)
        yp <- dnorm(xp, inp$mu0, xq.sd)
        xp <- c(xp, test$cu)
        yp <- c(yp, 0)
        polygon(xp, yp, col=mmstat$col[[16]], border=NA)
        
        xp <- c(test$cu, x[x>test$cu])
        yp <- dnorm(xp, var$mean, xq.sd)
        xp <- c(test$cu, xp)
        yp <- c(0, yp)
        polygon(xp, yp, col=mmstat$col[[9]], border=NA)
      }        
      if (inp$test=='greater') {
        xp <- c(test$co, x[x>test$co])
        yp <- dnorm(xp, inp$mu0, xq.sd)
        xp <- c(test$co, xp)
        yp <- c(0, yp)
        polygon(xp, yp, col=mmstat$col[[16]], border=NA)
        
        xp <- c(x[x<test$co], test$co)
        yp <- dnorm(xp, var$mean, xq.sd)
        xp <- c(test$co, xp)
        yp <- c(0, yp)
        polygon(xp, yp, col=mmstat$col[[9]], border=NA)
      }        
      
      lines(x, yt, col=mmstat$col[[1]])
      lines(x, y0, col=mmstat$col[[8]])
      
      abline(v=inp$mu0, col=mmstat$col[[16]], lwd=2)
      abline(v=sample$mean, col=mmstat$col[[2]], lwd=2, lty="dotted")
      abline(v=var$mean, col=mmstat$col[[1]], lwd=2, lty="dotted")
      
      legend("topright", fill=c(mmstat$col[[16]], mmstat$col[[9]]), title=gettext("Error"),
             legend=c(gettext("type I"), gettext("type II")), cex=inp$cex)
      box() 
    }
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
