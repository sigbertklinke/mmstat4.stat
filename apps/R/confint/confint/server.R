library("shiny")
source("helper2.R")
mmstat$UI <<- list(type       = list(inputId   = "type",
                                     label     = gettext("Choose plot"), 
                                     choices   = gettext(c('Distribution of sample mean', 'Fluctuation interval', 'Confidence interval')),
                                     value     = 1),
                   alpha      = list(inputId   = "alpha", 
                                     label     = HTML(gettext("Confidence level (1-&alpha;)")),
                                     min       = 1,
                                     max       = length(mmstat$alpha),
                                     step      = 1,
                                     value     = 6,
                                     ticks     = 100-mmstat$alpha),
                   size       = list(inputId   = "size", 
                                     label     = gettext("Sample size (n)"),
                                     min       = 1,
                                     # max   depends on var$ticks
                                     value     = 1,
                                     # ticks depends on var$n
                                     step      = 1),
                   go         = list(inputId  = "go",
                                     label    = gettext("Draw sample")),
                   speed      = list(inputId   = "speed", 
                                     label     = NULL,
                                     min       = 0, 
                                     max       = 20, 
                                     value     = 0),                   
                   sample     = list(inputId = "sample",
                                     label   = gettext("Show sample (orange, rugged"),
                                     value   = TRUE),
                   population = list(inputId = "population",
                                     label   = gettext("Show population (aquamarine, rugged)"),
                                     value   = FALSE),
                   cex        = list(inputId   = "cex", 
                                     label     = gettext("Font size"),
                                     min       = 1,
                                     max       = 1.5,
                                     step      = 0.05,
                                     value     = 1)   
)

mmstat$vartype <<- 'numvars'
mmstat.getDatasets('ALLBUS12')

means <<- c()

shinyServer(function(input, output, session) {

  output$typeUI <- renderUI({        
    args <- mmstat$UI$type
    args$value <- NULL
    do.call('radioButtons', args)
  })
  
  output$alphaUI <- renderUI({
    mmstat.log('alphaUI')  
    args <- mmstat$UI$alpha
    do.call('sliderInput', args)
  })

  output$sizeUI <- renderUI({
    mmstat.log('sizeUI')
    var  <- getVar()
    args <- mmstat$UI$size
    args$ticks <- var$ticks
    args$max   <- length(args$ticks)
    do.call('sliderInput', args)
  })

  output$goUI <- renderUI({
    args <- mmstat$UI$go
    do.call('actionButton', args)
  })

  output$speedUI <- renderUI({
    args <- mmstat$UI$speed
    do.call('sliderInput', args)
  })
  
  output$sampleUI <- renderUI({
    mmstat.log('sampleUI')
    args <- mmstat$UI$sample
    do.call('checkboxInput', args)
  })

  output$populationUI <- renderUI({
    mmstat.log('populationUI')
    args <- mmstat$UI$population
    do.call('checkboxInput', args)
  })
  
  getVar <- reactive({
    mmstat.log(paste('getVar'))
    var         <- mmstat.getVar('ALLBUS12', "NETINCOME")
    var$ticks   <- mmstat.ticks(var$n, nmin=30, tin=9)   
    dec         <- mmstat.dec(0.1*c(0, var$sd/sqrt(max(var$ticks))))
    var$decimal <- dec$decimal
    var
  })
  
  drawSample <- reactive ({
    mmstat.log('drawSample')
    input$go
    inp <- mmstat.getValues(NULL, speed=input$speed)
    if (inp$speed>0) invalidateLater(500/inp$speed, session)   
    size <- input$size
    var  <- getVar()
    if (is.null(size)) size<-var$ticks[1] else size<-var$ticks[size]
    index  <- sample(var$n, size, replace=T)
    values <- var$values[index]
    means <<- c(means, mean(values))
    list(values = values,
         jitter = runif(length(values)),
         index  = index,
         range  = range(values),
         mean   = mean(values),
         sd     = sd(values),
         n      = length(values)
    )
  })    

  observe({
    input$size + input$alpha
    isolate({
      sample <- drawSample()
      means <<- sample$mean
    })
  })    
  
  output$outputPlot <- renderPlot({
    mmstat.log('outputPlot')  
    var    <- getVar()
    sample <- drawSample()
    inp    <- mmstat.getValues(NULL,
                               alpha=input$alpha, 
                               population=input$population, 
                               sample=input$sample)
    xlim   <- c(0,4100)
    nclass <- nclass.Sturges(sample$values)
    breaks <- c(max(xlim)*(0:nclass)/nclass, max(var$range))
    par(mfrow=c(3,1))
    
    par(mar=c(0.5,0,3,0))
    hist(sample$values, breaks=breaks, freq=F, xlim=xlim, axes=F, main="Fluctation interval", cex.main=1.5, border="grey")
    legend("topright", legend=c(gettext("Population mean"), gettext("Sample mean"), gettext("Fluctuation interval")), border=0,
           fill=c(0, 0, mmstat$col[[9]]), col=c(mmstat$col[[1]], mmstat$col[[2]]), lty=c(1,1,-1), lwd=c(2,2,0), cex=1.5)
    usr <- par('usr')
    k   <- qnorm(1-mmstat$alpha[inp$alpha]/200)*var$sd/sqrt(sample$n)
    rect(var$mean-k, 0, var$mean+k, usr[4], 
         col=mmstat$col[[9]], border=NA) 
    abline(v=var$mean, col=mmstat$col[[1]], lwd=2)
    abline(v=sample$mean, col=mmstat$col[[2]], lwd=2)
    if (inp$sample) {
      clipped <- sample$values[sample$values<max(xlim)]
      rug(clipped, col=mmstat$col[[2]])
    }
    axis(1, labels=FALSE)
    hist(sample$values, breaks=breaks, freq=F, xlim=xlim, axes=F, add=T, border="grey")
    box()
    
    hist(sample$values, breaks=breaks, freq=F, xlim=xlim, axes=F, main="Confidence interval", cex.main=1.5, border="grey")
    legend("topright", legend=c(gettext("Population mean"), gettext("Sample mean"), gettext("Confidence interval")), border=0,
           fill=c(0, 0, mmstat$col[[10]]), col=c(mmstat$col[[1]], mmstat$col[[2]]), lty=c(1,1,-1), lwd=c(2,2,0), cex=1.5)
    rect(sample$mean-k, 0, sample$mean+k, usr[4], 
         col=mmstat$col[[10]], border=NA) 
    abline(v=var$mean, col=mmstat$col[[1]], lwd=2)
    abline(v=sample$mean, col=mmstat$col[[2]], lwd=2)
    if (inp$sample) rug(clipped, col=mmstat$col[[2]])
    axis(1, labels=FALSE)
    hist(sample$values, breaks=breaks, freq=F, xlim=xlim, axes=F, add=T, border="grey")
    box()
    
    par(mar=c(6,0,4,0))
    ylim <- c(0, sqrt(sample$n/pi)/var$sd)
    hist(means, freq=F, xlim=xlim, axes=F, xlab="NETINCOME - ALLBUS 2012 data", cex.lab=1.5, ylim=ylim,
         main=gettext("Distribution of sample mean"), cex.main=1.5, col=mmstat$col[[2]], border="grey",
         sub=sprintf(gettext("Observations larger than %.0f are not shown"), max(xlim)), cex.sub=1.25)
    x <- (0:600)/600*max(xlim)
    lines(x, dnorm(x, mean=var$mean, sd=var$sd/sqrt(sample$n)), lwd=2,  col=mmstat$col[[1]])
    axis(1, cex.axis=1.5)
    if (inp$population) {
      clipped <- var$values[var$values<max(xlim)]
      rug(clipped, col=mmstat$col[[1]])
    }
    box()
  })
            
  
})
  