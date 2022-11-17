library("shiny")
source("helper2.R")

gv <- list(dist='mmstat', xlim=NULL, ylim=NULL)
distrc <- gettext(c("EXP", "NORM"), "name")
distrd <- 'EXP'
radioc <- gettext(c("CPDF", "CCDF"), "name")

mmstat$UI <<- list(distribution      = list(inputId  = "distribution", 
                                            label    = gettext("Select a probability distribution"),
                                            choices  = distrc,
                                            selected = distrd,
                                            value    = distrd),
                   exp.lambda        = list(inputId = "exp.lambda", 
                                            label   = HTML(gettext("Parameter (&lambda;)")),
                                            min     = 0.1,
                                            max     = 5,
                                            value   = 1,
                                            step    = 0.1),
                   norm.mu           = list(inputId = "norm.mu", 
                                            label   = HTML(gettext("Mean (&mu;)")),
                                            min     = -3,
                                            max     = +3,
                                            value   = 0,
                                            step    = 0.1),
                   norm.sigma2       = list(inputId = "norm.sigma2", 
                                            label   = HTML(gettext("Variance (&sigma;<sup>2</sup>)")),
                                            min     = 0.1,
                                            max     = 5,
                                            value   = 1,
                                            step    = 0.1),
                   pdforcdf          = list(inputId = "pdforcdf", 
                                            label   = gettext("Show"),
                                            choices = radioc,
                                            value   = "CPDF"),
                   refit             = list(inputId  = "refit",
                                            label    = gettext("Refit axes")),
                   cex               = list(inputId   = "cex", 
                                            label     = gettext("Font size"),
                                            min       = 1,
                                            max       = 1.5,
                                            step      = 0.05,
                                            value     = 1)
)

shinyServer(function(input, output, session) {
  
  output$EXPUI <- renderUI({
    HTML(mmstat.html(gettext("INTRO_EXP")))
  })
  
  output$NORMUI <- renderUI({
    HTML(mmstat.html(gettext("INTRO_NORM")))
  })
  
  output$exp.lambdaUI <- renderUI({
    mmstat.log('exp.lambdaUI')  
    args <- mmstat$UI$exp.lambda
    do.call('sliderInput', args)
  })
  
  output$norm.muUI <- renderUI({
    mmstat.log('norm.muUI')  
    args <- mmstat$UI$norm.mu
    do.call('sliderInput', args)
  })
  
  output$norm.sigma2UI <- renderUI({
    mmstat.log('norm.sigma2UI')  
    args <- mmstat$UI$norm.sigma2
    do.call('sliderInput', args)
  })
  
  output$distributionUI <- renderUI({
    mmstat.log('distributionUI')
    args <- mmstat$UI$distribution
    args$value <- NULL
    do.call('selectInput', args)
  })
  
  output$pdforcdfUI <- renderUI({
    args <- mmstat$UI$pdforcdf
    args$value <- NULL
    do.call('radioButtons', args)
  })
  
  output$refitUI <- renderUI({
    args <- mmstat$UI$refit
    do.call('actionButton', args)
  })
  
  output$cexUI <- renderUI({
    mmstat.log('cexUI')
    args <- mmstat$UI$cex
    do.call('sliderInput', args)
  })
  
  refit <- reactive({
    input$refit
    gv$dist <<- 'mmstat'
  })
  
  output$distPlot <- renderPlot({
    refit()
    inp   <- mmstat.getValues(NULL, cex          = input$cex,
                                    distribution = input$distribution,
                                    exp.lambda   = input$exp.lambda,
                                    norm.sigma2  = input$norm.sigma2,
                                    norm.mu      = input$norm.mu,
                                    pdforcdf     = input$pdforcdf)
    
    if (gv$dist!=inp$distribution) {
      if ((gv$dist=='NORM') && (inp$distribution=='EXP')) { # switch from NORM to EXP
        updateSliderInput(session, inputId='exp.lambda', value=inp$norm.sigma2)
      }
      if ((gv$dist=='EXP') && (inp$distribution=='NORM')) { # switch from EXP to NORM
        updateSliderInput(session, inputId='norm.sigma2', value=inp$exp.lambda)
        updateSliderInput(session, inputId='norm.mu', value=0)
      }      
      gv$xlim <<- c(0,0)
      gv$ylim <<- c(0,0)
      gv$dist <<- inp$distribution
    }
    switch(inp$distribution,
           EXP = {
             gv$xlim <<- mmstat.merge(gv$xlim, c(0,qexp(0.999, inp$exp.lambda)))
             x       <-  (0:300)/300*gv$xlim[2]
             if (inp$pdforcdf=='CPDF') {
               height  <- dexp(x, inp$exp.lambda)
               gv$ylim <<- mmstat.merge(gv$ylim, c(0,height))
               plot(x, height, type="l", xlim=gv$xlim, ylim=gv$ylim, 
                    ylab="",
                    xlab="x",
                    main=mmstat.math(sprintf(gettext("Density function f(x) of EX(&lambda;=%.1f)"), inp$exp.lambda)),
                    cex.axis=inp$cex, cex.lab=inp$cex, cex.main=1.2*inp$cex, cex.sub=inp$cex)
             } else {
               gv$ylim <<- mmstat.merge(gv$ylim, c(0,1))
               height  <- pexp(x, inp$exp.lambda)
               plot(x, height, type="l", xlim=gv$xlim, ylim=gv$ylim, 
                    ylab="",
                    xlab="x",
                    main=mmstat.math(sprintf(gettext("Cumulative distribution function F(x) of EX(&lambda;=%.1f)"), inp$exp.lambda)),
                    cex.axis=inp$cex, cex.lab=inp$cex, cex.main=1.2*inp$cex, cex.sub=inp$cex)
               abline(h=0, col="gray70", lty="dashed")
               abline(h=1, col="gray70", lty="dashed")
             }
             },
           NORM = {
             sd <- sqrt(inp$norm.sigma2)
             gv$xlim <<- mmstat.merge(gv$xlim, c(qnorm(0.001, inp$norm.mu, sd),
                                                 qnorm(0.999, inp$norm.mu, sd)))
             x       <-  gv$xlim[1]+(0:300)/300*diff(gv$xlim)
             if (inp$pdforcdf=='CPDF') {
               height  <- dnorm(x, inp$norm.mu, sd)
               gv$ylim <<- mmstat.merge(gv$ylim, c(0,height))
               plot(x, height, type="l", xlim=gv$xlim, ylim=gv$ylim, 
                    ylab="",
                    xlab="x",
                    main=mmstat.math(sprintf(gettext("Density function f(x) of N(&mu;=%.1f,&sigma^2;=%.1f)"), 
                                             inp$norm.mu, inp$norm.sigma2)),
                    cex.axis=inp$cex, cex.lab=inp$cex, cex.main=1.2*inp$cex, cex.sub=inp$cex)
               abline(v=0, col="gray70")
             } else {
               gv$ylim <<- mmstat.merge(gv$ylim, c(0,1))
               height  <- pnorm(x, inp$norm.mu, sd)
               plot(x, height, type="l", xlim=gv$xlim, ylim=gv$ylim, 
                    ylab="",
                    xlab="x",
                    main=mmstat.math(sprintf(gettext("Cumulative distribution function F(x) of N(&mu;=%.1f,&sigma^2;=%.1f)"), 
                                             inp$norm.mu, inp$norm.sigma2)),
                    cex.axis=inp$cex, cex.lab=inp$cex, cex.main=1.2*inp$cex, cex.sub=inp$cex)
               abline(h=0, col="gray70", lty="dashed")
               abline(h=1, col="gray70", lty="dashed")
             }
           }
      )
  })
  
  output$logText <- renderText({
    mmstat.getLog(session)
  })
})
