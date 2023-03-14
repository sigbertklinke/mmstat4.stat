library("shiny")
source("helper2.R")

mmstat.ui.elem('rolls', 'sliderInput', label   = gettext("Number of rolls:"),
               min = 1, max = 20, value = 3)
mmstat.ui.elem("prob", 'sliderInput', label   = gettext("Probability for six with loaded die:"),
               min = 0, max = 1, step = 1/6, value = 2/6)
mmstat.ui.elem("cex", 'fontSize')
mmstat.ui.elem("sixes", 'sliderInput', label   = gettext("Number of sixes (X):"),
               min = 0, max = 3, value= 0)

ddbinom <- function (x, size, prob) {
  if (prob<=0) {
    return (as.numeric(x==0))
  }
  if (prob>=1) {
    return (as.numeric(x==size))
  }
  dbinom(x, size, prob)
}

shinyServer(function(input, output, session) {
  
  output$rollsUI <- renderUI({ mmstat.ui.call('rolls') })
  output$probUI  <- renderUI({ mmstat.ui.call('prob') })
  output$cexUI   <- renderUI({ mmstat.ui.call('cex') })
  output$sixesUI <- renderUI({ mmstat.ui.call('sixes') })

  output$distPlot <- renderPlot({
    inp   <- mmstat.getValues(NULL, cex        = input$cex,
                                    prob       = input$prob,
                                    rolls      = input$rolls)
    t  <- 0:inp$rolls
    w0 <- dbinom(t, inp$rolls, 1/6)
    w1 <- ddbinom(t, inp$rolls, inp$prob)
    
    mp<-barplot(rbind(w1,w0), main=gettext("P(Number of sixes | ... die)"), ylim=c(0,1), axes=F, 
                col=c(mmstat$col[[2]], mmstat$col[[1]]), beside=T, 
                cex.axis=inp$cex, cex.lab=inp$cex, cex.main=1.2*inp$cex, cex.sub=inp$cex)
    legend("topright", gettext(c("loaded die (W=1)", "fair die (W=0)")), cex=inp$cex, 
           fill=c(mmstat$col[[2]], mmstat$col[[1]]),)
    mp<-colMeans(mp)
    axis(1, at=mp, labels=sprintf("%.0f", t), cex.axis=inp$cex)
    axis(2, at=(0:5)/5, labels=sprintf("%.1f", (0:5)/5), cex.axis=inp$cex)
    box()
  })

  output$formula <- renderText({
    inp   <- mmstat.getValues(NULL, cex        = input$cex,
                                    prob       = input$prob,
                                    sixes      = input$sixes,
                                    rolls      = input$rolls)
    t  <- 0:inp$rolls
    w0 <- dbinom(t, inp$rolls, 1/6)
    w1 <- ddbinom(t, inp$rolls, inp$prob)
    p1 <- w1[1+inp$sixes]/(w1[1+inp$sixes]+w0[1+inp$sixes])
    p0 <- w0[1+inp$sixes]/(w1[1+inp$sixes]+w0[1+inp$sixes])
    
    paste0(sprintf('<table style="font-size:%.0f%%"><tr style="color:%s"><td>', 90*inp$cex, mmstat$col[[2]]),
           sprintf('P(W=1|X=%.0f)=', inp$sixes),
           '</td><td align="center">',
           sprintf('P(X=%.0f|W=1)*P(W=1)<img src="dorange.png" style="width:100%%;height:1px">P(X=%.0f|W=0)*P(W=0)+P(X=%.0f|W=1)*P(W=1)', inp$sixes, inp$sixes, inp$sixes),
           '</td><td>=</td><td align="center">',
           sprintf('%.3f*0.5<img src="dorange.png" style="width:100%%;height:1px">%.3f*0.5+%.3f*0.5', w1[1+inp$sixes], w0[1+inp$sixes], w1[1+inp$sixes]),
           '</td><td>=</td><td align="center">',
           sprintf('%0.3f', p1),
           '</td></tr><tr><td><br><br></td></tr>',
           sprintf('<tr style="color:%s"><td>', mmstat$col[[1]]),
           sprintf('P(W=0|X=%.0f)=', inp$sixes),
           '</td><td align="center">',
           sprintf('P(X=%.0f|W=0)*P(W=0)<img src="daquamarine.png" style="width:100%%;height:1px">P(X=%.0f|W=0)*P(W=0)+P(X=%.0f|W=1)*P(W=1)', inp$sixes, inp$sixes, inp$sixes),
           '</td><td>=</td><td align="center">',
           sprintf('%.3f*0.5<img src="daquamarine.png" style="width:100%%;height:1px">%.3f*0.5+%.3f*0.5', w0[1+inp$sixes], w0[1+inp$sixes], w1[1+inp$sixes]),
           '</td><td>=</td><td align="center">',
           sprintf('%0.3f', p0),
           '</td></tr><tr><td><br><br></td></tr></table>')
  })
  
  output$logText <- renderText({
    mmstat.getLog(session)
  })
})
