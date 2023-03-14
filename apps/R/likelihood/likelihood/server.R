library("shiny")

thetahats <<- 0.5
likelihood <<- vector("numeric", 0)
loglikelihood <<- vector("numeric", 0)

makeTable <- function(tab) {
  ret <- ''
  if (length(tab)) {
    cmp     <- (names(tab)=="")
    nonames <- is.na(cmp) | cmp
    names   <- ifelse(nonames, '', paste0('<tr><td bgcolor="#CCCCCC">', names(tab), '</td></tr>'))
    values  <- paste0('<tr><td align="right">', tab, '</td></tr>')
    ret     <- paste0('<table width="100%">', paste0(names, values, collapse=''), '</table>')
  }
  ret
}

shinyServer(function(input, output, session) {
  
  compute <- function(obs) {
    dist <- isolate(input$dist)
    nobs <- na.omit(obs)
    for (i in seq(thetahats)) {
      if (dist=="Bernoulli") {
        likelihood[i] <<- dbinom(sum(nobs), length(nobs), thetahats[i])
      }
      if (dist=="Poisson") {
        likelihood[i] <<- prod(dpois(nobs, thetahats[i]))
      }
      if (dist=="Exponential") {
        likelihood[i] <<- prod(dexp(nobs, thetahats[i]))
      }      
    }
    loglikelihood <<- log(likelihood)
  }
  
  getObs <- reactive ({
    obs <- c(input$obs1, input$obs2, input$obs3, input$obs4, input$obs5)
    if (input$dist=="Bernoulli") {
      obs[obs<0.5] <- 0
      obs[obs>0.5] <- 1      
    }
    if (input$dist=="Poisson") {
      obs[obs<0] <- 0
    }
    if (input$dist=="Exponential") {
      obs[obs<0] <- 0
    }        
    updateNumericInput(session, "obs1", value=obs[1])
    updateNumericInput(session, "obs2", value=obs[2])
    updateNumericInput(session, "obs3", value=obs[3])
    updateNumericInput(session, "obs4", value=obs[4])
    updateNumericInput(session, "obs5", value=obs[5])
    compute(obs)
    obs
  })
  
  output$sliderUI <- renderUI({
    thetahats     <<- 0.5
    likelihood    <<- vector("numeric", 0)
    loglikelihood <<- vector("numeric", 0)
    obs <- getObs()
    if (input$dist=="Bernoulli") {
      ret <- sliderInput(inputId = "thetahat", label = HTML("Anteilssch&auml;tzung (&pi;&#770)"),
                         min = 0, max = 1, value = 0.5) 
    }
    if (input$dist=="Poisson") {
      ret <- sliderInput(inputId = "thetahat", label = HTML("Intensit&auml;tssch&auml;tzung (&lambda;&#770)"),
                          min = 0, max = max(c(1,obs), na.rm=TRUE), value = 0.5, step=0.01) 
    }
    if (input$dist=="Exponential") {
      ret <- sliderInput(inputId = "thetahat", label = HTML("Intensit&auml;tssch&auml;tzung (&lambda;&#770)"),
                         min = 0, max = max(c(1,obs), na.rm=TRUE), value = 0.5, step=0.01)
    }
    ret
  })
  
  output$outputPlot <- renderPlot({
    obs  <- getObs()
    nobs <- na.omit(obs)
    par(mfrow=c(2,1))
    thetahats <<- c(thetahats, input$thetahat)      
    if (input$dist=="Bernoulli") {
      xlab <- expression(hat(pi))
    } else {
      xlab <- expression(hat(lambda))
    }
    compute(obs)
    nt    <- length(thetahats)
    if (nt) {
      index <- max(1, nt-10):nt
      index <- c(index, which.max(likelihood))
      ni    <- length(index)
      col   <- rep("black", ni)     
      col[ni]   <- "red"
      col[ni-1] <- "green"
      plot(thetahats[index], likelihood[index], main="Likelihoodfunktion", pch=19, xlab=xlab, col=col, ylab="")
      plot(thetahats[index], loglikelihood[index], main="Loglikelihoodfunktion", pch=19, xlab=xlab, col=col, ylab="")
    }
  })
  
  output$outputValues <- renderText({
    makeTable(values$table)
  })

})