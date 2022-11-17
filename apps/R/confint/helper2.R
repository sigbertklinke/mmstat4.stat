# Default parameters

mmstat.getValues <- function (local, ...) {
  ret <<- list(...)
  for (name in names(ret)) {
    if (is.null(ret[[name]]) || (length(ret[[name]])==0)) {
      if (is.null(local[[name]])) {
        stopif (is.null(mmstat$UI[[name]]$value), paste0('no default value(s) for "', name, '"'))
        ret[[name]] <- mmstat$UI[[name]]$value
      } else {
        ret[[name]] <- local[[name]]
      }
    }
  }
  ret
}

# Datasets and variables

mmstat.getDatasets <- function (...) {
  is.binary <- function(v) {
    x <- unique(v)
    length(x) - sum(is.na(x)) == 2L
  }
  
  files          <- list(...)
  dataset        <- vector("list", length(files))
  names(dataset) <- files
  mmstat$dataset <<- dataset
  for (i in seq(files)) {
    file    <- files[[i]]    
    data    <- read.csv(paste0(file, ".txt"))
    allvars <- names(data)
    mmstat$dataset[[file]] <<-list(data=data, allvars=allvars, 
                                   numvars=allvars[sapply(data, is.numeric)], 
                                   binvars=allvars[sapply(data, is.binary)])
  }
  names(dataset)[1]
}

mmstat.getDatasetNames <- function() {
  gettext(names(mmstat$dataset), "name")
}

mmstat.getVariableNames <- function(name) {
  gettext(names(mmstat$dataset[[name]]$data), "name")
}

mmstat.getVar <- function (dataname=NULL, varname=NULL, vartype=NULL) {
  match.name <- function(needle, haystack) {
    if (is.null(needle)) return(NA);
    return(match(needle, haystack))
  }
  
  # which variable type do we need
  if (is.null(vartype)) vartype <- mmstat$vartype
  deftype <- c('numvars', 'binvars')
  type   <- pmatch(vartype, deftype)
  if (is.na(type)) stop(paste0('Unknown variable type: ', vartype))
  type   <- deftype[type]
  # get dataset
  pos <- match.name(dataname, names(mmstat$dataset))
  if (is.na(pos)) pos <- 1  # assume that all datasets contain 
  dataname <- names(mmstat$dataset)[pos]
  if (is.null(mmstat$dataset[[dataname]])) stop(sprintf(gettext("Data file '%s' is missing"), dataname))
  # get variable
  pos <- match.name(varname, mmstat$dataset[[dataname]][[type]])
  if (is.na(pos)) pos <- 1  # assume that every datasets contains a variable of correct type 
  varname <- mmstat$dataset[[dataname]][[type]][pos]
  values  <- na.omit(mmstat$dataset[[dataname]]$data[[varname]])
  var     <- list(values   = values, 
                  n        = length(values),
                  name     = varname,
                  sub      = paste(gettext("Dataset:"), gettext(dataname)),
                  xlab     = gettext(varname),
                  data     = dataname,
                  dataname = gettext(dataname)
                 )
  if (type=='numvars') {
    var <- c(var, list(mean    = mean(values),
                       jitter  = runif(length(values)),
                       median  = median(values),
                       sd      = sd(values),
                       var    = var(values),
                       range   = range(values),
                       iqr     = IQR(values),
                       quart   = quantile(values, c(0.25, 0.75)),
                       min     = min(values),
                       max     = max(values)
             ))
  }
  if (type=='binvars') {
    tab  = table(values)
    var <- c(var, list(tab  = tab,
                       prop = prop.table(tab)
    ))
  }
  var
}

mmstat.dec <- function(x, ord=NULL) {
  order <- order(x)
  df    <- diff(x[order])
  # take only postive differences
  df    <- min(df[df>0])
  if (!is.null(ord)) order <- order(order[ord])
  list(decimal = max(0, ceiling(-log10(df))),
       order   = order)
}

mmstat.axis <- function(side, range, at, labels, ...) {
  at  <- pretty(range)
  dec <- mmstat.dec(at)
  axis(side, at=at, labels=sprintf("%.*f", dec$decimal, at), ...)
}

mmstat.baraxis <- function(side, range, at, labels, ...) {
  pos <- 1+pretty(range)
  axis(side, at=at[pos], labels=labels[pos], ...)
}

mmstat.merge <- function (range1, range2) {
  return (c(min(range1, range2), max(range1, range2)))
}

mmstat.range <- function (...) {
  ranges <- list(...)
  isn    <- sapply(ranges, is.numeric)
  if (all(isn)) {
    mins   <- sapply(ranges, min)
    maxs   <- sapply(ranges, max)
  } else {
    mins <- maxs <- NA
  }
  return (c(min(mins), max(maxs)))
}

mmstat.round.up <- function (x, digits=0) {
  xr <- round(x, digits)
  tf <- (xr<x)
  xr[tf] <- xr[tf]+10^(-digits) 
  xr
}

mmstat.round.down <- function (x, digits=0) {
  xr <- round(x, digits)
  tf <- (xr>x)
  xr[tf] <- xr[tf]-10^(-digits)
  xr
}

mmstat.pos <- function (minmax, pos) {
  min(minmax)+diff(minmax)*pos
}

mmstat.ticks <- function (nin, nmin=3, tin=11) {
  nmax <- nin
  nt   <- tin-1
  repeat {
    n           <- nmin*exp((0:nt)/nt*log(nmax/nmin))
    pow         <- 10^trunc(log10(n))
    fsd         <- n%/%pow
    ssd         <- (n%%pow)%/%(pow/10)
    ssd[pow==1] <- 0
    ssd[ssd<3]  <- 0
    ssd[(ssd>2)&(ssd<8)] <- 5
    fsd[ssd>7]  <- fsd[ssd>7]+1
    ssd[ssd>7]  <- 0
    nret        <- fsd*pow+ssd*pow/10
    if(nret[nt+1]>nmax) nret[nt+1]<-nret[nt+1]-pow[nt+1]/2
    if (length(unique(nret))==nt+1) return(nret)
    nt <- nt-1
  }  
}

mmstat.math <- function (txt) {
  dollar <- strsplit(txt, '&', fixed=T)[[1]]
  if (length(dollar)<2) return(txt)
  res <- paste0('expression(paste("', dollar[1], '"')
  for (i in 2:length(dollar)) {
    percent <- strsplit(dollar[i], ';', fixed=T)[[1]]
    lp      <- length(percent)
    if (lp==1) res <- paste0(res, ',"', percent[1], '"')
    else {
      if (lp>2) percent[2] <- paste(percent[2:lp], sep=';')
      res <- paste0(res, ',', percent[1], ',"', percent[2], '"')
    }
  }
  res <- paste0(res, '))')
  eval(parse(text=res))
}

is.ASCII <- function (txt) { all(charToRaw(txt) <= as.raw(127)) }
stopif   <- function (cond, txt) { if (cond) stop(txt) }

mmstat.html <- function(file, ...) {
  stopif (!file.exists(file), sprintf("File '%s' does not exist", file))
  html     <- paste0(readLines(file), collapse="")
  stopif (!is.ASCII(html), sprintf("File '%s' contains non-ASCII symbols", file))
  args <- list(...)
  cond <- sapply(args, length)
  stopif(!all(cond), paste('mmstat.html - Zero length arguments:', paste(names(args)[!cond], collapse=', ')))
  if (length(args)) {
    stopif (any(sapply(args, is.null)), 'One or more arguments contain a NULL')
    args$fmt <- html
    html     <- do.call("sprintf", args)
  }
  return(html)
}

mmstat.plotTestRegions <- function (crit, xlim, ylim, cex, close=F) {
  lines(xlim, c(ylim[1], ylim[1]))
  lines(xlim, c(ylim[2], ylim[2]))
  if (close) {
    lines(c(xlim[1],xlim[1]), ylim)
    lines(c(xlim[2],xlim[2]), ylim)
  }
  cu <- max(crit[1], xlim[1])
  if (crit[1]>=xlim[1]) {
    lines(c(cu,cu), ylim)
    text((cu+xlim[1])/2, mean(ylim), mmstat.math("\\\"&H[1];\\\""), cex=cex)
  } 
  co <- min(crit[2], xlim[2])
  if (crit[2]<=xlim[2]) {
    lines(c(co,co), ylim)
    text((co+xlim[2])/2, mean(ylim), mmstat.math("\\\"&H[1];\\\""), cex=cex)
  }
  text((co+cu)/2, mean(ylim), mmstat.math("\\\"&H[0];\\\""), cex=cex)
}

mmstat.htest <- function (...) {
  
  addnames <- function (txt1, txt2) {
    if (length(txt1)) {
      cont <- txt2 %in% txt1
      ret  <- c(txt1, txt2[!cont])
    } else {
      ret <- txt2
    }
    ret
  }
  
  htest <- list(method      = list(attr=NULL,         names='',                     fmt="%s",   lines=0),
                alternative = list(attr=NULL,         names='Alternative:',         fmt="%s",   lines=0),
                null.value  = list(attr='names',      names=vector("character", 0), fmt="%.4f", lines=1),
                data.name   = list(attr=NULL,         names='Data:',                fmt="%s",   lines=0),
                estimate    = list(attr='names',      names=vector("character", 0), fmt="%.4f", lines=0),
                conf.int    = list(attr='conf.level', names=vector("character", 0), fmt="%s",   lines=1),
                statistic   = list(attr='names',      names=vector("character", 0), fmt="%.4f", lines=0),
                parameter   = list(attr='names',      names=vector("character", 0), fmt="%.0f", lines=0),
                p.value     = list(attr=NULL,         names='p-value:',             fmt="%.4f", lines=0)
  )
  
  tests  <- list(...)
  nhtest <- names(htest)
  nrow   <- vector("numeric", length(htest))
  lines  <- 0
  for (j in seq(nhtest)) {
    name <- nhtest[j]
    attr <- htest[[nhtest[j]]]$attr
    if (!is.null(attr)) {
      # find all names
      for (i in seq(tests)) {
        htest[[name]]$names <- addnames(htest[[name]]$names, attr(tests[[i]][[name]], attr))
      }
    }
    # grab all values
    nrow[j] <- length(htest[[name]]$names)
    htest[[name]]$tab <- matrix('', nrow=nrow[j], ncol=length(tests))
    for (i in seq(tests)) {
      telem <- tests[[i]][[name]]
      if (!is.null(telem)) {
        if (is.null(attr)) {
          htest[[name]]$tab[1, i] <- sprintf(htest[[name]]$fmt, telem)
        } else if (attr=='conf.level') {
          htest[[name]]$tab[match(as.character(attr(telem, attr)), htest[[name]]$names), i] <- 
            paste0('[', round(telem[1],4), ', ', round(telem[2],4), ']')   
        } else {
          htest[[name]]$tab[match(as.character(attr(telem, attr)), htest[[name]]$names), i] <- 
            sprintf(htest[[name]]$fmt, telem)
        }
      }
    }
    if (!is.null(attr)) {
      if (attr=='conf.level') {
        htest[[name]]$names <- sprintf("%.1f%% CI", 100*as.numeric(htest[[name]]$names))
      }
    }
    lines <- lines+htest[[name]]$lines
  }
  tab <- matrix('', nrow=sum(nrow)+lines, ncol=1+length(tests))
  pos <- 1
  for (j in seq(nhtest)) {
    name <- nhtest[j]
    len  <- length(htest[[name]]$names)
    tab[pos:(pos+len-1), 1] <- htest[[name]]$names
    tab[pos:(pos+len-1), 2:(1+length(tests))] <- htest[[name]]$tab
    pos <- pos+len+htest[[name]]$lines
  }
  maxlen <- apply(nchar(tab), 2, max)
  for (j in seq(tab)) {
    if (j<=nrow(tab))
      tab[j] <- sprintf('%-*s', maxlen[1+((j-1)%/%nrow(tab))], tab[j])
    else 
      tab[j] <- sprintf('%*s', maxlen[1+((j-1)%/%nrow(tab))], tab[j])
  }
  paste(apply(tab, 1, paste, collapse="   "), collapse="\n")
}

mmstat.lang <- function(deflang=NULL) {
  pof  <- list.files(pattern="*.po$")
  if (is.null(deflang)) {
    lang <- sapply(strsplit(pof, '.', fixed=T), function(elem) { elem[1] })
    pat  <- paste0('_', lang, '$')
    path <- getwd()
    path <- strsplit(path, '/', fixed=T)[[1]]
    pos  <- -1
    lind <- -1
    for (i in seq(pat)) {
      p <- grep(pat[i], path)
      if (length(p)) {
        p <- max(p)
        if (p>pos) { pos <- p; lind <- i; }
      }
    }
  } else {
    lind <- match (paste0(deflang, '.po'), pof)
    if (is.na(lind)) lind <- (-1)
  }
  msgfn <- ifelse(lind>0, pof[lind], "default.po")
  mmstat.warn (!file.exists(msgfn), sprintf("File '%s' does not exist", msgfn))
  msg    <- paste(readLines(msgfn), collapse=" ")
  msgid  <- regmatches(msg, gregexpr('msgid\\s*".*?"', msg))
  tmp    <- strsplit(msgid[[1]], '"')
  msgid  <- sapply(tmp, function (vec) { paste0(vec[2:length(vec)]) } )
  msgstr <- regmatches(msg, gregexpr('msgstr\\s*".*?"', msg))
  tmp    <- strsplit(msgstr[[1]], '"')
  msgstr <- sapply(tmp, function (vec) { paste0(vec[2:length(vec)]) } )
  mmstat$messages <<- list(id=msgid, str=msgstr)
}

ucfirst <- function (txt) { return (paste0(toupper(substr(txt, 1, 1)), substring(txt, 2))); }

gettext <- function (msg, utype="vector") {
  type <- pmatch(utype, c("name", "numeric"))
  if (is.na(type)) {
    ret <- paste0(ifelse(mmstat$debug>2, '?', ''), msg)
    pos <- match(msg, mmstat$messages$id)
    ind <- (1:length(pos))[!is.na(pos)]
    ret[ind] <- mmstat$messages$str[pos[ind]]
  } else if (type==1) {
    ret        <- as.list(msg)
    names(ret) <- gettext(msg)
  } else if (type==2) {
    ret        <- as.list(seq(msg))
    names(ret) <- gettext(msg)
  }
  return (ret)
}

# Logging

mmstat.getLog <- function (session) {
  if (!mmstat$debug) return ("")
  invalidateLater(100, session)
  paste0('<hr>', paste(mmstat$log, collapse="<br>"))
}

mmstat.log  <- function (txt) { if (mmstat$debug>1) mmstat$log <<- cbind(sprintf("%s (Info): %s", date(), txt), mmstat$log) } 
mmstat.warn <- function (cond, txt) { if ((mmstat$debug>0) && cond) mmstat$log <<- cbind(sprintf("%s (Warn): %s", date(), txt), mmstat$log) } 
mmstat.input <- function (input) {
  if (mmstat$debug>1) {
    ni  <-names(input)
    for (i in seq(ni)) {
      out <- capture.output(input[[ni[i]]])
      out[1] <- paste0(ni[i], ': ', out[1])
      for (j in seq(out)) mmstat$log <<- cbind(sprintf("%s (Input): %s", date(), out[j]), mmstat$log)  
    }
  }
}

# Main

mmstat <- list(debug = 1,
               col   = list(daquamarine="#1B9E77",  dorange="#D95F02",   dblue="#7570B3",   dpink="#E7298A", 	
                            dgreen="#66A61E",     	dyellow="#E6AB02", 	dgold="#A6761D", 	dgray="#666666",
                            laquamarine="#66C2A5",  lorange="#FC8D62",  lblue="#8DA0CB",  lpink="#E78AC3", 	
                            lgreen="#A6D854",     	lyellow="#FFD92F", 	lgold="#E5C494", 	lgray="#B3B3B3"),
               alpha = c(0.1, 0.25, 0.5, 1, 2.5, 5, 10, 20)
              )
mmstat.log("Starting app")
mmstat.lang()
Sys.setlocale("LC_ALL", gettext("LC_ALL"))






