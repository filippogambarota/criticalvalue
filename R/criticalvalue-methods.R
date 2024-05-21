

#' @export
critical <- function(x, ...){
  UseMethod("critical")
}

#' @export
critical.htest <- function(x){
  # all implemented subtype of htest objects
  mtds <- c("Two Sample", "One Sample", "correlation", "Paired")
  
  if(all(!grepl(paste0(mtds, collapse = "|"), x$method))){
    stop(
      sprintf("method %s of class %s not implemented yet!",
              x$method, class(x))
    )
  } else {
    D <- insight::get_data(x)
    conf.level <- attributes(x$conf.int)$conf.level
    
    # hypothesis <- ifelse(x$alternative == "two.sided", "2t", "1t")
    hypothesis <- x$alternative
    
    alpha <- .get_alpha(conf.level, hypothesis)
    df <- x$parameter
    tc <- qt(alpha, df)
    t <- x$statistic
    se <- x$stderr
    b <- x$estimate
    method <- x$method
  }
  
  if(grepl("correlation", method)){
    n <- df + 2
    cc <- critical_cor(n = n, conf.level = conf.level, hypothesis = hypothesis)
    dc <- bc <- cc$rc
    d <- b
    class(x) <- c("critvalue", "ctest", "htest")
  }else{ # if t-test
    if(grepl("Two Sample", method)){
      n <- tapply(D$x, D$y, length)
      if(grepl("Welch", method)){
        ss <- tapply(D$x, D$y, sd)
        mm <- tapply(D$x, D$y, mean)
        tt <- critical_t2s(m1 = mm[1], m2 = mm[2],
                           sd1 = ss[1], sd2 = ss[2], 
                           n1 = n[1], n2 = n[2],
                           se = se,
                           hypothesis = hypothesis,
                           var.equal = FALSE)
      }else{
        tt <- critical_t2s(t = t, se = se, n1 = n[1], n2 = n[2], var.equal = TRUE,
                           conf.level = conf.level, hypothesis = hypothesis)
      }
    } else if(grepl("One Sample", method)){
      n <- length(D$x)
      tt <- critical_t1s(t = t, se = se, n = n, hypothesis = hypothesis, conf.level = conf.level)
    } else if(grepl("Paired", method)){
      n <- length(D$x)
      r12 <- cor(D$x[D$y == 1], D$x[D$y == 2])
      tt <- critical_t2sp(t = t, 
                          se = se, 
                          r12 = r12,
                          n = n,
                          hypothesis = hypothesis, 
                          conf.level = conf.level)
      x$dz <- tt$dz
      x$dzc <- tt$dzc
      x$gz <- tt$gz
      x$gzc <- tt$gzc
    }

    d <- tt$d
    bc <- tt$bc
    dc <- tt$dc
    x$g <- tt$g
    x$gc <- tt$gc
    class(x) <- c("critvalue", "ttest", "htest")
    
  }
  
  # common elements
  x$d <- unname(d)
  x$bc <- unname(bc)
  x$dc <- unname(dc)
  
  return(x)
}

#' @export
critical.lm <- function(x, conf.level = 0.95,
                        standardize = FALSE,
                        ...){
  
  # always two.sided
  hypothesis <- "two.sided"
  alpha <- .get_alpha(conf.level, hypothesis)
  df <- x$df.residual
  
  if(standardize){
    x <- standardize(x, ...)
  }
  
  se <- sqrt(diag(vcov(x)))
  ll <- critical_coef(se, df = df, conf.level = conf.level, hypothesis = hypothesis)
  d <- NA
  dc <- NA
  x$d <- NA
  x$dc <- NA
  x$bc <- unname(ll$bc)
  class(x) <- c("critvalue", "lm")
  return(x)
}

#' @export
critical.rma <- function(x, conf.level = 0.95, h0 = 0){
  if(inherits(x, "rma.uni")){
    hypothesis <- "two.sided"
    se <- x$se
    df <- x$k.eff
    ll <- critical_coef(se, df = df, conf.level = conf.level, hypothesis = hypothesis)
    d <- NA
    dc <- NA
    x$d <- NA
    x$dc <- NA
    x$bc <- unname(ll$bc)
    class(x) <- c("critvalue", "rma.uni", "rma")
  } else{
    stop(paste("class", class(x)[1], "not supported yet!"))
  }
  return(x)
}

#' @export
print.critvalue <- function(x, digits = getOption("digits"), ...){
  NextMethod(x, ...)
  # TODO vedi se c'è un modo di fare un metodo con due classi
  # nested in modo da fare critvalue/ttest e critvalue/htest
  x <- .round_list(x, digits)
  if(inherits(x, "ttest")){
    cat("|== Effect Size and Critical Value ==|", "\n")
    if(x$alternative == "two.sided"){
      cat("d =", x$d, "dc = ±", abs(x$dc), "bc = ±", abs(x$bc),"\n")
      cat("g =", x$g, "gc = ±", abs(x$gc), "\n")
    } else if(x$alternative == "greater"){
      cat("d =", x$d, "dc =", abs(x$dc), "bc =", abs(x$bc),"\n")
      cat("g =", x$g, "gc =", abs(x$gc), "\n")
    }else{
      cat("d =", x$d, "dc =", -abs(x$dc), "bc =", -abs(x$bc),"\n")
      cat("g =", x$g, "gc =", -abs(x$gc), "\n")
    }
    
    if(grepl("Paired", x$method)){
      if(x$alternative == "two.sided"){
        cat("dz =", x$dz, "|dzc| =", abs(x$dzc), "\n")
        cat("gz =", x$gz, "|gzc| =", abs(x$dzc), "\n")
      } else if(x$alternative == "greater"){
        cat("dz =", x$dz, "dzc =", abs(x$dzc), "\n")
        cat("gz =", x$gz, "gzc =", abs(x$dzc), "\n")
      } else{
        cat("dz =", x$dz, "dzc =", -abs(x$dzc), "\n")
        cat("gz =", x$gz, "gzc =", -abs(x$dzc), "\n")
      }
    }
    
  }else if(inherits(x, "ctest")){
    cat("|== Critical Value ==|", "\n")
    if(x$alternative == "two.sided"){
      cat("|rc| =", abs(x$dc),"\n\n")
    } else if(x$alternative == "greater"){
      cat("rc =", abs(x$dc),"\n\n")
    } else{
      cat("rc =", -abs(x$dc),"\n\n")
    }
    
  } else if(inherits(x, "lm")){
    cat("\nCritical |Coefficients| \n\n")
    bc <- x$bc
    names(bc) <- names(coef(x))
    print(abs(bc), ...)
    cat("\n")
  } else if(inherits(x, "rma.uni")){
    crit <- data.frame(x$b)
    crit[, 1] <- x$bc
    if(x$int.only){
      rownames(crit) <- ""
    }
    names(crit) <- "|critical estimate|"
    cat(capture.output(print(crit)), sep = "\n")
  }
  invisible(x)
}

#' @export
summary.critvalue <- function(x, ...){
  NextMethod(x, ...)
  if(inherits(x, "lm")){
    # taken from https://github.com/cran/lm.beta/blob/master/R/summary.lm.beta.R
    x2 <- x
    attr(x2, "class") <- "lm"
    x.summary <- summary(x2, ...)
    x.summary$coefficients <- cbind(x.summary$coefficients[, 
                                                           1, drop = F], `|Critical Estimate|` = abs(x$bc), 
                                    x.summary$coefficients[, -1, drop = F])
    class(x.summary) <- c("summary.critvalue", class(x.summary))
  } else if(inherits(x, "rma.uni")){
    x.summary <- x
  }
  x.summary
}