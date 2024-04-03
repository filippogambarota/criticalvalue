crit_from_t_t2s <- function(t = NULL, n1, n2, se = NULL,
                            conf.level, 
                            hypothesis,
                            var.equal = FALSE){
  if(!var.equal){
    warning("When var.equal = FALSE the critical value calculated from t assume sd1 = sd2!")
  }
  alpha <- .get_alpha(conf.level, hypothesis)
  df <- n1 + n2 - 2
  tc <- abs(qt(alpha, df))
  if(is.null(t)){
    warning("When t is NULL, d cannot be computed, returning NA")
    d <- NA
  }else{
    d <- t * (1/n1 + 1/n2)
  }
  dc <- tc * (1/n1 + 1/n2)
  if(is.null(se)){
    warning("When se = NULL bc cannot be computed, returning NA!")
    bc <- NA
  }else{
    bc <- (tc * se)
  }
  out <- list(d = d, dc = dc, bc = bc)
  return(out)
}

crit_from_data_t2s <- function(m1, m2, 
                               sd1, sd2, 
                               n1, n2, 
                               conf.level,
                               hypothesis,
                               var.equal = FALSE){
  alpha <- .get_alpha(conf.level, hypothesis)
  b <- m1 - m2
  if(!var.equal){ # welch
    se1 <- sd1 / sqrt(n1)
    se2 <- sd2 / sqrt(n2)
    se <- sqrt((se1^2 + se2^2)/2)
    s <- sqrt((sd1^2 * (n1 - 1) + sd2^2 * (n2 - 1))/(n1 + n2 - 2))
    df <- se^4/(se1^4/(n1-1) + se2^4/(n2-1))
  }else{ # standard
    s <- sqrt((sd1^2 * (n1 - 1) + sd2^2 * (n2 - 1)) / (n1 + n2 - 2))
    se <- s * (1/n1 + 1/n2)
    df <- n1 + n2 - 2
  }
  tc <- abs(qt(alpha, df))
  d <- b / s
  bc <- tc * se
  dc <- tc * (1/n1 + 1/n2)
  out <- list(d = d, dc = dc, bc = bc, df = df)
  return(out)
}


crit_from_t_t1s <- function(t = NULL, n, se = NULL,
                            conf.level,
                            hypothesis){
  alpha <- .get_alpha(conf.level, hypothesis)
  df <- n - 1
  tc <- abs(qt(alpha, df))
  dc <- tc * sqrt(1/n)
  if(is.null(t)){
    warning("When t is NULL, d cannot be computed, returning NA")
    d <- NA
  }else{
    d <- t * sqrt(1/n)
  }
  if(is.null(se)){
    warning("When se = NULL bc cannot be computed, returning NA!")
    bc <- NA
  }else{
    bc <- tc * se
  }
  out <- list(d = d, dc = dc, bc = bc, df = df)
  return(out)
}

crit_from_data_t1s <- function(m, s, n, conf.level, hypothesis){
  alpha <- .get_alpha(conf.level, hypothesis)
  df <- n - 1
  tc <- abs(qt(alpha, df))
  se <- s / sqrt(n)
  d <- m / s
  dc <- tc * sqrt(1/n)
  bc <- tc * se
  out <- list(d = d, dc = dc, bc = bc, df = df)
  return(out)
}

crit_from_t_t2sp <- function(t = NULL, n, se = NULL, r12 = NULL, hypothesis, conf.level){
  alpha <- .get_alpha(conf.level, hypothesis)
  df <- n - 1
  tc <- abs(qt(alpha, df))
  
  # d on differences
  dzc <- tc * sqrt(1/n)
  
  if(is.null(r12)) r12 <- 0
  dc <- dzc * sqrt(2 * (1 - r12))
  
  if(is.null(t)){
    warning("when t is NULL, dz and d cannot be computed, returning NA")
    dz <- NA
    d <- NA
  }else{
    dz <- t * sqrt(1/n)
    d <- dz * sqrt(2 * (1 - r12)) 
  }
  
  if(is.null(se)){
    warning("When se is NULL, bc cannot be computed, returning NA")
    bc <- NA
  }else{
    bc <- tc * se
  }
  
  out <- list(dz = dz, dzc = dzc, d = d, dc = dc, bc = bc, df = df)
  return(out)
}

crit_from_data_t2sp <- function(m1, m2 = NULL, 
                                sd1, sd2 = NULL, 
                                se = NULL,
                                r12 = NULL, n, 
                                conf.level,
                                hypothesis){
  alpha <- .get_alpha(conf.level, hypothesis)
  df <- n - 1
  tc <- abs(qt(alpha, df))
  
  if(!is.null(m2) & !is.null(sd2) & is.null(r12)){
    warning("when m2 and sd2 are provided and r12 is NULL dz and dzc and cannot be computed, returning NA")
  }
  
  if(is.null(r12)) r12 <- 0
  
  if(is.null(m2)){
    b <- m1
  }else{
    b <- m1 - m2
  }
  
  if(is.null(sd2)){
    sd <- sd1
  }else{
    sd <- sqrt(sd1^2 + sd2^2 - 2*r12*sd1*sd2)
  }
  
  dz <- b / sd
  dzc <- tc * (1/n)
  d <- dz * sqrt(2 * (1 - r12))
  dc <- dzc * sqrt(2 * (1 - r12))
  out <- list(dz = dz, dzc = dzc, d = d, dc = dc, bc = bc, df = df)
  return(out)
}

# CRITICAL FUNS -----------------------------------------------------------

critical_t1s <- function(m = NULL, s = NULL, t = NULL,
                         n, se = NULL,
                         hypothesis = c("two.sided", "greater", "less"),
                         conf.level = 0.95){
  hypothesis <- match.arg(hypothesis)
  if(!is.null(m)){
    out <- crit_from_data_t1s(m, s, n, conf.level, hypothesis)
  }else{
    out <- crit_from_t_t1s(t, n, se, conf.level, hypothesis)
  }
  
  # Hedges's Correction
  
  J <- .get_J(out$df)
  
  if(!is.na(out$d)){
    out$g <- J * out$d
  }
  if(!is.na(out$dc)){
    out$gc <- J * out$dc
  }
  return(out)
}

critical_t2s <- function(m1 = NULL, m2 = NULL, t = NULL,
                         sd1 = NULL, sd2 = NULL,
                         n1, n2, se = NULL,
                         var.equal = FALSE,
                         hypothesis = c("two.sided", "greater", "less"),
                         conf.level = 0.95){
  hypothesis <- match.arg(hypothesis)
  if(!is.null(m1) | !is.null(m2)){
    out <- crit_from_data_t2s(m1, m2, 
                       sd1, sd2, 
                       n1, n2, 
                       conf.level, 
                       hypothesis, 
                       var.equal)
  }else{
    out <- crit_from_t_t2s(t, 
                    n1, n2, 
                    se, 
                    conf.level, 
                    hypothesis, 
                    var.equal)
  }
  # Hedges's Correction
  J <- .get_J(out$df)
  
  if(!is.na(out$d)){
    out$g <- J * out$d
  }
  if(!is.na(out$dc)){
    out$gc <- J * out$dc
  }
  return(out)
}


critical_t2sp <- function(m1 = NULL, m2 = NULL, t = NULL,
                          sd1 = NULL, sd2 = NULL, r12 = NULL,
                          n, se = NULL,
                          hypothesis = c("two.sided", "greater", "less"),
                          conf.level = 0.95){
  hypothesis <- match.arg(hypothesis)
  if(!is.null(m1)){
    out <- crit_from_data_t2sp(m1, m2, sd1, sd2, n, se, r12, conf.level, hypothesis)
  }else{
    out <- crit_from_t_t2sp(t, n, se, r12, hypothesis, conf.level)
  }
  # Hedges's Correction
  
  J <- .get_J(out$df)
  if(!is.na(out$d)){
    out$g <- J * out$d
  }
  if(!is.na(out$dc)){
    out$gc <- J * out$dc
  }
  if(!is.na(out$dz)){
    out$gz <- J * out$dz
  }
  if(!is.na(out$dzc)){
    out$gzc <- J * out$dzc
  }
  return(out)
}

critical_cor <- function(r = NULL, n, 
                         conf.level = 0.95, 
                         hypothesis = c("two.sided", "greater", "less"), 
                         test = c("t", "z")){
  df <- n - 2
  test <- match.arg(test, test)
  hypothesis <- match.arg(hypothesis)
  
  alpha <- .get_alpha(conf.level, hypothesis)
  
  if(test == "t"){
    tc <- abs(qt(alpha, df))
    rc <- tc / sqrt(n - 2 + tc^2)
    se_rc <- sqrt((1 - rc^2)/(n - 2))
    if(!is.null(r)){
      se_r <- sqrt((1 - r^2)/(n - 2))
    }else{
      se_r <- NA
    }
  }else{
    zc <- abs(qnorm(alpha))
    rc <- tanh(zc / sqrt(n - 3))
    se_rc <- 1 / sqrt(n - 3)
    if(!is.null(r)){
      se_r <- se_rc
    }else{
      se_r <- NA
    }
  }
  out <- list(rc = rc, df = df, test = test)
  return(out)
}

critical_coef_lm <- function(seb, n = NULL, p = NULL,df = NULL,
                             conf.level = 0.95,
                             hypothesis = c("two.sided", "greater", "less"),
                             test = c("t", "z")){
  if(is.null(df) & (is.null(p) & is.null(n))){
    stop("df or p and n need cannot be NULL")
  }
  
  test <- match.arg(test, test)
  hypothesis <- match.arg(hypothesis)
  alpha <- .get_alpha(conf.level, hypothesis)
  
  if(is.null(df)){
    df <- n - p - 1
  }
  
  if(test == "t"){
    qc <- abs(qt(alpha, df))
  }else{
    qc <- abs(qnorm(alpha, df))
  }
  
  bc <- qc * seb
  out <- list(bc = bc, test = test)
  return(out)
}


