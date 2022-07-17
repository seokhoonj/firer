backtest <- function(gr, wt, init, cont, rebal = 12) {
  rcpp_backtest(gr, wt, init, cont, rebal)
}

fvc <- function(..., 
                wt, mmt = F, init = 1e8, cont = 0, rebal = 12, usd = FALSE,
                fxc = .0175, fxcr = .05, taxr =.22, divt = .154, rfr = .0065,
                term, from = 1, to, sdate, edate, chart = F, strategy = "buy-and-hold") {
  
  # epsilon
  eps <- 1e-8
  
  # portfolio
  df_lst <- copy(list(...))
  # df_lst <- copy(list(spy, efa, eem, agg, lqd, shy, ief))
  # df_lst <- copy(list(acwi, agg))
  
  # number of asset
  m <- length(df_lst)
  
  # each years & months
  ns <- sapply(df_lst, function(x) round(nrow(x) / 12, 1))
  ms <- sapply(df_lst, nrow)
  
  # asset data header without ym
  df_lst <- lapply(seq_len(m), function(x) setnames(df_lst[[x]], paste0(names(df_lst[[x]]), zpad2(x))))
  df_lst <- lapply(df_lst, function(x) `names<-`(x, gsub("ym[0-9]+", "ym", names(x))))
  
  # merge asset and calculate n(term)
  df <- Reduce(function(...) merge(..., by = "ym"), df_lst)
  
  # allocation(weights) variable
  if (missing(wt)) wt <- rep(1./m, m)
  
  if (is.matrix(wt)) {
    if (nrow(wt) != nrow(df)) stop("The number of rows are not the same.")
    if (ncol(wt) != m) stop("The length of weights is not the same with the number of stocks.")
    if (any(abs(rowSums(wt)-1) > eps)) stop("Sum of asset allocation is not 1.\n")
  }
  
  if (is.vector(wt)) {
    if (length(wt) != m) stop("The length of weights is not the same with the number of stocks.")
    if (abs(sum(wt)-1) > eps) stop("Sum of asset allocation is not 1.\n")
  }
  
  # point base
  if (missing(to)) {
    df <- df[from:nrow(df)]
  } else {
    df <- df[from:to]
  }
  # date base
  if (!missing(sdate)) {
    df <- df[ym >= sdate & ym <= edate]
  }
  # term base
  if (!missing(term)) {
    if (term < nrow(df)) df <- df[(nrow(df)-term+1):nrow(df)]
  }
  
  # order labeling
  n  <- nrow(df) 
  nv <- seq_len(n) 
  
  # re-balancing timing
  reb <- ifelse(nv %% rebal == 0, 1, 0)
  if (rebal == 0) reb <- rep(.0, n)
  
  # krw
  df[.krw, on = .(ym), currency := i.currency]
  fx <- df$currency[n]
  
  # initial payment
  if (init == 0) init <- cont
  
  # currency
  if (!usd) {
    
    # krw
    init_krw <- c(init, rep(0, n-1))
    cont_krw <- rep(cont, n)
    tot_krw  <- init_krw + cont_krw
    sum_tot_krw <- sum(tot_krw)
    csum_tot_krw <- cumsum(tot_krw)
    
    # usd
    init_usd <- init_krw / (df$currency * (1 + (fxc * (1-fxcr))))
    cont_usd <- cont_krw / (df$currency * (1 + (fxc * (1-fxcr))))
    tot_usd  <- init_usd + cont_usd
    sum_tot_usd <- sum(tot_usd)
    csum_tot_usd <- cumsum(tot_usd)
    
  } else {
    
    # usd
    init_usd <- c(init, rep(0, n-1))
    cont_usd <- rep(cont, n)
    tot_usd  <- init_usd + cont_usd
    sum_tot_usd <- sum(tot_usd)
    csum_tot_usd <- cumsum(tot_usd)
    
    # fake krw
    init_krw <- init_usd * df$currency * (1-(fxc*(1-fxcr)))
    cont_krw <- cont_usd * df$currency * (1-(fxc*(1-fxcr)))
    tot_krw  <- init_krw + cont_krw
    sum_tot_krw <- sum(tot_krw)
    csum_tot_krw <- cumsum(tot_krw)
    
  }
  
  set(df, j = "n"           , value = nv          )
  set(df, j = "tot_usd"     , value = tot_usd     )
  set(df, j = "csum_tot_usd", value = csum_tot_usd)
  set(df, j = "reb"         , value = reb         )
  
  # prepare columns for calculating future value
  col_price  <- regmatch_cols(df, "^price[0-9][0-9]") 
  col_i      <- regmatch_cols(df, "^i[0-9][0-9]"     )
  col_i_div  <- regmatch_cols(df, "^i_div[0-9][0-9]" ) 
  col_i_adj  <- regmatch_cols(df, "^i_adj[0-9][0-9]" ) 
  
  # 11~4 mon
  if (strategy == "halloween") {
    df[, (col_i)     := lapply(.SD, function(x) ifelse( month(ym) %in% c(1:4, 11:12), x, 0)), .SDcol = col_i    ]
    df[, (col_i_div) := lapply(.SD, function(x) ifelse( month(ym) %in% c(1:4, 11:12), x, 0)), .SDcol = col_i_div]
    df[, (col_i_adj) := lapply(.SD, function(x) ifelse( month(ym) %in% c(1:4, 11:12), x, 0)), .SDcol = col_i_adj]
  } else if (strategy == "anti-halloween") {
    df[, (col_i)     := lapply(.SD, function(x) ifelse(!month(ym) %in% c(1:4, 11:12), x, 0)), .SDcol = col_i    ]
    df[, (col_i_div) := lapply(.SD, function(x) ifelse(!month(ym) %in% c(1:4, 11:12), x, 0)), .SDcol = col_i_div]
    df[, (col_i_adj) := lapply(.SD, function(x) ifelse(!month(ym) %in% c(1:4, 11:12), x, 0)), .SDcol = col_i_adj]
  }
  
  # growth rate
  gr_bal     <- as.matrix(1 + df[, ..col_i])     # 1 + i
  gr_bal_div <- as.matrix(1 + df[, ..col_i_adj]) # 1 + i_adj
  # gr_bal_div <- as.matrix(1 + df[, ..col_i] + df[, ..col_i_div])                     # 1 + i + div rate
  # gr_bal_div <- as.matrix(1 + df[, ..col_i] + (df[, ..col_div] / df[, ..col_price])) # 1 + i + div rate
  
  # weight
  
  if (is.vector(wt)) wtm <- matrix(rep(wt, n), ncol = length(wt), byrow = T)
  if (is.matrix(wt)) {
    # point base
    if (missing(to)) {
      wt <- wt[from:nrow(wt),]
    } else {
      wt <- wt[from:to,]
    }
    # term base
    if (!missing(term)) {
      if (term < nrow(wt)) wt <- wt[(nrow(wt)-term+1):nrow(wt),]
    }
    wtm <- copy(wt)
  }
  
  # fv_bal
  fv_bal_mat <- backtest(gr = gr_bal, wt = wtm, init = init_usd, cont = cont_usd, rebal)
  fv_bal <- data.table(fv_bal_mat)
  setnames(fv_bal, paste0("fv_bal", zpad2(1:m)))
  
  # bal (from fv_bal)
  bal <- data.table(apply(fv_bal, 2, function(x) shift(x, type = "lag")))
  for (s in 1:m) {
    bal[1L, s] <- init_usd[1L] * wtm[1L, s]
  }
  setnames(bal, paste0("bal", zpad2(1:m)))
  
  # fv_bal_div
  fv_bal_div_mat <- backtest(gr = gr_bal_div, wt = wtm, init = init_usd, cont = cont_usd, rebal)
  fv_bal_div <- data.table(fv_bal_div_mat)
  setnames(fv_bal_div, paste0("fv_bal_div", zpad2(1:m)))
  
  # bal_div (from fv_bal_div)
  bal_div <- data.table(apply(fv_bal_div, 2, function(x) shift(x, type = "lag")))
  for (s in 1:m) {
    bal_div[1L, s] <- init_usd[1L] * wtm[1L, s]
  }
  setnames(bal_div, paste0("bal_div", zpad2(1:m)))
  
  # merge balance and future value
  df <- cbind(df, bal, bal_div, fv_bal, fv_bal_div)
  
  # columns
  col_cd         <- regmatch_cols(df, '^stockcode[0-9][0-9]' )
  col_nm         <- regmatch_cols(df, '^stockname[0-9][0-9]' )
  col_bal        <- regmatch_cols(df, '^bal[0-9][0-9]'       )
  col_bal_div    <- regmatch_cols(df, '^bal_div[0-9][0-9]'   )
  col_fv_bal     <- regmatch_cols(df, '^fv_bal[0-9][0-9]'    )
  col_fv_bal_div <- regmatch_cols(df, '^fv_bal_div[0-9][0-9]')
  col_mmt        <- regmatch_cols(df, '^mmt[0-9][0-9]'       )
  
  df[, bal        := rowSums(.SD), .SDcols = col_bal       ]
  df[, bal_div    := rowSums(.SD), .SDcols = col_bal_div   ]
  df[, fv_bal     := rowSums(.SD), .SDcols = col_fv_bal    ]
  df[, fv_bal_div := rowSums(.SD), .SDcols = col_fv_bal_div]
  df[, fv         := rowSums(.SD), .SDcols = col_fv_bal_div]
  
  # future value
  fv_bal <- copy(df$fv_bal)
  fv     <- copy(df$fv)
  profit <- fv - csum_tot_usd
  set(df, j = "profit", value = profit)
  
  # future value sum total
  fv_bal_tot <- fv_bal[length(fv_bal)]
  fv_tot     <- fv[length(fv)]
  
  # tax
  tax_bal <- ifelse(fv_bal_tot > sum_tot_usd, (fv_bal_tot - sum_tot_usd) * taxr, 0)
  fv_bal_tot_tax <- fv_bal_tot - tax_bal
  tax <- ifelse(fv_tot > sum_tot_usd, (fv_tot - sum_tot_usd) * taxr, 0)
  fv_tot_tax <- fv_tot - tax 
  
  # vs
  vs_bal     <- fv_bal_tot      / sum_tot_usd
  vs_bal_tax <- fv_bal_tot_tax  / sum_tot_usd
  vs         <- fv_tot          / sum_tot_usd
  set(df, j = "vs", value = vs)
  vs_tax     <- fv_tot_tax      / sum_tot_usd
  vs_krw     <- fv_tot     * fx / sum_tot_krw
  vs_tax_krw <- fv_tot_tax * fx / sum_tot_krw
  
  # cagr
  cagr_bal     <- vs_bal^(1/(n/12))-1
  cagr_bal_tax <- vs_bal_tax^(1/(n/12))-1
  cagr         <- vs^(1/(n/12))-1
  cagr_tax     <- vs_tax^(1/(n/12))-1
  
  # return
  ret_bal    <- calc_capital_gains_yield(fv_bal)
  ret_bal_sd <- sd(ret_bal) * sqrt(12)
  ret        <- calc_capital_gains_yield(fv)
  ret_sd     <- sd(ret) * sqrt(12)
  ret_yld    <- (fv - cont_usd) / df$bal_div - 1 # yield before contribution
  df[, ret_bal := ret_bal]
  df[, ret     := ret    ]
  df[, ret_yld := ret_yld]
  
  # cair
  cair <- prod(1+df$ret_yld)^(1/(n/12))-1
  
  # yield per year
  yld <- unlist(sapply(split(df$ret_yld, year(df$ym)), function(x) round((prod(1+x) - 1) * 100, 2)))
  max_yld <- max(yld)
  max_yld_ym <- names(which(yld == max_yld))
  min_yld <- min(yld)
  min_yld_ym <- names(which(yld == min_yld))
  
  # mdd
  dd_bal  <- cumprod(1+ret_bal) / cummax(cumprod(1+ret_bal))-1
  mdd_bal <- min(dd_bal)
  mdd_bal_ym <- substr(df$ym[which(dd_bal == mdd_bal)], 1, 7)
  dd  <- cumprod(1+ret) / cummax(cumprod(1+ret))-1
  mdd <- min(dd)
  df[, dd  := dd] # add columns
  if (mdd < 0) {
    mdd_ym  <- df[which(dd == mdd)]$ym[1L]
    mdd_rcv <- which(df[ym > mdd_ym]$dd == 0)[1L] # months turned to 0 from maximum drawdown
  } else {
    mdd_ym  <- NA
    mdd_rcv <- 0
  }
  
  # recover
  rcv <- diff(df[dd == 0]$n)
  max_rcv <- ifelse(length(rcv) > 1, max(rcv[-1]), 1)
  if (max_rcv > 1) {
    rcv_ym <- sapply(which(rcv == max_rcv), function(x) sum(rcv[1:x])) + 1
    max_rcv_ym <- df[n %in% rcv_ym]$ym
  } else {
    rcv_ym <- 0
    max_rcv_ym <- NA
  }
  
  # sharpe ratio
  sharpe <- (cagr - rfr) / ret_sd
  
  # risk parity asset allocation
  rpar <- riskParityPortfolio(cov(df[, ..col_i]))$w
  names(rpar) <- unlist(df[, ..col_cd][1L])
  
  # win / lose
  win  <- length(ret_yld[ret_yld      >  eps])
  tie  <- length(ret_yld[abs(ret_yld) <  eps])
  lose <- length(ret_yld[ret_yld      < -eps])
  
  # attributes
  names(wt) <- names(rpar)
  attr(df, "wt") <- wt
  attr(df, "summary") <- data.table(sdate   = min(df$ym)           ,
                                    edate   = max(df$ym)           ,
                                    n       = n                    ,
                                    tp      = sum_tot_krw          ,
                                    fv      = fv_tot * fx * (1-fxc),
                                    vs      = vs_krw               ,
                                    cair    = cair                 ,
                                    cagr    = cagr                 ,
                                    sd      = ret_sd               ,
                                    sharpe  = sharpe               ,
                                    best    = max_yld              ,
                                    worst   = min_yld              ,
                                    mdd     = mdd                  ,
                                    win_pct = win / (n-1))
  attr(df, "rpar") <- rpar
  attr(df, "yld")  <- yld
  
  # txt
  txt <- paste0("\f",
               sttline(),
               "SJ Portfolio\n",
               sttline(),
               "assets: ", paste0(paste0(str_pad(1:m, width = 2, pad = '0')                                      , ". ("    , 
                                         if (is.vector(wt)) str_pad(sprintf("%.1f", round(wt*100, 1)), width = 5, pad = " ") else "?"    , " %) "   , 
                                         format(ns, digit = 3)                                                 , ' yrs | ', 
                                         str_pad(unlist(df[1, ..col_cd]), width = 9, side = "right", pad = " "), ": "     , 
                                         unlist(df[1, ..col_nm])), collapse = "\n        ")                            , "\n"     ,
               divline(),
               "term      : ", paste(gsub("-", ".", substr(range(df$ym), 1, 7)), collapse = " ~ ") , "\n",
               "n         : ", pad14(round(n/12, 1))          , " yrs\n",
               "m         : ", pad14(n)                       , " mon\n",
               "init      : ", pad14(comm1(init))             , if(!usd) " KRW\n" else " $\n",
               "cont      : ", pad14(comm1(cont))             , if(!usd) " KRW\n" else " $\n",
               "TP        : ", pad14(comm1(sum_tot_krw))      , " KRW\n",
               "FV        : ", pad14(comm1(fv_tot*fx*(1-fxc))), " KRW (after-tax: ", pad14(comm1(fv_tot_tax*fx*(1-fxc))), " KRW)\n",
               "Profit    : ", pad14(comm1(fv_tot*fx*(1-fxc)-sum_tot_krw)), " KRW (after-tax: ", pad14(comm1(fv_tot_tax*fx*(1-fxc)-sum_tot_krw)), " KRW)\n",
               "vs        : ", pad14(sprintf("%.2f", round(vs_krw, 2)))  , " T   (after-tax: ", pad14(sprintf("%.2f", round(vs_tax_krw, 2))), " T)\n",
               "TP($)     : ", pad14(comm1(sum_tot_usd)), " $\n",
               "FV($)     : ", pad14(comm1(fv_tot))     , " $\n",
               "Profit($) : ", pad14(comm1(fv_tot-sum_tot_usd)), " $\n",
               "vs($)     : ", pad14(sprintf("%.2f", round(vs, 2))), " T   (after-tax: ", pad14(sprintf("%.2f", round(vs_tax, 2))), " T)\n",
               "cair      : ", pad14(sprintf("%.2f", round(cair  * 100, 2))), " %\n",
               "cagr      : ", pad14(sprintf("%.2f", round(cagr  * 100, 2))), " %   (after-tax: ", pad14(sprintf("%.2f", round(cagr_tax * 100, 2))) , " %)\n",
               "sd        : ", pad14(sprintf("%.2f", round(ret_sd * 100, 2))), " %\n",
               "best      : ", pad14(sprintf("%.2f", max_yld)), " %   (", max_yld_ym, ")\n",
               "worst     : ", pad14(sprintf("%.2f", min_yld)), " %   (", min_yld_ym, ")\n",
               "mdd       : ", pad14(sprintf("%.2f", round(mdd * 100, 2))), " %   (", gsub('-', '.', substr(mdd_ym, 1, 7)), ", rcv ", mdd_rcv, " mon)\n",
               "rcv       : ", pad14(max_rcv), " mon (", paste0(gsub('-', '.', substr(max_rcv_ym, 1, 7)), collapse=', '), ")\n",
               "sharpe    : ", pad14(sprintf("%.2f", round(sharpe, 2))), "\n",
               "WL        : ", sprintf("%s Win %s Tie %s Lose (winning percent: %.2f %%, odds ratio: %.2f)\n", win, tie, lose, (win/(n-1) * 100), win/lose),
               divline()
  )
  cat(txt)
  print(yld)
  cat(endline())
  
  if (chart) {
    # return chart
    g1 <- ggplot(df, aes(x = ym, y = fv)) + 
      geom_bar(stat = 'identity') +
      geom_line(aes(x = ym, y = csum_tot_usd), col = 'red') +
      scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 year", date_labels = "'%y") +
      scale_y_continuous(label = comma)
    
    # mdd chart
    g2 <- ggplot(df, aes(x = ym, y = dd)) + 
      geom_line() + 
      scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 year", date_labels = "'%y") +
      geom_hline(yintercept = c(-.15, -.10, -.05), col = colors) +
      scale_y_continuous(labels = percent)
    
    # all chart
    g <- do.call('rbind', lapply(list(g1, g2), ggplotGrob))
    grid.arrange(g, top = paste0("SJ Portfolio \n(",
                                 "TERM: "  , sprintf("%.2f", round(n/12      , 1)), " YRS, ",
                                 "VS: "    , sprintf("%.2f", round(vs        , 2)), ", "    ,
                                 "CAIR: "  , sprintf("%.2f", round(cair  *100, 2)), " %, "  ,
                                 "CAGR: "  , sprintf("%.2f", round(cagr  *100, 2)), " %, "  ,
                                 "SD: "    , sprintf("%.2f", round(ret_sd*100, 2)), " %, "  ,
                                 "MDD:  "  , sprintf("%.2f", round(mdd   *100, 2)), " %, "  ,
                                 "RCV: "   , sprintf("%.2f", max_rcv)             , " M, "  ,
                                 "SR: "    , sprintf("%.2f", round(sharpe  , 2))  , ")\n"
    ))
  }
  return(df)
}

# fvcd --------------------------------------------------------------------

fvcd <- function(..., term = 6 * 12, wt, init = 1e8, cont = 4e6, rebal = 12, usd = F, chart = F, strategy = "buy-and-hold") {
  # epsilon
  eps <- 1e-8
  
  # length of list
  m <- length(list(...))
  
  # allocation(weights) variable
  if (missing(wt)) wt <- rep(1./m, m)
  
  if (is.matrix(wt)) {
    if (ncol(wt) != m) stop("The length of weights is not the same with the number of stocks.")
    if (any(abs(rowSums(wt)-1) > eps)) stop("Sum of asset allocation is not 1.\n")
  }
  
  if (is.vector(wt)) {
    if (length(wt) != m) stop("The length of weights is not the same with the number of stocks.")
    if (abs(sum(wt)-1) > eps) stop("Sum of asset allocation is not 1.\n")
  }
  
  n <- min(unlist(sapply(list(...), nrow)))
  if (n < term) stop(paste0('Data has only ', n ,' rows.'))
  
  df_lst <- vector(mode = "list", length = n-term)
  for (x in 1:(n-term)) {
    df_lst[[x]] <- fvc(..., init = init, wt = wt, cont = cont, rebal = rebal, from = x, to = x+term-1, chart = chart, strategy = strategy)
  }
  for (x in 1:length(df_lst)) {
    df_lst[[x]]$sdate <- min(df_lst[[x]]$ym)
    df_lst[[x]]$edate <- max(df_lst[[x]]$ym)
  }
  
  df <- Reduce('rbind', df_lst)
  
  # attributes
  attr(df, "summary") <- rbindlist(lapply(df_lst, function(s) attr(s, "summary")))
  attr(df, "rpar")    <- rbindlist(lapply(df_lst, function(s) as.data.table(as.list(attr(s, "rpar")))))
  attr(df, "yld")     <- rbindlist(lapply(df_lst, function(s) as.data.table(as.list(attr(s, "yld")))), use.names = T, fill = T)
  
  cat("\r")
  cat("Sample      : ", pad14(n-term), "s\n")
  cat("FV mean     : ", pad14(comm1(mean(attr(df, "summary")$fv)))            , "KRW\n")
  cat("FV sd       : ", pad14(comm1(  sd(attr(df, "summary")$fv)))            , "KRW\n")
  cat("Profit mean : ", pad14(comm1(mean(attr(df, "summary")[, mean(fv-tp)]))), "KRW\n")
  cat("Vs mean     : ", pad14(round(mean(attr(df, "summary")$vs), 2)), "T\n")
  cat("Best  mdd   : ", pad14(round(max( attr(df, "summary")$mdd) * 100, 2)), "%\n")
  cat("Worst mdd   : ", pad14(round(min( attr(df, "summary")$mdd) * 100, 2)), "%\n")
  cat("Prob < 1    : ", pad14(round(mean(ifelse(attr(df, "summary")$vs < 1, 1, 0)) * 100, 2)), "%\n")
  
  # mean return
  g1 <- ggplot(df[n == term], aes(x = ym, y = vs)) +
    geom_line() +
    scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 year", date_labels = "'%y") +
    geom_hline(yintercept = c(1, min(df[n == term]$vs), mean(df[n == term]$vs)), 
               col = colors, lty = c("dashed", "longdash", "solid")) +
    scale_y_continuous(label = comma)
  
  # mdd
  df_mdd <- df[, .(mdd = min(dd)), .(edate)]
  g2 <- ggplot(df_mdd, aes(x = edate, y = mdd)) + 
    geom_line() +
    scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 year", date_labels = "'%y") +
    geom_hline(yintercept = c(-.15, -.10, -.05), col = colors) +
    scale_y_continuous(labels = percent) +
    xlab('ym')
  g <- do.call('rbind', lapply(list(g1, g2), ggplotGrob))
  grid.arrange(g, top = paste(round(term/12, 1), " yrs window test. (min vs:", 
                              round(min(df[n == term]$vs), 2), ')'))
  return(df)
}

# cr ----------------------------------------------------------------------

cr <- function(df, name = FALSE) {
  col_cd <- regmatch_cols(df, "^stockcode")
  if (name) col_cd <- regmatch_cols(df, "stockname")
  col_i <- regmatch_cols(df, "^i[0-9]+")
  df_i  <- df[, ..col_i]
  setnames(df_i, unlist(df[1, ..col_cd]))
  # pca
  g <- autoplot(prcomp(df_i), loadings.label = TRUE)
  cat("term:", round(attr(df, "summary")$n / 12, 1), "yrs\n")
  cat(get_code_name(df), "\n")
  print(g)
  # risk parity
  rpar <- riskParityPortfolio(cov(df_i))$w
  # correlation
  corr <- round(cor(df_i), 3)
  corr[lower.tri(corr)] <- .0
  diag(corr) <- .0
  corr <- corr[-nrow(corr), -1, drop = FALSE]
  return(corr)
} 


# salloc ------------------------------------------------------------------

salloc <- function(df, budget = 8500, from = Sys.Date()-5L) {
  
  col_cd <- regmatch_cols(df, "stockcode")
  col_nm <- regmatch_cols(df, "stockname")
  
  stockcode <- unlist(df[nrow(df), ..col_cd])
  stockname <- unlist(df[nrow(df), ..col_nm])
  
  df_yh <- do.call("rbind", lapply(seq_along(stockcode), function(x) get_data(stockcode[x], from = from)))
  df_mx <- df_yh[, .(tdate = max(tdate)), .(stockcode, stockname)]
  df_pr <- df_yh[df_mx, on = .(stockcode, stockname, tdate)]
  
  stockcode <- df_pr$stockcode
  stockname <- df_pr$stockname
  pr <- df_pr$closep
  
  wt  <- attr(df, "wt")
  wta <- wt / pr # weight adjusted for 1 stock price price
  qtt <- round(wta / min(wta) * 100, 0)
  amt <- sum(pr * qtt)
  siz <- budget / amt
  qtt <- round(qtt * siz)
  
  z <- data.table(stockcode, stockname, quantity = qtt, closep = pr, amount = qtt * pr, ratio = round(prop.table(pr * qtt), 3), wt)
  z[, diff := ratio - wt]
  z[, isok := ifelse(abs(diff) < 0.01, T, F)]
  
  tot <- sum(z[, .(quantity * closep)])
  cat(sttline())
  cat("total amount:", comma(ceiling(tot), accuracy = 1), "$\n")
  cat(sttline())
  
  return(z)
}
