
# ggi ---------------------------------------------------------------------

ggi <- function(..., wt, rfr = 0.0065, cum = T, sdate, edate) {
  
  dots <- list(spy, tlt)
  dots <- list(...)
  m <- length(dots)
  if (missing(wt)) wt <- rep(1./m, m)
  if (sum(wt) != 1) stop('Sum of asset allocation is not 1.\n')
  
  if (missing(sdate)) 
    sdate <- max(sapply(dots, function(x) min(as.character(x$ym))))
  if (missing(edate)) 
    edate   <- min(sapply(dots, function(x) max(as.character(x$ym))))
  
  # df
  dots <- lapply(dots, function(x) x[ym >= sdate & ym <= edate])
  dots <- lapply(dots, function(x) x[, n := 1:nrow(x)])
  dots <- lapply(dots, function(x) x[, i := i_adj])
  # dots <- lapply(dots, function(x) x[, i := i+div/price])
  dots <- lapply(dots, function(x) x[, gr := cumprod(1+i)])
  dots <- lapply(dots, function(x) x[, ticker := get_code_name(x, pad = F)])
  df <- do.call('rbind', dots)
  set(df, j = "linetype", value = "2. Assets")
  set(df, j = "size"    , value = "2. Assets")
  df <- df[, .(stockcode, stockname, ym, n, i, gr, ticker, linetype, size)]
  final_n <- min(sapply(dots, nrow))
  # weighted growth rate
  i_tot <- sapply(dots, function(x) x$i)
  i_sum <- as.vector(i_tot %*% wt)
  gr_sum <- cumprod(1 + i_sum)
  df_sum <- data.table(
    stockcode = 'SJ',
    stockname = 'Portfolio',
    ym = dots[[1]]$ym, 
    n  = dots[[1]]$n,
    i  = i_sum,
    gr = gr_sum,
    ticker = '(SJ) Portfolio',
    linetype = '1. Portfolio',
    size = '1. Portfolio'
  )
  # bind all
  df <- rbind(df, df_sum)
  if (cum) {
    g <- ggplot(df, aes(x = ym, y = gr, group = ticker, col = ticker, linetype = linetype)) + 
      geom_line() + 
      geom_hline(yintercept = c(0.8, 0.9, 1.0), col = colors, lty = rep('dashed', 3)) +
      scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 year", date_labels = "'%y") +
      ggtitle(paste('Compare returns from ', ymdot(sdate), "~", ymdot(edate), '(', round(final_n/12, 1), 'yrs )')) +
      guides(color = guide_legend(order = 1)) +
      theme(legend.position = c(0.05, 0.95),
            legend.justification = c(0, 1),
            legend.background = element_blank())
  } else {
    g <- ggplot(df, aes(x = ym, y = i, group = ticker, col = ticker, linetype = linetype, size = size)) + 
      geom_line() + 
      geom_hline(yintercept = c(-.15, -.10, -.05), col = colors, lty = rep('dashed', 3)) +
      scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 year", date_labels = "'%y") +
      scale_size_manual(values = c(1, .5)) +
      ggtitle(paste('Compare returns from ', ymdot(sdate), "~", ymdot(edate), '(', round(final_n/12, 1), 'yrs )')) +
      guides(color = guide_legend(order = 1)) +
      theme(legend.position = c(0.05, 0.95),
            legend.justification = c(0, 1),
            legend.background = element_blank())
  }
  print(g)
  
  df_gr <- df[n == final_n][, cagr := gr^(1/(final_n/12))-1][, .(stockcode, stockname, cagr)]
  df_sd <- df[, .(volatility = sd(i) * sqrt(12)), .(stockcode, stockname)]
  
  z <- df_sd[df_gr, on = .(stockcode, stockname)][, cv := volatility / cagr]
  
  set(z, j = "cagr"      , value = round(z$cagr       * 100, 1))
  set(z, j = "volatility", value = round(z$volatility * 100, 1))
  set(z, j = "cv"        , value = round(z$cv              , 2))
  set(z, j = "sharpe"    , value = round((z$cagr - (rfr * 100)) / z$volatility, 2))
  set(z, j = "rpar"      , value = round(c(riskParityPortfolio(cov(i_tot))$w, 1) * 100, 1))
  
  # risk parity weighting
  return(z[order(abs(cv), -rpar)])
}