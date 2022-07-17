create_calendar <- function() {
  tdate <- seq.Date(as.Date("1970-01-01"), as.Date("2070-12-31"), by = "day")
  z <- data.table(tdate = tdate, year = year(tdate), mon = month(tdate), day = mday(tdate), 
                  weekday = wday(tdate), holiday = ifelse(wday(tdate) %in% c(1, 7), 1, 0))
  z[holiday == 0, bizday_asc := rank( as.integer(tdate)), .(year, mon)]
  z[holiday == 0, bizday_dsc := rank(-as.integer(tdate)), .(year, mon)]
  z[is.na(z)] <- 0
  return(z)
}

calc_i_m <- function(i, m) (1+i)^(1/m)-1

calc_dividend_yield <- function(price, div) {
  yield <- div / price
  return(yield)
}

calc_capital_gains_yield <- function(price) {
  yield <- shift(price, type = "lead") / price - 1
  yield[is.na(yield)] <- 0
  return(yield)
}

calc_total_yield <- function(price, div) {
  yield <- calc_capital_gains_yield(price) + calc_dividend_yield(price, div)
  return(yield)
}

calc_growth_rate <- function(x) {
  gr <- cumprod(1+calc_yield(x))
  return(gr)
}

s_n <- function(i = 0.08, n = 10, m = 12, init = 1e8, cont = 4e6, age = 39, taxr = .154, unit = "KRW") {
  v <- 1/(1+i)
  d <- i/(1+i) # d = iv
  i_m <- m * ((1+i)^(1/m)-1)
  d_m <- m * (1-(1-d)^(1/m))
  tp  <- init + m*n*cont
  fv  <- ifelse(i != 0, init * (1+i)^n + m * cont * ( (1+i)^n - 1 ) / d_m, tp)
  tax <- (fv-tp) * taxr
  vs  <- fv / tp
  fvt <- fv - tax
  vs_tax <- fvt / tp
  cagr <- vs^(1/n)-1
  cagr_tax <- vs_tax^(1/n)-1
  width <- nchar(comma(round(fv , 0), accuracy=1))
  txt <- paste0("\r"      ,
                "n     : ", pad14(n), " yrs\n",
                "m     : ", pad14(m), " per 1yr\n",
                "i     : ", pad14(sprintf("%.2f", round(i*100  , 2))), " %\n",
                "init  : ", pad14(comma(round(init , 0), accuracy = 1)), " ", unit, "\n",
                "cont  : ", pad14(comma(round(cont , 0), accuracy = 1)), " ", unit, "\n",
                "TP    : ", pad14(comma(round(tp   , 0), accuracy = 1)), " ", unit, "\n",
                "FV    : ", pad14(comma(round(fv   , 0), accuracy = 1)), " ", unit, "\n",
                "Profit: ", pad14(comma(round(fv-tp, 0), accuracy = 1)), " ", unit, "\n",
                "tax   : ", pad14(comma(round(tax  , 0), accuracy = 1)), " ", unit, "\n",
                "FVt   : ", pad14(comma(round(fvt  , 0), accuracy = 1)), " ", unit, "\n",
                "vs    : ", pad14(round(vs , 2)), " T (a.tax: ", round(vs_tax, 2), " T)\n",
                "cagr  : ", pad14(sprintf("%.2f", round(cagr * 100, 2))), " % (a.tax: ", round(cagr_tax * 100, 2), " %)\n\n"
  )
  cat(txt)
  z <- data.table(age = age+n-1, n, i, v, d, i_m, d_m, tp, fv, tax, fvt, vs)
  return(z)
} 

add_mon <- function (date, mon) {
  date <- as.POSIXlt(date)
  date$mon <- date$mon + mon
  as.Date(date)
}

change_per_year <- function(wt) {
  v <- ifelse(c(0, diff(apply(wt, 1, function(x) which.max(x)))) == 0, 0, 1)
  z <- round(sum(v) / length(v) * 12)
  return(z)
}
 

