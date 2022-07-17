
get_stock_symbols <- function() {
  stock_symbols <- setDT(stockSymbols())
  setnames(stock_symbols, tolower(gsub("\\.", "_", names(stock_symbols))))
  return(stock_symbols)
}

get_symbol_info <- function(symbol) {
  url <- sprintf("https://query1.finance.yahoo.com/v7/finance/options/%s", symbol)
  content <- fromJSON(getURLContent(url))
  list(
    symbol           = content$optionChain$result$underlyingSymbol,
    expiration_dates = content$optionChain$result$expirationDates[[1]],
    strikes          = content$optionChain$result$strikes[[1]],
    has_mini_options = content$optionChain$result$hasMiniOptions,
    quote            = content$optionChain$result$quote,
    calls            = content$optionChain$result$options[[1]]$calls[[1]],
    puts             = content$optionChain$result$options[[1]]$puts[[1]],
    error            = content$optionChain$error
  )
}

get_stockname <- function(symbol) {
  z <- get_symbol_info(symbol)$quote
  cols <- match_cols(z, c("shortName", "longName", "displayName"))
  nm <- unique(unname(unlist(z[, cols])))
  nm[order(nchar(nm), decreasing = T)][1L]
}

get_code <- function(df) {
  col_cd <- regmatch_cols(df, "stockcode")
  unname(unlist(df[1, ..col_cd]))
}

get_name <- function(df) {
  col_cd <- regmatch_cols(df, "stockname")
  unname(unlist(df[1, ..col_cd]))
}

get_code_name <- function(df, pad = TRUE) {
  col_cd <- regmatch_cols(df, "stockcode")
  col_nm <- regmatch_cols(df, "stockname")
  if (pad) {
    txt <- paste("(", 
                 str_pad(unlist(df[1, ..col_cd]), width = 5, side = "right", pad = " "), 
                 ")", 
                 unlist(df[1, ..col_nm]), collapse = "\n")
  } else {
    txt <- paste("(", 
                 unlist(df[1, ..col_cd]), 
                 ")", 
                 unlist(df[1, ..col_nm]), collapse = "\n")
  } 
  return(txt)
}


# get data ----------------------------------------------------------------

# *Close price adjusted for splits.**Adjusted close price adjusted for both dividends and splits.

get_data <- function(code, from = "1970-01-01") {
  code <- toupper(code)
  
  if (!grepl("\\^|=", code)) {
    name <- get_symbol_info(code)$quote$longName
  } else {
    name <- get_symbol_info(code)$quote$shortName
  }
  
  suppressWarnings({
    df     <- getSymbols(Symbols = code, from = from, auto.assign = F, src = "yahoo")
    df_div <- getDividends(Symbol = code, from = from)
    df_spl <- getSplits(Symbol = code, from = from)
  })
  
  if (length(index(df_div)) == 0 | class(index(df_div)) != 'Date') {
    df_div <- zoo(x = data.frame(div = NA), order.by = index(df))
  }
  
  if (length(index(df_spl)) == 0 | class(index(df_spl)) != 'Date') {
    df_spl <- zoo(x = data.frame(spl = NA), order.by = index(df))
  }
  
  df <- merge(df, df_div, df_spl)
  df <- as.data.table(df)
  set(df, j = c("stockcode", "stockname"), value = list(code, name))
  setnames(df,  c('tdate', 'openp', 'highp', 'lowp', 'closep', 'vol', 'adj', 'div', 'spl', 'stockcode', 'stockname'))
  setcolorder(df, c('stockcode', 'stockname', 'tdate', 'openp', 'highp', 'lowp', 'closep', 'vol', 'adj', 'div', 'spl'))
  set(df, j = "div", value = ifelse(is.na(df$div), 0, df$div))
  return(df[!is.na(closep)])
} 

set_formula <- function(lh, rh) {
  formula(sprintf("%s ~ %s", paste(lh, collapse = " + "), rh))
}

max_day <- function(df, day) {
  apply(df, 1, function(x) tryCatch(max(x[seq_len(day)], na.rm = T), 
                                    warning = function(w) max(x[seq_along(x)], na.rm = T)))
}

# daily_to_monthly --------------------------------------------------------

daily_to_monthly <- function(df) {
  # add variables
  set(df, j = "i_div", value = df$div / df$closep)
  set(df, j = "ym"   , value = as.Date(format(df$tdate, "%Y-%m-01")))
  set(df, j = "day"  , value = mday(df$tdate))
  
  # dividends
  if (!any(grepl("div", names(df)))) df[, div := .0]
  df_div <- df[, .(div = sum(div, na.rm = T), i_div = sum(i_div, na.rm = T)), .(ym)]
  
  # split
  if (!any(grepl("spl", names(df)))) df[, spl := .0]
  df_spl <- df[, .(spl = sum(spl, na.rm = T)), .(ym)]
  
  # ascending & descending day
  df[, dayasc := rank( day), .(ym)]
  df[, daydsc := rank(-day), .(ym)]
  df <- df[daydsc == 1, .(stockcode, stockname, ym, adj, price = closep)]
  
  # transform
  set(df, j = "n", value = seq_len(nrow(df)))
  
  # i adjusted
  set(df, j = "adj"  , value = ifelse(df$adj < 0, 0, df$adj)) # like nvo
  set(df, j = "i_adj", value = calc_capital_gains_yield(df$adj))
  set(df, j = "i_adj", value = ifelse(is.infinite(df$i_adj), 0, df$i_adj))
  
  # i
  set(df, j = "i", value = calc_capital_gains_yield(df$price))
  
  # lead ym
  set(df, j = "ym", value = shift(df$ym, type = "lead")) # join dividend, splits after leading ym
  
  # join dividend data
  df[df_div, on = .(ym), `:=`(div = i.div, i_div = i.i_div)]
  
  # join split data
  df[df_spl, on = .(ym), `:=`(spl = i.spl)]
  
  # remove data 
  df <- df[!is.na(ym)] # same as df = df[-nrow(df)]  # to align portfolio visualization standard
  
  # set column order
  setcolafter(df, col = div, after = price)
  setcolafter(df, col = n  , after = ym   )
  
  # absolute momentum
  set(df, j = "abs_mmt", value = absolute_momentum(df))
  
  # average momentum score
  set(df, j = "avg_mmt", value = average_momentum_score(df))
  
  # momentum score
  set(df, j = "vaa_mmt", value = vaa_momentum_score(df))
  
  return(df)
}


# set data ----------------------------------------------------------------

set_data <- function(code, er, from = "1970-01-01") {
  
  # stock name
  if (!grepl("\\^|=", code)) {
    name <- get_symbol_info(code)$quote$longName
  } else {
    name <- get_symbol_info(code)$quote$shortName
  }
  
  # monthly data
  df <- daily_to_monthly(get_data(code, from = from))
  
  # expense ratio
  if (!missing(er)) attr(df, "er") <- er
  
  # number of years
  cat(sprintf("( %s yrs ) %s : %s\n", 
      str_pad(sprintf("%.1f", round(nrow(df)/12, 1)), width = 4, pad = " "), 
      str_pad(toupper(code), width = 7, side = "right", pad = " "), 
      name))
  
  return(df)
}


# merge data --------------------------------------------------------------

merge_data <- function(...) {
  
  # portfolio
  df_lst <- copy(list(...))
  
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
  
  return(df)
}


# set cash ----------------------------------------------------------------

set_cash <- function() {
  cash <- set_data("^gspc")
  set(cash, j = "stockcode", value = "^CASH")
  set(cash, j = "stockname", value = "Cash")
  set(cash, j = "adj"      , value = 1)
  set(cash, j = "price"    , value = 1)
  set(cash, j = "div"      , value = 0)
  set(cash, j = "i_adj"    , value = 0)
  set(cash, j = "i"        , value = 0)
  set(cash, j = "i_div"    , value = 0)
  set(cash, j = "spl"      , value = 0)
  set(cash, j = "abs_mmt"  , value = 0)
  set(cash, j = "avg_mmt"  , value = 0)
  set(cash, j = "vaa_mmt"  , value = 0)
  return(cash)
}


# get columns -------------------------------------------------------------

get_i_adj <- function(df) {
  cols <- regmatch_cols(df, "^i_adj[0-9]")
  df[, ..cols]
}
get_i <- function(df) {
  cols <- regmatch_cols(df, "^i[0-9]+")
  df[, ..cols]
}
get_i_div <- function(df) {
  cols <- regmatch_cols(df, "^i_div[0-9]+")
  df[, ..cols]
}
get_fv_bal <- function(df) {
  cols <- regmatch_cols(df, "^fv_bal[0-9]+")
  df[, ..cols]
}
get_fv_bal_div <- function(df) {
  cols <- regmatch_cols(df, "^fv_bal_div[0-9]+")
  df[, ..cols]
}
