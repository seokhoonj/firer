get_past_krw <- function() {
  # ecos
  krw <- setDT(ecos::statSearch(.api_key, stat_code = "036Y001", item_code = "0000001"))
  krw <- krw[, .(tdate = time, currency = data_value)]
  krw[, ym  := as.Date(format(tdate, "%Y-%m-01"))]
  krw[, day := mday(tdate)]
  
  # ascending & descending day
  krw[, dayasc := rank( day), .(ym)]
  krw[, daydsc := rank(-day), .(ym)]
  krw <- krw[daydsc == 1, .(ym, currency)]
  krw_past <- krw[ym < as.Date("2004-01-01")]
  
  save(krw_past, file = "data/krw_past.rda")
  return(krw_past)
}

get_krw <- function() {
  krw <- set_data("KRW=X")[, .(ym, currency = adj)]
  krw <- rbind(firer::krw_past, krw)
  return(krw)
}

