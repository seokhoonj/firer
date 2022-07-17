
# VAA ---------------------------------------------------------------------

# VAA (Vigilant Asset Allocation) is a dual-momentum based investment strategy

# (12 * (p0 / p1 – 1)) + (4 * (p0 / p3 – 1)) + (2 * (p0 / p6 – 1)) + (p0 / p12 – 1)
vaa_momentum_score <- function(df, past = c(1, 3, 6, 12), wt = c(12, 4, 2, 1), unit = 1, forward = F) {
  v <- df$adj
  z <- sapply(seq_along(v), function(x) sum( (v[x] / v[ifelse(x-past > 0, x-past, 1)] - 1) * wt ) * unit)
  if (forward) {
    z <- shift(z, type = "lead")
    z[is.na(z)] <- 0
  }
  return(z)
}

vaa_offense_weight <- function(...) {
  z <- merge_data(...)
  cols <- regmatch_cols(z, "^vaa_mmt")
  z <- z[, ..cols]
  neg <- which(apply(z, 1L, min) < 0)
  wt <- ifelse(z / apply(z, 1L, max) == 1, 1, 0)
  wt[neg,] <- 0
  return(wt) 
}

vaa_defense_weight <- function(...) {
  z <- merge_data(...)
  cols <- regmatch_cols(z, "^vaa_mmt")
  z <- z[, ..cols]
  wt <- ifelse(z / apply(z, 1L, max) == 1, 1, 0)
  return(wt) 
}


# average momentum score --------------------------------------------------

average_momentum_score <- function(df, past = 12, forward = F) { # one of the absolute momentum
  v <- df$adj
  z <- sapply(seq_along(v), function(x) mean(v[x] >= v[x-min(past, x-1):1])) # ym is shifted in yh function
  if (forward) {
    z <- shift(z, type = "lead")
    z[is.na(z)] <- 0
  }
  return(z)
}


# basic momentum ----------------------------------------------------------

absolute_momentum <- function(df, past = 12, binary = F, forward = F) {
  v <- df$adj
  z <- sapply(seq_along(v), function(x) v[x] / v[ifelse(x-past > 0, x-past, 1)] - 1)
  if (binary) {
    z <- ifelse(z > 0, 1, 0)
  }
  if (forward) {
    z <- shift(z, type = "lead")
    z[is.na(z)] <- 0
  }
  return(z)
}

relative_momentum <- function(..., mmt = "abs_mmt", cash = F) {
  df <- merge_data(...)
  cols <- regmatch_cols(df, sprintf("^%s", mmt))
  z <- df[, ..cols]
  z <- z + 1 # to avoid NaN
  if (cash) {
    z[z <= 1] <- 0 # negative yield
  }
  wt <- ifelse(apply(z, 1, sum) > 0 & z / apply(z, 1, max) == 1, 1, 0)
  return(wt)
}

relative_momentum_weight <- function(..., mmt = c("abs_mmt", "avg_mmt")) {
  df <- merge_data(...)
  if (mmt[1L] == "abs_mmt") {
    cols <- regmatch_cols(df, sprintf("^%s", mmt[1L]))
    z <- df[, ..cols]
    wt <- ifelse((apply(z, 1L, max) > 0 & z / apply(z, 1L, max) == 1), 1, 0)
  } 
  else if (mmt[1L] == "avg_mmt") {
    cols <- regmatch_cols(df, sprintf("^%s", mmt[1L]))
    z <- df[, ..cols]
    m <- ifelse(z / apply(z, 1L, max) == 1 | apply(z, 1L, sum) == 0, 1, 0) # if all assets are 0, allocate equal amount
    wt <- m / rowSums(m)
  }
  return(wt)
} 

