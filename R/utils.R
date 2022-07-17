zeros <- function(x) array(0, dim(x))
ones  <- function(x) array(1, dim(x))
minmax_scaler <- function(x) return((x - min(x)) / (max(x) - min(x)))

match_cols <- function (df, cols) {
  colnames(df)[match(cols, colnames(df), 0L)]
}

regmatch_cols <- function(df, pattern) {
  colnames(df)[grepl(pattern, names(df))]
}

setcolafter <- function (df, col, after = NA) {
  col <- match_cols(df, vapply(substitute(col), deparse, "character"))
  aft <- deparse(substitute(after))
  columns <- names(df)
  col_ord <- sapply(col, function(x) which(columns == x), USE.NAMES = F)
  rem_ord <- which(!columns %in% col)
  if (missing(aft)) {
    new_ord <- c(col_ord, rem_ord)
  }
  else {
    aft_ord <- which(columns == aft)
    new_ord <- c(rem_ord[rem_ord <= aft_ord], col_ord, rem_ord[rem_ord > aft_ord])
  }
  new_columns <- columns[new_ord]
  setcolorder(df, neworder = new_columns)
}

fvcd_attr <- function(x, col) {
  z <- vector(mode = "list", )
  for (i in seq_alon(col)) {
    z[[i]] <- rbindlist(lapply(x, function(s) as.data.table(as.list(s[[col]]))), use.names = T, fill = T)
  }
  if (any(grepl("^V[0-9]+", names(z)))) names(z) <- col
  return(z)
}
