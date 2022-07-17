
sttline <- function(x) {                                                                                                        
  if (missing(x))                                                                                                               
    x <- options()$width                                                                                                        
  paste0(paste0(rep("=", times = min(x, options()$width)), collapse = ""), "\n")                                                
}                                                                                                                               
divline <- function(x) {                                                                                                        
  if (missing(x))                                                                                                               
    x <- options()$width                                                                                                        
  paste0(paste0(rep("-", times = min(x, options()$width)), collapse = ""), "\n")                                                
}                                                                                                                               
endline <- function(x) {                                                                                                        
  if (missing(x))                                                                                                               
    x <- options()$width                                                                                                        
  paste0(paste0(rep("=", times = min(x, options()$width)), collapse = ""), "\n\n")                                              
}
colors = RColorBrewer::brewer.pal(3, "Set1")

zpad2 <- function(x) str_pad(x, width =  2, pad = "0")
pad14 <- function(x) str_pad(x, width = 14, pad = " ") 
comm1 <- function(x) comma(round(x), accuracy = 1)

ymdot <- function(x) gsub("-", ".", substr(x, 1, 7))
fcagr <- function(x, n) x^(1/(n/12))-1
