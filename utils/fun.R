
#' Get historic stock market data from Yahoo Finance
#'
#' @param tkr vector of character strings
#' @description Call quantmod function to save the returned xts to the global environment.
#' @return None
#' @export
getYF = function(tkr) { for(s in tkr) getSymbols(paste("^", s, sep=""), env=globalenv(), src="yahoo", from="1800-01-01") }

#' Rolling window function
#'
#' @param x numeric vector
#' @description Calculate the return in a window
#' @return numeric value of return
#' @export
look_inside = function(x) { tail(x,1)/x[1]-1 }

#' Calculate the percent rise needed to recover a given percent of decline
#'
#' @param d numeric value
#' @description For a given % of decline, how much does it have to rise to reach breakeven
#'    For example, if a stock is trading at 100, then it drops 10%, it will require 11.1% 
#'    rise to reach 100 again.
#' @return numeric value
#' @export
recov = function(d) {-d/(d+1)}

#' Find V shape pattern in data (TO DO)
#' 
#' @param x numeric time series
#' @param ep tolerance
#' @param height size of drawdown
#' @description
#' @return
#' @export
find_V_shape = function(x, ep=0.01, height=0.2) {
  n = length(x)
  x1 = x[1]
  xn = tail(x,1)
  if ( abs(xn/x1 - 1) < ep ) { # first and last are close
    xmin = min(x, na.rm=TRUE)
    xmax = max(x, na.rm=TRUE)
    loc.min = which.min(x)
    loc.max = which.max(x)
    if ( abs(x1/xmax-1) < ep & (xmax/xmin - 1) > height ) {
      # TO DO
    }
  } else {
    geometry = NA
  }
  return(geometry)
}







