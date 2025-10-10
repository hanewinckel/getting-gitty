
#' Annual Interest Rate
#'
#' @param i_nominal numeric nominal interest rate
#' @param n integer compounding periods
#'
#' @description 
#' Returns annual rate from n-thly nominal
#'
#' @returns a numeric annualized rate
#' @export
#'
#' @examples get_apr(.05, 4)
get_apr <- function(i_nominal, n = 12){
  checkmate::assert_numeric(i_nominal, any.missing = FALSE, lower = 0)
  checkmate::assert_numeric(n, any.missing = FALSE)
  if (is.infinite(n)) {return(exp(i_nominal) - 1)}
  if (!is.infinite(n)){
    periodic_rate <- i_nominal / n
    apr <- (1 + periodic_rate) ^ n - 1
    return(apr)
  }
}

#' Annual Discount Rate
#'
#' @param d_nominal numeric nominal discount rate
#' @param n integer compounding periods
#'
#' @description 
#' Returns annual discount rate from n-thly nominal
#'
#' @returns a numeric annualized rate
#' @export
#'
#' @examples get_adr(.05, Inf)
get_adr <- function(d_nominal, n = 12){
  checkmate::assert_numeric(d_nominal, any.missing = FALSE, lower = 0)
  checkmate::assert_numeric(n, any.missing = FALSE)
  if (is.infinite(n)) {return(1 - exp(-d_nominal))}
  if (!is.infinite(n)){
    periodic_rate <- d_nominal / n
    adr <- 1 - (1 - periodic_rate) ^ n
    return(adr)
  }
}