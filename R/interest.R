
#' Title
#'
#' @param i_nominal numeric nominal interest rate
#' @param n integer compounding periods
#'
#' @description 
#' Returns annual rate from monthly nominal
#'
#' @returns a numeric annualized rate
#' @export
#'
#' @examples
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