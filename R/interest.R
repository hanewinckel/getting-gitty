
#' Title
#'
#' @param i_nominal_monthly - numeric annual nominal rate as a decimal
#' 
#' @description 
#' Returns annual rate from monthly nominal
#'
#' @returns a numeric annualized rate
#' @export
#'
#' @examples
get_apr <- function(i_nominal_monthly){
  checkmate::assert_numeric(i_nominal_monthly, any.missing = FALSE, lower = 0)
  periodic_rate <- i_nominal_monthly / 12
  apr <- (1 + periodic_rate) ^ 12 - 1
  return(apr)
}