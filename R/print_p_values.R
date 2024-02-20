#' Symbolize p values
#'
#' Simple function to convert p values to asterisks for printing.
#'
#'
#' @param x vector of p values.
#' @param sig_levels optional. Cutoff points for p values.
#' @param symbol_levels optional. Different symbols to represent p values
#' @export
#' @examples
#'  data.frame(p_values=c(0.05,0.023,0.0009,0.0001,0.003)) %>%
#' dplyr::mutate(p_print=print_p_values(p_values))

print_p_values<-function(x,sig_levels=c(0.001, 0.01, 0.049999),symbol_levels=c("***", "**", "*", " ")) {


    symnum(x, corr = FALSE, na = FALSE,
           cutpoints = c(0,sig_levels,1),
           symbols = symbol_levels)  %>%
  return()
}







