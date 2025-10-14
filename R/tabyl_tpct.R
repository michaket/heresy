#' A Table Function
#'
#' This function creates a table with percentage formatting and totals using 
#' functions from the janitor package
#' @param x Your vector or data frame column
#' @keywords table
#' @export
#' @examples
#' tabyl_tpct()

tabyl_tpct <- function(x){
  janitor::tabyl(x) %>%
    janitor::adorn_pct_formatting() %>%
    janitor::adorn_totals()
}