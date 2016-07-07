# BSD_2_clause

#' Simple bootstrap for distances
#'
#' @param df A data.frame from assembling differences
#' @return A vector of bootstrap replicate means
#' @export
boot_dists <- function(df, n = 1000, B = 1000) {
  res <- c()
  for(i in 1:B) {
    tmp <- sample(df$val, 1000)
    res <- c(res, mean(tmp))
  }
  return(res)
}