# BSD_2_clause

#' Calculate maximum possible edit distance between x, y
#'
#' The maximum edit distance is needed to calculate the distance (e.g.,
#' Levenshtein) ratio.
#' 
#' @param x A vector of strings
#' @param y A second vector of strings
#' @return A matrix of maximum edit distances for each combination of strings
#' in x, y
#' @export
#' @examples
#' \dontrun{
#' get_max_dist(x = sents1, y = sents2)
#' }
get_max_dist <- function(x, y) {
  res <- matrix(NA, nrow = length(x), ncol = length(y))
  for(i in 1:length(x)) {
    for(j in 1:length(y)) {
      res[i,j] <- nchar(x[i]) + nchar(y[j])
    }
  }
  return(res)
}

#' Get the minimum edit distance for a row in a matrix
#'
#' We often want to know which string from among a set of strings is the most-
#' similar. \link[stringdist]{stringdistmatrix} provides a matrix of distances
#' between one or two vectors of strings. This function returns the minimum 
#' distance for one row (x) of the matrix.
#' 
#' @param mat A matrix of edit distances
#' @param x The row number for which to find the minimum edit distance
#' @return The minimum edit distance between the string corresponding to row x
#' @export
#' @examples
#' \dontrun{
#' get_min_dist(mat=amat, x=2)
#' }
get_min_dist <- function(mat, x) {
  val <- mat[x, which(mat[x, ] == min(mat[x, ]))]
  return(val)
}

#' Get the minimum edit distance for rows in a matrix
#'
#' Applys \link{get_min_dist} over all rows of matrix \code{mat}
#' 
#' @param mat A matrix of edit distances
#' @return The minimum edit distance for each row of matrix mat
#' @export
#' @examples
#' \dontrun{
#' get_min_dist(mat=amat, x=2)
#' }
get_min_dists <- function(mat) {
  res <- lapply(1:length(mat[,1]), FUN = get_min_dist, mat = mat)
  return(res)
}

#' Get the maximum edit ratio for a row in a matrix
#'
#' We often want to know which string from among a set of strings is the most-
#' similar. \link[stringdist]{stringdistmatrix} provides a matrix of distances
#' between one or two vectors of strings. This function returns the minimum 
#' distance for one row (x) of the matrix.
#' 
#' @param mat A matrix of edit distances
#' @param x The row number for which to find the minimum edit distance
#' @return The minimum edit distance between the string corresponding to row x
#' @export
#' @examples
#' \dontrun{
#' get_min_dist(mat=amat, x=2)
#' }
get_min_dist <- function(mat, x) {
  val <- mat[x, which(mat[x, ] == min(mat[x, ]))]
  return(val)
}

#' Get the minimum edit distance for rows in a matrix
#'
#' Applys \link{get_min_dist} over all rows of matrix \code{mat}
#' 
#' @param mat A matrix of edit distances
#' @return The minimum edit distance for each row of matrix mat
#' @export
#' @examples
#' \dontrun{
#' get_min_dist(mat=amat, x=2)
#' }
get_min_dists <- function(mat) {
  res <- lapply(1:length(mat[,1]), FUN = get_min_dists, mat = mat)
  return(res)
}

rox-#' Summarize a vector of edit distances
#'
#' More detailed description of the function
#' @param x A vector of edit distances
#' @return A list with N, mean, median, and sd.
#' @seealso if any see alsos
#' @export
#' @examples
#' \dontrun{
#' summary_dists(min_dists)
#' }
summary_dists <- function(x) {
  x <- unlist(x)
  n <- length(x)
  mean <- mean(x, na.rm = TRUE)
  med <- median(x, na.rm = TRUE)
  sd <- sd(x, na.rm = TRUE)
  return(list(n=n, mean=mean, med=med, sd=sd))
}
