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
  pos <- which(mat[x, ] == min(mat[x, ]))
  val <- mat[x, which(mat[x, ] == min(mat[x, ]))]
  return(list(pos=pos, val=val))
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

#' Summarize a vector of edit distances
#'
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

#' Calculate the distance (OSA) ratio from distance and max distance matrices
#'
#' Raw string edit distances are of limited use because differences in the 
#' lengths of compared strings drives the distribution of expected distances.
#' The raw edit distances are normalized by element-wise division of the OSA
#' distance by the maximum possible distance (sum of compared string lengths).
#' Identical strings have a ratio of 0, completely different strings == 1.
#' 
#' @param dists A matrix of string edit distances (OSA)
#' @param maxes A matrix of maximum possible string edit distances
#' @return A matrix of edit distance ratios
#' @seealso \link{get_max_dist}
#' @export
calc_dist_ratio <- function(dists, maxes) {
  ratio <- list()
  for(i in names(dists)) {
    ratio[[i]] <- dists[[i]] / maxes[[i]]
  }
  return(ratio)
}

#' Create a data.frame of minimum ratio string matches
#'
#' To analyze and plot sentence similarity, we need the data out of the 
#' (ridiculous) nested list and into a useful data.frame, which this function
#' provides.
#' 
#' @param df A list created in \link{one_vs_all_dists}
#' @param name The name of the current (single) chapter being compared to all
#' @return A data.frame with best match data
#' @seealso \link{one_vs_all_dists}
#' @export
#' @importFrom dplyr bind_rows
create_mins_df <- function(dist_list, name) {
  mins_df <- list()
  for(i in names(dist_list)) {
    mins_df[[i]] <- dplyr::bind_rows(dist_list[[i]]) 
    mins_df[[i]]$cur_ch <- i
    n_reps <- lapply(dist_list[[i]], FUN = function(x) length(x$pos))
    sents <- seq(1:length(n_reps))
    n_sents <- list()
    for(j in 1:length(sents)) n_sents[[j]] <- rep(sents[j], n_reps[j])
    mins_df[[i]]$rev_sent <- unlist(n_sents)
  }
  mins_df <- dplyr::bind_rows(mins_df)
  mins_df$rev_ch <- rep(name, length(mins_df$pos))
  mins_df$rev_idx <- seq(1:length(mins_df$pos))
  return(mins_df)
}



