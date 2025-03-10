# BSD_2_clause

library(hcphb)

data("hcp_cur_sent")
data("hcp_rev_sent")

hcp_cur <- hcp_cur_sent
hcp_rev <- hcp_rev_sent

ls(hcp_cur)
length(hcp_cur$ch1)
length(hcp_rev$ch1)
ch1_mat <- stringdist::stringdistmatrix(hcp_cur$ch1, hcp_rev$ch1)
ch1_mat[1:10, 1:10]

ch1_max <- get_max_dist(hcp_cur$ch1, hcp_rev$ch1)
ch1_rat <- ch1_mat / ch1_max
ch1_rat[1:5,1:5]

# get the dist of the revised sentence most-similar to sent 2 of current hb:
ch1_mat[2,which(ch1_mat[2,] == min(ch1_mat[2,]))]
min_ch1 <- get_mins(ch1_rat)
hist(unlist(min_ch1), 
     breaks = 30,
     main = "Lev distances b/t rev Ch. 1 & cur Ch. 1", 
     xlab = "Optimal Levenshtein Distance")

# Get comparisons of revised ch 1 against each current chapter
ch1_dists <- lapply(hcp_cur, FUN = stringdistmatrix, b = hcp_rev$ch1)
ch1_maxes <- lapply(hcp_cur, FUN = get_maxes, hcp_rev$ch1)
ch1_ratio <- list()
for(i in names(ch1_dists)) {
  ch1_ratio[[i]] <- ch1_dists[[i]] / ch1_maxes[[i]]
}

min_ratios <- lapply(ch1_ratio, FUN = get_mins)

hist(unlist(get_mins(ch1_dists$ch2)), 
     main = "Lev distances b/t rev Ch. 1 & cur Ch. 2", 
     xlab = "Optimal Levenshtein Distance")

sum_ch1_ch1 <- summary_dists(get_mins(ch1_dists$ch1))
sum_ch1_ch2 <- summary_dists(get_mins(ch1_dists$ch2))
sum_ch1_ch3 <- summary_dists(get_mins(ch1_dists$ch3))
t.test(unlist(get_mins(ch1_dists$ch1)), unlist(get_mins(ch1_dists$ch2)))

##############################################################################
# Explore some text patterns:
rev_mep <- lapply(hcp_rev, agrep, pattern = "maximum extent practicable")
cur_mep <- lapply(hcp_cur, agrep, pattern = "maximum extent practicable")
rev_mep
cur_mep
hcp_cur$ch1[cur_mep$ch1]
