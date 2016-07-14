# doc1 <- c("chr", "-", "CUR", "current", 1, length(unlist(hcp_cur)), "grey")
# doc2 <- c("chr", "-", "REV", "revision", 1, length(unlist(hcp_rev)), "red")
# kary_dat <- rbind(doc1, doc2)
# cur_chs <- unique(all_dists_df$cur_ch)
# for(i in 1:length(cur_chs)) {
#   cur_sub <- dplyr::filter(all_dists_df, cur_ch == cur_chs[i])
#   cur_col <- ifelse(i %% 2 == 0, "gray", "white")
#   cur_st <- ifelse(i == 1,
#                    1,
#                    as.numeric(kary_dat[1+i,6]) + 1)
#   cur_dat <- c("band", "CUR", cur_chs[i], cur_chs[i], cur_st, 
#                cur_st + max(cur_sub$cur_sent), cur_col)
#   kary_dat <- rbind(kary_dat, cur_dat)
# }
# dim(kary_dat)
# 
# rev_chs <- unique(all_dists_df$rev_ch)
# for(i in 1:length(rev_chs)) {
#   cur_sub <- dplyr::filter(all_dists_df, rev_ch == rev_chs[i])
#   cur_col <- ifelse(i %% 2 == 0, "gray", "white")
#   cur_st <- ifelse(i == 1,
#                    1,
#                    kary_dat[32+i,6] + 1)
#   cur_dat <- c("band", "CUR", i, i, cur_st, cur_st + max(cur_sub$cur_sent), cur_col)
#   kary_dat <- rbind(kary_dat, cur_dat)
# }

# amat <- matrix(NA, nrow=length(unlist(hcp_cur)), ncol=length(unlist(hcp_rev)))
# cur_chs <- names(hcp_cur)
# cnames <- rep(NA, length(unlist(hcp_cur)))
# st <- 1
# for(i in cur_chs) {
#   for(j in 1:length(hcp_cur[[i]])) {
#     cnames[st] <- paste0("current.", i, ".", j)
#     st <- st + 1
#   }
# }
# rev_chs <- names(hcp_rev)
# rnames <- rep(NA, length(unlist(hcp_rev)))
# st <- 1
# for(i in rev_chs) {
#   for(j in 1:length(hcp_rev[[i]])) {
#     rnames[st] <- paste0("revised.", i, ".", j)
#     st <- st + 1
#   }
# }
# row.names(amat) <- cnames
# colnames(amat) <- rnames
# 

#### Unfortunately, edgebundleR doesn't work well for this, at least not yet...
#### I can try re-structuring the matrix to square form to see if that will work,
#### rather than using the JSON input (which is failing for some reason)
# for(i in 1:length(all_dists_df$cur_sent)) {
#   amat[all_dists_df[i,]$from, all_dists_df[i,]$to] <- 1 - all_dists_df[i,]$val  
# }
# 