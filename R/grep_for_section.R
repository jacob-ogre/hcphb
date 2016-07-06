abstracts <- lapply(mytxtfiles, function(i) {
  j <- paste0(scan(i, what = character()), collapse = " ")
  regmatches(j, gregexpr("(?<=Abstract).*?(?=Introduction)", j, perl=TRUE))
})