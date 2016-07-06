# BSD_2_clause

# library(dplyr)
# library(NLP)
# library(openNLP)
# library(pdftools)
# library(stringr)

rev <- pdftools::pdf_text("data-raw/HCP_handbook_rev_draft_28Jun2016.pdf")
length(rev)
main <- rev[13:length(rev)]
toc <- rev[1:12]
execsum <- rev[13:14]
acronyms <- rev[15:16]
glossary <- rev[17:42]
ch1 <- rev[44:57]
ch2 <- rev[58:76]
ch3 <- rev[77:104]
ch4 <- rev[105:123]
ch5 <- rev[124:131]
ch6 <- rev[132:140]
ch7 <- rev[141:162]
ch8 <- rev[163:169]
ch9 <- rev[170:213]
ch10 <- rev[214:247]
ch11 <- rev[248:273]
ch12 <- rev[274:282]
ch13 <- rev[283:296]
ch14 <- rev[297:331]
ch15 <- rev[332:342]
ch16 <- rev[343:359]
ch17 <- rev[360:372]
appA <- rev[373:383]
appB <- rev[384:388]
appC <- rev[389:391]

hcp_rev_foc <- list(execsum=execsum, glossary=glossary, ch1=ch1, ch2=ch2, 
                    ch3=ch3,  ch4=ch4, ch5=ch5, ch6=ch6, ch7=ch7, ch8=ch8, 
                    ch9=ch9,  ch10=ch10, ch11=ch11, ch12=ch12, ch13=ch13, 
                    ch14=ch14, ch15=ch15,  ch16=ch16, ch17=ch17, appA=appA, 
                    appB=appB, appC=appC)

make_sentences <- function(ch) {
  data <- paste(ch, collapse = "\f")
  data <- stringr::str_split(data, pattern = "[_]+\n")
  data <- data[[1]][length(data[[1]])]
  s <- NLP::as.String(data)
  stok <- openNLP::Maxent_Sent_Token_Annotator()
  ann <- NLP::annotate(s, stok)
  sents <- s[ann]
  return(sents)
}

# get all sentences:
all_sent <- lapply(hcp_rev_foc, make_sentences)
lens <- unlist(lapply(all_sent, FUN = length))
hcp_rev_sentences <- lapply(all_sent, gsub, pattern = "\n", replacement = " ")
hcp_rev_sentences <- lapply(hcp_rev_sentences, 
                            FUN = gsub, 
                            pattern = "[ ]{2,}", 
                            replacement = " ")
hcp_rev_sent <- hcp_rev_sentences

devtools::use_data(hcp_rev_foc, overwrite = TRUE)
devtools::use_data(hcp_rev_sent, overwrite = TRUE)

# frs <- lapply(hcp_rev_sentences, 
#               FUN = str_match_all, 
#               pattern = "[0-9]+ FR [0-9]+")
# frs <- unlist(frs)
# frs
# 
# cons <- lapply(hcp_rev_sentences, 
#                FUN = str_match_all, 
#                pattern = "\\w{0,10} [Cc]onsult[a-z]+ \\w{0,10}")
# cons <- unlist(cons)
# head(cons, 15)

