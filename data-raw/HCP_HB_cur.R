# BSD_2_clause

# library(dplyr)
# library(NLP)
# library(openNLP)
# library(pdftools)
# library(stringr)

cur <- pdftools::pdf_text("data-raw/HCP_handbook_current_2016.pdf")
length(cur)
hcp_cur_all <- cur

execsum <- cur[1:3]
toc <- cur[4:8]
apls <- cur[9]
fore <- cur[10]
pref <- cur[11:12]
ch1 <- cur[13:29]
ch2 <- cur[30:37]
ch3 <- cur[38:78]
ch4 <- cur[79:82]
ch5 <- cur[83:88]
ch6 <- cur[89:119]
ch7 <- cur[120:125]
ch8 <- cur[126:131] # glossary
ap1 <- cur[132:142]
ap2 <- cur[143:146]
ap3 <- cur[147:155]
ap4 <- cur[156:169]
ap5 <- cur[170:177]
ap6 <- cur[178:184]
ap7 <- cur[185:192]
ap8 <- cur[193:213]
ap9 <- cur[214:219]
ap10 <- cur[220:248]
ap11 <- cur[249:276]
ap12 <- cur[277:281]
ap15 <- cur[282:283]
ap16 <- cur[284:295]
ap17 <- cur[296:306]
ap18 <- cur[307:309]
addn <- cur[310:311]

hcp_cur_foc <- list(execsum=execsum, apls=apls, fore=fore, pref=pref, ch1=ch1, 
                    ch2=ch2, ch3=ch3, ch4=ch4, ch5=ch5, ch6=ch6, ch7=ch7, 
                    ch8=ch8, ap1=ap1, ap2=ap2, ap3=ap3, ap4=ap4, ap5=ap5, ap6=ap6, 
                    ap7=ap7, ap8=ap8, ap9=ap9, ap10=ap10, ap11=ap11, ap12=ap12, 
                    ap15=ap15, ap16=ap16, ap17=ap17, ap18=ap18, addn=addn)

make_sentences <- function(ch) {
  data <- paste(ch, collapse = "\f")
  s <- NLP::as.String(data)
  stok <- openNLP::Maxent_Sent_Token_Annotator()
  ann <- NLP::annotate(s, stok)
  sents <- s[ann]
  return(sents)
}

all_sent <- lapply(hcp_cur_foc, make_sentences)
lens <- unlist(lapply(all_sent, FUN = length))
hcp_cur_sentences <- lapply(all_sent, gsub, pattern = "\n", replacement = " ")
hcp_cur_sentences <- lapply(hcp_cur_sentences, 
                            FUN = gsub, 
                            pattern = "[ ]{2,}", 
                            replacement = " ")
hcp_cur_sent <- hcp_cur_sentences

devtools::use_data(hcp_cur_all, overwrite = TRUE)
devtools::use_data(hcp_cur_foc, overwrite = TRUE)
devtools::use_data(hcp_cur_sent, overwrite = TRUE)

# frs <- lapply(hcp_cur_sentences, 
#               FUN = str_match_all, 
#               pattern = "[0-9]+ FR [0-9]+")
# frs <- unlist(frs)
# frs
# 
# cons <- lapply(hcp_cur_sentences, 
#                FUN = str_match_all, 
#                pattern = "\\w{0,10} [Cc]onsult[a-z]+ \\w{0,10}")
# cons <- unlist(cons)
# head(cons, 15)