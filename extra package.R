#"corpus" package 
#Term Frequencies
term_counts(lg)
# remove punctuation and stop words
term_counts(lg, text_filter(drop_symbol = TRUE, drop = stopwords("english")))
#Term Frequency abulation
term_matrix(lg)
# select certain terms
term_matrix(lg, select = c("the", "america", "me"))
text_locate(lg, "america")