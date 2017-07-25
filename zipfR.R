#analyze word frequency
lgtdm2<-TermDocumentMatrix(lgstop, control = list(wordLengths = c(1,Inf)))
freq.terms <- findFreqTerms(lgtdm2, lowfreq = 0)
freq.terms
term.freq <- rowSums(as.matrix(lgtdm2))
term.freq <- subset(term.freq, term.freq >= 0)
df <- data.frame(term = names(term.freq), freq = term.freq)
View(df)
#transfer data-frame into spc classes
df$term <- as.character(df$term)
ind <- !grepl('aaaa', df$term)
df <- df[ind, ]
df$term <- as.character(df$term)
token_freq <- tapply(df$term, df$freq, length)
token_freq
email_spc <- spc(Vm = token_freq, m = as.numeric(dimnames(token_freq)[[1]]))
View(email_spc)
# summary and plot _spc
summary(email_spc)
plot(email_spc)
