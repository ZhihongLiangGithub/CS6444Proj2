library(tm)
library(tm.plugin.mail)
load("corpus.RData")
vclow <- tm_map(vc, content_transformer(tolower))
removeNumPunct <- function(x) {
  gsub("[^[:alpha:][:space:]]*", "", x)
}
# remove numbers and punctuation
vccl <- tm_map(vclow, content_transformer(removeNumPunct))
# remove stopwords
vcstop <- tm_map(vccl, removeWords, stopwords("english"))
# term document matrix
vctdm <- TermDocumentMatrix(vcstop, control = list(wordLengths = c(1, Inf), weighting = weightBin))
# remove sparse terms
vctdmNoSparse <- removeSparseTerms(vctdm, .50)
