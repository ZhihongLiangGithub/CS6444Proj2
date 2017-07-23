library(tm)
library(plyr)
load("corpus.RData")
vc <- tm_map(vc, content_transformer(tolower))
removeNumPunct <- function(x) {
  gsub("[^[:alpha:][:space:]]*", "", x)
}
# remove numbers and punctuation
vc <- tm_map(vc, content_transformer(removeNumPunct))
# remove stopwords
vc <- tm_map(vc, removeWords, stopwords("english"))
# term document matrix
vctdm <-
  TermDocumentMatrix(vc, control = list(wordLengths = c(1, Inf)))

# determine the 1st & 2nd topic of every document in corpus
tdm_topic <- function(tdm) {
  dd <-
    data.frame(id = integer(),
               topic1 = character(),
               topic2 = character())
  for (i in 1:vctdm$ncol) {
    m <- as.matrix(vctdm[, i])
    wordFreq <- sort(rowSums(m), decreasing = T)[1:2]
    d <-
      data.frame(
        id = i,
        topic1 = names(wordFreq[1]),
        topic2 = names(wordFreq[2])
      )
    print(i)
    dd <- merge(dd, d, all = T)
  }
  return(dd)
}

topics <- tdm_topic(vctdm)

topiccounts <-
  count(c(as.vector(topics$topic1), as.vector(topics$topic2)))

topiccounts <-
  topiccounts[order(topiccounts$freq, decreasing = T)[1:50],]

filedir <- paste(c(getwd(), "plots", "numberOfDocuments.png"), collapse = "/")
png(filename = filedir,
    width = 800,
    height = 800)
barplot(
  names.arg = topiccounts$x,
  height = topiccounts$freq,
  main = "Number of documents in each topic area Top 50",
  xlab = "topics",
  ylab = "frequency",
  las = 3
)
dev.off()