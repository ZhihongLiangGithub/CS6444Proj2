library(tm)
library(plyr)
library(wordcloud)
load("corpus_25largest.RData")
size <- ldply(lg, function(x) {
  id <- x$meta$id
  nc <- sum(nchar(x$content))
  print(paste(c(id, nc), collapse = "/"))
  return(data.frame(id = id, nc = nc))
}, .id = NULL)
size <- size[order(size$nc, decreasing = T)[1:5], ]
lg <- tm_filter(
  lg,
  FUN = function(x) {
    x$meta$id %in% size$id
  }
)
lg <- tm_map(lg, content_transformer(tolower))
removeNumPunct <- function(x) {
  gsub("[^[:alpha:][:space:]]*", "", x)
}
# remove numbers and punctuation
lg <- tm_map(lg, content_transformer(removeNumPunct))
# remove stopwords
lg <- tm_map(lg, removeWords, stopwords("english"))
# get term document matrix
lgtdm <-
  TermDocumentMatrix(lg, control = list(wordLengths = c(1, Inf)))
# draw wordcloud for every document in corpus
draw_wordcloud <- function(tdm) {
  dir <- paste(c(getwd(), "plots"), collapse = "/")
  pal <- brewer.pal(9, "BuGn")
  pal <- pal[-(1:4)]
  for (i in 1:tdm$ncol) {
    m <- as.matrix(tdm[, i])
    word.Freq <- sort(rowSums(m), decreasing = T)
    filedir <-
      paste(c(dir, "/wordcloud", i, ".png"), collapse = "")
    png(filename = filedir,
        width = 800,
        height = 800)
    wordcloud(
      words = names(word.Freq),
      freq = word.Freq,
      min.freq = mean(word.Freq),
      ramdom.order = F,
      colors = pal
    )
    dev.off()
    print(filedir)
  }
}
# draw dendrogram for every document in corpus
draw_dendrogram <- function(tdm) {
  dir <- paste(c(getwd(), "plots"), collapse = "/")
  tdm <- removeSparseTerms(tdm, sparse = 0.50)
  for (i in 1:tdm$ncol) {
    distMatrix <- dist(scale(tdm[, i]))
    fit <- hclust(distMatrix, method = "ward.D2")
    filedir <-
      paste(c(dir, "/dendrogram", i, ".png"), collapse = "")
    png(filename = filedir,
        width = 800,
        height = 800)
    plot(fit)
    dev.off()
    print(filedir)
  }
}

draw_wordcloud(lgtdm)
draw_dendrogram(lgtdm)