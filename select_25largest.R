library(tm)
library(plyr)
load("corpus.RData")
# calculate the char length of each document
size <- ldply(vc, function(x) {
  id <- x$meta$id
  nc <- sum(nchar(x$content))
  print(paste(c(id, nc), collapse = "/"))
  return(data.frame(id = id, nc = nc))
}, .id = NULL)
# the 25 largest
size <- size[order(size$nc, decreasing = T)[1:25], ]
# select the 25 largest
lg <- tm_filter(
  vc,
  FUN = function(x) {
    x$meta$id %in% size$id
  }
)
save(lg, file = "corpus_25largest.RData")
