library(tm)
library(plyr)
load("corpus.RData")
size <- ldply(vc, function(x) {
  id <- x$meta$id
  nc <- sum(nchar(x$content))
  print(paste(c(id, nc), collapse = "/"))
  return(data.frame(id = id, nc = nc))
}, .id = NULL)
#...after a long wait
size <- size[order(size$nc, decreasing = T)[1:25],]
lg <- tm_filter(
  vc,
  FUN = function(x) {
    x$meta$id %in% size$id
  }
)
save(lg, file = "corpus_25largest.RData")
