load("prolific.RData")
library(tm)
maildir <-
  "C:/Users/Liang/Desktop/BigData/enron_mail_20150507/maildir"
cordir <- "C:/Users/Liang/Desktop/BigData/corpus"
count <- 0
f <- function(path) {
  lines <- readLines(path)
  #extract the sender
  sender <- gsub("From: ", "", lines[grepl("^From: .*", lines)][1])
  if (sender %in% prolific$sender) {
    print(path)
    count <<- count + 1
    file.copy(path, paste(c(cordir, count), collapse = "/"))
  }
}
files <- list.files(path = maildir,
                    full.names = T,
                    recursive = T)
lapply(files, f)
vc <- VCorpus(DirSource(cordir))
save(vc, file = "corpus.RData")

