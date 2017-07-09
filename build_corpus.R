library(tm)
# central people
cp <-
  "david.forster@enron.com|jeff.dasovich@enron.com|tana.jones@enron.com|veronica.espinoza@enron.com|steven.kean@enron.com"
maildir <-
  "C:/Users/Liang/Desktop/BigData/enron_mail_20150507/maildir"
cordir <- "C:/Users/Liang/Desktop/BigData/corpus"
count <- 0
f <- function(path) {
  lines <- readLines(path)
  # extract the sender
  sender <- gsub("From: ", "", lines[grepl("^From: .*", lines)][1])
  # extract the receiver(s)
  start <- grep("^To:.*", lines)[1]
  end <- grep("^Subject:.*", lines)[1] - 1
  receiver <- character()
  if (is.na(start)) {
    receiver <- NA
  } else{
    if (start > end) {
      receiver <- NA
    } else{
      receiver <- gsub("\t", "", lines[start:end])
      receiver <- gsub("To: ", "", receiver)
      receiver <- paste(receiver, collapse = "")
    }
  }
  if (grepl(cp, sender) || grepl(cp, receiver)) {
    count <<- count + 1
    print(count)
    file.copy(path, paste(c(cordir, count), collapse = "/"))
  }
}
files <- list.files(path = maildir,
                    full.names = T,
                    recursive = T)
lapply(files, f)
vc <- VCorpus(DirSource(cordir))

save(vc, file = "corpus.RData")
