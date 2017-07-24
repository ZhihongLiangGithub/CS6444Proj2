tdm <- TermDocumentMatrix(lg)
inspect(tdm)
docs<-tm_map(content,content_transformer(tolower))
test25<-lg[[25]]
test25<-test25$content
test25<-toString(test25)
split_by_sentence <- function (text) {
  
  # split based on periods, exclams or question marks
  result <- unlist (strsplit (text, split = "[\\.!?]+"))
  
  # do not return empty strings
  result <- stri_trim_both (result)t
  result <- result [nchar (result) > 0]
  
  # ensure that something is always returned
  if (length (result) == 0)
    result <- ""
  
  return (result)
}

sentence2<-strsplit(test2," ")


head(sentence2[order(nchar(sentence2), decreasing = T)])
