library(NLP)
library(slam)
library(tm)
# inspect lg
inspect(lg)
# extracting a document
 Test1 <- lg[[1]]
 Test1
#create a term document matrix
 lgtdm <- TermDocumentMatrix(lg)
 lgtdm
 inspect(lgtdm[1:10,1:6])
#corpus management
 lglow <- tm_map(lg, content_transformer(tolower))
 lglow
 removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
 lgcl <- tm_map(lglow, content_transformer(removeNumPunct))
 lgcltdm <- TermDocumentMatrix(lgcl)
 lgcltdm
myStopwords <- c(stopwords('english'))
myStopwords
lgstop <- tm_map(lgcl, removeWords, myStopwords)
inspect(lgstop[1:2])
library(wordnet)
setDict("C:\\Program Files (x86)\\WordNet\\2.1\\dict")
Sys.setenv(WNHOME = "C:\\Program Files (x86)\\WordNet\\2.1")
#print adverb word's part of speech
> lapply(lg,function(x){
+     sapply(unlist(strsplit(as.character(x),"[[:space:]]+")), function(word) {
+         x.filter <- getTermFilter("StartsWithFilter", word, TRUE)
+         terms    <- getIndexTerms("NOUN",1,x.filter)
+         if(!is.null(terms)) sapply(terms,getLemma)
+     })
+ })
lg
head(terms_result)

