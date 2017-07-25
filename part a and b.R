install.packages("tm")
library(tm)
inspect(vc)
vctdm<-TermDocumentMatrix(vc)
vctdm
inspect(vctdm[1:10, 1:6])
test1<-vc[[1]]
test1
test1tf<-termFreq(test1)
test1tf
vclow<- tm_map(vc, content_transformer(tolower))
vclow
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
vccl<- tm_map(vclow, content_transformer(removeNumPunct))
vccltdm<- TermDocumentMatrix(vccl)
vccltdm
inspect(vccltdm[1:10,1:6])
myStopwords<- c(stopwords('english'))
myStopwords
vcstop<- tm_map(vccl, removeWords, myStopwords)
inspect(vcstop[1:2])
vctdm2 <- TermDocumentMatrix(vcstop, control= list(wordLengths= c(1, Inf), weighting=weightBin))
vctdm2
vcnosparse<-removeSparseTerms(vctdm2, 0.50)
vcnosparse
vctdm3<-TermDocumentMatrix(vcstop, control = list(wordLengths=c(1, Inf), weighting=weightBin))
vctdm3
vctdm4<-TermDocumentMatrix(vcstop, control = list(wordLengths=c(1, Inf), weighting=weightTfIdf))
vctdm4
freq.terms<- findFreqTerms(vctdm2, lowfreq= 10000)
freq.terms
findAssocs(vctdm2, "states", 0.25)
freq.terms3<- findFreqTerms(vctdm3, lowfreq = 10000)
freq.terms3
freq.terms4 <- findFreqTerms(vctdm4, lowfreq = 10000)
freq.terms4
term.freq<-rowSums(as.matrix(vctdm2))
term.freq<- subset(term.freq, term.freq>=10000)
df <- data.frame(term = names(term.freq), freq=term.freq)
term.freq
df
library(ggplot2)
ggplot(df, aes(x=term, y=freq))+ geom_bar(stat="identity")+xlab("Terms") +ylab("Count")+ coord_flip()+theme_grey(base_size = 8)
tdm2<- removeSparseTerms(vctdm, sparse=0.50)
tdm2
disMatrix<- dist(scale(tdm2))
disMatrix
fit<- hclust(disMatrix, method ="ward.D2")
plot(fit)
freqwords<- sort(rowSums(as.matrix(tdm2)), decreasing=TRUE)
freqwords
library(reshape2)
top50<- melt(freqwords[1:50])
top50
rh<- rect.hclust(fit, k=7)
rh
tm<- t(disMatrix)
k <- 6
kr <- kmeans(tm, k)
round(kr$centers, digits = 3)
m1<- as.matrix(tdm2)
word.freq<-sort(rowSums(m1), decreasing = T)
word.freq
install.packages("wordcloud")
library(wordcloud)
pal<-brewer.pal(9, "BuGn")
pal<-pal[-(1:4)]
wordcloud(words= names(word.freq), freq = word.freq, min.freq=3, random.order= F, colors=pal)

inspect(lg)
lgtdm<-TermDocumentMatrix(lg)
lgtdm
lglow<- tm_map(lg, content_transformer(tolower))
lglow
removeNumPunct2<- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
lgcl<- tm_map(lglow, content_transformer(removeNumPunct2))
lgcltdm<- TermDocumentMatrix(lgcl)
lgcltdm
inspect(lgcltdm[1:10,1:6])
lgstop<- tm_map(lgcl, removeWords, myStopwords)
inspect(lgstop[1:2])
lgtdm2 <- TermDocumentMatrix(lgstop, control= list(wordLengths= c(1, Inf), weighting=weightBin))
lgtdm2
lgnosparse<-removeSparseTerms(lgtdm2, 0.50)
lgnosparse
term.freq2<-rowSums(as.matrix(lgtdm2))
term.freq2<- subset(term.freq2, term.freq2>=10)
df2 <- data.frame(term = names(term.freq2), freq=term.freq2)
term.freq2
df2
tm_term_score(lgtdm, c("company", "change"))
