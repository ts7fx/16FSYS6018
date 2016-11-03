# SYS6018 case2, Tianye Song, ts7fx
library(readr)
library(tm)
setwd("~/Documents")
index<-read_csv('clean/to_from.csv')
# two tasks at hand:
# 1. a dataframe with 6 observations (Authors). Content being the collapsed content of all email written by one author.
summary(factor(index$From))
# Andrew Wright Daniel Parrish  Jordan Kaplan   Luis Miranda    Scott Comer  Zachary Allen 
# 281            149            917           1590            822            248 
Andrew.email.IDs <- index[index$From == "Andrew Wright",]
Daniel.email.IDs <- index[index$From == "Daniel Parrish",]
Jordan.email.IDs <- index[index$From == "Jordan Kaplan",]
Luis.email.IDs <- index[index$From == "Luis Miranda",]
Scott.email.IDs <- index[index$From == "Scott Comer",]
Zachary.email.IDs <- index[index$From == "Zachary Allen",]

# takes in a DF and a person name
readFile<- function(df, pname){
  for (i in c(1:nrow(df))){
    txt.name <- as.character(df[i,1]) #e.g."1198.txt"
    filename <- paste('clean/clean_body/',pname,'_',txt.name,sep='')#"clean/clean_body/andrew_1198.txt"
    content <- tryCatch({
      readChar(con <- file(filename,"r"), nchars = file.size(filename))
    }, warning = function(w) {
      return (NA)
    }, error = function(e) {
      return (NA)
    }, finally = {
      close(con)
    })
    df[i,"Content"] <- content
  }
  return (df)
}
Andrew.email.IDs <- readFile(Andrew.email.IDs, 'andrew')
Daniel.email.IDs <- readFile(Daniel.email.IDs, 'daniel')
Jordan.email.IDs <- readFile(Jordan.email.IDs, 'jordan')
Luis.email.IDs <- readFile(Luis.email.IDs, 'luis')
Scott.email.IDs <- readFile(Scott.email.IDs, 'scott')
Zachary.email.IDs <- readFile(Zachary.email.IDs, 'zach')

andrew <- ''
daniel <- ''
jordan <- ''
luis <- ''
scott <- ''
zach <- ''
for (i in c(1:length(Andrew.email.IDs$Content))){
  andrew <- paste(andrew, Andrew.email.IDs[i,4], sep = ' ')
}
for (i in c(1:length(Daniel.email.IDs$Content))){
  daniel <- paste(daniel, Daniel.email.IDs[i,4], sep = ' ')
}
for (i in c(1:length(Jordan.email.IDs$Content))){
  jordan <- paste(jordan, Jordan.email.IDs[i,4], sep = ' ')
}
for (i in c(1:length(Luis.email.IDs$Content))){
  luis <- paste(luis, Luis.email.IDs[i,4], sep = ' ')
}
for (i in c(1:length(Scott.email.IDs$Content))){
  scott <- paste(scott, Scott.email.IDs[i,4], sep = ' ')
}
for (i in c(1:length(Zachary.email.IDs$Content))){
  zach <- paste(zach, Zachary.email.IDs[i,4], sep = ' ')
}

ready.to.go <- data.frame(c(andrew,daniel,jordan,luis,scott,zach), row.names = c("andrew","daniel","jordan","luis","scott","zach"))

# conver to VCorpus object
emails = VCorpus(DataframeSource(ready.to.go))
# compute tfidf matrix
emails.tfidf = DocumentTermMatrix(emails, control = list(weighting = weightTfIdf))

# cleaning up
emails.clean = tm_map(emails, stripWhitespace)                          # remove extra whitespace
emails.clean = tm_map(emails.clean, removeNumbers)                      # remove numbers
emails.clean = tm_map(emails.clean, removePunctuation)                  # remove punctuation
emails.clean = tm_map(emails.clean, content_transformer(tolower))       # ignore case
emails.clean = tm_map(emails.clean, removeWords, stopwords("english"))  # remove stop words
# emails.clean = tm_map(emails.clean, stemDocument)                     # stem all words (line doesn't run both here and in Dr. Gerber's code.)

# re-compute tfidf matrix for cleaned version of email corpus
emails.clean.tfidf = DocumentTermMatrix(emails.clean, control = list(weighting = weightTfIdf))

# reinspect the first 5 documents and first 5 terms
as.matrix(emails.clean.tfidf[1:5,1:5])
emails.clean.tfidf[1:5,1:5]

# playing around with threshold
tfidf.99 = removeSparseTerms(emails.clean.tfidf, 0.99)  # remove terms that are absent from at least 99% of documents (keep most terms)
as.matrix(tfidf.99[1:5,1:5])
tfidf.99

tfidf.70 = removeSparseTerms(emails.clean.tfidf, 0.70)  # remove terms that are absent from at least 98% of documents
as.matrix(tfidf.70[1:5, 1:5])
tfidf.70

tfidf.50 = removeSparseTerms(emails.clean.tfidf, 0.50)  # remove terms that are absent from at least 90% of documents
as.matrix(tfidf.50[1:5, 1:5])
tfidf.50


