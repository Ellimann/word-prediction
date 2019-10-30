library(tm)
library(stringi)
library(R.utils)
require(RWeka)
library(dplyr)

dataPath<-"C:/Users/Nirave/Documents/Coursera/capstone/Coursera-SwiftKey/final/en_US/"

#Read in the files
blogDataFile<-paste(dataPath , "/en_US.blogs.txt", sep="")
NumberInBlog = countLines(blogDataFile)
newsDataFile<-paste(dataPath , "/en_US.news.txt", sep="")
NumberInNews = countLines(newsDataFile)
twitterDataFile<-paste(dataPath , "/en_US.twitter.txt", sep="")
NumberInTwitter = countLines(twitterDataFile)

twitterFile<-readLines(twitterDataFile)
blogFile<-readLines(blogDataFile)
newsFile<-readLines(newsDataFile)

getSampleDoc<-function(x, nthLine=100) {
    #1% sample if nthLine is blank
    nthLine = 100
    
    #twitterSample<-sample(twitterFile, percentSample*NumberInTwitter)
    #Get every nth line
    twitterSample<-twitterFile[1:(NumberInTwitter/100 - 1)*nthLine + x]
    
    #Remove non-ascii characters
    twitterSample<-iconv(twitterSample, "latin1", "ASCII", sub="")
    twitterSample<-paste(twitterSample, collapse = "")
    
    #blogSample<-sample(blogFile, percentSample*NumberInBlog)
    #Get every nth line
    blogSample<-blogFile[1:(NumberInBlog/100 - 1)*nthLine + x]
    
    ##Remove non-ascii characters
    blogSample<-iconv(blogSample, "latin1", "ASCII", sub="")
    blogSample<-paste(blogSample, collapse = "")
    
    #newsSample<-sample(newsFile, percentSample*NumberInBlog)
    ##Remove non-ascii characters
    #Get every nth line
    newsSample<-newsFile[1:(NumberInNews/100 - 1)*nthLine + x]
    
    newsSample<-iconv(newsSample, "latin1", "ASCII", sub="")
    newsSample<-paste(newsSample, collapse = "")
    
    SampleText<-paste(twitterSample, blogSample, newsSample, collapse = "")
    
    sampleDocs<-c(Corpus(VectorSource(twitterSample)), Corpus(VectorSource(blogSample)), Corpus(VectorSource(newsSample)))
    
    #Clean the data
    #sampleDocs <-Corpus(DirSource(samplePath))
    
    sampleDocs <- tm_map(sampleDocs, removeNumbers)
    sampleDocs <- tm_map(sampleDocs, removePunctuation)
    sampleDocs <- tm_map(sampleDocs, stripWhitespace)
    sampleDocs <- tm_map(sampleDocs, tolower)
    
    #Convert to plain Text
    sampleDocs <- tm_map(sampleDocs, PlainTextDocument)
    
    return(sampleDocs)
}

bigramPart<-function(x) {
    
    TwogramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
    twodm <- TermDocumentMatrix(sampleDocs, control = list(tokenize = TwogramTokenizer))
    #twodm <- removeSparseTerms(twodm, 0.75)
    #inspect(twodm[1:10,1:1])
    
    twodmMatrix<-as.data.frame(as.matrix(twodm))
    twodmMatrix$sum<-rowSums(twodmMatrix)
    twodMatrixOrdered<-twodmMatrix[order (-twodmMatrix[,4]), ]
    
    return (twodmMatrix)
}


#Get every 100th line
trigramPart<-function(x) {
    TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
    tdm <- TermDocumentMatrix(sampleDocs, control = list(tokenize = TrigramTokenizer))
    #tdm <- removeSparseTerms(tdm, 0.75)
    #inspect(tdm[1:10,1:1])
    
    tdmMatrix<-as.data.frame(as.matrix(tdm))
    tdmMatrix$sum<-rowSums(tdmMatrix)
    write.table(tdmMatrix, file="temp3", quote=FALSE, row.names = FALSE, col.names = FALSE, sep="", append = TRUE)
    
    return(tdmMatrix)
}


sampleDocs<-getSampleDoc(0, 10)
#Construct a onegram, but of somewhat unique words
#(less than 100 occurances in 10% of the dataset)
adtm<-DocumentTermMatrix(sampleDocs)
oneGramDF <- as.data.frame(colSums(as.matrix(adtm)))
colnames(oneGramDF)<-c("sum")
oneGramOrdered<-oneGramDF[order(-oneGramDF$sum), , drop=FALSE]

onegram<-subset(oneGramOrdered, sum < 100)

sampleDocs<-getSampleDoc(0)

tdmMatrix<-trigramPart(0)
twodmMatrix<-bigramPart(0)
for (i in 1:9) {
    sampleDocs<-getSampleDoc(i)
    tempMatrix<-trigramPart(i)
    tdmMatrix<-rbind(tdmMatrix, tempMatrix)
    tempMatrix<-bigramPart(i)
    twodmMatrix<-rbind(twodmMatrix, tempMatrix)
}

tdMatrixOrdered<-tdmMatrix[order (-tdmMatrix[,4]), ]

twodMatrixOrdered<-twodmMatrix[order (-twodmMatrix[,4]), ]

#Create a data frame that can easily be searched
trigram<-data.frame(do.call('rbind', strsplit(as.character(rownames(tdMatrixOrdered)), " ", fixed=TRUE)))
trigram$sum = tdMatrixOrdered$sum

bigram<-data.frame(do.call('rbind', strsplit(as.character(rownames(twodMatrixOrdered)), " ", fixed=TRUE)))
bigram$sum = twodMatrixOrdered$sum

bigram$X2<-gsub('[0-9]+', "", bigram$X2)
bigram<-group_by(bigram, X1, X2)
bigram<-summarize(bigram, sum=sum(sum))

trigram$X3<-gsub('[0-9]+', "", trigram$X3)
trigram<-group_by(trigram, X1, X2, X3)
trigram<-summarize(trigram, sum=sum(sum))

save(onegram, file="onegram_multi.persistent")
save(trigram, file="trigram_multi.persistent")
save(bigram, file="bigram_multi.persistent")

#Sample - remove this

word1="cant"
word2="wait"

tempdf<-trigram[which(trigram$X1==word1 & trigram$X2==word2), ]
tempdfOrdered<-arrange(tempdf, -sum)

#load(file="trigram_multi.persistent")


