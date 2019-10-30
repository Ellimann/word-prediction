library(tm)
library(stringi)
library(R.utils)
library(dplyr)

#Load the data frame again
load("bigram_multi.persistent")
load("trigram_multi.persistent")
load("onegram_multi.persistent")

#top 10 words from onegram - just hard code it instead of loading
top10Words = c("the", "and", "for", "you", "that", "with", "was", "this", "have", "are")

processSentence<-function(sentence, fast=FALSE) {
    cleanedupSentence<-removePunctuation(sentence)
    cleanedupSentence<-tolower(cleanedupSentence)
    words<-unlist(strsplit(cleanedupSentence, " "))
    
    lastIndex = length(words)
    if (length(words) < 2) {
        return("..Please enter more words....")
    }
    
    tempVal = getNextWordTrigram(words[lastIndex - 1], words[lastIndex])
    if (length(tempVal) > 4)
        answers<-tempVal
    else if (length(tempVal) > 0) {
        answers<-tempVal
        tempVal2 = getNextWordBigram(words[lastIndex], words[-c(1,1)])
        answers<-append(tempVal, tempVal2)
        answers<-unique(answers)
    }
    else {
       answers<-getNextWordBigram(words[lastIndex], words[-c(1,1)])
    }
    commonAnswers = NULL
    if(!fast) {
        commonAnswers<-seeOtherWords(answers, words)
    }
    
    answers<-answers[c(1:10)]
    commonAnswers<-commonAnswers[c(1:10)]
    topAnswers<-intersect(answers, commonAnswers)
    topAnswers<-append(topAnswers, answers)
    topAnswers<-append(topAnswers, commonAnswers)
    topAnswers<-unique(topAnswers)
    topAnswers<-lapply(topAnswers, na.omit)
    topAnswers<-lapply(topAnswers, function(x) x[!is.null(x)])

        if (length(topAnswers) == 0)
        topAnswers = top10Words
    
    if #((length(topAnswers == 1) && 
         (is.na(topAnswers[1]))
        topAnswers = top10Words
    
    topAnswers<-topAnswers[c(1:10)]
        
    topAnswers<-lapply(topAnswers, function(x) x[!is.null(x)])
    topAnswers<-lapply(topAnswers, na.omit)
    returnString<-paste("Top word is: ", toupper(topAnswers[1]) , "....  Other possible words are....")
    topAnswers<-topAnswers[-1]
    
    for (ans in topAnswers) {
        returnString<-paste(returnString, " ", ans)
    }
    return(returnString)
}

rownamesOneGram<-row.names(onegram)

seeOtherWords<-function(results, words) {
    otherMatches = NULL
    for (i in 2:length(words)) {
        otherMatches<-unique(append(otherMatches, getNextWordTrigram(words[i-1], words[i])))
    }
    
    betterWords<-intersect(results, otherMatches)
    betterWords<-intersect(betterWords, rownamesOneGram)
}

getNextWordTrigram <- function(word1, word2="") {
    nextWord = ""
    if (word1 != "" & word2 != "") {
        tempdf<-trigram[which(trigram$X1==word1 & trigram$X2==word2),  ]
        tempdfOrdered<-arrange(tempdf, -sum)

        if (nrow(tempdf) == 1 ) {
            nextWord = as.String(tempdf[1, "X3"])
        #If there are multiple, use a markov chain to get Maximum Likelihood Estimate
        #Since we only use bigrams and trigrams, it is pretty simple
        #P(w3|w2,w1) = count(w3, w2, w1)/count(w2, w1)
        } else if (nrow(tempdf)) {
            nextWord = as.String(tempdfOrdered[1, "X3"])
            j = 0
            tempdfOrdered$markov = -1
            for (word in tempdfOrdered$X3) {
                j = j+1
                tempdf2<-bigram[which(bigram$X1==word1, bigram$X2==word), ]
                countBigram = max(tempdf2$sum)
                tempdfOrdered$markov[j]<-as.integer(tempdfOrdered[j, "sum"])/countBigram
                if (j > 10)
                    break
            }
            tempdfOrdered<-arrange(tempdfOrdered, -markov)
            return(tempdfOrdered$X3)
        }
    }
}

getNextWordBigram <- function(word1, restOfwords=list()) {
    tempdf<-bigram[which(bigram$X1==word1), ]
    
    if (nrow(tempdf)) {
        nextWord = as.String(tempdf[1, "X2"])
    }
    tempdf<-arrange(tempdf, -sum)
    return(tempdf$X2)
}