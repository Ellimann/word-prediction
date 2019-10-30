library(tm)

load("data2.RData")

pre <- function(str,n){
        vs <- VectorSource(str)
        db <- VCorpus(vs, readerControl = list(reader = readPlain, language = "en",load=TRUE))
        db <- tm_map(db, tolower)
        db <- tm_map(db, removePunctuation)
        db <- tm_map(db, removeNumbers)
        db <- tm_map(db, removeWords, profanity)
        db <- tm_map(db, removeWords, stopwords("english"))
        db <- tm_map(db, stemDocument)
        db <- tm_map(db, stripWhitespace)
        
        content <- db$content[[1]]
        temp <- tail(unlist(strsplit(content,split=" ")),n)
        return(paste(temp,collapse =" "))
        
}

topword <- function(str,n){
        tail1 <- data2[grep(paste("^",pre(str,1)," ",sep=""),names(data2))]
        topn <- names(sort(tail1,decreasing = T))[1:n]
        sapply(strsplit(topn ,split=" "),function(x) x[2])
}



shinyServer(
        function(input, output) {
                output$topn = renderPrint({
                        input$goButton
                        isolate(topword(input$text1,input$n))
                        })
                
                output$top1 = renderPrint({
                        input$goButton
                        isolate(topword(input$text1,1))
                        })

        }
)