##finding the most frequent terms
library(tm)
ctrl <- list(language = "en_US", removePunctuation = T, 
    removeNumbers = T)
termFreq <- termFreq(dataSmallPreProc, control = ctrl)
termFreq <- sort(termFreq, decreasing = T)

##Plotting the frequency distribution of the words 
library(ggplot2)

qFreqHist1 <- ggplot(as.data.frame(termFreq), 
    aes(y= termFreq, x= seq_along(termFreq)))
qFreqHist1 <- qFreqHist1 + geom_line() +
    scale_y_log10(name = "Amount, log10-scaling")
qFreqHist1 <- qFreqHist1 + labs(title= "Frequency of all words")
qFreqHist1

qFreqHist2 <- ggplot(as.data.frame(termFreq[1:100]), 
                                   aes(y= termFreq[1:100], x= 1:100))
qFreqHist2 <- qFreqHist2 + geom_line() +
    scale_y_log10(name = "Amount, log10-scaling")
qFreqHist2 <- qFreqHist2 + labs(title= "Frequency of top 100 words")
qFreqHist2

rownm <- names(termFreq)[1:10]
rownm <- factor(rownm, levels = rownm[order(termFreq, decreasing= T)])

qFreqHist3 <- ggplot(as.data.frame(termFreq[1:10]), 
        aes(x = rownm, y= termFreq[1:10]))
qFreqHist3 <- qFreqHist3 + geom_col(aes(fill = termFreq[1:10]))
qFreqHist3 <- qFreqHist3 + labs(title= "Top 10 words", 
        x= "Words", y = "Frequency")
qFreqHist3

library(gridExtra)
grid.arrange(qFreqHist1, qFreqHist2, qFreqHist3)

##Frequency of 2-grams and 3-grams
library(RWeka)

termFreqNGram <- function(data, ngram = 1, length = NULL) {
    termFreqTemp <- NGramTokenizer(data, 
        Weka_control(min = as.integer(ngram), max = as.integer(ngram)))
    len <- length(termFreqTemp)
    termFreqTemp <- data.frame(table(termFreqTemp))
    termFreqTemp <- termFreqTemp[order(termFreqTemp$Freq, decreasing = T),]
    colnames(termFreqTemp) <- c("n-gram", "count")
    if (is.null(length)) {
        data <- termFreqTemp[1:as.integer(len),]
    } else {
        data <- termFreqTemp[1:as.integer(length)]
    }
    return(data)
}

termFreqBigram <- termFreqNGram(dataSmallPreProc, 2)
termFreqTrigram <- termFreqNGram(dataSmallPreProc, 3)

##Amount of unique words to cover all word use
### 1-gram
tWordUse <- sum(termFreq, na.rm =T)
goal <- .5
cover <- 0
cutOff <- 0
tCover <- 0

while(cover < goal) {
    cutOff <- cutOff + 1
    tCover <- tCover + termFreq[cutOff]
    cover <- tCover/tWordUse
    if (cover >= goal) {
        print(cutOff)
    }
} 

### 2-gram
termFreq <- termFreqBigram
tWordUse <- sum(termFreq$count, na.rm =T)
goal <- .5
cover <- 0
cutOff <- 0
tCover <- 0

while(cover < goal) {
    cutOff <- cutOff + 1
    tCover <- tCover + termFreq$count[cutOff]
    cover <- tCover/tWordUse
    if (cover >= goal) {
        print(cutOff)
    }
}

### 3-gram
termFreq <- termFreqTrigram
tWordUse <- sum(termFreq$count, na.rm =T)
goal <- .5
cover <- 0
cutOff <- 0
tCover <- 0

while(cover < goal) {
    cutOff <- cutOff + 1
    tCover <- tCover + termFreq$count[cutOff]
    cover <- tCover/tWordUse
    if (cover >= goal) {
        print(cutOff)
    }
}

### 4-gram
termFreqQuagram <- termFreqNGram(dataSmallPreProc, 4)

termFreqQuagram <- quagramFreq
tGramUse <- sum(termFreqQuagram$count, na.rm = T)
goal <- .5
cover <- 0
cutOff <- 0
tCover <- 0

while(cover < goal) {
    cutOff <- cutOff + 1
    tCover <- tCover + termFreqQuagram$count[cutOff]
    cover <- tCover/tGramUse
    if (cover >= goal) {
        print(cutOff)
    }
}

data <- NULL
temp <- NULL

####This code turns the frequency n-gram model into a sequence of these n-grams
####with the correct count.
for(i in 1:cutOff) {
    temp <- rep(as.character(termFreqQuagram$`n-gram`[i]), termFreqQuagram$count[i])
    data <- c(data, temp)
}


