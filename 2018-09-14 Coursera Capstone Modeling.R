##Next step is to test the accuracy of the model by redoing the quiz.
##Creating frequency database
library(here)
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

##I decided to exclude 5,4 and 3-grams that didn't have at least 2 occurances to 
##slim down the model while retaining accuracy. For 2 n-grams the amount
##of words needed for 50% coverage was calculated and the was taken as a cutOff.
##Note: the quagram started off as a 500mg file, so it's unlikely for more data
##to be included with current processing power.
sixgramFreq <- termFreqNGram(dataSmallPreProc, 6)
sixgramFreq <- sixgramFreq[sixgramFreq$count >= 2,]
sixgramFreq <- data.frame(lapply(sixgramFreq, as.character), stringsAsFactors=FALSE)
sixgramFreq <- sixgramFreq$`n.gram`
sixgramFreq <- str_split_fixed(sixgramFreq, pattern = " ", n = Inf)
sixgramFreq <- as.data.frame(sixgramFreq)

fivegramFreq <- termFreqNGram(dataSmallPreProc, 5)
fivegramFreq <- fivegramFreq[fivegramFreq$count >= 2,]
fivegramFreq <- data.frame(lapply(fivegramFreq, as.character), stringsAsFactors=FALSE)
fivegramFreq <- fivegramFreq$`n.gram`
fivegramFreq <- str_split_fixed(fivegramFreq, pattern = " ", n = Inf)
fivegramFreq <- as.data.frame(fivegramFreq)

quagramFreq <- termFreqNGram(dataSmallPreProc, 4)
quagramFreq <- quagramFreq[quagramFreq$count >= 2,]
quagramFreq <- data.frame(lapply(quagramFreq, as.character), stringsAsFactors=FALSE)
quagramFreq <- quagramFreq$`n.gram`
quagramFreq <- str_split_fixed(quagramFreq, pattern = " ", n = Inf)
quagramFreq <- as.data.frame(quagramFreq)

trigramFreq <- termFreqNGram(dataSmallPreProc, 3)
trigramFreq <- trigramFreq[trigramFreq$count >= 2,]
trigramFreq <- data.frame(lapply(trigramFreq, as.character), stringsAsFactors=FALSE)
trigramFreq <- trigramFreq$`n.gram`
trigramFreq <- str_split_fixed(trigramFreq, pattern = " ", n = Inf)
trigramFreq <- as.data.frame(trigramFreq)

digramFreq <- termFreqNGram(dataSmallPreProc, 2)
digramFreq <- digramFreq[digramFreq$count >= 9,]
digramFreq <- data.frame(lapply(digramFreq, as.character), stringsAsFactors=FALSE)
digramFreq <- digramFreq$`n.gram`
digramFreq <- str_split_fixed(digramFreq, pattern = " ", n = Inf)
digramFreq <- as.data.frame(digramFreq)

idiomPreProc <- readRDS(here("serverData", "procIdiom.rds"))

count <- matrix(nrow = length(proverbPreProc), ncol = 1)
for (i in 1:length(proverbPreProc)) {
    count[i] <- wordCount(proverbPreProc[i])
}
proverbTwo <- proverbPreProc[count == 2]
proverbThree <- proverbPreProc[count == 3]
proverbFour <- proverbPreProc[count == 4]
proverbFive <- proverbPreProc[count == 5]
proverbSix <- proverbPreProc[count == 6]

proverbTwo <- str_split_fixed(proverbTwo, pattern = " ", n = Inf)
proverbThree <- str_split_fixed(proverbThree, pattern = " ", n = Inf)
proverbFour <- str_split_fixed(proverbFour, pattern = " ", n = Inf)
proverbFive <- str_split_fixed(proverbFive, pattern = " ", n = Inf)
proverbSix <- str_split_fixed(proverbSix, pattern = " ", n = Inf)

sixgramFreq <- rbind(sixgramFreq, proverbSix)
fivegramFreq <- rbind(fivegramFreq, proverbFive)
quagramFreq <- rbind(quagramFreq, proverbFour)
trigramFreq <- rbind(trigramFreq, proverbThree)
digramFreq <- rbind(digramFreq, proverbTwo)

##Choose to put them at the bottom, as they would naturally come up by 
##themselves if they are often use. By not matching them beforehand, you loose
##some space, but given the limited amount of idioms, that's okay.

saveRDS(sixgramFreq, here("serverData", "6gram.rds"))
saveRDS(fivegramFreq, here("serverData", "5gram.rds"))
saveRDS(quagramFreq, here("serverData", "4gram.rds"))
saveRDS(trigramFreq, here("serverData", "3gram.rds"))
saveRDS(digramFreq, here("serverData", "2gram.rds"))

