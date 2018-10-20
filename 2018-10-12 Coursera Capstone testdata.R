##Formating the test-dataset

library(RWeka)

con <- file(here("data/en_US", "en_US.twitter.txt"), "r")
testData <- readLines(con, 1e5)
testData <- testData[80000:90000]
close(con)

ASCchar <- readRDS("specialChar.rds")

testData <- tolower(testData)
testData <- gsub(pattern = specialChar, 
                         replacement = "", x = testData)
testData <- gsub(pattern = "[[:digit:]]|[[:punct:]]|", 
                         replacement = "", x = testData)
testData <- stripWhitespace(testData)

testGram <- NGramTokenizer(testData, Weka_control(min = as.integer(6), max = as.integer(6)))
testGram <- str_split_fixed(testGram, pattern = " ", n = Inf)
testOutcome <- testGram[,6]

tempw12345 <- NULL 
tempw12345df <- matrix(nrow = nrow(testGram), ncol = 1)

for (i in 1:nrow(testGram)) {
    tempw12345 <- paste(testGram[i, 1:5], collapse = " ")
    tempw12345df[i,] <- tempw12345
}
testQuery <- tempw12345df

#Running test
source("2018-10-12 Coursera Capstone Word Selection.R")

temp <- NULL
testPredict <- matrix(nrow = 1000, ncol = 1)

startTime <- proc.time()
pb <- txtProgressBar(min = 0, max = length(testPredict), style = 3)
for (i in 1:1000) {
    temp <- wordPreProc(testQuery[i])
    testPredict[i,] <- wordPredict(temp, 5)
    setTxtProgressBar(pb, i)
}
close(pb)
proc.time() - startTime

#Outcome
table(testPredict[1:length(testPredict)]==testOutcome[1:length(testPredict)])
fails <- testPredict[1:length(testPredict)]==testOutcome[1:length(testPredict)]
failView <- cbind(testGram[!fails,], testPredict[!fails])
failView <- failView[1:sum(fails),]
View(failView)
