##building the functions to do the actual text prediction
library(stringr)

#Pre-coding of files
specialChar <- readRDS(here("serverData", "specialChar.rds"))

temp <- NULL

wordCount <- function(input){
    temp <- str_split(input, pattern = " ", n = Inf, simplify = T)
    length(temp)
}

wordPreProc <- function(x){
    temp <- tolower(x)
    temp <- gsub(pattern = specialChar, 
        replacement = "", x = temp)
    temp <- gsub(pattern = "[[:digit:]]|[[:punct:]]|",
        replacement = "", x = temp)
    temp <- stripWhitespace(temp)
    temp <- gsub(pattern = " $", replacement = '', x = temp)
    if(wordCount(temp) > 5) {
        temp <- word(temp, start = -5L, end = -1L)
    }
    temp <- str_split(temp, pattern = " ", n = Inf, simplify = T)
    temp <- as.character(temp)
    return(temp)
}
##part of the word() function is to split up the words

sixGram <- readRDS(here("serverData", "6gram.rds"))
fiveGram <- readRDS(here("serverData", "5gram.rds"))
fourGram <- readRDS(here("serverData", "4gram.rds"))
threeGram <- readRDS(here("serverData", "3gram.rds"))
twoGram <- readRDS(here("serverData", "2gram.rds"))

#actual function
wordPredict <- function(input, count = 5) {
    if (count == 5) {
        temp <- sixGram[sixGram$V1 == input[1] & 
                             sixGram$V2 == input[2] & 
                             sixGram$V3 == input[3] & 
                             sixGram$V4 == input[4] & 
                             sixGram$V5 == input[5], 6]
        temp <- as.character(temp[1])
        if (!is.na(temp)) {
            outcome <- temp
        }
        if (is.na(temp)) {
            count <- 4
            input <- input[2:5]
        }
    }
    if (count == 4) {
        temp <- fiveGram[fiveGram$V1 == input[1] & 
            fiveGram$V2 == input[2] & 
            fiveGram$V3 == input[3] & 
            fiveGram$V4 == input[4], 5]
        temp <- as.character(temp[1])
        if (!is.na(temp)) {
            outcome <- temp
        }
        if (is.na(temp)) {
            count <- 3
            input <- input[2:4]
        }
    }
    if (count == 3) {
        temp <- fourGram[fourGram$V1 == input[1] & 
            fourGram$V2 == input[2] & 
            fourGram$V3 == input[3], 4]
        temp <- as.character(temp[1])
        if (!is.na(temp)) {
            outcome <- temp
        }
        if (is.na(temp)) {
            count <- 2
            input <- input[2:3]
        }
    }
    if (count == 2) {
        temp <- threeGram[threeGram$V1 == input[1] & 
            threeGram$V2 == input[2], 3]
        temp <- as.character(temp[1])
        if (!is.na(temp)) {
            outcome <- temp
        }
        if (is.na(temp)) {
            count <- 1
            input <- input[2]
        }
    }
    if (count == 1) {
        temp <- twoGram[twoGram$V1 == input[1], 2]
        temp <- as.character(temp[1])
        if (!is.na(temp)) {
            outcome <- temp
        }
        if (is.na(temp)) {
            outcome <- "the"
        }
    } 
    return(outcome)
}

