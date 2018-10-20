library(here)
library(tm)

##Code in a the downloading of the files

set.seed(2018091413)
lines <- sample(77259, 40000)
###77259 because it's the amount of lines in the news file

#Opening files and sampling lines
con <- file(here("data/en_US", "en_US.twitter.txt"), "r")
twitDataSmall <- readLines(con, 1e5)
twitDataSmall <- twitDataSmall[lines]
close(con)

con <- file(here("data/en_US", "en_US.blogs.txt"), "r")
blogDataSmall <- readLines(con, 1e5)
blogDataSmall <- blogDataSmall[lines]
close(con)

con <- file(here("data/en_US", "en_US.news.txt"), "r")
newsDataSmall <- readLines(con)
newsDataSmall <- newsDataSmall[lines]
close(con)

#Making new dataset
dataSmall <- c(twitDataSmall, blogDataSmall, newsDataSmall)

###source bad words: "https://www.freewebheaders.com/download/files/base-list-of-bad-words_text-file_2018_07_30.zip"
badWords <- readLines(here("data/en_US", "bad-words.txt"))

##Getting rid of offensive words
badWords <- paste("\\b", badWords, "\\b", sep="", collapse = "|")
badLines <- grep(badWords, dataSmall)

if (class(badLines) == "integer") {
    dataSmall <- dataSmall[-badLines]
}

##pre-processecing data
ascCode <- 128:255 ###ASCII for special characters
ascCode <- as.raw(ascCode)
specialChar <- sapply(ascCode, rawToChar)
specialChar <- paste(specialChar, sep = "", collapse= "|")

dataSmallPreProc <- tolower(dataSmall)
dataSmallPreProc <- gsub(pattern = specialChar, 
                         replacement = "", x = dataSmallPreProc)
dataSmallPreProc <- gsub(pattern = "[[:digit:]]|[[:punct:]]|", 
                         replacement = "", x = dataSmallPreProc)
dataSmallPreProc <- stripWhitespace(dataSmallPreProc)

saveRDS(specialChar, here("serverData", "specialChar.rds"))

#Scraping all idioms off wikipedia
library(rvest)
library(httr)
library(dplyr)
library(stringr)

##first making a list of all letters.
frame <- sapply(letters, FUN=function(x) paste(x, letters, sep=""))

aazz <- NULL
temp <- NULL
for (i in 1:26) {
    for (x in 1:26){
        temp[x] <- frame[x,i]
    }
    aazz <- c(aazz, temp)
}

sample <- NA
sample2 <- NA
sample3 <- NA
sample4 <- NA
proverbAll <- NULL
pb <- txtProgressBar(min = 0, max = 676, style = 3)
for (i in 1:676){
    proverb <- NULL
    proverb2 <- NULL
    proverb3 <- NULL
    proverb4 <- NULL
    
    url <- paste("https://en.wiktionary.org/w/index.php?title=Category:English_idioms&from=", 
        aazz[i], sep = '')
    
    sample <- url %>%
        read_html() %>%
        html_node(xpath = '//*[@id="mw-pages"]/div/div/div')
    sample2 <- url %>%
        read_html() %>%
        html_node(xpath = '//*[@id="mw-pages"]/div/div/div[2]')
    if(!is.na(sample2)){
        sample3 <- url %>%
            read_html() %>%
            html_node(xpath = '//*[@id="mw-pages"]/div/div/div[3]')
        sample4 <- url %>%
            read_html() %>%
            html_node(xpath = '//*[@id="mw-pages"]/div/div/div[4]')
    }
    
    temp <- xml_child(sample, 2)
    tempLength <- xml_length(temp)
    for (x in 1:tempLength) {
        rawText <- xml_child(temp, x)
        proverb[x] <- str_extract(pattern = "(?<=title=\").*(?=\")", string = as.character(rawText))
    }
    
    if (!is.na(sample2)){
        temp <- xml_child(sample2, 2)
        tempLength <- xml_length(temp)
        for (x in 1:tempLength) {
            rawText <- xml_child(temp, x)
            proverb2[x] <- str_extract(pattern = "(?<=title=\").*(?=\")", string = as.character(rawText))
        }
    }
    
    if (!is.na(sample3)){
        temp <- xml_child(sample3, 2)
        tempLength <- xml_length(temp)
        for (x in 1:tempLength) {
            rawText <- xml_child(temp, x)
            proverb3[x] <- str_extract(pattern = "(?<=title=\").*(?=\")", string = as.character(rawText))
        }
    }
    
    if (!is.na(sample4)){
        temp <- xml_child(sample4, 2)
        tempLength <- xml_length(temp)
        for (x in 1:tempLength) {
            rawText <- xml_child(temp, x)
            proverb4[x] <- str_extract(pattern = "(?<=title=\").*(?=\")", string = as.character(rawText))
        }
    }
    proverbAll <- c(proverbAll, proverb, proverb2, proverb3, proverb4)
    setTxtProgressBar(pb, i)
}
close(pb)

saveRDS(proverbAll, here("serverData", "rawIdiom.rds"))

proverbPreProc <- tolower(proverbAll)
proverbPreProc <- gsub(pattern = specialChar, 
                         replacement = "", x = proverbPreProc)
proverbPreProc <- gsub(pattern = "[[:digit:]]|[[:punct:]]|", 
                         replacement = "", x = proverbPreProc)
proverbPreProc <- stripWhitespace(proverbPreProc)
proverbPreProc <- unique(proverbPreProc)

for (i in 1:length(proverbPreProc)) {
    if(wordCount(proverbPreProc[i]) > 6) {
        proverbPreProc[i] <- word(proverbPreProc[i], start = -6L, end = -1L)
    }
}
proverbPreProc <- unique(proverbPreProc)

saveRDS(proverbPreProc, here("serverData", "procIdiom.rds"))

