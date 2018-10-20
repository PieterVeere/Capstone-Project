##Attempt to make a machine learning algorithm.
library(stringr)
library(RWeka)

##creating machine learning dataframe
g4token <- NGramTokenizer(dataSmallPreProc, Weka_control(min = 4L, max = 4L))
g4token <- str_split_fixed(g4token, pattern = " ", n = Inf)

tempw123 <- NULL 
tempw123df <- matrix(nrow = nrow(g4token), ncol = 1)
tempw23 <- NULL 
tempw23df <- matrix(nrow = nrow(g4token), ncol = 1)
tempw13 <- NULL 
tempw13df <- matrix(nrow = nrow(g4token), ncol = 1)
###creates properly sized temporary matrixes

pb <- txtProgressBar(min = 0, max = nrow(g4token), style = 3)
###Creates a progress bar
for (i in 1:nrow(g4token)) {
    tempw123 <- paste(g4token[i, 1:3], collapse = " ")
    tempw123df[i,] <- tempw123
    
    tempw23 <- paste(g4token[i, c(2, 3)], collapse = " ")
    tempw23df[i,] <- tempw23
    
    tempw13 <- paste(g4token[i, c(1,3)], collapse = " ")
    tempw13df[i,] <- tempw13
    setTxtProgressBar(pb, i)
}
close(pb)

g4token <- cbind(tempw123df, tempw23df, tempw13df, g4token)
g4token <- as.data.frame(g4token)
g4token <- data.frame(lapply(g4token, as.character), stringsAsFactors=FALSE)
names(g4token) <- c('w123', 'w23', 'w13', 'w1', 'w2', 'w3', 'w4')

##Splitting train, validating and testing datasets
library(caret)
inTraing4 <- createDataPartition(g4token$w4, p = .9)

datag4Split <- g4token[inTraing4$Resample1,]
data4gTest <- g4token[-inTraing4$Resample1,]

inValidateg4 <- createDataPartition(datag4Split$w4, p = .05)

datag4Train <- datag4Split[-inValidateg4$Resample1,]
datag4Validate <- datag4Split[inValidateg4$Resample1,]

###This model doesn't work because the model is 700+gb big.
LogiMod <- Logistic(w4 ~ w123, data = dataTrain, na.action = na.omit)
glmMod <- glm(w4 ~ w123 + w13 + w23 + w1 + w2 + w3, data = dataTrain)

glmMod <- glm(w4 ~ w123, data = datag4Train)
randomForestMod <- train(w4 ~ w123, data = datag4Train, method = "rf")

test <- dataValidate[, c("w123", "w4")]