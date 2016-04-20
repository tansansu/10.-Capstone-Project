# START : 2016.04.15

# Loading Packages I need ####
library(tm)
library(stringr)
library(dplyr)
library(ggplot2)
library(SnowballC)
library(RWeka)
library(tidyr)

# 6. Evaluation of the model ####
## train set: I used the sample of the data frame that used making prediction model
index_train <- sample(1:nrow(grams_app), 1000)
trainset <- grams_app[index_train, ]
trainset$result <- sapply(trainset$Lookup, pred_word)
### accuracy => 0.506
sum(trainset$Recommend == trainset$result) / nrow(trainset)

## test set: I used the resampled data from raw(!) sample dataset 
# that is 20,000 sampled from each raw text sets(blogs, news, twitter)
sam_testset <- sample(c(sam_blogs, sam_news, sam_twitter), 1000)
corpus_testset <- ExtWords(sam_testset)
save(file = "data/corpus_testset.RData", corpus_testset)

### Creating N-gram for testset
#### Creating N-grams
test_gram_2 <- ngramer(corpus_testset, 2)
test_gram_3 <- ngramer(corpus_testset, 3)
test_gram_4 <- ngramer(corpus_testset, 4)
## Seperating a cloumn for comparison between the n-grams and inputted words
temp_2 <- sapply(str_split(test_gram_2[, 1], pattern = " "), "[") %>% 
    t() %>% 
    as.data.frame() %>% 
    cbind(Freq = test_gram_2[, 2]) %>% 
    # I set up the filter to get only bigram appeared twice in the courpus for the accuracy of the recommendation
    mutate(Ngram = 2)
names(temp_2)[1:2] <- c("Lookup", "Recommend")

temp_3 <- sapply(str_split(test_gram_3[, 1], pattern = " "), "[") %>% 
    t() %>% 
    as.data.frame() %>% 
    unite(lookup, V1, V2, sep = " ") %>% 
    cbind(Freq = test_gram_3[, 2]) %>% 
    mutate(Ngram = 3)
names(temp_3)[1:2] <- c("Lookup", "Recommend")

temp_4 <- sapply(str_split(test_gram_4[, 1], pattern = " "), "[") %>% 
    t() %>% 
    as.data.frame() %>% 
    unite(lookup, V1, V2, V3, sep = " ") %>% 
    cbind(Freq = test_gram_4[, 2]) %>% 
    mutate(Ngram = 4)
names(temp_4)[1:2] <- c("Lookup", "Recommend")

test_grams <- rbind(temp_2, temp_3, temp_4) %>% arrange(desc(Ngram, Freq))
test_grams$Lookup <- as.character(test_grams$Lookup)
test_grams$Recommend <- as.character(test_grams$Recommend)
test_grams$Recommend[test_grams$Recommend == "i"] <- "I"
rm(temp_4, temp_3, temp_2)

## Verifying the accuracy of testset
index_test <- sample(1:nrow(test_grams), 1000)
testset <- test_grams[index_test, ]
testset$result <- sapply(testset$Lookup, pred_word)
### accurate => 0.215
sum(testset$Recommend == testset$result) / nrow(testset)




